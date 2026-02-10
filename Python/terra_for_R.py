# generic_terra.py
# -----------------------------------------------------------
# Generische TerraTorch-Pipeline, gesteuert durch YAML.
#
# Unterstützte Layouts:
# 1) Multi-temporales Crop-Beispiel (dein Dropbox-Datensatz):
#    dataset:
#      train_chips_dir: "training_chips"
#      val_chips_dir:   "validation_chips"
#      image_suffix: "_merged.tif"
#      mask_suffix:  ".mask.tif"
#
# 2) HLS-Layout (z.B. Burn-Scars):
#    dataset:
#      hls_dir:   "." oder "S2Hand"
#      mask_dir:  "LabelHand"
#      image_suffix: "_S2Hand.tif"
#      mask_suffix:  "_LabelHand.tif"
#
# Zweck dieses Skripts in deinem Backend:
#   -> NUR CKPT erzeugen (oder externes CKPT herunterladen) und Pfad bereitstellen.
#   -> KEINE Prediction, keine PNG/TIFF-Ausgabe.
# -----------------------------------------------------------

import os
import sys
from pathlib import Path
from urllib.parse import urlparse, parse_qs, urlencode, urlunparse
import zipfile

def _ensure_import(mod_name, pip_name=None):
    try:
        return __import__(mod_name)
    except ImportError:
        pkg = pip_name or mod_name
        print(f"[Fehlt] Paket '{pkg}'. Installiere z.B.:  pip install {pkg}")
        raise

# Pflicht-Imports
yaml = _ensure_import("yaml", "pyyaml")
albumentations = _ensure_import("albumentations")
torch = _ensure_import("torch")
np = _ensure_import("numpy")
try:
    import lightning.pytorch as pl
except Exception:
    print("[Fehlt] Paket 'lightning'. Installiere z.B.:  pip install lightning")
    raise

try:
    import requests
except Exception:
    requests = None
    print("[Hinweis] requests nicht installiert – für URL/Dropbox-Download bitte `pip install requests`.")

from torch.utils.data import DataLoader, Dataset
from terratorch.tasks import SemanticSegmentationTask

# ------------------ Utilities ------------------
def _read_ids(path: Path):
    return [l.strip() for l in Path(path).read_text().splitlines() if l.strip()]

def prepare_dataset(data_pfad: str | None, dst_root: Path) -> Path:
    """
    Bereitet das Dataset-Verzeichnis vor:

    - Wenn data_pfad = None:
        -> nutze vorhandenes dst_root (kein Download).
    - Wenn data_pfad ein existierender lokaler Pfad ist:
        -> diesen Pfad als dataset_root verwenden.
    - Wenn data_pfad eine Dropbox- oder andere HTTP/HTTPS-URL ist:
        -> ZIP herunterladen, nach dst_root entpacken.

    Gibt den effektiven dataset_root zurück (kann auch ein Unterordner sein).
    """
    dst_root.mkdir(parents=True, exist_ok=True)

    if data_pfad is None:
        print(f"[INFO] Kein data_pfad angegeben – verwende vorhandenes Dataset-Verzeichnis: {dst_root}")
        return dst_root

    # 1) Lokaler Pfad?
    p = Path(data_pfad)
    if p.exists():
        print(f"[INFO] Nutze lokales Dataset: {p}")
        return p

    # 2) URL-Fall
    parsed = urlparse(data_pfad)
    if parsed.scheme not in ("http", "https"):
        raise RuntimeError(f"paths.data_pfad ist weder existierender Pfad noch HTTP/HTTPS-URL: {data_pfad}")

    if requests is None:
        raise RuntimeError("requests ist nicht installiert – bitte `pip install requests` für URL-Download.")

    # Wenn schon etwas drin ist (nicht nur .DS_Store), nutzen wir das und laden nicht erneut
    non_hidden = [x for x in dst_root.iterdir() if x.name != ".DS_Store"]
    if non_hidden:
        print(f"[INFO] Dataset-Verzeichnis {dst_root} ist nicht leer – verwende vorhandene Daten.")
        return dst_root

    # Dropbox-URL? -> dl=1 setzen, damit direkt ZIP kommt
    download_url = data_pfad
    if "dropbox.com" in parsed.netloc:
        qs = parse_qs(parsed.query)
        qs["dl"] = ["1"]   # erzwinge direkten Download
        new_query = urlencode(qs, doseq=True)
        parsed = parsed._replace(query=new_query)
        download_url = urlunparse(parsed)
        print(f"[INFO] Dropbox-Download-URL: {download_url}")

    zip_path = dst_root / "dataset.zip"
    print(f"[INFO] Lade ZIP von {download_url} nach {zip_path} …")
    with requests.get(download_url, stream=True) as r:
        r.raise_for_status()
        with open(zip_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=8192):
                if chunk:
                    f.write(chunk)
    print("[OK] ZIP-Download fertig, entpacke …")

    with zipfile.ZipFile(zip_path, "r") as zf:
        zf.extractall(dst_root)

    zip_path.unlink(missing_ok=True)

    # Einige einfache Heuristiken für den Root-Ordner
    hls_candidate = dst_root / "hls"
    mask_candidate = dst_root / "masks"
    if hls_candidate.is_dir() and mask_candidate.is_dir():
        print(f"[OK] hls/ und masks/ direkt unter {dst_root} gefunden.")
        return dst_root

    children = [x for x in dst_root.iterdir() if x.is_dir()]
    if len(children) == 1:
        sub = children[0]
        print(f"[INFO] Verwende Unterordner als Dataset-Root: {sub}")
        return sub

    print(f"[WARN] Konnte Dataset-Root nicht eindeutig erkennen, verwende {dst_root}")
    return dst_root

def _load_hls_merged(hls_path: Path, num_frames: int, num_bands: int):
    """Beliebiger Mehrkanal-TIFF -> (C=num_bands, T=num_frames, H, W) float32."""
    try:
        import tifffile as tiff
        img = tiff.imread(str(hls_path))
        if img.ndim == 2:
            img = img[None, ...]
        elif img.shape[0] not in (num_frames * num_bands, num_bands):
            img = np.transpose(img, (2, 0, 1))
    except Exception:
        import imageio.v3 as iio
        img = iio.imread(str(hls_path))
        if img.ndim == 2:
            img = img[None, ...]
        elif img.shape[0] not in (num_frames * num_bands, num_bands):
            img = np.transpose(img, (2, 0, 1))

    if img.shape[0] == num_frames * num_bands:
        img = img.astype(np.float32)
        img = img.reshape(num_frames, num_bands, img.shape[1], img.shape[2])  # (T,C,H,W)
        img = np.transpose(img, (1, 0, 2, 3))  # -> (C,T,H,W)
    elif img.shape[0] == num_bands:
        img = img.astype(np.float32)
        # auch bei num_frames=1 ok
        img = np.stack([img] * num_frames, axis=1)  # (C,T,H,W)
    else:
        raise RuntimeError(f"Unerwartete Kanalzahl: {hls_path} -> {img.shape}")
    return img.astype(np.float32)

def _load_mask(mask_path: Path):
    """Masken-TIFF -> (H,W) int64"""
    try:
        import tifffile as tiff
        m = tiff.imread(str(mask_path))
    except Exception:
        import imageio.v3 as iio
        m = iio.imread(str(mask_path))
    if m.ndim > 2:
        m = m.squeeze()
    return m.astype(np.int64)

def _collect_pairs(
    split_file: Path,
    img_dir: Path,
    mask_dir: Path | None,
    img_suffix: str,
    mask_suffix: str | None,
):
    """
    Finde (Bild, Maske)-Paare für kurzes Finetuning.

    IDs kommen aus split_file, Dateinamen werden über Suffixe gebaut:
      Bild:  img_dir / (ID + img_suffix)
      Maske: mask_dir / (ID + mask_suffix)   (optional)
    """
    if not split_file.is_file():
        return [], [], []

    if mask_dir is None:
        mask_dir = img_dir

    ids = _read_ids(split_file)
    have, missing_img, missing_mask = [], [], []

    for cid in ids:
        hp = img_dir / f"{cid}{img_suffix}"
        mp = mask_dir / f"{cid}{mask_suffix}" if mask_suffix is not None else None

        img_ok = hp.exists()
        mask_ok = (mp is None) or mp.exists()

        if img_ok and mask_ok:
            have.append(cid)
        else:
            if not img_ok:
                missing_img.append(cid)
            if mp is not None and not mask_ok:
                missing_mask.append(cid)

    return have, missing_img, missing_mask

def resolve_split_file(dataset_root: Path, dcfg: dict, kind: str, override_name: str | None = None) -> Path:
    """
    Sucht eine Split-Datei für 'train', 'val' oder 'pred'.

    Logik:
    - Wenn override_name gesetzt: danach suchen.
    - Sonst:
      * Wenn dcfg["<kind>_split"] existiert: nur diesen Namen verwenden.
      * Sonst:
        - Default-Listen für train/val oder dcfg["<kind>_split_names"].
    - Zuerst in splits_dir (dcfg.splits_dir, default 'splits'), dann im dataset_root.
    """
    splits_dir_name = dcfg.get("splits_dir", "splits")
    splits_dir = dataset_root / splits_dir_name

    search_dirs: list[Path] = []
    if splits_dir.is_dir():
        search_dirs.append(splits_dir)
    search_dirs.append(dataset_root)

    if override_name:
        candidates = [override_name]
    else:
        exact_key = f"{kind}_split"
        if exact_key in dcfg:
            candidates = [dcfg[exact_key]]
        else:
            if kind == "train":
                candidates = dcfg.get(
                    "train_split_names",
                    ["train.txt", "train_data.txt", "training_data.txt", "train_split.txt", "training_split.txt"],
                )
            elif kind == "val":
                candidates = dcfg.get(
                    "val_split_names",
                    ["val.txt", "val_data.txt", "validation_data.txt", "val_split.txt", "validation_split.txt"],
                )
            else:  # "pred"
                candidates = dcfg.get(
                    "pred_split_names",
                    dcfg.get(
                        "val_split_names",
                        ["val.txt", "val_data.txt", "validation_data.txt", "val_split.txt", "validation_split.txt"],
                    ),
                )

    for d in search_dirs:
        for name in candidates:
            p = d / name
            if p.is_file():
                print(f"[OK] {kind}_split gefunden: {p}")
                return p

    raise RuntimeError(f"Konnte keinen {kind}_split in {search_dirs} für Kandidaten {candidates} finden.")

# ------------------ Dataset (Training) ------------------
class TrainDataset(Dataset):
    def __init__(self, ids, hls_dir: Path, mask_dir: Path | None,
                 num_frames: int, band_names,
                 img_suffix: str, mask_suffix: str | None):
        self.ids = list(ids)
        self.hls_dir = hls_dir
        self.mask_dir = mask_dir if mask_dir is not None else hls_dir
        self.num_frames = num_frames
        self.num_bands = len(band_names)
        self.img_suffix = img_suffix
        self.mask_suffix = mask_suffix

    def __len__(self):
        return len(self.ids)

    def __getitem__(self, idx):
        cid = self.ids[idx]
        hp = self.hls_dir / f"{cid}{self.img_suffix}"
        mp = self.mask_dir / f"{cid}{self.mask_suffix}" if self.mask_suffix is not None else None
        img = _load_hls_merged(hp, self.num_frames, self.num_bands)
        if mp is None:
            raise RuntimeError("TrainDataset erwartet eine Maske, mask_suffix ist aber None.")
        msk = _load_mask(mp)
        return {
            "image": torch.from_numpy(img).float(),
            "mask":  torch.from_numpy(msk).long(),
        }

# ------------------ Modell ------------------
def build_model(cfg: dict) -> SemanticSegmentationTask:
    dcfg = cfg["dataset"]
    mcfg = cfg["model"]
    tcfg = cfg["train"]

    num_frames = dcfg["num_frames"]
    bands = dcfg["bands"]

    backbone_name = mcfg["backbone"]
    backbone_pretrained = mcfg.get("backbone_pretrained", True)
    backbone_coords_encoding = mcfg.get("backbone_coords_encoding", [])
    neck_indices = mcfg["neck_indices"]
    decoder_name = mcfg.get("decoder", "UNetDecoder")
    decoder_channels = mcfg["decoder_channels"]
    head_dropout = mcfg.get("head_dropout", 0.0)
    num_classes = mcfg["num_classes"]
    ignore_index = mcfg["ignore_index"]
    loss_name = mcfg.get("loss", "ce")
    optimizer_name = mcfg.get("optimizer", "AdamW")
    freeze_backbone = mcfg.get("freeze_backbone", True)
    freeze_decoder = mcfg.get("freeze_decoder", False)

    model = SemanticSegmentationTask(
        model_factory=mcfg.get("factory", "EncoderDecoderFactory"),
        model_args={
            "backbone": backbone_name,
            "backbone_pretrained": backbone_pretrained,
            "backbone_num_frames": num_frames,
            "backbone_bands": bands,
            "backbone_coords_encoding": backbone_coords_encoding,
            "necks": [
                {"name": "SelectIndices", "indices": neck_indices},
                {"name": "ReshapeTokensToImage", "effective_time_dim": num_frames},
                {"name": "LearnedInterpolateToPyramidal"},
            ],
            "decoder": decoder_name,
            "decoder_channels": decoder_channels,
            "head_dropout": head_dropout,
            "num_classes": num_classes,
        },
        loss=loss_name,
        lr=tcfg["lr"],
        optimizer=optimizer_name,
        ignore_index=ignore_index,
        freeze_backbone=freeze_backbone,
        freeze_decoder=freeze_decoder,
        plot_on_val=False,
    )
    return model

# ------------------ CKPT-Resolver & Training ------------------
def find_existing_ckpt(ckpt_dir: Path, cfg: dict) -> Path | None:
    inf_cfg = cfg["inference"]
    target_name = inf_cfg.get("ckpt_filename", "best.ckpt")
    candidate = ckpt_dir / target_name
    if candidate.is_file():
        print(f"[OK] Verwende vorhandenen CKPT: {candidate}")
        return candidate
    # Fallbacks
    bests = sorted(ckpt_dir.glob("best*.ckpt"), key=lambda p: p.stat().st_mtime, reverse=True)
    if bests:
        print(f"[OK] Verwende best-CKPT: {bests[0]}")
        return bests[0]
    last = ckpt_dir / "last.ckpt"
    if last.is_file():
        print(f"[OK] Verwende last.ckpt: {last}")
        return last
    any_ckpt = sorted(ckpt_dir.glob("*.ckpt"), key=lambda p: p.stat().st_mtime, reverse=True)
    if any_ckpt:
        print(f"[OK] Verwende neuestes CKPT: {any_ckpt[0]}")
        return any_ckpt[0]
    return None

def resolve_external_ckpt_if_any(paths_cfg: dict, ckpt_dir: Path, cfg: dict) -> Path | None:
    """
    Wenn paths.ckpt_pfad gesetzt ist:
    - Lokale Datei: direkt verwenden.
    - URL (z.B. Dropbox): nach ckpt_dir laden (falls nicht schon da).
    """
    ckpt_spec = paths_cfg.get("ckpt_pfad")
    if not ckpt_spec:
        return None

    p = Path(ckpt_spec)
    if p.is_file():
        print(f"[INFO] Nutze lokales externes CKPT: {p}")
        return p

    # URL-Fall
    parsed = urlparse(ckpt_spec)
    if parsed.scheme not in ("http", "https"):
        raise RuntimeError(f"ckpt_pfad ist weder existierende Datei noch HTTP/HTTPS-URL: {ckpt_spec}")

    existing = list(ckpt_dir.glob("*.ckpt"))
    if existing:
        ck = find_existing_ckpt(ckpt_dir, cfg)
        print(f"[INFO] Externes CKPT bereits vorhanden: {ck}")
        return ck

    if requests is None:
        raise RuntimeError("requests ist nicht installiert – kann ckpt_pfad-URL nicht herunterladen.")

    download_url = ckpt_spec
    if "dropbox.com" in parsed.netloc:
        qs = parse_qs(parsed.query)
        qs["dl"] = ["1"]
        new_query = urlencode(qs, doseq=True)
        parsed = parsed._replace(query=new_query)
        download_url = urlunparse(parsed)
        print(f"[INFO] Dropbox-CKPT-URL: {download_url}")

    out_path = ckpt_dir / "external.ckpt"
    print(f"[INFO] Lade CKPT von {download_url} nach {out_path} …")
    with requests.get(download_url, stream=True) as r:
        r.raise_for_status()
        with open(out_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=8192):
                if chunk:
                    f.write(chunk)
    print(f"[OK] Externes CKPT gespeichert: {out_path}")
    return out_path

def train_if_needed_and_get_ckpt(cfg: dict, dataset_root: Path, work_root: Path) -> Path:
    dcfg = cfg["dataset"]
    tcfg = cfg["train"]
    inf_cfg = cfg["inference"]

    ckpt_dir = work_root / "output" / "checkpoints"
    ckpt_dir.mkdir(parents=True, exist_ok=True)

    # 1) Existierende CKPTs bevorzugen (falls gewünscht)
    if inf_cfg.get("prefer_existing_ckpt", True):
        local_ck = find_existing_ckpt(ckpt_dir, cfg)
        if local_ck is not None:
            return local_ck

    # 2) Kurzes Training
    print("⚠️ Kein vorhandener CKPT – starte kurzes Head-Finetuning …")

    img_suffix = dcfg.get("image_suffix", "_merged.tif")
    mask_suffix = dcfg.get("mask_suffix", ".mask.tif")

    # Splits finden
    train_split = resolve_split_file(dataset_root, dcfg, "train")
    val_split   = resolve_split_file(dataset_root, dcfg, "val")

    num_frames = dcfg["num_frames"]
    band_names = dcfg["bands"]

    # Layout-Variante bestimmen: Chips-Dirs oder hls/masks
    if "train_chips_dir" in dcfg or "val_chips_dir" in dcfg or "chips_dir" in dcfg:
        chips_train_name = dcfg.get("train_chips_dir", dcfg.get("chips_dir"))
        chips_val_name   = dcfg.get("val_chips_dir",   dcfg.get("chips_dir", chips_train_name))

        if chips_train_name is None:
            raise RuntimeError("train_chips_dir oder chips_dir muss im dataset-Block gesetzt sein.")

        train_hls_dir = dataset_root / chips_train_name
        train_mask_dir = train_hls_dir
        val_hls_dir = dataset_root / chips_val_name
        val_mask_dir = val_hls_dir
    else:
        hls_dir_name = dcfg.get("hls_dir")
        if hls_dir_name is None:
            raise RuntimeError("Weder train_chips_dir/val_chips_dir noch hls_dir im dataset-Block gesetzt.")
        train_hls_dir = dataset_root / hls_dir_name
        train_mask_dir = dataset_root / dcfg.get("mask_dir", hls_dir_name)
        val_hls_dir = train_hls_dir
        val_mask_dir = train_mask_dir

    train_have, _, _ = _collect_pairs(train_split, train_hls_dir, train_mask_dir, img_suffix, mask_suffix)
    val_have,   _, _ = _collect_pairs(val_split,   val_hls_dir,   val_mask_dir, img_suffix, mask_suffix)

    if not train_have:
        raise RuntimeError("Weder train_split noch val_split enthalten verwendbare Paare (train_have leer).")
    if not val_have:
        n = len(train_have)
        cut = max(1, int(0.8 * n))
        print(f"[WARN] val_have leer – erstelle val aus train: train={cut}, val={n-cut}")
        valid_ids = train_have[cut:] if cut < n else train_have[:1]
        train_ids = train_have[:cut]
    else:
        train_ids = train_have
        valid_ids = val_have
        print(f"[OK] train_split Paare: {len(train_ids)}; val_split Paare: {len(valid_ids)}")

    train_ds = TrainDataset(train_ids, train_hls_dir, train_mask_dir,
                            num_frames, band_names, img_suffix, mask_suffix)
    val_ds   = TrainDataset(valid_ids,   val_hls_dir,   val_mask_dir,
                            num_frames, band_names, img_suffix, mask_suffix)

    train_dl = DataLoader(
        train_ds,
        batch_size=tcfg["batch_size"],
        num_workers=tcfg["num_workers"],
        shuffle=True,
        persistent_workers=tcfg["num_workers"] > 0,
    )
    val_dl   = DataLoader(
        val_ds,
        batch_size=tcfg["batch_size"],
        num_workers=tcfg["num_workers"],
        shuffle=False,
        persistent_workers=tcfg["num_workers"] > 0,
    )

    model = build_model(cfg)

    ckpt_cb = pl.callbacks.ModelCheckpoint(
        dirpath=str(ckpt_dir),
        monitor=tcfg["metric_monitor"],
        mode=tcfg["metric_mode"],
        filename="best",
        save_top_k=1,
    )
    save_last_cb = pl.callbacks.ModelCheckpoint(
        dirpath=str(ckpt_dir),
        filename="last",
        save_last=True,
    )
    trainer = pl.Trainer(
        accelerator="auto",
        devices=1,
        max_epochs=tcfg["max_epochs"],
        precision="32-true",
        enable_checkpointing=True,
        callbacks=[ckpt_cb, save_last_cb, pl.callbacks.RichProgressBar()],
        default_root_dir=str(work_root / "output"),
        log_every_n_steps=5,
    )
    trainer.fit(model, train_dataloaders=train_dl, val_dataloaders=val_dl)
    best = ckpt_cb.best_model_path
    if not best:
        cands = sorted(ckpt_dir.glob("*.ckpt"), key=lambda p: p.stat().st_mtime, reverse=True)
        if not cands:
            raise RuntimeError("Konnte keinen Checkpoint erzeugen.")
        best = str(cands[0])
    print("✅ Head-CKPT erzeugt:", best)
    return Path(best)

# ------------------ Main ------------------
def main():
    import argparse
    print("=== TerraTorch generische YAML-Pipeline ===")
    # Default ist hier egal, weil dein R-Backend --config IMMER explizit setzt.
    DEFAULT_CONFIG_PATH = Path("config.yaml")

    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--config",
        default=str(DEFAULT_CONFIG_PATH),
        help=f"Pfad zur YAML-Konfiguration (Standard: {DEFAULT_CONFIG_PATH})",
    )
    ap.add_argument(
        "--work-dir",
        default="./work",
        help="Arbeitsverzeichnis (Standard: ./work)",
    )
    args = ap.parse_args()

    cfg_path = Path(args.config)
    if not cfg_path.is_file():
        raise FileNotFoundError(f"Config-Datei nicht gefunden: {cfg_path}")

    cfg = yaml.safe_load(open(cfg_path))
    exp_name = cfg.get("experiment_name", "experiment")
    paths_cfg = cfg["paths"]

    work_root = Path(args.work_dir) / exp_name
    dataset_root = work_root / "dataset"
    out_dir = work_root / "output"
    ckpt_dir = out_dir / "checkpoints"
    out_dir.mkdir(parents=True, exist_ok=True)
    ckpt_dir.mkdir(parents=True, exist_ok=True)

    print("=== TerraTorch (YAML-gesteuert): CKPT erzeugen (falls nötig) ===")
    print(f"[Config]     {cfg_path}")
    print(f"[Experiment] {exp_name}")
    print(f"[Work Root]  {work_root}")
    print(f"[Dataset]    {dataset_root}")
    print(f"[Output]     {out_dir}")

    # 1) Daten holen (lokaler Pfad oder URL/Dropbox)
    data_url = paths_cfg.get("data_pfad")
    dataset_root = prepare_dataset(data_url, dataset_root)

    # 2) Externes CKPT (falls angegeben) oder Training
    ckpt_path = resolve_external_ckpt_if_any(paths_cfg, ckpt_dir, cfg)
    if ckpt_path is None:
        ckpt_path = train_if_needed_and_get_ckpt(cfg, dataset_root, work_root)

    # Fertig – nur CKPT-Pfad ausgeben
    print(f"CKPT_PATH={ckpt_path}")
    print("✅ Training/CKPT-Setup abgeschlossen.")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nAbgebrochen.")
        sys.exit(1)
