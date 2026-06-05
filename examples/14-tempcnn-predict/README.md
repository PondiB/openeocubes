# Example 14: TempCNN classification

End-to-end **TempCNN** workflow on the local openEO backend:

1. Load Sentinel-2 (`sentinel-2-l2a` on Planetary Computer) and gap-fill time series
2. Extract training samples from `examples/train_data/land_train_data.geojson`
3. Train with `mlm_class_tempcnn` + `ml_fit` (returns a `.pt` model path)
4. Predict on a (by default smaller) AOI with `ml_predict`
5. Export GeoTIFF via async job and download to `results/`

Note: `save_ml_model()` is omitted here because ONNX export requires Python `torch`. The trained `.pt` from `ml_fit()` is enough for `ml_predict()`.

## Prerequisites

- Local backend running (`Rscript startLocal.R` from repo root)
- R packages: `openeo`, `torch` (backend worker), `terra` (optional summary)

## Run

```bash
cd examples/14-tempcnn-predict
OPENEO_WAIT_FOR_FINISH=true Rscript run_tempcnn_job.R
```

## Useful environment variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `OPENEO_BACKEND_URL` | `http://localhost:8000` | Backend URL |
| `OPENEO_COLLECTION_ID` | `sentinel-2-l2a` | STAC collection (use `sentinel-2-l2a` for Germany training AOI on PC) |
| `TEMP_CNN_EPOCHS` | `25` | Training epochs (use `5` for a quick smoke test) |
| `TEMP_CNN_BATCH_SIZE` | `32` | Mini-batch size |
| `OPENEO_PREDICT_BBOX` | smaller subset of example 04 AOI | `west,south,east,north,crs` |
| `OPENEO_DOWNLOAD_DIR` | `./results` | Where to save job assets |

Quick smoke test:

```bash
TEMP_CNN_EPOCHS=5 OPENEO_WAIT_FOR_FINISH=true Rscript run_tempcnn_job.R
```

Outputs land in `results/` (e.g. `prediction.tif`).
