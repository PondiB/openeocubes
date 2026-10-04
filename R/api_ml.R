#' @include Session-Class.R
NULL

# ML capability endpoints of the proposed openEO L3-ML API profile:
#
#   GET /ml_runtimes             ML1-ML8: frameworks, versions and capabilities
#   GET /ml_models               ML10:    models stored on this backend (STAC MLM)
#   GET /ml_models/{model_id}    ML11:    one stored model as a full STAC MLM Item
#
# Runtimes are derived from the R packages installed in this deployment unless
# `SessionConfig()$ml_runtimes` overrides them. Models are the STAC MLM Items
# that `save_ml_model` writes to SHARED_TEMP_DIR; a model id is the Item id and
# can be passed to `load_ml_model`.

`%ml_or%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

ML_TRAINING_DATA_FORMAT_VECTOR_CUBE <- "vector-cube"

# Static description of the ML runtimes this backend implements. Keys follow
# the STAC MLM `mlm:framework` naming; `package` is the R package whose
# installed version becomes the runtime version. Artifact types match the
# `mlm:artifact_type` values written by `save_ml_model` and read by
# `load_ml_model` / `load_stac_ml`.
.ml_runtime_specs <- function() {
  list(
    caret = list(
      title = "caret (R)",
      description = "Classic machine learning through the R caret framework (Random Forest via randomForest, SVM via kernlab).",
      package = "caret",
      libraries = c("randomForest", "kernlab"),
      training = TRUE,
      inference = TRUE,
      artifact_types = list(load = list("R (RDS)", "onnx"), save = list("R (RDS)", "onnx")),
      workflow_types = list("feature"),
      processes = list("mlm_class_random_forest", "mlm_regr_random_forest", "mlm_class_svm", "mlm_regr_svm"),
      gpu = FALSE
    ),
    xgboost = list(
      title = "XGBoost (R)",
      description = "Gradient boosted trees trained directly with the R xgboost engine.",
      package = "xgboost",
      libraries = character(0),
      training = TRUE,
      inference = TRUE,
      artifact_types = list(load = list("R (RDS)", "onnx"), save = list("R (RDS)", "onnx")),
      workflow_types = list("feature"),
      processes = list("mlm_class_xgboost", "mlm_regr_xgboost"),
      gpu = FALSE
    ),
    torch = list(
      title = "torch (R)",
      description = "Deep learning through the R torch package (TempCNN, MLP, LightTAE, STGF).",
      package = "torch",
      libraries = character(0),
      training = TRUE,
      inference = TRUE,
      artifact_types = list(load = list("torch.jit.save", "onnx"), save = list("torch.jit.save", "onnx", "R (Raw RDS)")),
      workflow_types = list("feature", "time_series"),
      processes = list("mlm_class_tempcnn", "mlm_class_mlp", "mlm_class_lighttae", "mlm_class_stgf"),
      gpu = TRUE
    )
  )
}

.ml_package_version <- function(pkg) {
  tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
}

# CPU accelerator in the STAC MLM `mlm:accelerator` vocabulary.
.ml_cpu_accelerator <- function() {
  info <- Sys.info()
  machine <- tolower(info[["machine"]] %ml_or% "")
  if (identical(info[["sysname"]], "Darwin") && machine %in% c("arm64", "aarch64")) {
    return("macos-arm")
  }
  if (machine %in% c("x86_64", "amd64", "x86-64")) {
    return("amd64")
  }
  machine
}

.ml_accelerator_cache <- new.env(parent = emptyenv())

# CUDA probing loads libtorch, so the result is computed once per process.
.ml_cuda_available <- function() {
  if (is.null(.ml_accelerator_cache$cuda)) {
    .ml_accelerator_cache$cuda <- tryCatch(
      requireNamespace("torch", quietly = TRUE) &&
        isTRUE(torch::torch_is_installed()) &&
        isTRUE(torch::cuda_is_available()),
      error = function(e) FALSE
    )
  }
  .ml_accelerator_cache$cuda
}

.ml_runtime_version_info <- function(spec) {
  accelerators <- list(.ml_cpu_accelerator())
  if (isTRUE(spec$gpu) && .ml_cuda_available()) {
    accelerators <- c(accelerators, list("cuda"))
  }

  libraries <- list()
  for (lib in spec$libraries) {
    version <- .ml_package_version(lib)
    if (!is.na(version)) libraries[[lib]] <- list(version = version)
  }

  info <- list(
    training = isTRUE(spec$training),
    inference = isTRUE(spec$inference),
    artifact_types = spec$artifact_types,
    workflow_types = spec$workflow_types,
    training_data_formats = if (isTRUE(spec$training)) list(ML_TRAINING_DATA_FORMAT_VECTOR_CUBE) else list(),
    accelerators = accelerators,
    processes = spec$processes
  )
  if (length(libraries) > 0) info$libraries <- libraries
  info
}

#' Build the GET /ml_runtimes document from the installed R packages.
#' Runtimes whose package is not installed are omitted.
.ml_runtimes_default <- function() {
  specs <- .ml_runtime_specs()
  runtimes <- list()
  for (name in names(specs)) {
    spec <- specs[[name]]
    version <- .ml_package_version(spec$package)
    if (is.na(version)) next

    versions <- list()
    versions[[version]] <- .ml_runtime_version_info(spec)

    runtimes[[name]] <- list(
      title = spec$title,
      description = spec$description,
      language = "R",
      default = version,
      versions = versions
    )
  }
  runtimes
}

.ml_runtimes_document <- function() {
  config <- tryCatch(Session$getConfig(), error = function(e) NULL)
  if (!is.null(config$ml_runtimes)) {
    return(config$ml_runtimes)
  }
  .ml_runtimes_default()
}

# ---------------------------------------------------------------------------
# Stored model catalogue
# ---------------------------------------------------------------------------

.ml_models_dir <- function() {
  # Same default as save_ml_model, so both agree on where models live.
  Sys.getenv("SHARED_TEMP_DIR", tempdir())
}

.ml_is_stac_mlm_item <- function(x) {
  is.list(x) &&
    identical(x$type, "Feature") &&
    is.character(x$id) && length(x$id) == 1 && nzchar(x$id) &&
    is.list(x$properties) &&
    any(startsWith(names(x$properties) %ml_or% character(0), "mlm:"))
}

# Read every STAC MLM Item in the model directory, keyed and sorted by id.
# Files that are not STAC MLM Items (ONNX label maps, job outputs, ...) are
# skipped, as are files whose name does not match their Item id.
.ml_model_catalog <- function(dir = .ml_models_dir()) {
  if (!dir.exists(dir)) {
    return(list())
  }
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE, ignore.case = TRUE)
  catalog <- list()
  for (path in files) {
    item <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
    if (!.ml_is_stac_mlm_item(item)) next
    if (!identical(item$id, tools::file_path_sans_ext(basename(path)))) next
    attr(item, "path") <- normalizePath(path)
    catalog[[item$id]] <- item
  }
  if (length(catalog) == 0) {
    return(list())
  }
  catalog[order(names(catalog))]
}

.ml_download_base_url <- function() {
  base <- Sys.getenv("DOWNLOAD_BASE_URL", "")
  if (!nzchar(base)) {
    config <- tryCatch(Session$getConfig(), error = function(e) NULL)
    base <- paste0(config$base_url %ml_or% "http://localhost:8000", "/download/")
  }
  if (!endsWith(base, "/")) base <- paste0(base, "/")
  base
}

.ml_href_for_file <- function(path, dir) {
  local <- sub("^file://", "", path)
  if (!file.exists(local)) {
    return(path)
  }
  if (!identical(normalizePath(dirname(local)), normalizePath(dir))) {
    return(path)
  }
  paste0(.ml_download_base_url(), utils::URLencode(basename(local), reserved = TRUE))
}

# Model artifacts that save_ml_model writes next to the Item but does not list
# in it. ONNX names follow the exporters in save_ml_model.
.ml_sibling_artifacts <- function(model_id, dir) {
  stem <- file.path(dir, model_id)
  candidates <- list(
    onnx = c(
      paste0(stem, ".onnx"),
      paste0(stem, "_rf_teclassifier.onnx"),
      Sys.glob(paste0(stem, "_svm_ovo_*.onnx"))
    ),
    torchscript = paste0(stem, ".pt")
  )
  artifact_types <- list(onnx = "onnx", torchscript = "torch.jit.save")
  media_types <- list(onnx = "application/octet-stream; application=onnx", torchscript = "application/octet-stream; application=pytorch")

  out <- list()
  for (key in names(candidates)) {
    hit <- candidates[[key]][file.exists(candidates[[key]])]
    if (length(hit) == 0) next
    out[[key]] <- list(path = hit[[1]], artifact_type = artifact_types[[key]], type = media_types[[key]])
  }
  out
}

# Turn a stored Item into its public representation: valid STAC (null
# geometry instead of `{}`), download URLs instead of server paths, extra
# artifacts listed as assets, and openEO links.
.ml_model_public_item <- function(item, dir = .ml_models_dir()) {
  out <- item
  attr(out, "path") <- NULL

  if (!is.list(out$geometry) || length(out$geometry) == 0) {
    out["geometry"] <- list(NULL)
    out$bbox <- NULL
  }
  if (is.null(out$stac_version)) out$stac_version <- "1.0.0"

  assets <- out$assets
  if (!is.list(assets) || length(assets) == 0) assets <- list()
  for (key in names(assets)) {
    if (is.character(assets[[key]]$href)) {
      assets[[key]]$href <- .ml_href_for_file(assets[[key]]$href, dir)
    }
  }

  # Additional formats are listed with role "data" so that the Item keeps a
  # single "mlm:model" asset, which load_stac_ml requires by default.
  siblings <- .ml_sibling_artifacts(item$id, dir)
  for (key in names(siblings)) {
    if (!is.null(assets[[key]])) next
    sibling <- siblings[[key]]
    assets[[key]] <- list(
      href = .ml_href_for_file(sibling$path, dir),
      type = sibling$type,
      title = paste(item$id, key),
      "mlm:artifact_type" = sibling$artifact_type,
      roles = list("data")
    )
  }
  out$assets <- if (length(assets) > 0) assets else structure(list(), names = character(0))

  base_url <- tryCatch(Session$getConfig()$base_url, error = function(e) NULL) %ml_or% ""
  out$links <- c(
    Filter(function(l) !(l$rel %ml_or% "") %in% c("self", "root", "parent"), out$links %ml_or% list()),
    list(
      list(rel = "self", href = paste0(base_url, "/ml_models/", utils::URLencode(item$id, reserved = TRUE)), type = "application/json"),
      list(rel = "parent", href = paste0(base_url, "/ml_models"), type = "application/json"),
      list(rel = "root", href = paste0(base_url, "/"), type = "application/json")
    )
  )
  out
}

# ---------------------------------------------------------------------------
# Model resolution for load_ml_model (ML9, ML11)
# ---------------------------------------------------------------------------

# Map an `mlm:framework` value to a runtime of GET /ml_runtimes. Accepts the
# runtime keys themselves and the "R (<package>)" values written by
# save_ml_model.
.ml_runtime_for_framework <- function(framework, runtimes = .ml_runtimes_document()) {
  if (!is.character(framework) || length(framework) != 1 || !nzchar(framework)) {
    return(NA_character_)
  }
  fw <- tolower(trimws(framework))
  keys <- names(runtimes)
  direct <- keys[tolower(keys) == fw]
  if (length(direct) > 0) return(direct[[1]])

  if (grepl("torch", fw, fixed = TRUE) && "torch" %in% keys) return("torch")
  if (grepl("xgboost", fw, fixed = TRUE) && "xgboost" %in% keys) return("xgboost")
  if (grepl("^r \\(", fw) && "caret" %in% keys) return("caret")
  NA_character_
}

.ml_model_error <- function(model_id, field, detail) {
  throwError(
    "ModelIncompatible",
    sprintf("Model '%s' cannot be loaded by this backend: field '%s' %s", model_id, field, detail),
    status = 400L
  )
}

.ml_runtime_load_types <- function(runtime_info) {
  version <- runtime_info$default %ml_or% names(runtime_info$versions)[[1]]
  unlist(runtime_info$versions[[version]]$artifact_types$load %ml_or% list())
}

#' Resolve a stored model id (as listed by GET /ml_models) to a local model
#' file that load_ml_model can read. Returns NULL when the id is unknown.
#' Signals a "ModelIncompatible" error naming the offending STAC MLM field
#' when the stored model does not match a runtime of this backend.
.ml_resolve_stored_model <- function(model_id, dir = .ml_models_dir()) {
  catalog <- tryCatch(.ml_model_catalog(dir), error = function(e) list())
  item <- catalog[[model_id]]
  if (is.null(item)) {
    return(NULL)
  }

  runtimes <- .ml_runtimes_document()
  framework <- item$properties[["mlm:framework"]]
  runtime <- .ml_runtime_for_framework(framework, runtimes)
  if (is.na(runtime)) {
    .ml_model_error(
      model_id, "mlm:framework",
      sprintf("has value '%s', which matches none of the ML runtimes (%s).",
              as.character(framework %ml_or% "<missing>"), paste(names(runtimes), collapse = ", "))
    )
  }
  if (!isTRUE(runtimes[[runtime]]$versions[[runtimes[[runtime]]$default]]$inference)) {
    .ml_model_error(model_id, "mlm:framework", sprintf("maps to runtime '%s', which does not support inference.", runtime))
  }

  loadable <- .ml_runtime_load_types(runtimes[[runtime]])

  # Candidate files in order of preference, each with its artifact type.
  candidates <- list()
  if ("torch.jit.save" %in% loadable) {
    candidates <- c(candidates, list(list(path = file.path(dir, paste0(model_id, ".pt")), type = "torch.jit.save")))
  }
  for (asset in item$assets %ml_or% list()) {
    if (!("mlm:model" %in% unlist(asset$roles))) next
    href <- sub("^file://", "", as.character(asset$href %ml_or% ""))
    candidates <- c(candidates, list(list(path = href, type = asset$`mlm:artifact_type` %ml_or% "")))
  }
  if ("R (RDS)" %in% loadable) {
    candidates <- c(candidates, list(list(path = file.path(dir, paste0(model_id, ".rds")), type = "R (RDS)")))
  }
  if ("onnx" %in% loadable) {
    onnx <- .ml_sibling_artifacts(model_id, dir)$onnx
    if (!is.null(onnx)) candidates <- c(candidates, list(list(path = onnx$path, type = "onnx")))
  }

  declared_types <- unique(vapply(candidates, function(c) c$type, character(1)))
  for (candidate in candidates) {
    if (candidate$type %in% loadable && nzchar(candidate$path) && file.exists(candidate$path)) {
      return(normalizePath(candidate$path))
    }
  }

  .ml_model_error(
    model_id, "mlm:artifact_type",
    sprintf("offers %s, but runtime '%s' loads only %s, or the artifact file is missing.",
            if (length(declared_types) > 0) paste0("'", declared_types, "'", collapse = ", ") else "no artifact",
            runtime, paste0("'", loadable, "'", collapse = ", "))
  )
}

# ---------------------------------------------------------------------------
# Handlers
# ---------------------------------------------------------------------------

.ml_api_error <- function(res, e) {
  body <- handleError(e)
  res$status <- if (inherits(e, "OpenEOError")) e$status else 500L
  body
}

# Router does not install plumber's query string filter, so query parameters
# are read from the raw QUERY_STRING.
.ml_query_param <- function(req, name) {
  qs <- req$QUERY_STRING %ml_or% ""
  qs <- sub("^\\?", "", qs)
  if (!nzchar(qs)) {
    return(NULL)
  }
  for (pair in strsplit(qs, "&", fixed = TRUE)[[1]]) {
    kv <- strsplit(pair, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 1 && identical(utils::URLdecode(kv[[1]]), name)) {
      return(if (length(kv) >= 2) utils::URLdecode(paste(kv[-1], collapse = "=")) else "")
    }
  }
  NULL
}

.ml_parse_count <- function(value, name, minimum) {
  if (is.null(value) || identical(value, "")) {
    return(NULL)
  }
  n <- suppressWarnings(as.numeric(value))
  if (length(n) != 1 || is.na(n) || n != floor(n) || n < minimum) {
    throwError(
      "ParameterValueInvalid",
      sprintf("Query parameter '%s' must be an integer >= %d.", name, minimum),
      status = 400L
    )
  }
  as.integer(n)
}

.ml_runtimes <- function(req, res) {
  tryCatch(.ml_runtimes_document(), error = function(e) .ml_api_error(res, e))
}

.ml_models <- function(req, res) {
  tryCatch({
    limit <- .ml_parse_count(.ml_query_param(req, "limit"), "limit", 1L)
    offset <- .ml_parse_count(.ml_query_param(req, "offset"), "offset", 0L) %ml_or% 0L

    dir <- .ml_models_dir()
    catalog <- .ml_model_catalog(dir)
    total <- length(catalog)
    end <- if (is.null(limit)) total else min(total, offset + limit)
    page <- if (offset < end) catalog[(offset + 1L):end] else list()

    base_url <- Session$getConfig()$base_url
    page_href <- function(off) sprintf("%s/ml_models?limit=%d&offset=%d", base_url, limit, off)
    links <- list(list(rel = "self", href = paste0(base_url, "/ml_models"), type = "application/json"))
    if (!is.null(limit) && end < total) {
      links <- c(links, list(list(rel = "next", href = page_href(end), type = "application/json")))
    }
    if (!is.null(limit) && offset > 0) {
      links <- c(links, list(list(rel = "prev", href = page_href(max(0L, offset - limit)), type = "application/json")))
    }

    list(
      models = unname(lapply(page, .ml_model_public_item, dir = dir)),
      links = links
    )
  }, error = function(e) .ml_api_error(res, e))
}

.ml_model_by_id <- function(req, res, model_id) {
  tryCatch({
    dir <- .ml_models_dir()
    item <- .ml_model_catalog(dir)[[model_id]]
    if (is.null(item)) {
      throwError("ModelNotFound", sprintf("Model '%s' does not exist.", model_id), status = 404L)
    }
    .ml_model_public_item(item, dir)
  }, error = function(e) .ml_api_error(res, e))
}

#' Register the ML capability endpoints on the active Session.
addMlEndpoints <- function() {
  # STAC requires `null` for an absent geometry; the default serializer
  # would turn NULL into `{}`.
  stac_serializer <- serializer_unboxed_json(null = "null", na = "null")

  Session$createEndpoint(path = "/ml_runtimes",
                         method = "GET",
                         handler = .ml_runtimes)

  Session$createEndpoint(path = "/ml_models",
                         method = "GET",
                         handler = .ml_models,
                         serializer = stac_serializer)

  Session$createEndpoint(path = "/ml_models/{model_id}",
                         method = "GET",
                         handler = .ml_model_by_id,
                         serializer = stac_serializer)
}
