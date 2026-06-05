#!/usr/bin/env Rscript

# Train a TempCNN on polygon training samples and predict land cover on a
# Sentinel-2 AOI via the local openEO backend (async job + result download).

suppressPackageStartupMessages({
  library(openeo)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(y)
  }
  x
}

script_file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1]])))
} else {
  getwd()
}

backend_url <- Sys.getenv("OPENEO_BACKEND_URL", unset = "http://localhost:8000")
username <- Sys.getenv("OPENEO_USER", unset = "user")
password <- Sys.getenv("OPENEO_PASSWORD", unset = "password")
poll_seconds <- as.numeric(Sys.getenv("OPENEO_POLL_SECONDS", unset = "15"))
max_polls <- as.integer(Sys.getenv("OPENEO_MAX_POLLS", unset = "480"))
wait_for_finish <- tolower(Sys.getenv("OPENEO_WAIT_FOR_FINISH", unset = "true")) %in%
  c("1", "true", "yes")
download_dir <- Sys.getenv(
  "OPENEO_DOWNLOAD_DIR",
  unset = file.path(script_dir, "results")
)
training_data <- Sys.getenv(
  "OPENEO_TRAINING_GEOJSON",
  unset = normalizePath(file.path(script_dir, "..", "train_data", "land_train_data.geojson"))
)
training_data_check <- Sys.getenv(
  "OPENEO_TRAINING_GEOJSON_HOST",
  unset = training_data
)
if (!file.exists(training_data_check)) {
  stop(
    "Training GeoJSON not found: ", training_data_check,
    if (!identical(training_data_check, training_data)) {
      paste0(" (backend path: ", training_data, ")")
    } else {
      ""
    }
  )
}
temporal_extent <- c("2017-05-01T00:00:00Z", "2017-09-30T23:59:59Z")
collection_id <- Sys.getenv("OPENEO_COLLECTION_ID", unset = "sentinel-2-l2a")
bands <- c("blue", "green", "red", "nir")
epochs <- as.integer(Sys.getenv("TEMP_CNN_EPOCHS", unset = "25"))
batch_size <- as.integer(Sys.getenv("TEMP_CNN_BATCH_SIZE", unset = "32"))

cat("Backend URL   :", backend_url, "\n")
cat("Training data :", training_data, "\n")
cat("Collection    :", collection_id, "\n")
cat("TempCNN epochs:", epochs, " batch:", batch_size, "\n")

connect(backend_url)
login(user = username, password = password)
p <- processes()
formats <- list_file_formats()

# Cube for extracting training features (same region as examples/04-save-model)
datacube_train <- p$load_collection(
  id = collection_id,
  spatial_extent = list(
    west = 385305,
    south = 5702894,
    east = 401458,
    north = 5720076,
    crs = 25832
  ),
  temporal_extent = temporal_extent,
  bands = bands
)
datacube_train <- p$array_interpolate_linear(datacube_train)

# Smaller AOI for prediction (override with OPENEO_PREDICT_BBOX=w,s,e,n,crs)
predict_bbox <- strsplit(
  Sys.getenv(
    "OPENEO_PREDICT_BBOX",
    unset = "402920.7,5755389.9,405920.7,5758389.9,25832"
  ),
  ",",
  fixed = TRUE
)[[1]]
if (length(predict_bbox) != 5L) {
  stop("OPENEO_PREDICT_BBOX must be west,south,east,north,crs")
}

datacube_aoi <- p$load_collection(
  id = collection_id,
  spatial_extent = list(
    west = as.numeric(predict_bbox[[1]]),
    south = as.numeric(predict_bbox[[2]]),
    east = as.numeric(predict_bbox[[3]]),
    north = as.numeric(predict_bbox[[4]]),
    crs = as.integer(predict_bbox[[5]])
  ),
  temporal_extent = temporal_extent,
  bands = bands
)
datacube_aoi <- p$array_interpolate_linear(datacube_aoi)

tempcnn <- p$mlm_class_tempcnn(
  cnn_layers = list(64L, 64L),
  cnn_kernels = list(5L, 5L),
  cnn_dropout_rates = list(0.2, 0.2),
  dense_layer_nodes = 64L,
  dense_layer_dropout_rate = 0.3,
  epochs = epochs,
  batch_size = batch_size,
  optimizer = "adam",
  learning_rate = 1e-3,
  seed = 42L
)

training_dat <- p$aggregate_spatial(
  data = datacube_train,
  geometries = training_data,
  reducer = "mean"
)

model <- p$ml_fit(
  model = tempcnn,
  training_set = training_dat,
  target = "Label"
)

# ml_fit returns a Torch .pt path; pass it directly to ml_predict.
# save_ml_model() is optional (ONNX/STAC export) and needs Python torch for ONNX.
prediction <- p$ml_predict(
  data = datacube_aoi,
  model = model
)

result <- p$save_result(
  data = prediction,
  format = formats$output$GTiff
)

get_job_status <- function(job_id) {
  status <- tryCatch({
    info <- describe_job(job_id)
    info$status %||% NA_character_
  }, error = function(e) {
    NA_character_
  })
  if (!is.na(status) && nzchar(status)) {
    return(status)
  }
  jobs <- list_jobs()
  idx <- which(jobs$id == job_id)
  if (length(idx) < 1L) {
    return("not_found")
  }
  as.character(jobs$status[idx[[1]]])
}

cat("Submitting async job...\n")
start.time <- Sys.time()
job <- create_job(
  result,
  title = "TempCNN train + predict",
  description = "examples/14-tempcnn-predict/run_tempcnn_job.R",
  format = "GTiff"
)
job_id <- job$id
if (is.null(job_id) || !nzchar(job_id)) {
  stop("Failed to retrieve job id from create_job() response.")
}
start_job(job_id)
cat("Created job:", job_id, "\n")

if (!wait_for_finish) {
  cat("Job status:", get_job_status(job_id), "\n")
  quit(save = "no", status = 0)
}

final_status <- "unknown"
for (i in seq_len(max_polls)) {
  status <- get_job_status(job_id)
  cat(sprintf("Poll %d/%d - status: %s\n", i, max_polls, status))
  final_status <- status
  if (status %in% c("finished", "error", "canceled", "not_found")) {
    break
  }
  Sys.sleep(poll_seconds)
}

if (final_status == "error") {
  cat("Job failed. Backend logs:\n")
  print(logs(job_id))
  stop("Job ended with status 'error'.")
}
if (final_status != "finished") {
  stop("Job ended with status: ", final_status)
}

dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
download_results(job_id, folder = download_dir)
cat("Results downloaded to:", download_dir, "\n")
cat("Elapsed:", format(Sys.time() - start.time), "\n")

pred_path <- file.path(download_dir, "prediction.tif")
if (file.exists(pred_path) && requireNamespace("terra", quietly = TRUE)) {
  r <- terra::rast(pred_path)
  v <- terra::values(r)
  cat(
    "prediction.tif non-NA:", sum(!is.na(v)), "of", length(v),
    "| unique:", paste(sort(unique(stats::na.omit(v))), collapse = ", "), "\n"
  )
}
