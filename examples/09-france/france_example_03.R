#'@description
#' Runs an example workflow for crop type classification using a classical
#' machine learning algorithm (e.g. Random Forest).
library(openeo)
library(terra)
library(sf)
library(grid)
library(ggplot2)
library(stars)
# Connect to the openEO backend (in this case, a local backend)
con <- connect("http://localhost:8000")
# Retrieve available processes from the backend (e.g., for ML, preprocessing)
p <- processes()
# Log in with user credentials
login(user = "user", password = "password")
# Get a list of supported file formats (for input/output)
formats <- list_file_formats()
# Path to the GeoJSON training data (containing points and labels)
aoi <- ("../train_data/aot_.geojson")
training_data <- ("../train_data/big_area_aot.geojson")

trainings_data <- sf::st_read(training_data)
transfor_aot <- sf::st_transform(trainings_data, 25832)
aot_bbox <- sf::st_bbox(transfor_aot)

aoi_data <- sf::st_read(aoi, quiet = TRUE)
aoi_transform <- sf::st_transform(aoi_data, 25832)
aoi_bbox <- sf::st_bbox(aoi_transform)
# Load a Sentinel-2 data cube covering the training area


datacube_crop <- p$load_collection(
  id = "sentinel-s2-l2a-cogs",
  spatial_extent = list(
    west  = as.numeric(aot_bbox["xmin"]),
    south = as.numeric(aot_bbox["ymin"]),
    east  = as.numeric(aot_bbox["xmax"]),
    north = as.numeric(aot_bbox["ymax"]),
    crs   = 25832
  )
  ,
  temporal_extent = c("2017-09-01", "2017-12-30"),
  bands = c("B02", "B03", "B04", "B08")
)

#Area of Interst Data Cube
datacube_aoi <- p$load_collection(
  id = "sentinel-s2-l2a-cogs",
  spatial_extent = list(
    west  = as.numeric(aoi_bbox["xmin"]),
    south = as.numeric(aoi_bbox["ymin"]),
    east  = as.numeric(aoi_bbox["xmax"]),
    north = as.numeric(aoi_bbox["ymax"]),
    crs   = 25832
  ),
  temporal_extent = c("2017-09-01", "2017-12-30"),
  bands = c("B02", "B03", "B04","B08")
)

ndvi_aoi <- p$ndvi(
  data = datacube_aoi,
  nir = "B08",
  red = "B04",
  target_band = "NDVI"
)

ndvi_crop <- p$ndvi(
  data = datacube_crop,
  nir = "B08",
  red = "B04",
  target_band = "NDVI"
)

datacube_crop <- p$array_interpolate_linear(ndvi_crop)
datacube_aoi <- p$array_interpolate_linear(ndvi_aoi)

training_dat <- p$aggregate_spatial(
  data = datacube_crop,
  geometries = training_data,
  reducer = "mean"
)

tempcnn <- p$mlm_class_tempcnn(
  cnn_layers = list(256L, 256L, 256L),
  cnn_kernels = list(7L, 7L, 7L),
  cnn_dropout_rates = list(0.2, 0.2, 0.2),
  dense_layer_nodes = 256L,
  dense_layer_dropout_rate= 0.5,
  epochs = 100L,
  batch_size = 64L,
  optimizer = "adam",
  learning_rate = 1e-3,
  seed = 42L
)

model <- p$ml_fit(
  model = tempcnn,
  training_set = training_dat,
  target = "class_name"
)

prediction <- p$ml_predict(
  data = datacube_aoi,
  model = model
)

result_predict <- p$save_result(
  data = prediction,
  format = formats$output$GTiff
)

start.time <- Sys.time()
job_id <- openeo::create_job(result_predict, format = "GTiff")
openeo::list_results()
openeo::start_job(job_id)
## Wait until the job has finished and the backend reports that the result is ready
## (for example, when the job status in the terminal indicates that it is "Done").
# The result can then be downloaded (e.g. "Copied job results to download dir").
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#'
#'The data can then be further processed.
#'



