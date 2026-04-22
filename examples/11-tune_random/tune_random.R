library(openeo)
library(terra)
# Connect to the openEO backend (in this case, a local backend)
con <- openeo::connect("http://localhost:8000")
# Retrieve available processes from the backend (e.g., for ML, preprocessing)
p <- processes()
# Log in with user credentials
login(user = "user", password = "password")
# Get a list of supported file formats (for input/output)
formats <- list_file_formats()
# Path to the GeoJSON training data (containing points and labels)
training_data <-("../train_data/land_train_data.geojson")
a <- sf::st_read(training_data)
ab <- sf::st_transform(a, 25832)
bbox_aot <- sf::st_bbox(ab)
bbox_aot
typeof(bbox_aot)
# Load a Sentinel-2 data cube covering the training area
datacube_crop <- p$load_collection(
  id = "sentinel-2-l2a",
  spatial_extent = list(
    west = as.numeric(bbox_aot["xmin"]),
    south = as.numeric(bbox_aot["ymin"]),
    east = as.numeric(bbox_aot["xmax"]),
    north = as.numeric(bbox_aot["ymax"]),
    crs = 25832
  ),
  temporal_extent = c("2021-06-01T00:00:00Z", "2021-07-30T23:59:59Z"),
  bands = c("blue", "green", "red", "nir")
)
# Load a Sentinel-2 data cube covering the area of interest (AOI) for prediction
datacube_aoi <- p$load_collection(
  id = "sentinel-2-l2a",
  spatial_extent = list(
    west = 402920.7,
    south = 5755389.9,
    east = 408605.3,
    north = 5759460.3,
    crs = 25832
  ),
  temporal_extent = c("2021-06-01T00:00:00Z", "2021-07-30T23:59:59Z"),
  bands = c("blue", "green", "red", "nir")
)

datacube_aoi <- p$ndvi(
  data = datacube_aoi,
  nir = "nir",
  red = "red",
  target_band = "NDVI"
)

datacube_crop <- p$ndvi(
  data = datacube_crop,
  nir = "nir",
  red = "red",
  target_band = "NDVI"
)


datacube_crop <- p$array_interpolate_linear(datacube_crop)
datacube_aoi <- p$array_interpolate_linear(datacube_aoi)


tempcnn <- p$mlm_class_tempcnn(
  cnn_layers = list(256L, 256L, 256L),
  cnn_kernels = list(7L, 7L, 7L),
  cnn_dropout_rates = list(0.2, 0.2, 0.2),
  dense_layer_nodes = 256L,
  dense_layer_dropout_rate = 0.5,
  epochs = 100L,
  batch_size = 64L,
  optimizer = "adam",
  learning_rate = 1e-3,
  seed = 33L
)
training_dat <- p$aggregate_spatial(data = datacube_crop, geometries = training_data, reducer = "mean")

model <- p$ml_tune_random(
  model = tempcnn,
  training_set = training_dat,
  target = "Label",
  parameters = list(
    learning_rate = list(type = "log_uniform", min = 1e-5, max = 1e-2),
    cnn_dropout_rates = list(type = "choice", values = list(
      list(0.1, 0.1, 0.1),
      list(0.2, 0.2, 0.2),
      list(0.3, 0.3, 0.3),
      list(0.4, 0.4, 0.4),
      list(0.5, 0.5, 0.5)
    ))
  ),
  n_iter = 10,
  scoring = "accuracy",
  cv = 0
)

prediction_mlp <- p$ml_predict(data = datacube_aoi, model = model)

# Define how the prediction result should be saved (e.g., as GeoTIFF)
result_predict <- p$save_result(
  data = prediction_mlp,
  format = formats$output$GTiff
)

result <- openeo::compute_result(result_predict)
plot(rast(result))







