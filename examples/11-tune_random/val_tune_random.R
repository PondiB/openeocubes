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
training_data <-("../train_data/train_data.geojson")
trainings_data <- sf::st_read(training_data)

table(trainings_data$class_name)
length(table(trainings_data$class_name))
ab <- sf::st_transform(trainings_data, 4326)
aot_bbox <- sf::st_bbox(ab)



validatio_data <- ("../train_data/val_dat.geojson")
validatio_data_load <- sf::st_read(validatio_data)
table(validatio_data_load$class_name)
validatio_data_transform <- sf::st_transform(validatio_data_load, 4326)
vali_bbox <- sf::st_bbox(validatio_data_transform)
vali_bbox

table(validatio_data_load$class_name)
table(trainings_data$class_name)


aoi <- ("../train_data/n_aoi.geojson")
aoi_data <- sf::st_read(aoi, quiet = TRUE)
aoi_transform <- sf::st_transform(aoi_data, 4326)
aoi_bbox <- sf::st_bbox(aoi_transform)
aoi_bbox


# Load a Sentinel-2 data cube covering the training area
datacube_crop <- p$load_collection(
  id = "sentinel-2-l2a",
  spatial_extent = list(
    west  = as.numeric(aot_bbox["xmin"]),
    south = as.numeric(aot_bbox["ymin"]),
    east  = as.numeric(aot_bbox["xmax"]),
    north = as.numeric(aot_bbox["ymax"]),
    crs   = 4326
  ),
  temporal_extent = c("2017-05-01T00:00:00Z", "2017-09-30T23:59:59Z"),
  bands = c("coastal", "blue", "green", "red", "rededge1", "rededge2", "rededge3", "nir", "nir08", "nir09", "swir16", "swir22"),
  cloud_cover = 50
)



datacube_validation <- p$load_collection(
  id = "sentinel-2-l2a",
  spatial_extent = list(
    west = as.numeric(vali_bbox["xmin"]),
    south = as.numeric(vali_bbox["ymin"]),
    east = as.numeric(vali_bbox["xmax"]),
    north = as.numeric(vali_bbox["ymax"]),
    crs = 4326
  ),
  temporal_extent = c("2017-05-01T00:00:00Z", "2017-09-30T23:59:59Z"),
  bands = c("coastal", "blue", "green", "red", "rededge1", "rededge2", "rededge3", "nir", "nir08", "nir09", "swir16", "swir22"),
  cloud_cover = 50
)

datacube_aoi <- p$load_collection(
  id = "sentinel-2-l2a",
  spatial_extent = list(
    west  = as.numeric(aoi_bbox["xmin"]),
    south = as.numeric(aoi_bbox["ymin"]),
    east  = as.numeric(aoi_bbox["xmax"]),
    north = as.numeric(aoi_bbox["ymax"]),
    crs   = 4326
  ),
  temporal_extent = c("2017-05-01T00:00:00Z", "2017-09-30T23:59:59Z"),
  bands = c("coastal", "blue", "green", "red", "rededge1", "rededge2", "rededge3", "nir", "nir08", "nir09", "swir16", "swir22"),
  cloud_cover = 50
)

datacube_crop <- p$array_interpolate_linear(datacube_crop)
datacube_validation <- p$array_interpolate_linear(datacube_validation)
datacube_aoi  <- p$array_interpolate_linear(datacube_aoi)



datacube_crop <- p$aggregate_temporal_period(datacube_crop, period = "month", reducer = "median")
datacube_validation <- p$aggregate_temporal_period(datacube_validation, period = "month", reducer = "median")
datacube_aoi <- p$aggregate_temporal_period(datacube_aoi, period = "month", reducer = "median")

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

datacube_validation <- p$ndvi(
  data = datacube_validation,
  nir = "nir",
  red = "red",
  target_band = "NDVI"
)


training_dat <- p$aggregate_spatial(data = datacube_crop, geometries = training_data, reducer = "mean")


model_svm_poly <- p$mlm_class_svm(kernel = "poly", C = 1, gamma = 0.1, degree = 2, coef0 = 1, seed = 42)

model <- p$ml_tune_random(
  model = model_svm_poly,
  training_set = training_dat,
  target = "class_name",
  parameters = list(
    C = list(type = "log_uniform", min = 0.01, max = 100),
    degree = list(type = "int_uniform", min = 2, max = 5),
    scale = list(type = "log_uniform", min = 0.001, max = 1)
  ),
  n_iter = 10,
  scoring = "f1",
  cv = 5,
  seed = 42
)

vali_data <- p$aggregate_spatial(data = datacube_validation, geometries = validatio_data, reducer = "mean")

validate <- p$ml_validate(model = model, validation_set = vali_data, target = "class_name", scoring = "per_class")

prediction <- p$ml_predict(data = datacube_aoi, model = model)



# Define how the prediction result should be saved (e.g., as GeoTIFF)
result_predict <- p$save_result(
  data = prediction,
  format = formats$output$GTiff
)

result <- openeo::compute_result(result_predict)
plot(rast(result))


