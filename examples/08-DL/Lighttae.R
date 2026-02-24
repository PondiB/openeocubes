#load required librarys
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
  bands = c("blue", "green", "red", "nir", "swir16", "swir22")
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
  bands = c("blue", "green", "red", "nir", "swir16", "swir22")
)




datacube_crop <- p$array_interpolate_linear(datacube_crop)
datacube_aoi <- p$array_interpolate_linear(datacube_aoi)


lighttae <- p$mlm_class_lighttae(
  epochs = 150L,
  batch_size = 128L,
  optimizer = "adam",
  learning_rate = 5e-4,
  epsilon  = 1e-8,
  weight_decay = 7e-4,
  lr_decay_epochs = 50L,
  lr_decay_rate = 1.0,
  seed = 42L
)

training_dat <- p$aggregate_spatial(data = datacube_crop, geometries = training_data, reducer = "mean")


# Fit the defined model to the preprocessed training data
model <- p$ml_fit(
  model = lighttae,
  training_set = training_dat,
  target = "Label"
)

save <- p$save_ml_model(model, "lighttae", tasks = list("classification"))

prediction_lighttae <- p$ml_predict(
  data = datacube_aoi,
  model = save
)

# Define how the prediction result should be saved (e.g., as GeoTIFF)
result_predict <- p$save_result(
  data = prediction_lighttae,
  format = formats$output$GTiff
)

result <- openeo::compute_result(result_predict)
plot(rast(result))
