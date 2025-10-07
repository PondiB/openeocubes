#load required librarys
library(openeo) 
library(terra)
# Connect to the openEO backend (in this case, a local backend)
con <- connect("http://localhost:8000")
# Retrieve available processes from the backend (e.g., for ML, preprocessing)
p <- processes()
# Log in with user credentials
login(user = "user", password = "password")
# Get a list of supported file formats (for input/output)
formats <- list_file_formats()
# Path to the GeoJSON training data (containing points and labels)

# Load a Sentinel-2 data cube covering the area of interest (AOI) for prediction
datacube_aoi <- p$load_collection(
  id = "sentinel-s2-l2a-cogs",
  spatial_extent = list(
    west = 402920.7,
    south = 5755389.9,
    east = 408605.3,
    north = 5759460.3,
    crs = 25832
  ),
  temporal_extent = c("2021-06-01", "2021-07-30"),
  bands = c("B02", "B03", "B04", "B08")
)

#'@description External model;
#' A local model can be loaded as rds, .pt, or onnx. 
#'Furthermore, a model can also be imported via a URL to a public Google Drive location.
model <- (".../path_to_the_model")

#'@description Prediction on an AOI with an external model 
prediction <- p$ml_predict(data = datacube_aoi,
                           model = model)


# Define how the prediction result should be saved (e.g., as GeoTIFF)
result_predict <- p$save_result(
  data = prediction_mlp,
  format = formats$output$GTiff
)
# Execute the full process chain and measure the execution time
start.time <- Sys.time()
svm <- compute_result(result_predict)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Visualize the prediction result as a raster
plot(rast(svm))
r <- rast(svm)
table(values(r))
