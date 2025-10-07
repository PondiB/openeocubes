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
training_data <-(".../path_to_the_trainings_data")

# Load a Sentinel-2 data cube covering the training area
datacube_crop <- p$load_collection(
  id = "sentinel-s2-l2a-cogs",
  spatial_extent = list(
    west = 385305,
    south = 5702894,
    east = 401458,
    north = 5720076,
    crs = 25832
  ),
  temporal_extent = c("2021-06-01", "2021-07-30"),
  bands = c("B02", "B03", "B04", "B08")
)
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


# model shell for the tempcnn
tempcnn <- p$mlm_class_tempcnn(
  cnn_layers              = list(256L, 256L, 256L),
  cnn_kernels             = list(7L, 7L, 7L),
  cnn_dropout_rates       = list(0.2, 0.2, 0.2),
  dense_layer_nodes       = 256L,
  dense_layer_dropout_rate= 0.5,
  epochs                  = 100L,
  batch_size              = 64L,      
  optimizer               = "adam",
  learning_rate           = 1e-3,
  seed                    = 42L
)

#'
#'@description Extract the data from the cube with the training data.
#' As a reducer, we use the mean value per profile if it is a polygon or multi-polygon.
training_dat <- p$aggregate_spatial(data = datacube_crop, geometries = training_data, reducer = "mean")


#'@description This is where model training for classification with tempcnn takes place. 
model <- p$ml_fit(
  model = tempcnn,
  training_set = training_dat,
  target = "Label"
)

#'@description save the Model as Onnx, RDS and the meta data inforation in STAC-MLM
save <- p$save_ml_model(model, "tempcnn_test", tasks = list("classification"))


#'@description prediction
prediction_mlp <- p$ml_predict(
  data = datacube_aoi,
  model = save
)

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





