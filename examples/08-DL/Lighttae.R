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
  temporal_extent = c("2021-06-01", "2021-08-30"),
  bands = c("B02","B03","B04","B05","B06","B07","B08")
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
  temporal_extent = c("2021-06-01", "2021-08-30"),
  bands = c("B02","B03","B04","B05","B06","B07","B08")
)


lighttae <- p$mlm_class_lighttae(
  epochs          = 150L,
  batch_size      = 128L,      
  optimizer       = "adam",
  learning_rate   = 5e-4,
  epsilon         = 1e-8,
  weight_decay    = 7e-4,      
  lr_decay_epochs = 50L,       
  lr_decay_rate   = 1.0,       
  seed            = 42L
)

  
  
training_dat <- p$aggregate_spatial(data = datacube_crop, geometries = training_data, reducer = "mean")
  
  
# Fit the defined model to the preprocessed training data
  model <- p$ml_fit(
    model = lighttae,
    training_set = training_dat,
    target = "target"
  )
  save <- p$save_ml_model(model, "lighttae", tasks = list("classification"))
  
  
  # Apply the trained model to the area of interest (AOI)
  prediction_mlp <- p$ml_predict(
    data = datacube_aoi,
    model = save
  )
  
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
 
  
 