#load required librarys
library(openeo)
library(terra)
library(sf)
library(grid)
library(cowplot)
library(stars)
library(ggplot2)
# Connect to the openEO backend (in this case, a local backend)
con <- connect("http://localhost:8000")
# Retrieve available processes from the backend (e.g., for ML, preprocessing)
p <- processes()
# Log in with user credentials
login(user = "user", password = "password")
# Get a list of supported file formats (for input/output)
formats <- list_file_formats()
# Path to the GeoJSON training data (containing points and labels)
training_data <- ("../train_data/train_data.geojson")
aoi <- ("../train_data/aoi_area.geojson")


trainings_data <- sf::st_read(training_data)
table(trainings_data$class_name)
transfor_aot <- sf::st_transform(trainings_data, 4326)
aot_bbox <- sf::st_bbox(transfor_aot)
aot_bbox

aoi_data <- sf::st_read(aoi, quiet = TRUE)
aoi_transform <- sf::st_transform(aoi_data, 4326)
aoi_bbox <- sf::st_bbox(aoi_transform)
aoi_bbox

table(trainings_data$class_name)
table(aoi_data$class_name)
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
  bands = c("coastal", "blue", "green", "red", "rededge1", "rededge2", "rededge3", "nir", "nir08", "nir09", "swir16", "swir22", "scl"),
  cloud_cover = 50
)
# Load a Sentinel-2 data cube covering the area of interest (AOI) for prediction
datacube_aoi <- p$load_collection(
  id = "sentinel-2-l2a",
  spatial_extent = list(
    west  = -4.02,
    south = 48.10,
    east  = -3.74,
    north = 48.20,
    crs   = 4326
  ),
  temporal_extent = c("2017-05-01T00:00:00Z", "2017-09-30T23:59:59Z"),
  bands = c("coastal", "blue", "green", "red", "rededge1", "rededge2", "rededge3", "nir", "nir08", "nir09", "swir16", "swir22", "scl"),
  cloud_cover = 50
)




datacube_crop <- p$mask(datacube_crop, c(3, 8, 9))
datacube_aoi  <- p$mask(datacube_aoi, c(3,8,9))


datacube_crop <- p$aggregate_temporal_period(datacube_crop, period = "month", reducer = "median")
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

training_dat <- p$aggregate_spatial(
  data = datacube_crop,
  geometries = training_data,
  reducer = "median"
)

rf_class <-p$mlm_class_random_forest(max_variables = "sqrt", num_trees = 150, seed = 42)

model <- p$ml_fit(
  model = rf_class,
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
openeo::start_job(job_id)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#### Code for the map
rf_st <- read_stars("..")


names(rf_st) <- "cover"

rf_st[["cover"]] <- factor(
  rf_st[["cover"]],
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c("barley", "corn", "permanent meadows", "rapeseed", "temporary meadows", "wheat")
)

bb_ll <- st_bbox(rf_st)

plot <- ggplot() +
  geom_stars(data = rf_st, aes(fill = cover)) +
  scale_fill_viridis_d(
    name = "Cover",
    drop = FALSE,
    na.translate = FALSE,
    na.value = "white"
  ) +
  coord_sf(
    crs = st_crs(4326),
    default_crs = st_crs(4326),
    xlim = c(bb_ll["xmin"], bb_ll["xmax"]),
    ylim = c(bb_ll["ymin"], bb_ll["ymax"]),
    expand = FALSE
  ) +
  labs(x = "Longitude (°)", y = "Latitude (°)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    legend.position  = c(0.01, 0.01),
    legend.justification = c(0, 0),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.75),
      colour = "grey30",
      linewidth = 0.3
    ),
    legend.key.size = unit(5, "mm"),
    legend.spacing.y = unit(1, "mm"),
    legend.margin = margin(6, 8, 6, 8),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(
    fill = guide_legend(
      ncol = 1,
      keywidth = unit(5, "mm"),
      keyheight = unit(4, "mm"),
      title.position = "top",
      label.hjust = 0
    )
  )

print(plot)







