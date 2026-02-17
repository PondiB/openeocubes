#load required librarys
library(openeo)
library(terra)
library(sf)
library(grid)
library(cowplot)
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
aoi <- ("../train_data/aoi_full.geojson")

trainings_data <- sf::st_read(training_data)
transfor_aot <- sf::st_transform(trainings_data, 4326)
aot_bb_llox <- sf::st_bbox(transfor_aot)

aoi_data <- sf::st_read(aoi, quiet = TRUE)
aoi_transform <- sf::st_transform(aoi_data, 4326)
aoi_bb_llox <- sf::st_bbox(aoi_transform)
aoi_bb_llox
# Load a Sentinel-2 data cube covering the training area
datacube_crop <- p$load_collection(
  id = "sentinel-s2-l2a-cogs",
  spatial_extent <- list(
    west  = as.numeric(aot_bb_llox["xmin"]),
    south = as.numeric(aot_bb_llox["ymin"]),
    east  = as.numeric(aot_bb_llox["xmax"]),
    north = as.numeric(aot_bb_llox["ymax"]),
    crs   = 4326
  ),
  temporal_extent = c("2017-03-01", "2017-05-30"),
  bands = c("B02", "B03", "B04", "B08")
)
# Load a Sentinel-2 data cube covering the area of interest (AOI) for prediction
datacube_aoi <- p$load_collection(
  id = "sentinel-s2-l2a-cogs",
  spatial_extent = list(
    west  = as.numeric(aoi_bb_llox["xmin"]),
    south = as.numeric(aoi_bb_llox["ymin"]),
    east  = as.numeric(aoi_bb_llox["xmax"]),
    north = as.numeric(aoi_bb_llox["ymax"]),
    crs   = 4326
  ),
  temporal_extent = c("2017-03-01", "2017-05-30"),
  bands = c("B02", "B03", "B04","B08")
)


datacube_crop <- p$aggregate_temporal_period(datacube_crop, period = "month", reducer = "median")
datacube_aoi <- p$aggregate_temporal_period(datacube_aoi, period = "month", reducer = "median")

datacube_aoi <- p$ndvi(
  data = datacube_aoi,
  nir = "B08",
  red = "B04",
  target_band = "NDVI"
)

datacube_crop <- p$ndvi(
  data = datacube_crop,
  nir = "B08",
  red = "B04",
  target_band = "NDVI"
)

datacube_crop <- p$array_interpolate_linear(datacube_crop)
datacube_aoi <- p$array_interpolate_linear(datacube_aoi)

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

##The result is ready when, after: GeoTiff_output detected in the session, the following is displayed in the terminal: Done.
#The result can be downloaded.
# Copied job results to download dir

prediction <- ("..insert model..")
r <- rast(prediction)
plot(r)
table(values(r))

# This part is only necessary for crop type classification.
aoi_v  <- terra::vect(aoi_transform)

r_crop <- terra::crop(r, aoi_v)
r_mask <- terra::mask(r_crop, aoi_v)


levs <- c("barley","corn","orchards","permanent meadows","rapeseed","temporary meadows")
vals <- sort(unique(stats::na.omit(values(r_mask))))

rf <- as.factor(r_mask)
levels(rf)[[1]] <- data.frame(ID = vals, label = levs[seq_along(vals)], stringsAsFactors = FALSE)
names(rf) <- "cover"

aoi_ll <- st_transform(aoi_transform, 4326)
train_ll <- st_transform(transfor_aot, 4326)
bb_ll <- sf::st_bbox(aoi_ll)

#30m spatial resolution
lat   <- as.numeric(mean(c(bb_ll["ymin"], bb_ll["ymax"])))
m_per_deg_lat <- 111320
m_per_deg_lon <- 111320 * cos(pi*lat/180)
res_m <- terra::res(rf)
res_deg_x <- res_m[1] / m_per_deg_lon
res_deg_y <- res_m[2] / m_per_deg_lat

ext_ll  <- terra::ext(bb_ll["xmin"], bb_ll["xmax"], bb_ll["ymin"], bb_ll["ymax"])
tmpl_ll <- terra::rast(ext = ext_ll, crs = "EPSG:4326",
                       resolution = c(res_deg_x, res_deg_y))


rf_ll <- terra::project(rf, tmpl_ll, method = "near")

rf_st <- st_as_stars(rf_ll)
id_vec <- levels(rf_ll)[[1]]$ID
label_vec <- levels(rf_ll)[[1]]$label
rf_st[[1]] <- factor(as.integer(rf_st[[1]]),
                     levels = id_vec,
                     labels = label_vec)

pal <- c(
  "barley"= "#d73027",
  "corn" = "#8c510a",
  "orchards" = "#006400",
  "permanent meadows"  = "#7fc97f",
  "rapeseed" = "#1f78b4",
  "temporary meadows"  = "#ffd92f"
)

##ggplot
plot <- ggplot() +
  geom_stars(data = rf_st) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    labels = names(pal),
    name   = "cover",
    drop   = FALSE,
    na.value = "white",
    na.translate = FALSE
  ) +
  geom_sf(data = aoi_ll,   fill = NA, colour = "black", linewidth = 0.25) +
  geom_sf(data = train_ll, fill = NA, colour = "grey20", linewidth = 0.10) +
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

    legend.position = c(0.05, 0.05),
    legend.justification  = c(0, 0),
    legend.direction = "vertical",
    legend.background = element_rect(fill = scales::alpha("white", 0.96),
                                         colour = "grey25", linewidth = 0.6),
    legend.key = element_rect(fill = "white"),
    legend.box.margin = margin(3, 6, 3, 6),
    legend.margin = margin(6, 8, 6, 8),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key.size = unit(8, "mm")
  ) +
  guides(
    fill = guide_legend(
      ncol = 1, byrow = TRUE,
      keywidth  = unit(12, "mm"),
      keyheight = unit(7,  "mm"),
      title.position = "top",
      label.hjust = 0
    )
  )

print(plot)

g <- ggplotGrob(plot)
p_rot <- ggdraw() + draw_grob(editGrob(g, vp = viewport(angle = -4)))

ggsave("map_rotated.png", p_rot, width = 12, height = 9, dpi = 300, bg = "white")

g <- ggplotGrob(plot)

panel_id <- which(g$layout$name == "panel")
angle_deg <- -3.5

g$grobs[[panel_id]] <- editGrob(
  g$grobs[[panel_id]],
  vp = viewport(angle = angle_deg)
)

grid.newpage()
grid.draw(g)




