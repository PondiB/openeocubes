library(openeo)

# connect  to the back-end
con = connect("http://127.0.0.1:8000")

# basic login with default params
login(user = "user",
      password = "password",
      login_type = "basic")

# get the collection list
collections = list_collections()

# print an overview of the available collections (printed as data.frame or tibble)
collections

# to check description of a collection
collections$`sentinel-s2-l2a-cogs`$description

# Check that required processes are available.
processes = list_processes()

# to check specific process e.g. ndvi
describe_process(processes$ndvi)

# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
datacube_init = p$load_collection(id = "sentinel-s2-l2a-cogs",
                          spatial_extent = list(west=13.77795,
                                                south=52.376139,
                                                east=13.854731,
                                                north=52.408004),
                         temporal_extent = c("2019-01-01", "2020-12-31"),
                         # extra optional args -> courtesy of gdalcubes
                         pixels_size = 500,
                         time_aggregation = "P1Y"
                         )

# filter the data cube for the desired bands
datacube_filtered = p$filter_bands(data = datacube_init, bands = c("B04", "B08"))


# ndvi calculation
datacube_ndvi = p$ndvi(data = datacube_filtered, red = "B04", nir = "B08")

# User-Defined Function -> generate NDVI Trend
ndvi_trend = "function(x) {
  z = data.frame(t=1:ncol(x), ndvi=x[\"NDVI\",])
  result = NA
  if (sum(!is.na(z$ndvi)) > 3) {
    result = coef(lm(ndvi ~ t, z, na.action = na.exclude))[2]
  }
  return(result)}"

# run User-Defined Function
datacube_ndvitrend = p$run_udf(data = datacube_ndvi, udf = ndvi_trend, names = c("ndvi_trend"))

# supported formats
formats = list_file_formats()

# save as GeoTiff or NetCDF
result = p$save_result(data = datacube_ndvitrend, format = formats$output$GTiff)

# Process and download data synchronously
start.time <- Sys.time()
compute_result(graph = result, output_file = "ndvi_trend.tif")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("End of processes")


