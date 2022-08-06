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
print(collections)

# to check available processes and their descriptions
processes = list_processes()

# to check specific process e.g. ndvi
describe_process(processes$ndvi)

# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
data.cube = p$load_collection(id = "sentinel-s2-l2a-cogs",
                          spatial_extent = list(west=7.1,
                                                south=51.8,
                                                east=7.2,
                                                north=52.8),
                         temporal_extent = c("2021-01-01", "2021-06-30"),
                         # extra optional args -> courtesy of gdalcubes
                         pixels_size = 500,
                         time_aggregation = "P1M"
                         )

# filter the data cube for the desired bands
data.cube = p$filter_bands(data = data.cube, bands = c("B04", "B08"))

# rename bands
#data.cube = rename_dimension(data = data.cube, "B04" = "red", "B08" = "nir")

# ndvi calculation
data.cube = p$ndvi(data = data.cube, red = "B04", nir = "B08" )

# reducer User-Defined Function -> NDVI Trend
ndvi.trend = "function(x) {
  z = data.frame(t=1:ncol(x), ndvi=x[\"NDVI\",])
  result = NA
  if (sum(!is.na(z$ndvi)) > 3) {
    result = coef(lm(ndvi ~ t, z, na.action = na.exclude))[2]
  }
  return(result)}"

# run User-Defined Function
# data.cube = p$run_udf(data = data.cube, udf = ndvi.trend)

# supported formats
formats = list_file_formats()

# save as GeoTiff or NetCDF
data.cube = p$save_result(data = data.cube, format = formats$output$GTiff )

# Process and download data synchronously
compute_result(graph = data.cube, output_file = "ndvi.tif")

# create a job
#job = create_job(graph = data.cube, title = "ndviTrend", description = "NDVI Trend")

# then start the processing of the job
#start_job(job = job)

# an overview of the job
#describe_job(job = job)

# an overview of the created files
#list_results(job = job)

# download them to the desired folder
#download_results(job = job, folder = "/Users/brianpondi/Downloads/processed_data")

