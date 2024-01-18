library(openeo)

# connect  to the back-end when deployed locally
# con = connect("http://localhost:8000")
# connect  to the back-end when deployed on aws
con = connect("http://<AWS-IPv4-ADDRESS>:8000")

# basic login with default params
login(user = "user",
      password = "password")

# get the collection list
collections = list_collections()

# to check description of a collection
collections$`sentinel-s2-l2a-cogs`$description

# Check that required processes are available.
processes = list_processes()

# to check specific process e.g. filter_bands
describe_process(processes$filter_bands)

# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
datacube_init = p$load_collection(id = "sentinel-s2-l2a-cogs",
                                  spatial_extent = list(west = 416812.2,
                                                        south = 5803577.5,
                                                        east = 422094.8,
                                                        north = 5807036.1,
                                                        crs = 32633),
                                  temporal_extent = c("2016-01-01", "2020-12-31"))

# filter the data cube for the desired bands
datacube_filtered = p$filter_bands(data = datacube_init,
                                   bands = c("B04", "B08"))
# aggregate data cube to monthly
datacube_agg = p$aggregate_temporal_period(data = datacube_filtered,
                                           period = "month", reducer = "median")

# user defined R function - bfast change detection method
change_detection = 'function(x) {
  knr <- exp(-((x["B08",]/10000)-(x["B04",]/10000))^2/(2))
  kndvi <- (1-knr) / (1+knr)
  if (all(is.na(kndvi))) {
    return(c(NA,NA))
  }
    kndvi_ts = ts(kndvi, start = c(2016, 1), frequency = 12)
    library(bfast)
    tryCatch({
        result = bfastmonitor(kndvi_ts, start = c(2020,1), level = 0.01)
        return(c(result$breakpoint, result$magnitude))
      }, error = function(x) {
        return(c(NA,NA))
      })
  }'

# run udf
datacube_udf = p$run_udf(data = datacube_agg, udf = change_detection, context =  c("change_date", "change_magnitude"))

# supported formats
formats = list_file_formats()

# save as GeoTiff or NetCDF
result = p$save_result(data = datacube_udf, format = formats$output$NetCDF)

# Process and download data synchronously
start.time <- Sys.time()
compute_result(graph = result, output_file = "detected_changes.nc")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("End of processes")

