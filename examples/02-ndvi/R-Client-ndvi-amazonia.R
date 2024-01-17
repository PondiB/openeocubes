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

# to check specific process e.g. ndvi
describe_process(processes$ndvi)

# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
datacube_init = p$load_collection(id = "sentinel-s2-l2a-cogs",
                                  spatial_extent = list(west = -7338335,
                                                        south = -1027138,
                                                        east = -7329987,
                                                        north = -1018790,
                                                        crs = 3857),
                                  temporal_extent = c("2022-01-01", "2022-12-31"))



# filter the data cube for the desired bands
datacube_filtered = p$filter_bands(data = datacube_init, bands = c("B04", "B08"))

# aggregate data cube to yearly
datacube_agg = p$aggregate_temporal_period(data = datacube_filtered, period = "year", reducer = "median")

# ndvi calculation
datacube_ndvi = p$ndvi(data = datacube_agg, red = "B04", nir = "B08")

# supported formats
formats = list_file_formats()

# save as GeoTiff or NetCDF
result = p$save_result(data = datacube_ndvi, format = formats$output$GTiff)

# Process and download data synchronously
start.time <- Sys.time()
compute_result(graph = result, output_file = "amazonia_2022_ndvi.tif")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("End of processes")


