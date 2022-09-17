library(openeo)

# connect  to the back-end
con = connect("http://127.0.0.1:8000")

# basic login with default params
login(user = "user",
      password = "password",
      login_type = "basic")

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
                                spatial_extent = list(west=-7347259,
                                                      south=-1025490,
                                                      east=-7314864,
                                                      north=-995476.1),
                                temporal_extent = c("2022-01-01", "2022-03-30"),
                                # extra optional args -> courtesy of gdalcubes
                                pixels_size = 60,
                                time_aggregation = "P3M",
                                crs = 3857)

# filter the data cube for the desired bands
datacube_filtered = p$filter_bands(data = datacube_init, bands = c("B04", "B08"))


# ndvi calculation
datacube_ndvi = p$ndvi(data = datacube_filtered, red = "B04", nir = "B08")

# supported formats
formats = list_file_formats()

# save as GeoTiff or NetCDF
result = p$save_result(data = datacube_ndvi, format = formats$output$GTiff)

# Process and download data synchronously
start.time <- Sys.time()
compute_result(graph = result, output_file = "amazonia_2022_summer.tif")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("End of processes")


