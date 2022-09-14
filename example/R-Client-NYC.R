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


# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
datacube_init = p$load_collection(id = "sentinel-s2-l2a-cogs",
                                  spatial_extent = list(west=582182,
                                                        south=4505883,
                                                        east=587019,
                                                        north=4508997),
                                  temporal_extent = c("2021-06-01", "2021-06-30"),
                                  # extra optional args -> courtesy of gdalcubes
                                  pixels_size = 10,
                                  time_aggregation = "P1M",
                                  crs = 32618)
# filter the data cube for the desired bands
datacube_filtered = p$filter_bands(data = datacube_init, bands = c("B02","B03","B04"))


# supported formats
formats = list_file_formats()

result = p$save_result(data = datacube_filtered , format = formats$output$NetCDF)

# Process and download data synchronously
start.time <- Sys.time()
compute_result(graph = result, output_file = "nyc_manhattan.nc")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("End of processes")
