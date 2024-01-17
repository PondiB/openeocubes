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


# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
datacube_init = p$load_collection(id = "sentinel-s2-l2a-cogs",
                                  spatial_extent = list(west = 563080.6,
                                                        south = 4483092.4,
                                                        east = 609472,
                                                        north = 4530135,
                                                        crs = 32618),
                                  temporal_extent = c("2021-06-01", "2021-06-30")
                                  )
# filter the data cube for the desired bands
datacube_filtered = p$filter_bands(data = datacube_init, bands = c("B02","B03","B04"))


# supported formats
formats = list_file_formats()

result = p$save_result(data = datacube_filtered , format = formats$output$GTiff)

# Process and download data synchronously
start.time <- Sys.time()
compute_result(graph = result, output_file = "nyc_june_2021.tif")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("End of processes")
