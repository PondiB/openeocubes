library(openeo)

# connect  to the back-end
con = connect("http://127.0.0.1:8000")

# basic login
login(user = "user",
      password = "password",
      login_type = "basic")

# get the process collection to use the predefined processes of the back-end
p = processes()

# load the initial data collection and limit the amount of data loaded
data.cube = p$load_collection(id = "sentinel-s2-l2a-cogs",
                         spatial_extent = list(west=16.06,
                                               south=48.06,
                                               east=16.65,
                                               north=48.35),
                         temporal_extent = c("2021-01-01", "2021-06-30"))

# filter the data cube for the desired bands
data.cube= p$filter_bands(data = data.cube, bands = c("B04", "B08"))



## TO Do  -> reducer UDF vs apply per pixel UDF
