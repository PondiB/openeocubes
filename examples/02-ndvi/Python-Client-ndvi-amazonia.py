import openeo
import time

# Connect to the back-end
#on localhost con = openeo.connect("http://localhost:8000")
#con = openeo.connect("http://<AWS-IPv4-ADDRESS>:8000")
connection = openeo.connect("http://localhost:8000")

# Basic login with default params
connection.authenticate_basic(username="user", password="password")

# Get the collection list
collections = connection.list_collections()

# Check description of a collection
#print(collections)

# Check that required processes are available
processes = connection.list_processes()
#print(processes)


# Load the initial data collection and limit the amount of data loaded
datacube= connection.load_collection(
    'sentinel-s2-l2a-cogs',
    spatial_extent= {'west': -66.27866,'south': -9.34489,'east': -66.26212, 'north': -9.33131},
    temporal_extent=['2021-05-01', '2022-06-30']
    )


# Filter the data cube for the desired bands
datacube = datacube.process(
        process_id="filter_bands", 
        arguments={
        "data": datacube, 
        "bands": ["B08", "B04"]}
        )



# NDVI calculation
datacube = datacube.process(
        process_id="ndvi", 
        arguments={
        "data": datacube, 
        "nir": "B08", 
        "red": "B04"}
        )


# Save as GeoTiff or NetCDF
result = datacube.save_result("GTiff")

# Process and download data synchronously
start_time = time.time()

# Creating a new job at the back-end by sending the datacube information.
job = result.save_result(format='GTiff').create_job()
# Starts the job and waits until it finished to download the result.
job.start_and_wait().get_results().download_file("./amazonia_2022_ndvi.tif")
end_time = time.time()
time_taken = end_time - start_time
print("Time taken:", time_taken)
print("End of processes")
