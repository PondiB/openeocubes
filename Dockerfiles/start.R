# Install package from GitHub
remotes::install_github("PondiB/openeogdalcubes", ref = "main", dependencies=TRUE, force = TRUE)

# Start service
library(openeogdalcubes)

#AWSHOST=""
aws.host <-Sys.getenv("AWSHOST")
message("AWS host port id is....")
message(aws.host)

config = SessionConfig(api.port = 8000, host = "0.0.0.0", aws.ipv4 = NULL)
config$workspace.path = "/var/openeo/workspace"
createSessionInstance(config)
Session$startSession()
