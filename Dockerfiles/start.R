# Install package from GitHub
remotes::install_github("PondiB/openeogdalcubes", ref = "main", force = TRUE)

# Start service
library(openeogdalcubes)

config = SessionConfig(api.port = 8000, host = "0.0.0.0")
config$workspace.path = "/var/openeo/workspace"
createSessionInstance(config)
Session$startSession()
