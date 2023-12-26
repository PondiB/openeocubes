# build and install package locally (use for development)
remotes::install_local("./", dependencies = TRUE, force = TRUE)

# Start service
library(openeocubes)

config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
# set workspace for testing
config$workspace.path = paste0(getwd(), "/test_workspace")
createSessionInstance(config)
Session$startSession()
