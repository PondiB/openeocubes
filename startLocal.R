# build and install package locally (use for development)
remotes::install_local("./", dependencies = TRUE, force = TRUE)

# Start service
library(openeocubes)

# Hard safety timeout for async workers. Must be generous enough to cover the
# full compute (e.g. ML fit + per-pixel prediction) before export starts; the
# stale-output finalizer below is the fast path for post-export hangs.
if (Sys.getenv("OPENEO_ASYNC_JOB_TIMEOUT_SEC", unset = "") == "") {
  Sys.setenv(OPENEO_ASYNC_JOB_TIMEOUT_SEC = "1800")
}
if (Sys.getenv("OPENEO_STALE_OUTPUT_FINALIZE_SEC", unset = "") == "") {
  Sys.setenv(OPENEO_STALE_OUTPUT_FINALIZE_SEC = "30")
}
message(
  "Async safeguards: timeout=", Sys.getenv("OPENEO_ASYNC_JOB_TIMEOUT_SEC"),
  "s, stale-finalize=", Sys.getenv("OPENEO_STALE_OUTPUT_FINALIZE_SEC"), "s"
)

aws.host <- Sys.getenv("AWSHOST")

if (aws.host == "") {
  aws.host <- NULL
} else {
  message("AWS host port id is: ", aws.host)
}


config <- SessionConfig(api.port = 8000, host = "0.0.0.0", aws.ipv4 = aws.host)
config$workspace.path = paste0(getwd(), "/test_workspace")
createSessionInstance(config)
Session$startSession()
