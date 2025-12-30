# Setup file for testthat tests
# This file runs before each test file

# Create a Session instance for tests that require it
# Note: This will be available in the global environment during tests
if (!exists("Session", envir = .GlobalEnv)) {
    config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
    config$workspace.path <- tempdir()
    createSessionInstance(config)
}

# Ensure Session lists are initialized (they should be, but just in case)
if (exists("Session", envir = .GlobalEnv)) {
    if (is.null(Session$jobs)) {
        Session$jobs <- list()
    }
    if (is.null(Session$data)) {
        Session$data <- list()
    }
    if (is.null(Session$processes)) {
        Session$processes <- list()
    }
    if (is.null(Session$graphs)) {
        Session$graphs <- list()
    }
}
