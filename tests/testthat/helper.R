# Helper functions for tests
# This file is automatically sourced by testthat

# Helper to create a test Session if it doesn't exist
ensure_test_session <- function() {
  if (!exists("Session", envir = .GlobalEnv)) {
    config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
    config$workspace.path <- tempdir()
    createSessionInstance(config)
  }
  return(Session)
}

# Helper to clean up test Session
cleanup_test_session <- function() {
  if (exists("Session", envir = .GlobalEnv)) {
    rm("Session", envir = .GlobalEnv)
  }
}

