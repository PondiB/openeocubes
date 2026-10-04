# Helpers for tests that drive the plumber router in-process.

# Helper files are evaluated in an environment that does not see
# .GlobalEnv, where createSessionInstance() puts `Session`.
global_session <- function() get("Session", envir = .GlobalEnv)

# Session with a router and all endpoints registered; restores the shared
# test Session afterwards.
local_api_session <- function(env = parent.frame()) {
  if (exists("Session", envir = .GlobalEnv)) {
    old <- get("Session", envir = .GlobalEnv)
    withr::defer(assign("Session", old, envir = .GlobalEnv), envir = env)
  } else {
    withr::defer(rm("Session", envir = .GlobalEnv), envir = env)
  }
  config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
  config$workspace.path <- tempdir()
  createSessionInstance(config)
  global_session()$.__enclos_env__$private$initRouter()
  suppressMessages(addEndpoint())
  global_session()
}

fake_res <- function() {
  res <- new.env()
  res$status <- 200L
  res$headers <- list()
  res$setHeader <- function(name, value) res$headers[[name]] <- value
  res
}

api_get <- function(path, query = "", authorization = NULL, method = "GET") {
  req <- new.env()
  req$REQUEST_METHOD <- method
  req$PATH_INFO <- path
  req$QUERY_STRING <- query
  req$HTTP_ORIGIN <- "http://localhost"
  req$rook.input <- list(read = function(...) raw(0), rewind = function() 0, read_lines = function() character(0))
  if (!is.null(authorization)) req$HTTP_AUTHORIZATION <- authorization

  out <- suppressMessages(global_session()$.__enclos_env__$private$router$call(req))
  body <- if (is.raw(out$body)) rawToChar(out$body) else paste(out$body, collapse = "")
  list(
    status = out$status,
    content_type = out$headers[["Content-Type"]],
    content_type_count = sum(tolower(names(out$headers)) == "content-type"),
    headers = out$headers,
    text = body,
    json = if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else NULL
  )
}
