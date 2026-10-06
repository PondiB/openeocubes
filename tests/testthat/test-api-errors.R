# HTTP status codes and error bodies of the API, driven through the router.

valid_token <- "b34ba2bdf9ac9ee1"
bearer <- function(token) paste0("Bearer basic//", token)

expect_openeo_error <- function(out, status, code) {
  expect_equal(out$status, status)
  expect_identical(out$json$code, code)
  expect_true(is.character(out$json$message) && nzchar(out$json$message))
  expect_equal(out$content_type_count, 1L)
}

test_that("job endpoints without a token return 401 AuthenticationRequired", {
  local_api_session()

  expect_openeo_error(api_get("/jobs"), 401L, "AuthenticationRequired")
  expect_openeo_error(api_get("/jobs", method = "POST"), 401L, "AuthenticationRequired")
  expect_openeo_error(api_get("/jobs/some-job"), 401L, "AuthenticationRequired")
})

test_that("job endpoints with a wrong token return 403 TokenInvalid", {
  local_api_session()
  Session$setToken(valid_token)

  expect_openeo_error(api_get("/jobs", authorization = bearer("wrong")), 403L, "TokenInvalid")
})

test_that("a token sent before any login returns 403 instead of failing", {
  local_api_session()
  expect_null(Session$getToken())

  expect_openeo_error(api_get("/jobs", authorization = bearer(valid_token)), 403L, "TokenInvalid")
})

test_that("job endpoints with a valid token pass authentication", {
  local_api_session()
  Session$setToken(valid_token)

  out <- api_get("/jobs", authorization = bearer(valid_token))

  expect_equal(out$status, 200L)
  expect_true("jobs" %in% names(out$json))
})

test_that("unknown jobs return 404 JobNotFound", {
  local_api_session()
  Session$setToken(valid_token)

  expect_openeo_error(api_get("/jobs/does-not-exist", authorization = bearer(valid_token)), 404L, "JobNotFound")
  expect_openeo_error(api_get("/jobs/does-not-exist/results", authorization = bearer(valid_token)), 404L, "JobNotFound")
})

test_that("unknown collections return 404 CollectionNotFound with a single Content-Type", {
  local_api_session()

  out <- api_get("/collections/does-not-exist")

  expect_openeo_error(out, 404L, "CollectionNotFound")
  expect_match(out$json$message, "does-not-exist", fixed = TRUE)
})

test_that("known collections are still served", {
  local_api_session()

  out <- api_get("/collections/sentinel-2-l2a")

  expect_equal(out$status, 200L)
  expect_identical(out$json$id, "sentinel-2-l2a")
  expect_equal(out$content_type_count, 1L)
})

test_that("CORS preflight returns 204 with CORS headers and no body", {
  local_api_session()

  out <- api_get("/jobs", method = "OPTIONS")

  expect_equal(out$status, 204L)
  expect_identical(out$text, "")
  expect_true("Access-Control-Allow-Methods" %in% names(out$headers))
})

test_that("unknown routes return 404 NotFound", {
  local_api_session()

  expect_openeo_error(api_get("/does/not/exist"), 404L, "NotFound")
})

test_that("handleError returns the openEO error body outside a request", {
  body <- suppressMessages(openeocubes:::handleError(
    tryCatch(throwError("JobNotFound"), error = function(e) e)
  ))
  expect_identical(body, list(code = "JobNotFound", message = "JobNotFound"))

  body <- suppressMessages(openeocubes:::handleError(simpleError("boom")))
  expect_identical(body, list(code = "Internal", message = "boom"))
})

test_that("JSON endpoints send exactly one Content-Type header", {
  local_api_session()

  for (path in c("/", "/collections", "/processes", "/file_formats", "/conformance", "/.well-known/openeo")) {
    out <- api_get(path)
    expect_equal(out$status, 200L, info = path)
    expect_equal(out$content_type_count, 1L, info = path)
  }
})
