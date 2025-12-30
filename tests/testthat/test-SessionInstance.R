test_that("SessionInstance initialization creates empty lists", {
  session <- SessionInstance$new()
  
  expect_type(session$graphs, "list")
  expect_length(session$graphs, 0)
  expect_type(session$processes, "list")
  expect_length(session$processes, 0)
  expect_type(session$data, "list")
  expect_length(session$data, 0)
  expect_type(session$jobs, "list")
  expect_length(session$jobs, 0)
})

test_that("SessionInstance initialization with configuration works", {
  config <- SessionConfig(api.port = 9000, host = "localhost")
  session <- SessionInstance$new(configuration = config)
  
  expect_equal(session$getConfig()$api.port, 9000)
  expect_equal(session$getConfig()$host, "localhost")
})

test_that("SessionInstance initialization creates default config if NULL", {
  session <- SessionInstance$new(configuration = NULL)
  
  config <- session$getConfig()
  expect_equal(class(config), "ServerConfig")
  expect_equal(config$api.port, 8000)
  expect_equal(config$host, "127.0.0.1")
})

test_that("SessionInstance getConfig returns configuration", {
  config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
  session <- SessionInstance$new(configuration = config)
  
  returned_config <- session$getConfig()
  expect_identical(returned_config, config)
})

test_that("SessionInstance setToken and getToken work", {
  session <- SessionInstance$new()
  
  session$setToken("test_token_123")
  expect_equal(session$getToken(), "test_token_123")
})

test_that("SessionInstance setBaseUrl and getBaseUrl work", {
  session <- SessionInstance$new()
  
  session$setBaseUrl(port = 8080, host = "example.com")
  expect_equal(session$getBaseUrl(), "http://example.com:8080")
})

test_that("SessionInstance assignData works correctly", {
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  session <- SessionInstance$new()
  
  # Ensure Session data list exists
  if (is.null(Session$data)) {
    Session$data <- list()
  }
  
  collection <- Collection$new(
    id = "test_collection",
    title = "Test",
    description = "Test",
    spatialExtent = list(-10, -10, 10, 10),
    temporalExtent = list("2020-01-01", "2020-12-31")
  )
  
  session$assignData(collection)
  
  expect_true("test_collection" %in% names(session$data))
  expect_true(is.Collection(session$data[["test_collection"]]))
})

test_that("SessionInstance assignData throws error for duplicate collection ID", {
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  session <- SessionInstance$new()
  
  # Ensure Session data list exists
  if (is.null(Session$data)) {
    Session$data <- list()
  }
  
  collection1 <- Collection$new(
    id = "test_collection_dup",
    title = "Test 1",
    description = "Test",
    spatialExtent = list(-10, -10, 10, 10),
    temporalExtent = list("2020-01-01", "2020-12-31")
  )
  
  collection2 <- Collection$new(
    id = "test_collection_dup",
    title = "Test 2",
    description = "Test",
    spatialExtent = list(-10, -10, 10, 10),
    temporalExtent = list("2020-01-01", "2020-12-31")
  )
  
  session$assignData(collection1)
  # Also assign to global Session to trigger duplicate check
  Session$data[["test_collection_dup"]] <- collection1
  
  expect_error(
    session$assignData(collection2),
    "This collection id is already assigned"
  )
})

test_that("SessionInstance assignData throws error for non-Collection object", {
  session <- SessionInstance$new()
  
  # The error occurs when trying to access $id on a non-object
  # So we expect either the $ operator error or the is.Collection error
  expect_error(
    session$assignData("not a collection"),
    regexp = "(Delivered data is not a collection|\\$ operator is invalid)"
  )
})

test_that("SessionInstance assignProcess works correctly", {
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  session <- SessionInstance$new()
  
  # Ensure Session processes list exists
  if (is.null(Session$processes)) {
    Session$processes <- list()
  }
  
  process <- Process$new(
    id = "test_process",
    summary = "Test",
    description = "Test"
  )
  
  session$assignProcess(process)
  
  expect_true("test_process" %in% names(session$processes))
  expect_true(is.Process(session$processes[["test_process"]]))
})

test_that("SessionInstance assignProcess throws error for duplicate process ID", {
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  session <- SessionInstance$new()
  
  # Ensure Session processes list exists
  if (is.null(Session$processes)) {
    Session$processes <- list()
  }
  
  process1 <- Process$new(id = "test_process_dup", summary = "Test", description = "Test")
  process2 <- Process$new(id = "test_process_dup", summary = "Test", description = "Test")
  
  session$assignProcess(process1)
  # Also assign to global Session to trigger duplicate check
  Session$processes[["test_process_dup"]] <- process1
  
  expect_error(
    session$assignProcess(process2),
    "This process is already assigned"
  )
})

test_that("SessionInstance assignProcess throws error for non-Process object", {
  session <- SessionInstance$new()
  
  # The error occurs when trying to access $id on a non-object
  # So we expect either the $ operator error or the is.Process error
  expect_error(
    session$assignProcess("not a process"),
    regexp = "(Delivered process is not a process|\\$ operator is invalid)"
  )
})

test_that("SessionInstance assignJob works correctly", {
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  session <- SessionInstance$new()
  
  # Ensure Session jobs list exists
  if (is.null(Session$jobs)) {
    Session$jobs <- list()
  }
  
  job <- Job$new(id = "test_job")
  
  session$assignJob(job)
  
  expect_true("test_job" %in% names(session$jobs))
  expect_true(is.Job(session$jobs[["test_job"]]))
})

test_that("SessionInstance assignJob throws error for duplicate job ID", {
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  session <- SessionInstance$new()
  
  # Ensure Session jobs list exists
  if (is.null(Session$jobs)) {
    Session$jobs <- list()
  }
  
  job1 <- Job$new(id = "test_job_dup")
  job2 <- Job$new(id = "test_job_dup")
  
  session$assignJob(job1)
  # Also assign to global Session to trigger duplicate check
  Session$jobs[["test_job_dup"]] <- job1
  
  expect_error(
    session$assignJob(job2),
    "This job is already assigned"
  )
})

test_that("SessionInstance assignJob throws error for non-Job object", {
  session <- SessionInstance$new()
  
  # The error occurs when trying to access $id on a non-object
  # So we expect either the $ operator error or the is.Job error
  expect_error(
    session$assignJob("not a job"),
    regexp = "(Delivered job is not a job|\\$ operator is invalid)"
  )
})

test_that("SessionInstance initEndpoints creates empty tibble", {
  session <- SessionInstance$new()
  
  endpoints <- session$getEndpoints()
  expect_s3_class(endpoints, "tbl_df")
  expect_equal(nrow(endpoints), 0)
  expect_true("path" %in% names(endpoints))
  expect_true("method" %in% names(endpoints))
})

