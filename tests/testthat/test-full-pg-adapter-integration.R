ensure_api_processes_for_graph_tests <- function() {
  testthat::skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  Session$processes <- list()
  needed_processes <- list(
    load_collection,
    save_result,
    aggregate_temporal_period,
    aggregate_spatial,
    array_element,
    filter_bands,
    ndvi,
    reduce_dimension,
    median,
    mask,
    mask_scl,
    ml_fit,
    ml_predict,
    mlm_class_random_forest
  )
  for (process in needed_processes) {
    Session$assignProcess(process)
  }
}

read_json_graph <- function(path) {
  if (!file.exists(path)) {
    testthat::skip(paste("Fixture not found:", path))
  }
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
}

test_that("raw full_pg fails compilation before adapter rewrite", {
  ensure_api_processes_for_graph_tests()

  full_pg_path <- testthat::test_path("..", "..", "examples", "13-ml-process-graph", "full_pg.json")
  raw_graph <- read_json_graph(full_pg_path)
  wrapped <- list(process_graph = raw_graph)
  process_graph <- ProcessGraph$new(process_graph = wrapped)

  expect_error(
    process_graph$buildExecutableProcessGraph(job = Job$new()),
    "Cannot load process|Unable to find"
  )
})

test_that("adapter allows full_pg to compile into executable graph", {
  ensure_api_processes_for_graph_tests()

  full_pg_path <- testthat::test_path("..", "..", "examples", "13-ml-process-graph", "full_pg.json")
  raw_graph <- read_json_graph(full_pg_path)
  adapted <- adaptProcessGraph(raw_graph)
  process_graph <- ProcessGraph$new(process_graph = adapted)

  executable <- process_graph$buildExecutableProcessGraph(job = Job$new())
  expect_true(is.ExecutableProcess(executable))
})

test_that("minimal full_pg fixture compiles after adapter rewrite", {
  ensure_api_processes_for_graph_tests()

  minimal_path <- testthat::test_path("..", "fixtures", "full_pg_minimal.json")
  raw_graph <- read_json_graph(minimal_path)
  adapted <- adaptProcessGraph(raw_graph)

  expect_true("process_graph" %in% names(adapted))
  process_graph <- ProcessGraph$new(process_graph = adapted)
  executable <- process_graph$buildExecutableProcessGraph(job = Job$new())
  expect_true(is.ExecutableProcess(executable))
})

test_that("adapted graph job exposes prediction asset path", {
  ensure_api_processes_for_graph_tests()
  Session$jobs <- list()

  minimal_path <- testthat::test_path("..", "fixtures", "full_pg_minimal.json")
  raw_graph <- read_json_graph(minimal_path)
  adapted <- adaptProcessGraph(raw_graph)
  job <- Job$new(process = adapted)
  Session$assignJob(job)

  job_results_dir <- file.path(Session$getConfig()$workspace.path, "jobs", job$id)
  dir.create(job_results_dir, recursive = TRUE, showWarnings = FALSE)
  tif_path <- file.path(job_results_dir, "prediction_source.tif")
  writeBin(as.raw(c(0x49, 0x49, 0x2A, 0x00)), tif_path)

  results <- openeocubes:::.getJobResults(
    req = list(),
    res = list(),
    job_id = job$id
  )

  expect_true("prediction.tif" %in% names(results$assets))
  expect_match(results$assets$prediction.tif$href, paste0("/jobs/", job$id, "/"))
})
