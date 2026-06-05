ensure_api_processes <- function() {
  testthat::skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  Session$processes <- list()
  Session$assignProcess(load_collection)
  Session$assignProcess(mask)
  Session$assignProcess(mask_scl)
}

test_that("mask process uses OpenEO-compatible API parameters", {
  expect_true(is.Process(mask))

  mask_param_names <- vapply(mask$parameters, function(p) p$name, character(1))
  expect_equal(mask_param_names, c("data", "mask", "replacement"))
})

test_that("mask_scl process preserves legacy SCL parameters", {
  expect_true(is.Process(mask_scl))

  mask_scl_param_names <- vapply(mask_scl$parameters, function(p) p$name, character(1))
  expect_equal(mask_scl_param_names, c("data", "invalid_values"))
})

test_that("mask still rejects unsupported argument names", {
  executable <- mask$clone(deep = TRUE)
  expect_error(
    executable$setParameter("invalid_values", c(3, 8, 9)),
    "Unable to find"
  )
})

test_that("process discovery includes both mask and mask_scl", {
  ensure_api_processes()
  process_info <- openeocubes:::.processes()
  process_ids <- vapply(process_info$processes, function(process) process$id, character(1))

  expect_true("mask" %in% process_ids)
  expect_true("mask_scl" %in% process_ids)
})

test_that("graph with OpenEO mask arguments compiles", {
  ensure_api_processes()

  graph <- list(
    process_graph = list(
      load1 = list(
        process_id = "load_collection",
        arguments = list(
          id = "sentinel-2-l2a",
          spatial_extent = list(west = 0, south = 0, east = 1, north = 1, crs = 4326),
          temporal_extent = list("2020-01-01", "2020-02-01"),
          bands = list("red", "nir", "scl")
        )
      ),
      mask1 = list(
        process_id = "mask",
        arguments = list(
          data = list(from_node = "load1"),
          mask = list(from_node = "load1"),
          replacement = NULL
        ),
        result = TRUE
      )
    )
  )

  executable <- ProcessGraph$new(process_graph = graph)$buildExecutableProcessGraph(job = Job$new())
  expect_true(is.ExecutableProcess(executable))
})
