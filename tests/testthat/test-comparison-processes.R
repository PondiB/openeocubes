run_comparison_process <- function(process, args) {
  params <- lapply(process$parameters, function(par) {
    cloned <- par$clone(deep = TRUE)
    if (par$name %in% names(args)) {
      cloned$value <- args[[par$name]]
    }
    cloned
  })
  executable <- Process$new(
    id = process$id,
    parameters = params,
    operation = process$operation
  )
  ExecutableProcess$new(process = executable)$execute()
}

test_that("eq follows OpenEO comparison examples", {
  expect_null(run_comparison_process(eq, list(x = 1, y = NULL)))
  expect_true(run_comparison_process(eq, list(x = 1, y = 1)))
  expect_false(run_comparison_process(eq, list(x = 1, y = "1")))
  expect_false(run_comparison_process(eq, list(x = 0, y = FALSE)))
  expect_false(run_comparison_process(eq, list(x = 1.02, y = 1, delta = 0.01)))
  expect_true(run_comparison_process(eq, list(x = -1, y = -1.001, delta = 0.01)))
  expect_true(run_comparison_process(eq, list(x = 115, y = 110, delta = 10)))
  expect_false(run_comparison_process(eq, list(x = "Test", y = "test")))
  expect_true(run_comparison_process(eq, list(x = "Test", y = "test", case_sensitive = FALSE)))
  expect_true(run_comparison_process(eq, list(x = "\u00c4", y = "\u00e4", case_sensitive = FALSE)))
  expect_false(run_comparison_process(
    eq,
    list(x = "2018-01-01T00:00:00Z", y = "2018-01-01T00:00:00+00:00")
  ))
})

test_that("lte follows OpenEO comparison examples", {
  expect_null(run_comparison_process(lte, list(x = 1, y = NULL)))
  expect_true(run_comparison_process(lte, list(x = 0, y = 0)))
  expect_true(run_comparison_process(lte, list(x = 1, y = 2)))
  expect_false(run_comparison_process(lte, list(x = -0.5, y = -0.6)))
  expect_false(run_comparison_process(
    lte,
    list(x = "2018-01-01T00:00:00Z", y = "2018-01-01T00:00:00+00:00")
  ))
  expect_false(run_comparison_process(lte, list(x = FALSE, y = TRUE)))
})

test_that("or follows OpenEO logic examples", {
  expect_true(run_comparison_process(or, list(x = TRUE, y = TRUE)))
  expect_false(run_comparison_process(or, list(x = FALSE, y = FALSE)))
  expect_true(run_comparison_process(or, list(x = TRUE, y = NULL)))
  expect_true(run_comparison_process(or, list(x = NULL, y = TRUE)))
  expect_null(run_comparison_process(or, list(x = FALSE, y = NULL)))
})

test_that("comparison processes are registered in the API", {
  testthat::skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  Session$processes <- list()
  Session$assignProcess(eq)
  Session$assignProcess(lte)
  Session$assignProcess(or)

  expect_true("eq" %in% names(Session$processes))
  expect_true("lte" %in% names(Session$processes))
  expect_true("or" %in% names(Session$processes))
})

test_that("nested eq/or graph compiles when comparison processes are registered", {
  testthat::skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  Session$processes <- list()
  Session$assignProcess(eq)
  Session$assignProcess(or)

  graph <- list(
    process_graph = list(
      eq1 = list(
        process_id = "eq",
        arguments = list(x = 3, y = 3)
      ),
      eq2 = list(
        process_id = "eq",
        arguments = list(x = 5, y = 3)
      ),
      or1 = list(
        process_id = "or",
        arguments = list(
          x = list(from_node = "eq1"),
          y = list(from_node = "eq2")
        ),
        result = TRUE
      )
    )
  )

  executable <- ProcessGraph$new(process_graph = graph)$buildExecutableProcessGraph(job = Job$new())
  expect_true(is.ExecutableProcess(executable))
  expect_true(executable$execute())
})
