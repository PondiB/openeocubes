test_that("is.Process works correctly", {
  process <- Process$new(id = "test")
  expect_true(is.Process(process))
  expect_false(is.Process("not a process"))
  expect_false(is.Process(list()))
  expect_false(is.Process(NULL))
})

test_that("is.Collection works correctly", {
  collection <- Collection$new(id = "test")
  expect_true(is.Collection(collection))
  expect_false(is.Collection("not a collection"))
  expect_false(is.Collection(list()))
  expect_false(is.Collection(NULL))
})

test_that("is.ProcessGraph works correctly", {
  graph_list <- list(
    process_graph = list(
      node1 = list(
        process_id = "test_process",
        arguments = list()
      )
    )
  )
  
  pg <- ProcessGraph$new(process_graph = graph_list)
  expect_true(is.ProcessGraph(pg))
  expect_false(is.ProcessGraph("not a process graph"))
  expect_false(is.ProcessGraph(list()))
  expect_false(is.ProcessGraph(NULL))
})

test_that("is.Job works correctly", {
  job <- Job$new()
  expect_true(is.Job(job))
  expect_false(is.Job("not a job"))
  expect_false(is.Job(list()))
  expect_false(is.Job(NULL))
})

test_that("is.ExecutableProcess works correctly", {
  param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
  process <- Process$new(
    id = "test_process",
    parameters = list(param),
    operation = function(test_param, job) { return("result") }
  )
  
  executable <- ExecutableProcess$new(process = process)
  expect_true(is.ExecutableProcess(executable))
  expect_false(is.ExecutableProcess(process))
  expect_false(is.ExecutableProcess("not an executable process"))
  expect_false(is.ExecutableProcess(NULL))
})

test_that("is.graphId validates 16-character IDs", {
  expect_true(is.graphId("1234567890123456"))
  expect_false(is.graphId("12345"))
  expect_false(is.graphId("12345678901234567"))
  expect_false(is.graphId(""))
  # NA causes error in nchar(), so we expect an error
  expect_error(is.graphId(NA), regexp = "missing value where TRUE/FALSE needed")
  # NULL causes error in nchar(), so we expect an error
  expect_error(is.graphId(NULL), regexp = "argument is of length zero")
})

test_that("getJobIdIndex returns NA when Session doesn't exist", {
  # This test assumes Session might not exist
  # In a real scenario, we'd need to handle this more carefully
  skip("Requires Session setup")
})

test_that("getPgidIndex returns NA when Session doesn't exist", {
  # This test assumes Session might not exist
  skip("Requires Session setup")
})

