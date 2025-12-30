test_that("Process class initialization works correctly", {
  process <- Process$new(
    id = "test_process",
    summary = "Test summary",
    description = "Test description",
    categories = c("test", "category"),
    parameters = list(),
    returns = list(type = "string"),
    operation = function() { return("test") }
  )
  
  expect_equal(process$id, "test_process")
  expect_equal(process$summary, "Test summary")
  expect_equal(process$description, "Test description")
  expect_equal(process$categories, c("test", "category"))
  expect_equal(process$returns, list(type = "string"))
  expect_type(process$operation, "closure")
})

test_that("Process with default values initializes correctly", {
  process <- Process$new()
  
  expect_true(is.na(process$id))
  expect_true(is.na(process$summary))
  expect_true(is.na(process$description))
  expect_true(is.na(process$categories))
  expect_true(is.na(process$parameters))
  expect_true(is.na(process$returns))
  expect_true(is.na(process$operation))
})

test_that("Process processInfo returns correct structure", {
  param1 <- Parameter$new(name = "param1", description = "First param", schema = list(type = "string"))
  param2 <- Parameter$new(name = "param2", description = "Second param", schema = list(type = "number"))
  
  process <- Process$new(
    id = "test_process",
    summary = "Test summary",
    description = "Test description",
    categories = c("test"),
    parameters = list(param1, param2),
    returns = list(type = "string")
  )
  
  info <- process$processInfo()
  
  expect_type(info, "list")
  expect_equal(info$id, "test_process")
  expect_equal(info$summary, "Test summary")
  expect_equal(info$description, "Test description")
  expect_equal(info$categories, c("test"))
  expect_length(info$parameters, 2)
  expect_equal(info$returns, list(type = "string"))
})

test_that("Process setParameter works correctly", {
  param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
  process <- Process$new(
    id = "test_process",
    parameters = list(param)
  )
  
  process$setParameter("test_param", "test_value")
  
  expect_equal(process$parameters[[1]]$value, "test_value")
})

test_that("Process setParameter throws error for invalid parameter name", {
  param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
  process <- Process$new(
    id = "test_process",
    parameters = list(param)
  )
  
  expect_error(
    process$setParameter("invalid_param", "value"),
    "Unable to find: 'invalid_param' in process 'test_process'"
  )
})

test_that("Process setParameter returns self invisibly", {
  param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
  process <- Process$new(
    id = "test_process",
    parameters = list(param)
  )
  
  result <- process$setParameter("test_param", "test_value")
  expect_identical(result, process)
})

test_that("is.Process correctly identifies Process objects", {
  process <- Process$new(id = "test")
  expect_true(is.Process(process))
  expect_false(is.Process("not a process"))
  expect_false(is.Process(list()))
  expect_false(is.Process(NULL))
})

