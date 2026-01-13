test_that("Parameter class initialization works correctly", {
  param <- Parameter$new(
    name = "test_param",
    description = "A test parameter",
    schema = list(type = "string"),
    optional = TRUE
  )
  
  expect_equal(param$name, "test_param")
  expect_equal(param$description, "A test parameter")
  expect_equal(param$schema, list(type = "string"))
  expect_true(param$optional)
  expect_null(param$value)
  expect_null(param$parameters)
})

test_that("Parameter with default values initializes correctly", {
  param <- Parameter$new()
  
  expect_true(is.na(param$name))
  expect_true(is.na(param$description))
  expect_true(is.na(param$schema))
  expect_false(param$optional)
  expect_null(param$value)
  expect_null(param$parameters)
})

test_that("Parameter parameterInfo returns correct structure", {
  param <- Parameter$new(
    name = "test_param",
    description = "A test parameter",
    schema = list(type = "string"),
    optional = TRUE
  )
  
  info <- param$parameterInfo()
  
  expect_type(info, "list")
  expect_equal(info$name, "test_param")
  expect_equal(info$description, "A test parameter")
  expect_equal(info$schema, list(type = "string"))
  expect_true(info$optional)
})

test_that("Parameter parameterInfo handles nested parameters", {
  nested_schema <- list(
    type = "object",
    subtype = "parameters",
    parameters = list(
      Parameter$new(name = "nested1", description = "Nested param 1", schema = list(type = "string")),
      Parameter$new(name = "nested2", description = "Nested param 2", schema = list(type = "number"))
    )
  )
  
  param <- Parameter$new(
    name = "parent_param",
    description = "Parent parameter",
    schema = nested_schema,
    optional = FALSE
  )
  
  info <- param$parameterInfo()
  
  expect_type(info, "list")
  expect_equal(info$name, "parent_param")
  expect_true("parameters" %in% names(info$schema))
  expect_length(info$schema$parameters, 2)
})

test_that("appendInfo function works correctly", {
  info <- appendInfo(
    name = "test",
    description = "Test description",
    schema = list(type = "string"),
    optional = TRUE
  )
  
  expect_type(info, "list")
  expect_equal(info$name, "test")
  expect_equal(info$description, "Test description")
  expect_equal(info$schema, list(type = "string"))
  expect_true(info$optional)
})

test_that("appendInfo handles NA optional parameter", {
  info <- appendInfo(
    name = "test",
    description = "Test description",
    schema = list(type = "string"),
    optional = NA
  )
  
  expect_false("optional" %in% names(info))
})

