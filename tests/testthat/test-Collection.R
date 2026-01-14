test_that("Collection class initialization works correctly", {
  collection <- Collection$new(
    id = "test_collection",
    title = "Test Collection",
    description = "A test collection",
    spatialExtent = list(-10, -10, 10, 10),
    temporalExtent = list("2020-01-01", "2020-12-31"),
    bands = c("B01", "B02", "B03"),
    constellation = list("test-satellite")
  )
  
  expect_equal(collection$id, "test_collection")
  expect_equal(collection$title, "Test Collection")
  expect_equal(collection$description, "A test collection")
  expect_equal(collection$spatialExtent, list(-10, -10, 10, 10))
  expect_equal(collection$temporalExtent, list("2020-01-01", "2020-12-31"))
  expect_equal(collection$bands, c("B01", "B02", "B03"))
  expect_equal(collection$constellation, list("test-satellite"))
})

test_that("Collection with default values initializes correctly", {
  collection <- Collection$new()
  
  expect_true(is.na(collection$id))
  expect_true(is.na(collection$title))
  expect_true(is.na(collection$description))
  expect_true(is.na(collection$spatialExtent))
  expect_true(is.na(collection$temporalExtent))
  expect_true(is.na(collection$bands))
  expect_true(is.na(collection$constellation))
})

test_that("is.Collection correctly identifies Collection objects", {
  collection <- Collection$new(id = "test")
  expect_true(is.Collection(collection))
  expect_false(is.Collection("not a collection"))
  expect_false(is.Collection(list()))
  expect_false(is.Collection(NULL))
})

test_that("Collection collectionInfo returns correct structure when Session exists", {
  # Create a mock session configuration
  config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
  
  # Create a temporary session instance for testing
  # Note: This test assumes Session exists or we need to mock it
  # For now, we'll test the structure without requiring Session
  collection <- Collection$new(
    id = "test_collection",
    title = "Test Collection",
    description = "A test collection",
    spatialExtent = list(-10, -10, 10, 10),
    temporalExtent = list("2020-01-01", "2020-12-31")
  )
  
  # Skip this test if Session is not available
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  info <- collection$collectionInfo()
  
  expect_type(info, "list")
  expect_equal(info$id, "test_collection")
  expect_equal(info$title, "Test Collection")
  expect_equal(info$description, "A test collection")
  expect_true("extent" %in% names(info))
  expect_true("links" %in% names(info))
})

test_that("Collection collectionInfoExtended returns correct structure when Session exists", {
  collection <- Collection$new(
    id = "test_collection",
    title = "Test Collection",
    description = "A test collection",
    spatialExtent = list(-10, -10, 10, 10),
    temporalExtent = list("2020-01-01", "2020-12-31"),
    bands = c("B01", "B02"),
    constellation = list("test-satellite")
  )
  
  skip_if_not(exists("Session", envir = .GlobalEnv), "Session not available")
  
  info <- collection$collectionInfoExtended()
  
  expect_type(info, "list")
  expect_equal(info$id, "test_collection")
  expect_true("cube:dimensions" %in% names(info))
  expect_true("summaries" %in% names(info))
})

