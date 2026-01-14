test_that("SessionConfig creates default configuration", {
  config <- SessionConfig()
  
  expect_equal(class(config), "ServerConfig")
  expect_equal(config$api.port, 8000)
  expect_equal(config$host, "127.0.0.1")
  expect_equal(config$api_version, "1.2.0")
  expect_equal(config$backend_version, "0.1.0")
  expect_equal(config$stac_version, "1.0.0")
})

test_that("SessionConfig with custom port and host works", {
  config <- SessionConfig(api.port = 9000, host = "localhost")
  
  expect_equal(config$api.port, 9000)
  expect_equal(config$host, "localhost")
  expect_equal(config$base_url, "http://localhost:9000")
})

test_that("SessionConfig with 0.0.0.0 host uses localhost in base_url", {
  config <- SessionConfig(api.port = 8000, host = "0.0.0.0")
  
  expect_equal(config$host, "0.0.0.0")
  expect_equal(config$base_url, "http://localhost:8000")
})

test_that("SessionConfig with 0.0.0.0 host and aws.ipv4 uses aws.ipv4", {
  config <- SessionConfig(api.port = 8000, host = "0.0.0.0", aws.ipv4 = "54.123.45.67")
  
  expect_equal(config$host, "0.0.0.0")
  expect_equal(config$base_url, "http://54.123.45.67:8000")
})

test_that("SessionConfig contains outputFormats", {
  config <- SessionConfig()
  
  expect_true("outputFormats" %in% names(config))
  expect_true("GTiff" %in% names(config$outputFormats))
  expect_true("NetCDF" %in% names(config$outputFormats))
  
  expect_equal(config$outputFormats$GTiff$title, "GeoTiff")
  expect_equal(config$outputFormats$NetCDF$title, "Network Common Data Form")
})

test_that("SessionConfig contains inputFormats", {
  config <- SessionConfig()
  
  expect_true("inputFormats" %in% names(config))
  expect_true("GTiff" %in% names(config$inputFormats))
  expect_equal(config$inputFormats$GTiff$title, "GeoTiff")
})

test_that("SessionConfig contains metadata fields", {
  config <- SessionConfig()
  
  expect_equal(config$id, "openeo-gdalcubes-R-backend")
  expect_equal(config$title, "openeo-gdalcubes-R-backend")
  expect_true(nchar(config$description) > 0)
  expect_equal(config$user, "user")
  expect_equal(config$password, "password")
})

