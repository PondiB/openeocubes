test_that("apply prediction works", {

  # create Session-object for testing
  config <- SessionConfig(api.port = 8000, host = "127.0.0.1")

  # set workspace for testing
  config$workspace.path = paste0(getwd(),"/", test_path("testData"))

  # this silently returns "Session"
  createSessionInstance(config)

  expected_bands = c("predicted_class", "class_confidence")
  expected_classes = c("apply_pixel_cube", "cube", "xptr")

  datacube = test_path("testData", "train_model_test_cube.nc") |> gdalcubes::ncdf_cube()

  # "myModel" must exist in the Session workspace path
  prediction_datacube = spsUtil::quiet(apply_prediction_opp(datacube, model_id = "myModel"))


  expect_equal(class(prediction_datacube), expected_classes)
  expect_equal(names(prediction_datacube), expected_bands)
})
