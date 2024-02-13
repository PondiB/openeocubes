test_that("train model works", {

  # create Session-object for testing
  config <- SessionConfig(api.port = 8000, host = "127.0.0.1")

  # set workspace for testing
  config$workspace.path = base::paste0(base::getwd(),"/", test_path("testData"))

  # this silently returns "Session"
  createSessionInstance(config)

  datacube = gdalcubes::ncdf_cube(test_path("testData", "train_model_test_cube.nc"))

  training_data_path = test_path("testData", "classes.geojson")
  training_data = sf::st_read(training_data_path, quiet = TRUE)

  training_data_class_levels = base::unique(training_data$class) |>
    base::as.numeric() |>
    base::sort()

  model = spsUtil:::quiet(train_model_opp(
    data = datacube,
    model_type = "RF",
    labeled_polygons = training_data_path,
    hyperparameters = base::list(mtry = 5, ntree = 50),
    ))

  # get model class levels and parse Integer value
  model_class_levels = base::levels(model$finalModel$y) |>
    base::as.character() |>
    stringr::str_extract_all("\\d+") |>
    base::as.numeric() |>
    base::sort()

  expect_equal(class(model), c("train", "train.formula"))
  expect_equal(model_class_levels, training_data_class_levels)
})
