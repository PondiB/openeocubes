test_that(".jobExportRasterFiles prefers prediction-prefixed export tifs", {
  files <- c(
    "cube_abc2017-05-01.tif",
    "prediction2017-05-01.tif",
    "prediction2017-06-01.tif"
  )
  export <- .jobExportRasterFiles(files)
  expect_equal(export, c("prediction2017-05-01.tif", "prediction2017-06-01.tif"))
})

test_that(".jobExportRasterFiles excludes intermediate cube cache tifs", {
  files <- c(
    "cube_abc2017-05-01.tif",
    "cube_abc2017-06-01.tif",
    "prediction_classes.json",
    "worker_stderr.log",
    "result_12ab34cd.tif"
  )
  export <- .jobExportRasterFiles(files)
  expect_equal(export, "result_12ab34cd.tif")
  expect_false(any(grepl("^cube_", export)))
})

test_that(".jobExportRasterFiles returns empty when only cube cache exists", {
  files <- c("cube_abc2017-05-01.tif", "prediction_classes.json")
  expect_equal(.jobExportRasterFiles(files), character(0))
})
