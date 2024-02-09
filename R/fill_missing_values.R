#' @export
fill_missing_values_opp = function(data, method, job) {
  message("Call parameters:")
  message("\ndata:")
  message(gdalcubes::as_json(data))

  message("\nmethod:")
  message(method)

  valid_methods <- c("near", "linear", "locf", "nocb")
  if (!(method %in% valid_methods)) {
    stop(paste("Invalid method. Please choose one of", toString(valid_methods)))
  }

  tryCatch({
    message("\nFill NA values...")

    gdalcubes::fill_time(data, method)

    message("NA values filled!")

  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop(toString(err$message))
  })

  return(data)
}


#' fill_missing_values
fill_missing_values <- Process$new(
  id = "fill_missing_values",
  description = "Fill NA data cube pixels by simple time series interpolation.",
  categories = as.array("cubes"),
  summary = "Fill out missing values",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A data cube with bands.",
      schema = list(
        type = "object",
        subtype = "raster-cube"
      )
    ),
    Parameter$new(
      name = "method",
      description = "Method of interpolation to fill NA values. Valid methods are: near,linear, locf, nocb. Look up the gdalcubes reference for further information.",
      schema = list(
        type = "string"
      ),
    )
  ),
  returns = eo_datacube,
  operation = fill_missing_values_opp
)
