#' @export
apply_prediction_opp = function(data, model_id, keep_bands = FALSE, job) {

  message("\napply_prediction called...")

  message("\nCall parameters:")
  message("\ndata:")
  print(data)

  message("\nmodel_id:")
  message(model_id)

  tryCatch({
    message("\nTry loading the model from user workspace...")

    path_to_model = paste0(Session$getConfig()$workspace.path, "/", model_id, ".rds")

    message("Path to model: ", path_to_model)

    # get model from user workspace
    model = readRDS(path_to_model)

    message("Model found in: ", path_to_model)
    message("\nModel loaded successfully!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop("No Model found!")
  })

  # get band names for to later create a data.frame
  band_names = names(data)

  # the model and band_names need to be loaded into a tempdir, so that it can be accessed in a new process in "FUN" from "apply_pixel" (see below)
  t = tempdir()

  message("Current tempdir: ", t)

  # save variable in tempdir
  saveRDS(model, paste0(t, "/model.rds"))
  saveRDS(band_names, paste0(t, "/band_names.rds"))

  # save tempdir string to later retrieve files in another process
  Sys.setenv(TEMP_DIR = t)

  # creates two bands "predicted_classes", "class_confidence" in the datacube
  cube = gdalcubes::apply_pixel(
    data,
    names = c("predicted_class", "class_confidence"),
    FUN = function(band_values_vector)
    {
      library(caret)
      library(stringr)
      library(randomForest)

      # load tempdir path from Global Env, to ensure its the same as in the process above
      tmp = Sys.getenv("TEMP_DIR")
      message("Tempdir in FUN: ", tmp)

      # load variables needed for prediction
      model = readRDS(paste0(tmp, "/model.rds"))
      band_names = readRDS(paste0(tmp, "/band_names.rds"))

      # named vector for df creation
      named_vector = setNames(band_values_vector, band_names)

      # create 1-row df per pixel of the datacube
      band_value_df = named_vector |> t() |> as.data.frame()

      tryCatch({
        predicted_class = stats::predict(model, newdata = band_value_df)
        class_confidence = stats::predict(model, newdata = band_value_df, type = "prob")

        # parse Integer value from string
        predicted_class <- predicted_class |>
          base::as.character() |>
          stringr::str_extract_all("\\d+") |>
          base::as.numeric()

        # determine confidence value for the classified class
        highest_class_confidence = base::apply(class_confidence, 1, base::max)

        return(c(predicted_class, highest_class_confidence))
      },
      error = function(err)
      {
        stop("Error in apply_pixel!")
      })

    },
    keep_bands = keep_bands)


  message("\nDatacube: ")
  print(cube)

  return(cube)
}


#' apply_prediction
apply_prediction <- Process$new(
  id = "apply_prediction",
  description = "Apply a machine-learning model on each pixel of the datacube. This creates 2 new bands in the cube containing the predicted classes per pixel and the propability of the predicted class (class confidence). Bands of the source cube can optionally be included. This Algorithm will only save integer values in the datacube. If the levels of the model are factor values without an integer part like: 'expl', gdalcubes will assign a integer value instead. Therefore it is advised to provide a model with levels in the form of 'X1' 'X2', ..., 'Xn'.",
  categories = as.array("cubes", "machine-learning"),
  summary = "Apply a machine-learning based prediction on a datacube",
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
      name = "model_id",
      description = "Id of the model that should be used for prediction. The model will be searched in the user workspace.",
      schema = list(
        type = "string"
      )
    ),
    Parameter$new(
      name = "keep_bands",
      description = "Keep bands of input data cube, defaults to FALSE, i.e. original bands will be dropped.",
      schema = list(
        type = "boolean"
      )
    )
  ),
  returns = eo_datacube,
  operation = apply_prediction_opp
)
