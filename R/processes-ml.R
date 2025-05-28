#' @include Process-class.R
#' @import gdalcubes
NULL

#' Get Data Cube Schema
#'
#' Returns a list containing the description and schema for a data cube object.
#' This schema is used to define the structure of data cubes in machine learning processes.
#'
#' @return A list containing:
#' \itemize{
#'   \item{description}{A character string describing the data cube}
#'   \item{schema}{A list defining the type and subtype of the data cube}
#' }
#' @export
ml_datacube_schema <- function() {
    info <- list(
        description = "A data cube with the predicted values. It removes the specified dimensions and adds new dimension for the predicted values.",
        schema = list(type = "object", subtype = "datacube")
    )
    return(info)
}

#' Get ML Model Schema
#'
#' Returns a list containing the description and schema for a machine learning model object.
#' This schema is used to define the structure of ML models in the system.
#'
#' @return A list containing:
#' \itemize{
#'   \item{description}{A character string describing the ML model}
#'   \item{schema}{A list defining the type and subtype of the ML model}
#' }
#' @export
ml_model_schema <- function() {
    info <- list(
        description = "A ML model that was trained with one of the ML training processes.",
        schema = list(type = "object", subtype = "mlm-model")
    )
    return(info)
}

# Global objects for process definitions
eo_datacube <- ml_datacube_schema()
ml_model <- ml_model_schema()

#' Machine Learning Prediction Process
#'
#' Applies a trained machine learning model to a data cube of input features and returns predictions.
#' This process handles the prediction workflow and error handling for ML model applications.
#'
#' @param data A data cube containing input features for prediction
#' @param model A trained machine learning model to use for prediction
#' @param dimension Optional dimensions to reduce by the model
#' @return A data cube containing the predicted values
#' @export
ml_predict <- Process$new(
    id = "ml_predict",
    description = "Applies a machine learning model to a data cube of input features and returns the predicted values.",
    categories = as.array("cubes"),
    summary = "Predict using ML",
    parameters = list(
        Parameter$new(
            name = "data",
            description = "A data cube with bands.",
            schema = list(
                type = "object",
                subtype = "datacube"
            )
        ),
        Parameter$new(
            name = "model",
            description = "The current name of the dimension.",
            schema = ml_model,
            optional = FALSE
        ),
        Parameter$new(
            name = "dimensions",
            description = "Zero or more dimensions that will be reduced by the model. Fails with a `DimensionNotAvailable` exception if one of the specified dimensions does not exist.",
            schema = list(
                type = list("string", "array")
            ),
            optional = FALSE
        )
    ),
    returns = eo_datacube,
    operation = function(data, model, dimension = NULL) {
        tryCatch(
            {
                # Perform prediction using gdalcubes
                prediction <- gdalcubes::predict(aoi_cube, model)
                print(prediction)
                message("Prediction calculated ....")
                message(gdalcubes::as_json(prediction))

                return(prediction)
            },
            error = function(e) {
                message("Error in prediction: ")
                message(conditionMessage(e))
            }
        )
    }
)

#' Save Machine Learning Model
#'
#' Saves a trained machine learning model in ONNX format with associated metadata.
#' This process handles model serialization and metadata management.
#'
#' @param data A trained machine learning model object
#' @param name A unique identifier for the model
#' @param task The use case(s) for which the model was trained
#' @param options Additional options for saving the model (optional)
#' @return The saved ML model object
#' @export
save_ml_model <- Process$new(
    id = "save_ml_model",
    description = "Saves Machine Learning Model as ONNX.",
    categories = as.array("cubes", "Machine Learning"),
    summary = "Save trained ML",
    parameters = list(
        Parameter$new(
            name = "data",
            description = "A trained ML model",
            schema = list(
                type = "object"
            )
        ),
        Parameter$new(
            name = "name",
            description = "The unique identifer of the model.",
            schema = list(
                type = "string"
            ),
            optional = FALSE
        ),
        Parameter$new(
            name = "task",
            description = "The use case(s) of the model.",
            schema = list(
                type = "list"
            ),
            optional = FALSE
        ),
        Parameter$new(
            name = "options",
            description = "Additional options for saving the model.",
            schema = list(
                type = list("dict")
            ),
            optional = TRUE
        )
    ),
    returns = ml_model,
    operation = function(data, name, task, options) {
        # TODO: Implement model saving functionality
        # The following code is a placeholder for future implementation
        # Current implementation will be refactored by Jonas S

        # if ("nn_module" %in% class(model) || !is.null(model$conv_layers)) {
        #   message("Erkannte ein Torch-Modell. Verwende Deep-Learning-Konvertierung...")
        #   onnx_path <- convert_r_torch_to_onnx(model, name)
        #
        # } else if ("train" %in% class(model)) {
        #   model_type <- detect_model_type(model)
        #   message("Erkannter Modelltyp: ", model_type)
        #   convert_model_to_pkl(model, model_type, name)
        #   onnx_path <- save_ml_model_as_onnx(model_type, name)
        #
        # } else {
        #   stop("Unbekannter Modelltyp: Bitte überprüfen Sie das übergebene Modell.")
        # }
        #
        # if (length(options) > 0) {
        #   add_metadata_to_onnx(onnx_path, options)
        # }
        #
        # message("Modell wurde erfolgreich exportiert: ", onnx_path)
        # return(onnx_path)
    }
)
