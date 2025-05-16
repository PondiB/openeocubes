#' @include Process-class.R
#' @include processes.R
#' @import gdalcubes
#' @import jsonlite
NULL



#' TO DO : Implement machine learning processes
#'


#' ml_predict
ml_predict <- do.call(Process$new, c(
  load_process_metadata("ml_predict"),
  list(
    operation = function(data, model, dimension = NULL) {
      tryCatch({
        prediction <- gdalcubes::predict(data, model)
        print(prediction)
        message("Prediction calculated ....")
        message(gdalcubes::as_json(prediction))
        return(prediction)
      }, error = function(e) {
        message("Error in prediction: ")
        message(conditionMessage(e))
      })
    }
  )
))

#' save_ml_model
save_ml_model <- do.call(Process$new, c(
  load_process_metadata("save_ml_model"),
  list(
    operation = function(data, name, task, options) {
      # TO DO : Demo, Jonas S will refactor this code.
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
))
