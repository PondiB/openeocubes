#' @include Process-class.R
#' @import gdalcubes
#' @import sf
#' @import rstac
#' @import randomForest
#' @import caret
#' @import xgboost
#' @import dplyr
#' @import readr
#' @import terra
#' @import jsonlite
#' @import stats
#' @import kernlab
#' @import reticulate
#' @import torch
#' @import abind
#' @import plumber
#' @import httr
#' @import tools
#' @import tidyr
#' @import rlang
NULL




#' ml_datacube_schema
#' @description Return a list with datacube description and schema
#'
#' @return datacube list
ml_datacube_schema <- function() {
  info <- list(
    description = "A data cube with the predicted values. It removes the specified dimensions and adds new dimension for the predicted values.",
    schema = list(type = "object", subtype = "raster-cube")
  )
  return(info)
}

#' ml_model_schema
#' @description Return a list with datacube description and schema
#'
#' @return model list
ml_model_schema <- function() {
  info <- list(
    description = "A ML model that was trained with one of the ML training processes.",
    schema = list(type = "object", subtype = "mlm-model")
  )
  return(info)
}

#' return objects for the processes
eo_datacube <- ml_datacube_schema()
ml_model <- ml_model_schema()

#############################################################################
# Read the environment variable "SHARED_TEMP_DIR" (returns "" if not set)
shared_dir <- Sys.getenv("SHARED_TEMP_DIR")
if (shared_dir == "") {
  shared_dir <- file.path(getwd(), "shared_temp")
  if (!dir.exists(shared_dir)) {
    dir.create(shared_dir, recursive = TRUE)
    message("Shared directory automatically created: ", normalizePath(shared_dir))
  } else {
    message("Using existing shared directory: ", normalizePath(shared_dir))
  }
  Sys.setenv(SHARED_TEMP_DIR = shared_dir)
} else {
  message("Using shared directory from environment: ", normalizePath(shared_dir))
}




#############################################################################



##### ml-processes #####

##### ml_fit #####
#' Train an ML or DL model on extracted features
#'
#' @description
#' Trains a Machine Learning or Deep Learning model based on the provided
#' preprocessed training data and specified target variable.
#'
#' @param model Object of subtype "mlm-model". The machine learning or deep learning model to train.
#' @param training_set Object of subtype "vector". Preprocessed training dataset containing features and target.
#' @param target Character. Name of the column in `training_set` that contains the target variable.
#'
#' @return A trained model object of subtype "mlm-model".
#'
ml_fit <- Process$new(
  id = "ml_fit",
  description = "Trains a Machine Learning or Deep Learning model based on input training data.",
  categories   = as.array("machine-learning", "model-training"),
  summary      = "Train an ML/DL model on extracted features.",
  parameters = list(
    Parameter$new(
      name = "model",
      description = "The machine learning model to train",
      schema = list(type = "object", subtype = "mlm-model")
    ),
    Parameter$new(
      name = "training_set",
      description = "Preprocessed training dataset for the model",
      schema = list(type = "object", subtype = "vector")
    ),
    Parameter$new(
      name = "target",
      description = "The column containing the target variable",
      schema = list(type = "string")
    )
  ),
  returns = list(
    description = "Trained model",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  
  operation = function(model, training_set, target, job) {
    
    ## ---------------- Helpers ----------------

    
    extract_time_series_features <- function(training_set, features_data, time_steps) {
      if (time_steps >= 1) {
        message("multi time steps")
        features <- array(
          data = as.matrix(training_set[, grep("_T\\d+$", colnames(training_set))]),
          dim  = c(nrow(training_set), length(features_data), time_steps)
        )
      } 
      features
    }
    
    identify_predictors <- function(training_set, pattern = "^(B\\d+|(?i)NDVI(_T\\d+)?)$") {
      predictor_names <- colnames(training_set)
      predictor_names <- predictor_names[
        grepl(pattern, predictor_names) &
          sapply(training_set[, predictor_names, drop = FALSE], is.numeric)
      ]
      if (length(predictor_names) == 0) stop("No valid predictors detected. Please check.")
      predictor_names
    }
    
    convert_to_wide_format <- function(train_data, target) {
      library(tidyr); library(dplyr); library(sf)
      band_names   <- grep("^B0?\\d{1,2}$", names(train_data), value = TRUE)
      has_ndvi     <- "NDVI" %in% colnames(train_data)
      bands_to_use <- c(band_names, if (has_ndvi) "NDVI")
      message("Found bands: ", paste(bands_to_use, collapse = ", "))
      
      train_data <- train_data %>% sf::st_drop_geometry() %>% dplyr::as_tibble()
      
      train_data_wide <- train_data %>%
        dplyr::select(fid, time, dplyr::all_of(bands_to_use)) %>%
        tidyr::pivot_wider(
          names_from  = time,
          values_from = dplyr::all_of(bands_to_use),
          names_glue  = "{.value}_T{match(time, sort(unique(time)))}"
        )
      
      time_order  <- sort(unique(train_data$time))
      cols_sorted <- unlist(lapply(seq_along(time_order), function(i) paste0(bands_to_use, "_T", i)))
      cols_sorted <- c("fid", cols_sorted)
      train_data_wide <- train_data_wide[, cols_sorted]
      
      train_data_clean <- train_data_wide %>% dplyr::filter(complete.cases(.))
      
      target_data <- train_data %>%
        dplyr::select(fid, !!rlang::sym(target)) %>%
        dplyr::distinct(fid, .keep_all = TRUE)
      
      train_data_clean <- dplyr::left_join(train_data_clean, target_data, by = "fid")
      message("Wide format done")
      train_data_clean
    }
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    ## ---------------- Start ----------------
    message("ml_fit is being prepared...")
    
    # DL-Erkennung: wir gehen UNITED über create_model (wie TempCNN).
    is_dl <- !is.null(model$create_model) && is.function(model$create_model)
    
    if (is_dl) {
      message("Deep learning model recognized. Start DL calculation...")
      
      time_steps <- length(unique(training_set$time))
      if (any(grepl("^X\\d+\\.", names(training_set)))) {
        names(training_set) <- gsub("^X\\d+\\.", "", names(training_set))
      }
      message("Detection of multiple time steps: ", time_steps)
      
      if (!"fid" %in% colnames(training_set))
        stop("The column 'fid' is missing in training_set. Please check your input data or preprocessing.")
      
      fid_counts <- table(training_set$fid)
      valid_fids <- as.integer(names(fid_counts[fid_counts == time_steps]))
      training_set <- training_set[training_set$fid %in% valid_fids, ]
      training_set <- training_set[order(training_set$fid, training_set$time), ]
      
      features_data <- grep("^B0?\\d{1,2}$", names(training_set), value = TRUE)
      if ("NDVI" %in% names(training_set)) features_data <- c(features_data, "NDVI")

      training_data <- convert_to_wide_format(train_data = training_set, target = target)
      features <- extract_time_series_features(training_set = training_data,
                                               features_data = features_data,
                                               time_steps = time_steps)
      
      labels <- as.numeric(as.factor(training_data[[target]]))
      
      library(torch)
      if (!is.null(model$parameters$seed)) torch_manual_seed(as.integer(model$parameters$seed))
      
      # Tensors
      x_train <- tryCatch({
        if (!is.array(features) && !is.matrix(features))
          stop("`features` must be array/matrix; got: ", paste(class(features), collapse = ", "))
        if (!is.numeric(features)) storage.mode(features) <- "double"
        dims <- dim(features); if (is.null(dims)) dims <- c(length(features), 1L, 1L)
        torch::torch_tensor(features, dtype = torch_float())
      }, error = function(e) {
      })
      
      y_train <- tryCatch({
        if (!is.numeric(labels)) labels <- as.numeric(labels)
        torch::torch_tensor(labels, dtype = torch_long())
      }, error = function(e) {
        message("torch_tensor(labels) failed: ", e$message); stop(e)
      })
      

      class_count <- length(unique(labels))
      message("Class number: ", class_count)
      
      # Modell durch Factory bauen (wie TempCNN)
      dl_model <- model$create_model(
        input_data_columns = features_data,
        time_steps = time_steps,
        class_count = class_count
      )

      # Sanity Forward (callable Wrapper via ())
      try({
        dl_model$eval()
        take <- as.integer(min(4, x_train$size(1)))
        xin  <- x_train$narrow(1, 1, take)$contiguous()
        out  <- dl_model(xin)
        arr  <- torch::as_array(out)
      }, silent = FALSE)
      
      # Optimizer + Loss
      opt_name <- tolower(model$parameters$optimizer %||% "adam")
      optimizer <- switch(
        opt_name,
        adam = optim_adam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3),
        radam = optim_radam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3),
        nadam = optim_nadam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3),
        optim_adam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3)
      )
      loss_fn <- nn_cross_entropy_loss()
      
      # Training (call via callable wrapper)
      n_epochs <- as.integer(model$parameters$epochs %||% 100L)
      for (epoch in 1:n_epochs) {
        dl_model$train()
        optimizer$zero_grad()
        preds <- dl_model(x_train)
        loss  <- loss_fn(preds, y_train)
        loss$backward()
        optimizer$step()
        if (epoch %% max(1L, n_epochs %/% 10L) == 0L) {
          message(sprintf("Epoch: %d/%d, Loss: %.4f", epoch, n_epochs, loss$item()))
        }
      }
      
      # Eval
      dl_model$eval()
      predictions <- dl_model(x_train)
      predicted_classes <- torch_argmax(predictions, dim = 2)
      acc <- mean(as.numeric(torch::as_array(predicted_classes) == torch::as_array(y_train)))
      message(sprintf("Accuracy: %.2f%%", acc * 100))
      
      confusion_matrix <- table(
        Predicted = as.integer(torch::as_array(predicted_classes)),
        Actual    = as.integer(torch::as_array(y_train))
      )
      message("Confusion Matrix:"); print(confusion_matrix)
      
      # >>> wichtige Metadaten anhängen <<<
      dl_model$time_steps         <- as.integer(time_steps)
      dl_model$input_channels     <- as.integer(length(features_data))
      dl_model$input_data_columns <- features_data
      dl_model$input_layout       <- "NCT"
      
      
      model_file <- tempfile(fileext = ".pt")
      torch_save(dl_model, model_file)
      message("Model saved in Torch file: ", model_file)
      
      return(model_file)
    }
    
    ## ---------------- Klassisches ML (Caret) ----------------
    message("Machine learning model recognized. Start ML calculation...")
    
    if (any(grepl("^X\\d+\\.", names(training_set)))) {
      names(training_set) <- gsub("^X\\d+\\.", "", names(training_set))
    }
    
    time_steps <- length(unique(training_set$time))
    if (time_steps > 1) {
      message("Detection of multiple time steps: ", time_steps)
      training_set <- convert_to_wide_format(train_data = training_set, target = target)
    } else {
      message("Recognizing a time step")
      band_names <- grep("^B0?\\d{1,2}$", names(training_set), value = TRUE)
      has_ndvi   <- "NDVI" %in% colnames(training_set)
      predictors_name <- c(band_names, if (has_ndvi) "NDVI")
      
      train_ids <- caret::createDataPartition(training_set$fid, p = 1.0, list = FALSE)
      train_data <- training_set[train_ids, ] %>% sf::st_drop_geometry()
      
      tryCatch({
        message("Check for complete rows based on predictors...")
        train_data_numeric <- train_data[, predictors_name, drop = FALSE]
        complete_rows <- complete.cases(train_data_numeric)
        train_data <- train_data[complete_rows, ]
      }, error = function(e) {
        message("Error when filtering for complete cases:", e$message)
      })
      
      training_set <- base::as.data.frame(train_data)
    }
    
    y <- training_set[[target]]
    is_classification <- !is.null(model$classification) && isTRUE(model$classification)
    
    if (is_classification) {
      if (!is.factor(y)) {
        message("Classification is carried out... converting numeric to factor.")
        training_set[[target]] <- as.factor(y)
      } else message("Classification is carried out...")
    } else {
      if (!is.numeric(y)) {
        message("Regression is carried out... converting factor to numeric.")
        training_set[[target]] <- as.numeric(y)
      } else message("Regression is carried out...")
    }
    
    predictor_names <- identify_predictors(training_set)
    if (length(predictor_names) == 0) stop("No valid predictors detected. Please check.")
    message("Automatically recognized predictors: ", paste(predictor_names, collapse = ", "))
    
    x <- as.data.frame(lapply(training_set[, predictor_names, drop = FALSE], as.numeric))
    if (ncol(x) == 0) stop("No predictors detected.")
    
    tryCatch({
      if (model$method == "rf") {
        if (is.null(model$tuneGrid) || is.na(model$tuneGrid$mtry)) {
          max_variables <- max(1, floor(sqrt(ncol(x))))
          model$tuneGrid <- expand.grid(mtry = max_variables)
          message("tuneGrid was automatically set to mtry = ", max_variables)
        } else if (model$tuneGrid$mtry < 1 || model$tuneGrid$mtry > ncol(x)) {
          warning("Invalid `mtry` value in tuneGrid. Set `mtry` to 1.")
          model$tuneGrid <- expand.grid(mtry = 1)
        }
      } else if (model$method %in% c("svmRadial","svmLinear","svmPoly")) {
        if (is.null(model$tuneGrid)) stop("SVM models require a defined tuneGrid")
        model$preProcess <- c("center","scale")
      } else if (model$method == "xgbTree") {
        if (is.null(model$tuneGrid)) stop("XGBoost model requires a defined tuneGrid")
        if (is.null(model$trControl)) {
          model$trControl <- caret::trainControl(method = "cv", number = 5, search = "grid")
        }
      } else {
        stop("Undetected method! Allowed: 'rf', 'svmRadial', 'svmLinear', 'svmPoly', 'xgbTree'.")
      }
      
      model <- caret::train(
        x = x, y = training_set[[target]],
        method = model$method,
        tuneGrid = model$tuneGrid,
        trControl = model$trControl,
        ntree = if (model$method == "rf") model$ntree else NULL,
        preProcess = if (model$method %in% c("svmRadial","svmLinear","svmPoly")) model$preProcess else NULL
      )
    }, error = function(e) {
      message("An error has occurred: ", e$message); traceback()
    })
    
    if (is_classification) {
      if ("Accuracy" %in% colnames(model$results)) {
        accuracy <- max(model$results$Accuracy, na.rm = TRUE)
        message("Accuracy: ", round(accuracy * 100, 2), "%")
      }
    } else {
      if ("RMSE" %in% colnames(model$results)) {
        rmse <- min(model$results$RMSE, na.rm = TRUE)
        message("RMSE: ", round(rmse, 2))
      }
    }
    
    return(model)
  }
)




########################################################
##### ml_predict #####
#' Apply a trained ML/DL model to a data cube and return predictions
#'
#' @description
#' Applies a machine learning or deep learning model to every pixel (and/or time step)
#' of the input raster-cube and returns a new data cube with the predicted values.
#'
#' @param cube Object of subtype "raster-cube". The area-of-interest data cube on which predictions will be made.
#' @param model Object of subtype "mlm-model" or a file path (ONNX, .rds, or .pt). The trained model to use for prediction.
#'
#' @return
#' A raster-cube object containing the predicted values in a new "prediction" band.
#'
ml_predict <- Process$new(
  id = "ml_predict",
  description = "Applies a machine learning model to a data cube and returns the predicted values.",
  categories = as.array("machine-learning", "prediction"),
  summary = "Predicts the aoi (area of interest) with the model",
  
  parameters = list(
    Parameter$new(
      name = "data",
      description = "The Area of interest cube, which we want to predict",
      schema = list(type = "objects", subtype = "raster-cube")
    ),
    Parameter$new(
      name = "model",
      description = "The trained machine learning model (ONNX, RDS or Model-data)",
      schema = list(type = "object", subtype ="mlm-model")
    )
    
  ),
  returns = eo_datacube,
  operation = function(data, model, job) {
    ################################################################################################
    
    is_torch_model <- function(model) {
      inherits(model, "nn_module") ||
        "nn_module" %in% class(model) ||
        (!is.null(model$conv_layers) && !is.null(model$dense))
    }
    
    mlm_single <- function(data_cube, model) {
      message("Preparing prediction with apply_pixel using temporary directory...")
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      
      if (is.character(model)) {
        message("Model is a file path: ", model)
        model_file <- model
      } else {
        if (is_torch_model(model)) {
          model_file <- tempfile(fileext = ".pt")
          torch::torch_save(model, model_file)
        } else {
          model_file <- file.path(tmp, "model.rds")
          saveRDS(model, model_file)
          message("Classic ML model saved to RDS: ", model_file)
        }
      }
      Sys.setenv(MODEL_FILE = model_file)
      
      cube_band_names <- gdalcubes::bands(data_cube)$name
      band_names_file <- file.path(tmp, "band_names.rds")
      tryCatch({
        saveRDS(cube_band_names, band_names_file)
        message("Band name file saved as: ", normalizePath(band_names_file))
      }, error = function(e) {
        stop("Error when saving the band names: ", e$message)
      })
      
      time_steps <- gdalcubes::dimension_values(data_cube)$t
      nsteps <- length(time_steps)
      Sys.setenv(TMPDIRPATH = tmp)
      Sys.setenv(NSTEPS = nsteps)
      
      
      predict_pixel_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        nsteps <- as.numeric(Sys.getenv("NSTEPS"))
        model_file <- Sys.getenv("MODEL_FILE")
        
        is_torch_model <- function(model) {
          inherits(model, "nn_module") ||
            "nn_module" %in% class(model) ||
            (!is.null(model$conv_layers) && !is.null(model$dense))
        }
        
        if (!is.matrix(x)) {
          x <- matrix(x, nrow = 1)
        }
        
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        if (is.null(colnames(x))) {
          colnames(x) <- local_bands
        }
        
        pixel_df <- as.data.frame(x)
        
        library(torch)
        if (endsWith(model_file, ".pt")) {
          local_model <- torch::torch_load(model_file)
        } else {
          local_model <- readRDS(model_file)
        }
        
        if (is_torch_model(local_model)) {
          message("Deep Learning model detected in predict_pixel_fun")
          pixel_matrix <- as.matrix(pixel_df)
          n_channels <- length(local_bands)
          pixel_tensor <- torch::torch_tensor(pixel_matrix, dtype = torch_float())
          pixel_tensor <- pixel_tensor$view(c(1, n_channels, 1))
          message("Shape of pixel_tensor: ", paste(dim(pixel_tensor), collapse = " x "))
          local_model$eval()
          with_no_grad({
            preds <- local_model(pixel_tensor)
          })
          pred_class_tensor <- torch::torch_argmax(preds, dim = 2)
          pred_class <- as.numeric(torch::as_array(pred_class_tensor))
          return(pred_class)
        } else {
          message("Classic ML model used in predict_pixel_fun")
          pred <- predict(local_model, newdata = pixel_df)
          return(as.numeric(pred))
        }
      }
      
      prediction_cube <- tryCatch({
        apply_pixel(
          data_cube,
          names = "prediction",
          keep_bands = FALSE,
          FUN = predict_pixel_fun
        )
      }, error = function(e) {
        message("Error during apply_pixel: ", conditionMessage(e))
        NULL
      })
      
      return(prediction_cube)
    }
    
    
    
    mlm_multi <- function(data_cube, model) {
      message("Preparing prediction with apply_time using temporary directory...")
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      
      if (is.character(model)) {
        model_file <- model
      } else {
        if (is_torch_model(model)) {
          model_file <- tempfile(fileext = ".pt")
          torch::torch_save(model, model_file)
        } else {
          model_file <- file.path(tmp, "model.rds")
          saveRDS(model, model_file)
          message("Classic ML model saved to RDS: ", model_file)
        }
      }
      
      Sys.setenv(MODEL_FILE = model_file)
      
      cube_band_names <- gdalcubes::bands(data_cube)$name
      band_names_file <- file.path(tmp, "band_names.rds")
      message(cube_band_names)
      tryCatch({
        saveRDS(cube_band_names, band_names_file)
      }, error = function(e) {
        stop("Error when saving the band names: ", e$message)
      })
      Sys.setenv(TMPDIRPATH = tmp)
      
      time_steps <- gdalcubes::dimension_values(data_cube)$t
      nsteps <- length(time_steps)
      Sys.setenv(NSTEPS = nsteps)
      message("Number of time steps: ", nsteps)
      
      
      predict_time_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        local_nsteps <- as.numeric(Sys.getenv("NSTEPS"))
        model_file <- Sys.getenv("MODEL_FILE")
        is_torch_model <- function(model) {
          inherits(model, "nn_module") ||
            "nn_module" %in% class(model) ||
            (!is.null(model$conv_layers) && !is.null(model$dense))
        }
        
        
        if (!is.matrix(x)) {
          x <- matrix(x, nrow = length(readRDS(file.path(local_tmp, "band_names.rds"))))
        }
        
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        n_bands <- length(local_bands)
        if (nrow(x) != n_bands || ncol(x) != local_nsteps) {
          stop("The dimensions of the pixel time series do not match: Expected ",
               n_bands, " Lines and ", local_nsteps, " Columns, but preserved: ",
               nrow(x), " x ", ncol(x))
        }
        
        
        
        if (endsWith(model_file, ".pt")) {
          library(torch)
          local_model <- torch::torch_load(model_file)
        } else {
          local_model <- readRDS(model_file)
        }
        
        if(is_torch_model(local_model)){
          message("Deep Learning model detected")
          library(torch)
          pixel_tensor <- torch::torch_tensor(x, dtype = torch_float())
          pixel_tensor <- pixel_tensor$view(c(1, n_bands, local_nsteps))
          
          local_model$eval()
          with_no_grad({
            preds <- local_model(pixel_tensor)
          })
          pred_class_tensor <- torch::torch_argmax(preds, dim = 2)
          pred_class <- as.numeric(torch::as_array(pred_class_tensor))
          return(matrix(rep(pred_class, local_nsteps), nrow = 1))
        }else{
          message("Classic machine learning detected")
          wide_vec <- as.vector(x)
          wide_mat <- matrix(wide_vec, nrow = 1)
          wide_names <- unlist(lapply(seq_len(local_nsteps), function(i) {
            paste0(local_bands, "_T", i)
          }))
          message("Wide names: ", paste(wide_names, collapse = ", "))
          
          if (length(wide_vec) != length(wide_names)) {
            stop("Mismatch: vector length is ", length(wide_vec), " but expected ", length(wide_names))
          }
          
          colnames(wide_mat) <- wide_names
          
          pixel_df <- as.data.frame(wide_mat)
          
          pred <- predict(local_model, newdata = pixel_df)
          
          pred_value <- as.numeric(pred)
          result_matrix <- matrix(rep(pred_value, local_nsteps), nrow = 1)
          
          
          
          return(result_matrix)
        }
        
      }
      
      prediction_cube <- tryCatch({
        gdalcubes::apply_time(
          data_cube,
          names = "prediction",
          keep_bands = FALSE,
          FUN = predict_time_fun
        )
      }, error = function(e) {
        message("Error during apply_time: ", conditionMessage(e))
        NULL
      })
      
      return(prediction_cube)
    }
    
    
    #######onnx prediction ################
    
    detected_model_type <- function(model) {
      if (endsWith(model, ".onnx")) {
        
        onnxruntime <- reticulate::import("onnxruntime")
        session <- onnxruntime$InferenceSession(model)
        input_details <- session$get_inputs()[[1]]
        
        if (length(input_details$shape) == 3) {
          Sys.setenv(ONNX_DL_FLAG = "TRUE")
          message("ONNX model detected as Deep Learning (3D input).")
        } else {
          Sys.setenv(ONNX_DL_FLAG = "FALSE")
          message("ONNX model detected as Classic ML (2D input).")
        }
        
        model <- list(
          session = session,
          input_name = input_details$name,
          input_shape = lapply(input_details$shape, function(x) if (is.null(x)) "None" else as.character(x)),
          is_onnx = TRUE
        )
        return(model)
      }
      stop("The transferred model is not an ONNX model.")
    }
    
    
    mlm_single_onnx <- function(data_cube, model) {
      #ensure_python_env(required_modules = c("numpy", "onnxruntime", "torch"))
      message("Preparing ONNX prediction (single time step) using apply_pixel()...")
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      
      if (is.character(model)) {
        message("Model is a file path: ", model)
        model_file <- model
      } else {
        stop("For ONNX predictions, model must be a path.")
      }
      Sys.setenv(MODEL_FILE = model_file)
      
      cube_band_names <- gdalcubes::bands(data_cube)$name
      band_names_file <- file.path(tmp, "band_names.rds")
      tryCatch({
        saveRDS(cube_band_names, band_names_file)
      }, error = function(e) {
        stop("Error when saving the band names: ", e$message)
      })
      Sys.setenv(TMPDIRPATH = tmp)
      
      # Die Anzahl der Zeitschritte sollte 1 sein – überprüfen:
      time_steps <- gdalcubes::dimension_values(data_cube)$t
      nsteps <- length(time_steps)
      Sys.setenv(NSTEPS = nsteps)
      message("Number of time steps: ", nsteps)
      
      model <- detected_model_type(model_file)
      message("Now go to the predict_pixel_fun (ONNX)")
      
      predict_pixel_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        nsteps <- as.numeric(Sys.getenv("NSTEPS"))
        model_file <- Sys.getenv("MODEL_FILE")
        
        if (!is.matrix(x)) {
          x <- matrix(x, nrow = 1)
        }
        
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        if (is.null(colnames(x))) {
          colnames(x) <- local_bands
        }
        
        onnx_dl_flag <- Sys.getenv("ONNX_DL_FLAG")
        if (onnx_dl_flag == "TRUE") {
          message("Deep Learning ONNX model detected; reshaping input to [1, n_channels, 1]")
          x <- array(x, dim = c(1, ncol(x), 1))
        } else {
          message("Classic ML ONNX model detected; using 2D input")
          x <- matrix(x, nrow = 1)
        }
        
        if (!reticulate::py_module_available("numpy")) {
          message("numpy not available - installation is being attempted")
          reticulate::py_install("numpy", pip = TRUE)
        }
        if (!reticulate::py_module_available("onnxruntime")) {
          message("onnxruntime not available - installation is attempted")
          reticulate::py_install("onnxruntime", pip = TRUE)
          message("onnxruntime installed")
        }
        np <- reticulate::import("numpy")
        onnxruntime <- reticulate::import("onnxruntime")
        
        session <- onnxruntime$InferenceSession(Sys.getenv("MODEL_FILE"))
        input_details <- session$get_inputs()[[1]]
        input_name <- input_details$name
        
        np_x <- reticulate::np_array(x, dtype = "float32")
        pred <- session$run(output_names = NULL,
                            input_feed = setNames(list(np_x), input_name))[[1]]
        if (onnx_dl_flag == "TRUE") {
          pred <- as.numeric(apply(pred, 1, which.max))
        }
        return(pred)
      }
      
      prediction_cube <- tryCatch({
        gdalcubes::apply_pixel(
          data_cube,
          names = "prediction",
          keep_bands = FALSE,
          FUN = predict_pixel_fun
        )
      }, error = function(e) {
        message("Error during apply_pixel: ", conditionMessage(e))
        NULL
      })
      
      return(prediction_cube)
    }
    
    mlm_multi_onnx <- function(data_cube, model) {
      message("Preparing ONNX prediction (multiple time steps) using apply_time()...")
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      
      if (is.character(model)) {
        model_file <- model
      } else {
        stop("For ONNX predictions, model must be a path")
      }
      Sys.setenv(MODEL_FILE = model_file)
      
      cube_band_names <- gdalcubes::bands(data_cube)$name
      print(cube_band_names)
      band_names_file <- file.path(tmp, "band_names.rds")
      saveRDS(cube_band_names, band_names_file)
      if (!file.exists(band_names_file)) {
        stop("The file band_names.rds was not saved successfully in : ", band_names_file)
      }
      Sys.setenv(TMPDIRPATH = tmp)
      
      time_steps <- gdalcubes::dimension_values(data_cube)$t
      nsteps <- length(time_steps)
      Sys.setenv(NSTEPS = nsteps)
      message("Number of time steps: ", nsteps)
      
      model <- detected_model_type(model_file)
      
      predict_time_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        nsteps_local <- as.numeric(Sys.getenv("NSTEPS"))
        model_file <- Sys.getenv("MODEL_FILE")
        
        if (!is.matrix(x)) {
          local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
          n_bands <- length(local_bands)
          x <- matrix(x, nrow = n_bands)
        }
        
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        n_bands <- length(local_bands)
        if (is.null(colnames(x))) {
          colnames(x) <- local_bands
        }
        
        onnx_dl_flag <- Sys.getenv("ONNX_DL_FLAG")
        if (onnx_dl_flag == "TRUE") {
          x <- array(x, dim = c(1, n_bands, nsteps_local))
        } else {
          wide_vec <- as.vector(x)
          wide_mat <- matrix(wide_vec, nrow = 1, ncol = n_bands * nsteps_local)
          wide_names <- unlist(lapply(seq_len(nsteps_local), function(i) {
            paste0(local_bands, "_T", i)
          }))
          if (ncol(wide_mat) != length(wide_names)) {
            stop("Mismatch: number of columns is ", ncol(wide_mat), " but expected ", length(wide_names))
          }
          colnames(wide_mat) <- wide_names
          x <- wide_mat
        }
        
        if (!reticulate::py_module_available("numpy")) {
          reticulate::py_install("numpy", pip = TRUE)
        }
        if (!reticulate::py_module_available("onnxruntime")) {
          reticulate::py_install("onnxruntime", pip = TRUE)
        }
        np <- reticulate::import("numpy")
        onnxruntime <- reticulate::import("onnxruntime")
        
        session <- onnxruntime$InferenceSession(Sys.getenv("MODEL_FILE"))
        input_details <- session$get_inputs()[[1]]
        input_name <- input_details$name
        
        np_x <- reticulate::np_array(x, dtype = "float32")
        
        pred <- session$run(output_names = NULL,
                            input_feed = setNames(list(np_x), input_name))[[1]]
        if (onnx_dl_flag == "TRUE") {
          pred_class <- as.numeric(apply(pred, 1, which.max))
        } else {
          #pred_class <- as.numeric(apply(pred, 1, which.max))
          pred_class <- as.numeric(pred)
        }
        result <- matrix(rep(pred_class, nsteps_local), nrow = 1)
        message("Prediction: ", result)
        
        return(result)
      }
      
      prediction_cube <- tryCatch({
        gdalcubes::apply_time(
          data_cube,
          names = "prediction",
          keep_bands = FALSE,
          FUN = predict_time_fun
        )
      }, error = function(e) {
        message("Error during apply_time: ", conditionMessage(e))
        NULL
      })
      
      return(prediction_cube)
    }
    
    
    
    time_steps_query <- function(data){
      time_steps <- gdalcubes::dimension_values(data)
      time_steps <- time_steps$t
      return(time_steps)
    }
    
    #######################################
    time <- time_steps_query(data)
    message("ml_pedict starting...")
    if (is.raw(model)) {
      message("RAW model recognized - try to determine type")
      model <- tryCatch({
        model_obj <- readRDS(tmp_file)
        message("RAW was a .rds model - model loaded.")
        con_rds <- rawConnection(model_obj, "rb")
        model <- torch::torch_load(con_rds)
      }, error = function(e) {
        tmp_file <- tempfile(fileext = ".onnx")
        writeBin(model, tmp_file)
        message("No .rds recognized - treat as .onnx: ", tmp_file)
        return(tmp_file)
      })
    }
    if (is.list(model) && !is.null(model$onnx) && endsWith(model$onnx, ".onnx")) {
      message("Model provided as list – using ONNX: ", model$onnx)
      model <- model$onnx
    }
    
    band_info <- gdalcubes::bands(data)
    band_names <- band_info$name
    cube_dimensions <- gdalcubes::dimensions(data)
    time_count <- cube_dimensions$t$count
    multi_timesteps <- time_count > 1
    input_channels <- length(band_names)
    
    if (is.character(model) && endsWith(model, ".onnx")) {
      if (multi_timesteps) {
        message("ONNX model detected – multi time steps")
        return(mlm_multi_onnx(data, model))
      } else {
        message("ONNX model detected – single time step")
        return(mlm_single_onnx(data, model))
      }
    }
    
    if (is.character(model)) {
      message("Loading external model...")
      
      if (endsWith(model, ".pt")) {
        library(torch)
        
        if (!file.exists(model)) {
          stop("Model file does not exist: ", model)
        }
        model <- torch::torch_load(model)
        message("Torch model successfully loaded.")
        
      } else if (endsWith(model, ".rds")) {
        model_obj <- readRDS(model)
        message("RDS model loaded successfully.")
        
        if(is.raw(model_obj)){
          message("raw RDS detetcted")
          con_rds <- rawConnection(model_obj, "rb")
          model <- torch::torch_load(con_rds)
        }
        else if (inherits(model_obj, "train")) {
          message("Caret model recognized")
          model <- model_obj
        } else {
          stop("Unknown model type in .rds - no nn_module, no caret model, no Torch state_dict")
        }
      }else {
        stop("Unsupported model format")
      }
    }
    if (multi_timesteps) {
      message("prediction for multi time steps")
      prediction <- mlm_multi(data, model)
      message("prediction successful")
      return(prediction)
    } else {
      message("Only one time step")
      prediction <- mlm_single(data, model)
      return(prediction)
    }
  }
)

########################################################
##### mlm_svm_process #####
#' Support Vector Machine for classification or regression
#'
#' @description
#' Model wrapper for Support Vector Machines (SVMs) using caret. Creates model setup and hyperparameter grid 
#' for classification or regression with specified kernel type and tuning parameters. 
#' Supports common kernel types including RBF, polynomial and linear.
#'
#' @param kernel Character. Kernel type to use: `'rbf'`, `'linear'` or `'poly'`.
#' @param C Numeric. Regularization parameter (cost). Must be > 0.
#' @param gamma Numeric, optional. Kernel coefficient for `'rbf'` and `'poly'`. Controls nonlinearity.
#' @param degree Integer, optional. Degree of the polynomial kernel (used only with `'poly'`).
#' @param coef0 Numeric, optional. Independent term in the kernel function (used for `'poly'`).
#' @param tolerance Numeric, optional. Tolerance for termination criterion (default is 0.001).
#' @param cachesize Integer, optional. Size of the kernel cache in MB (default is 1000).
#' @param seed Integer or NULL, optional. Random seed for reproducibility. If NULL, no seed is set.
#' @param classification Logical. If `TRUE`, sets up SVM for classification; if `FALSE`, for regression.
#'
#' @return
#' A list containing model parameters:
#' - `method`: caret-compatible model identifier (`"svmRadial"`, `"svmLinear"`, `"svmPoly"`)
#' - `tuneGrid`: a data.frame with hyperparameter combinations
#' - `trControl`: caret trainControl object with 5-fold cross-validation
#' - `tolerance`: termination criterion tolerance
#' - `cachesize`: kernel cache size in MB
#' - `seed`: random seed (if provided)
#' - `classification`: TRUE or FALSE
#'
#' @examples
#' # Classification with RBF kernel
#' params <- mlm_class_svm(kernel = "rbf", C = 1, gamma = 0.1)
#'
#' # Regression with polynomial kernel
#' params <- mlm_regr_svm(kernel = "poly", C = 0.5, degree = 4, gamma = 0.01, coef0 = 1)
#'


#### help-function
mlm_svm_envelope <- function(kernel,
                             C = 1,
                             gamma = 1,
                             degree = 3,
                             coef0 = 0,
                             tolerance = 0.001,
                             cachesize = 1000,
                             seed = NULL,
                             classification) {
  
  kernel_map <- list(
    rbf = "svmRadial",
    linear = "svmLinear",
    poly = "svmPoly")
  
  method <- kernel_map[[kernel]]
  if (is.null(method)) stop("Unsupported kernel type.")
  
  # Tune grid je nach Kernel
  tuneGrid <- switch(kernel,
                     rbf    = expand.grid(C = C, sigma = gamma),                 # OK für svmRadial
                     linear = expand.grid(C = C),                                # OK für svmLinear
                     poly   = expand.grid(C = C, degree = degree, scale = gamma) # <- ohne coef0
  )
  
  
  return(list(
    method = method,
    tuneGrid = tuneGrid,
    trControl = caret::trainControl(method = "cv", number = 5),
    tolerance = tolerance,
    cachesize = cachesize,
    seed = seed,
    classification = classification
  ))
}



mlm_class_svm <- Process$new(
  id = "mlm_class_svm",
  description = "Initialize an SVM classification model",
  summary = "Support Vector Machine for classification with rbf, linear, poly kernel.",
  categories = c("machine-learning"),
  parameters = list(
    Parameter$new(name = "kernel",
                  description = "Specifies the kernel type to be used in the algorithm.", 
                  schema = list(type = "string", 
                                enum = c("linear", "poly", "rbf"),
                                default = "rbf"), 
                  optional = TRUE),
    Parameter$new(name = "C",
                  description = "Regularization parameter",
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "gamma", 
                  description = "Kernel coefficient for 'rbf' and 'poly'", 
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "degree", 
                  description = "Degree of the poly kernel (only relevant for 'poly').", 
                  schema = list(type = "integer", 
                                minimum = 1, 
                                default = 3), 
                  optional = TRUE),
    Parameter$new(name = "coef0", 
                  description = "Independent term in kernel function (for 'poly').", 
                  schema = list(type = "number", 
                                default = 0), 
                  optional = TRUE),
    Parameter$new(name = "tolerance", 
                  description = "Tolerance of termination criterion.", 
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 0.001), 
                  optional = TRUE),
    Parameter$new(name = "cachesize", 
                  description = "Size of kernel cache in MB.", 
                  schema = list(type = "integer", 
                                minimum = 1, 
                                default = 1000),
                  optional = TRUE),
    Parameter$new(name = "seed", 
                  description = "Random seed for reproducibility.", 
                  schema = list(type = list("integer", "null"), 
                                default = NULL),
                  optional = TRUE)
  ),
  returns = list(
    description = "A model object that can be trained using `ml_fit`.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(kernel = "rbf", C = 1, gamma = 1, degree = 3,
                       coef0 = 0, tolerance = 0.001, cachesize = 1000,
                       seed = NULL, job) {
    message("Creating SVM classification model.")
    mlm_svm_envelope(
      kernel = kernel,
      C = C,
      gamma = gamma,
      degree = degree,
      coef0 = coef0,
      tolerance = tolerance,
      cachesize = cachesize,
      seed = seed,
      classification = TRUE
    )
  }
)


mlm_regr_svm <- Process$new(
  id = "mlm_regr_svm",
  description = "Initialize an SVM regression model",
  summary = "Support Vector Machine for regression with rbf, linear, poly kernel.",
  categories = c("machine-learning"),
  parameters = list(
    Parameter$new(name = "kernel",
                  description = "Specifies the kernel type to be used in the algorithm.", 
                  schema = list(type = "string", 
                                enum = c("linear", "poly", "rbf"),
                                default = "rbf"), 
                  optional = TRUE),
    Parameter$new(name = "C",
                  description = "Regularization parameter",
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "gamma", 
                  description = "Kernel coefficient for 'rbf' and 'poly'", 
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "degree", 
                  description = "Degree of the poly kernel (only relevant for 'poly').", 
                  schema = list(type = "integer", 
                                minimum = 1, 
                                default = 3), 
                  optional = TRUE),
    Parameter$new(name = "coef0", 
                  description = "Independent term in kernel function (for 'poly').", 
                  schema = list(type = "number", 
                                default = 0), 
                  optional = TRUE),
    Parameter$new(name = "tolerance", 
                  description = "Tolerance of termination criterion.", 
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 0.001), 
                  optional = TRUE),
    Parameter$new(name = "cachesize", 
                  description = "Size of kernel cache in MB.", 
                  schema = list(type = "integer", 
                                minimum = 1, 
                                default = 1000),
                  optional = TRUE),
    Parameter$new(name = "seed", 
                  description = "Random seed for reproducibility.", 
                  schema = list(type = list("integer", "null"), 
                                default = NULL),
                  optional = TRUE)
  ),
  returns = mlm_class_svm$returns,
  operation = function(kernel = "rbf", C = 1, gamma = 1, degree = 3,
                       coef0 = 0, tolerance = 0.001, cachesize = 1000,
                       seed = NULL, job) {
    message("Creating SVM regression model.")
    mlm_svm_envelope(
      kernel = kernel,
      C = C,
      gamma = gamma,
      degree = degree,
      coef0 = coef0,
      tolerance = tolerance,
      cachesize = cachesize,
      seed = seed,
      classification = FALSE
    )
  }
)




########################################################
#' Random forest for classification or regression
#'
#' @description
#' Model case for the random forest. Creates a parameter list and trainControl
#' settings for Random Forest models (classification or regression) with specified
#' number of trees and splitting criteria.
#'
#' @param max_variables String. Number of features to consider when looking for the best split (e.g., `"sqrt"` or a numeric value).
#' @param num_trees Integer. Number of trees in the forest.
#' @param seed Integer, optional. Random seed for reproducibility.
#' @param classification Logical. If `TRUE`, sets up Random Forest for classification; if `FALSE`, for regression.
#'
#' @return
#' A list of model parameters for caret, including:
#' - `method`: `"rf"`
#' - `tuneGrid`: a `data.frame` with the `mtry` value
#' - `trControl`: a `trainControl` object with 5-fold cross-validation
#' - `ntree`: number of trees
#' - `seed`: the random seed (if provided)
#'
#' @examples
#' # Classification with 100 trees, sqrt(max_variables)
#' params <- mlm_class_random_forest(
#'                             max_variables = "sqrt"
#'                             num_trees = 100,
#'                             seed = 42,
#'                             classification = TRUE)
#'
#' # Regression with 200 trees, 5 features per split
#' params <- mlm_regr_random_forest(num_trees = 200,
#'                             max_variables = "5",
#'                             seed = 42,
#'                             classification = FALSE)
#'

### help function ###
mlm_random_forest_envelope <- function(max_variables,
                                       num_trees,
                                       seed = NULL,
                                       classification) {
  
  # Bestimme den mtry-Wert, wie caret ihn erwartet
  mtry_grid <- if (is.character(max_variables)) {
    if (max_variables %in% c("sqrt", "log2", "onethird")) {
      data.frame(mtry = NA)  # caret interpretiert das intern
    } else if (max_variables == "all") {
      data.frame(mtry = NA)  # alle Features → caret entscheidet
    } else {
      stop("Invalid string value for max_variables")
    }
  } else if (is.numeric(max_variables)) {
    data.frame(mtry = as.integer(max_variables))
  } else {
    stop("max_variables must be character or numeric.")
  }
  
  model_params <- list(
    method = "rf",
    tuneGrid = mtry_grid,
    trControl = caret::trainControl(method = "cv", number = 5),
    ntree = num_trees,
    seed = seed,
    classification = classification
  )
  
  return(model_params)
}

### Process
mlm_class_random_forest <- Process$new(
  id = "mlm_class_random_forest",
  description = "Random forest for classification.",
  categories = c("machine-learning"),
  summary = "Random forest for classification with specified parameters.",
  parameters = list(
    Parameter$new(
      name = "max_variables",
      description = "Specifies how many split variables will be used at a node.\n\nThe following options are available:\n\n- *integer*: The given number of variables are considered for each split.\n- `all`: All variables are considered for each split.\n- `log2`: The logarithm with base 2 of the number of variables are considered for each split.\n- `onethird`: A third of the number of variables are considered for each split.\n- `sqrt`: The square root of the number of variables are considered for each split. This is often the default for classification.",
      schema = list(
        list(type = "integer", 
             minimum = 1),
        list(type = "string", 
             enum = c("all", "log2", "onethird", "sqrt"))
      )
    ),
    Parameter$new(
      name = "num_trees",
      description = "The number of trees build within the Random Forest classification.",
      optional = TRUE,
      schema = list(type = "integer", 
                    minimum = 1,
                    default = 100)
    ),
    Parameter$new(
      name = "seed",
      description = "A randomization seed to use for the random sampling in training. If not given or `null`, no seed is used and results may differ on subsequent use.",
      optional = TRUE,
      schema = list(type = c("integer", "null"),
                    default = NULL)
    )
  ),
  returns = list(
    description = "Model parameters for Random Forest classification.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(max_variables,
                       num_trees = 100,
                       seed = NULL,
                       job) {
    message("Creating Random Forest classification model.")
    mlm_random_forest_envelope(
      max_variables = max_variables,
      num_trees = num_trees,
      seed = seed,
      classification = TRUE
    )
  }
)

mlm_regr_random_forest <- Process$new(
  id = "mlm_regr_random_forest",
  description = "Random forest for regression.",
  categories = c("machine-learning"),
  summary = "Random forest for regression with specified parameters.",
  parameters = list(
    Parameter$new(
      name = "max_variables",
      description = "Specifies how many split variables will be used at a node.\n\nThe following options are available:\n\n- *integer*: The given number of variables are considered for each split.\n- `all`: All variables are considered for each split.\n- `log2`: The logarithm with base 2 of the number of variables are considered for each split.\n- `onethird`: A third of the number of variables are considered for each split.\n- `sqrt`: The square root of the number of variables are considered for each split. This is often the default for classification.",
      schema = list(
        list(type = "integer", 
             minimum = 1),
        list(type = "string", 
             enum = c("all", "log2", "onethird", "sqrt"))
      )
    ),
    Parameter$new(
      name = "num_trees",
      description = "The number of trees build within the Random Forest classification.",
      optional = TRUE,
      schema = list(type = "integer", 
                    minimum = 1,
                    default = 100)
    ),
    Parameter$new(
      name = "seed",
      description = "A randomization seed to use for the random sampling in training. If not given or `null`, no seed is used and results may differ on subsequent use.",
      optional = TRUE,
      schema = list(type = c("integer", "null"),
                    default = NULL)
    )
  ),
  returns = list(
    description = "Model parameters for Random Forest regression.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(max_variables,
                       num_trees,
                       seed = NULL,
                       job) {
    message("Creating Random Forest regression model.")
    mlm_random_forest_envelope(
      max_variables = max_variables,
      num_trees = num_trees,
      seed = seed,
      classification = FALSE
    )
  }
)


########################################################
#' Extreme Gradient Boosting for classification or regression
#' @description
#' Model case for XGBoost. Creates a parameter list and trainControl settings
#' for XGBoost models (classification or regression) with specified hyperparameters.
#'
#' @param learning_rate Numeric. Step size shrinkage (`eta`) to prevent overfitting.
#' @param max_depth Integer. Maximum depth of a tree.
#' @param min_child_weight Numeric. Minimum sum of instance weight (hessian) needed in a child.
#' @param subsample Numeric. Subsample ratio of the training instances.
#' @param min_split_loss Numeric. Minimum loss reduction required to make a further partition on a leaf node (a.k.a. `gamma`).
#' @param seed Integer or NULL. Optional random seed for reproducibility.
#' @param classification Logical. If `TRUE`, sets up XGBoost for classification; if `FALSE`, for regression.
#'
#' @return
#' A list of model parameters for caret's `xgbTree` method, including:
#' - `method`: `"xgbTree"`
#' - `tuneGrid`: a `data.frame` with hyperparameters (`nrounds`, `max_depth`, `eta`, `gamma`, `min_child_weight`, `subsample`)
#' - `trControl`: a `trainControl` object with 5-fold cross-validation
#' - `seed`: the random seed (if provided)
#' - `classification`: logical flag
#'
#' @examples
#' # Classification example
#' params <- mlm_class_xgboost(
#'   learning_rate    = 0.1,
#'   max_depth        = 6,
#'   min_child_weight = 1,
#'   subsample        = 0.8,
#'   min_split_loss   = 0,
#'   seed             = 123
#' )
#'
#' # Regression example
#' params <- mlm_regr_xgboost(
#'   learning_rate    = 0.05,
#'   max_depth        = 4,
#'   min_child_weight = 3,
#'   subsample        = 0.7,
#'   min_split_loss   = 1,
#'   seed             = 42
#' )


### help function ###
mlm_xgboost_envelope <- function(learning_rate = 0.15,
                                 max_depth = 5,
                                 min_child_weight = 1,
                                 subsample = 0.8,
                                 min_split_loss = 1,
                                 seed = NULL,
                                 classification) {
  list(
    method = "xgbTree",
    tuneGrid = expand.grid(
      nrounds = 100,  # Fix oder optional, falls gewünscht
      max_depth = max_depth,
      eta = learning_rate,
      gamma = min_split_loss,
      colsample_bytree = 1,  # Fixwert, da nicht mehr steuerbar
      min_child_weight = min_child_weight,
      subsample = subsample
    ),
    trControl = caret::trainControl(method = "cv", number = 5, search = "grid"),
    seed = seed,
    classification = classification
  )
}

#### Process

mlm_class_xgboost <- Process$new(
  id = "mlm_class_xgboost",
  summary = "Initialize an XGBoost classification model",
  description = "Initializes an XGBoost classification model. This component sets up the model structure but does not perform training or handle data splitting. The resulting model can be trained later using `ml_fit`.",
  categories = c("machine learning"),
  parameters = list(
    Parameter$new(name = "learning_rate", 
                  description = "Step size shrinkage used in update to prevent overfitting.", 
                  schema = list(type = "number", 
                                minimum = 0, 
                                default = 0.15), 
                  optional = TRUE),
    Parameter$new(name = "max_depth", 
                  description = "Maximum depth of a tree.", 
                  schema = list(type = "integer",
                                minimum = 1,
                                default = 5), 
                  optional = TRUE),
    Parameter$new(name = "min_child_weight", 
                  description = "Minimum sum of instance weight (hessian) needed in a child.", 
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "subsample", 
                  description = "Subsample ratio of the training instance.", 
                  schema = list(type = "number", 
                                minimum = 0, 
                                maximum = 1,
                                default = 0.8), 
                  optional = TRUE),
    Parameter$new(name = "min_split_loss", 
                  description = "Minimum loss reduction required to make a further partition on a leaf node of the tree.", 
                  schema = list(type = "number", 
                                minimum = 0, 
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "seed", 
                  description = "Randomization seed for reproducibility.", 
                  schema = list(type = list("integer", "null"),
                                default = NULL), 
                  optional = TRUE)
  ),
  returns = list(
    description = "A model object that can be trained using `ml_fit`.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(learning_rate = 0.15,
                       max_depth = 5,
                       min_child_weight = 1,
                       subsample = 0.8,
                       min_split_loss = 1,
                       seed = NULL,
                       job) {
    message("Creating XGBoost classification model.")
    mlm_xgboost_envelope(
      learning_rate = learning_rate,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      min_split_loss = min_split_loss,
      seed = seed,
      classification = TRUE
    )
  }
)


mlm_regr_xgboost <- Process$new(
  id = "mlm_regr_xgboost",
  summary = "Initialize an XGBoost regression model",
  description = "Initializes an XGBoost regression model. This component sets up the model structure but does not perform training or handle data splitting. The resulting model can be trained later using `ml_fit`.",
  categories = c("machine learning"),
  parameters = list(
    Parameter$new(name = "learning_rate", 
                  description = "Step size shrinkage used in update to prevent overfitting.", 
                  schema = list(type = "number", 
                                minimum = 0, 
                                default = 0.15), 
                  optional = TRUE),
    Parameter$new(name = "max_depth", 
                  description = "Maximum depth of a tree.", 
                  schema = list(type = "integer",
                                minimum = 1,
                                default = 5), 
                  optional = TRUE),
    Parameter$new(name = "min_child_weight", 
                  description = "Minimum sum of instance weight (hessian) needed in a child.", 
                  schema = list(type = "number", 
                                minimum = 0,
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "subsample", 
                  description = "Subsample ratio of the training instance.", 
                  schema = list(type = "number", 
                                minimum = 0, 
                                maximum = 1,
                                default = 0.8), 
                  optional = TRUE),
    Parameter$new(name = "min_split_loss", 
                  description = "Minimum loss reduction required to make a further partition on a leaf node of the tree.", 
                  schema = list(type = "number", 
                                minimum = 0, 
                                default = 1), 
                  optional = TRUE),
    Parameter$new(name = "seed", 
                  description = "Randomization seed for reproducibility.", 
                  schema = list(type = list("integer", "null"),
                                default = NULL), 
                  optional = TRUE)
  ),
  returns = list(
    description = "A model object that can be trained using `ml_fit`.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(learning_rate = 0.15,
                       max_depth = 5,
                       min_child_weight = 1,
                       subsample = 0.8,
                       min_split_loss = 1,
                       seed = NULL,
                       job) {
    message("Creating XGBoost regression model.")
    mlm_xgboost_envelope(
      learning_rate = learning_rate,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      min_split_loss = min_split_loss,
      seed = seed,
      classification = FALSE
    )
  }
)



####################################################
#' TempCNN for classification or regression
#'
#' @description
#' Model case for the TempCNN model. Defines convolutional and dense layer settings,
#' optimizer parameters, and returns a factory function to create the TempCNN module.
#'
#' @param cnn_layers Integer vector. Number of filters in each convolutional layer.
#' @param cnn_kernels Integer vector. Kernel sizes for each convolutional layer.
#' @param cnn_dropout_rates Numeric vector. Dropout rates for each convolutional layer.
#' @param dense_layer_nodes Integer. Number of neurons in the dense (fully-connected) layer.
#' @param dense_layer_dropout_rate Numeric. Dropout rate for the dense layer.
#' @param optimizer Character. Name of the optimizer (`"adam"`, `"adabound"`, `"adabelief"`, `"madagrad"`, `"nadam"`, `"qhadam"`, `"radam"`, `"swats"` and `"yogi"`).
#' @param learning_rate Numeric. Learning rate for the optimizer.
#' @param epochs Integer. Total number of training epochs.
#' @param batch_size Integer. Batch size for training.
#' @param seed Integer, optional. Random seed for reproducibility.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{parameters}{A named list of all TempCNN hyperparameters.}
#'   \item{create_model}{A function that, given `input_data_columns`, `time_steps`, and `class_count`, returns an initialized TempCNN `nn_module`.}
#' }
mlm_class_tempcnn <- Process$new(
  id = "mlm_class_tempcnn",
  description = "Model case for the TempCNN model.",
  categories = as.array("machine-learning"),
  summary = "TempCNN for classification or regression",
  parameters = list(
    Parameter$new(
      name = "cnn_layers",
      description = "Array: Number of filters per convolutional layer",
      schema = list(type = "array", items = list(type = "integer"),
                    minimum = 1, default = list(256L, 256L, 256L))
    ),
    Parameter$new(
      name = "cnn_kernels",
      description = "Array: Kernel sizes for the convolutional layers",
      schema = list(type = "array", items = list(type = "integer"),
                    minimum = 1, default = list(7L, 7L, 7L))
    ),
    Parameter$new(
      name = "cnn_dropout_rates",
      description = "Array: Dropout rates for the convolutional layers",
      schema = list(type = "array", items = list(type = "number"),
                    minimum = 0, maximum = 1, default = list(0.2, 0.2, 0.2))
    ),
    Parameter$new(
      name = "dense_layer_nodes",
      description = "Number of neurons in the dense layer",
      schema = list(type = "integer", minimum = 1, default = 256L)
    ),
    Parameter$new(
      name = "dense_layer_dropout_rate",
      description = "Dropout rate for the dense layer",
      schema = list(type = "number", minimum = 0, maximum = 1, default = 0.5)
    ),
    Parameter$new(
      name = "epochs",
      description = "Number of training epochs",
      schema = list(type = "integer", minimum = 1, default = 100L),
      optional = TRUE
    ),
    Parameter$new(
      name = "batch_size",
      description = "Batch size for training",
      schema = list(type = "integer", minimum = 1, default = 64L),
      optional = TRUE
    ),
    Parameter$new(
      name = "optimizer",
      description = "Optimizer name (e.g. 'adam')",
      schema = list(type = "string",
                    enum = c("adam","adabound","adabelief","madagrad","nadam","qhadam","radam","swats","yogi"),
                    default = "adam")
    ),
    Parameter$new(
      name = "learning_rate",
      description = "Learning rate for the optimizer",
      schema = list(type = "number", minimum = 0, default = 0.001),
      optional = TRUE
    ),
    Parameter$new(
      name = "seed",
      description = "Random generator for reproducibility",
      schema = list(type = "integer", default = NULL),
      optional = TRUE
    )
  ),
  returns = list(
    description = "Model parameters as a list for the TempCNN model",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(cnn_layers = list(256L,256L,256L),
                       cnn_kernels = list(7L,7L,7L),
                       cnn_dropout_rates = list(0.2,0.2,0.2),
                       dense_layer_nodes = 256L,
                       dense_layer_dropout_rate = 0.5,
                       epochs = 100L,
                       batch_size = 64L,
                       optimizer = "adam",
                       learning_rate = 0.001,
                       seed = NULL, job) {
    
    model_parameter <- list(
      cnn_layers = cnn_layers,
      cnn_kernels = cnn_kernels,
      cnn_dropout_rates = cnn_dropout_rates,
      dense_layer_nodes = dense_layer_nodes,
      dense_layer_dropout_rate = dense_layer_dropout_rate,
      epochs = epochs,
      batch_size = batch_size,
      optimizer = optimizer,
      learning_rate = learning_rate,
      seed = seed
    )
    
    # >>> EINHEITLICHER CONTRACT WIE BEIM MLP/LIGHTTAE <<<
    return(list(
      kind = "tempcnn",                  # optional, nur fürs Logging
      classification = TRUE,             # wichtig für ml_fit (Klassifikation)
      subtype = "mlm-model",             # passt zum Schema deiner Process-Defs
      parameters = model_parameter,
      create_model = function(input_data_columns, time_steps, class_count) {
        library(torch)
        
        dl_temp_cnn_dynamic <- nn_module(
          "TempCNN",
          initialize = function(settings, input_data_columns, time_steps, class_count) {
            self$conv_layers <- nn_module_list()
            self$input_data_columns <- input_data_columns
            
            reduced_time_steps <- time_steps
            for (i in seq_along(settings$cnn_layers)) {
              in_ch  <- if (i == 1) length(self$input_data_columns) else settings$cnn_layers[[i - 1]]
              out_ch <- settings$cnn_layers[[i]]
              k      <- settings$cnn_kernels[[i]]
              drop   <- settings$cnn_dropout_rates[[i]]
              
              self$conv_layers$append(
                nn_sequential(
                  nn_conv1d(in_channels = in_ch,
                            out_channels = out_ch,
                            kernel_size = k,
                            stride = 1,
                            padding = k %/% 2),
                  nn_batch_norm1d(out_ch),
                  nn_relu(),
                  nn_dropout(p = drop)
                )
              )
              # mit padding=k//2 bleibt T effektiv ~gleich; die folgende Zeile
              # ist nicht nötig für die finale T-Länge – wir berechnen Flatten
              # einfach dynamisch
            }
            
            self$flatten <- nn_flatten()
            # Dense-Head: In-Features = out_ch * T  (werden im forward dynamisch ermittelt)
            self$dense1 <- nn_linear(in_features = 1L, out_features = settings$dense_layer_nodes) # placeholder, richtige Größe im forward gesetzt
            self$act1   <- nn_relu()
            self$drop1  <- nn_dropout(p = settings$dense_layer_dropout_rate)
            self$out    <- nn_linear(in_features = settings$dense_layer_nodes, out_features = class_count)
          },
          forward = function(x) {
            # x erwartet: (N, C, T)
            for (i in seq_along(self$conv_layers)) {
              x <- self$conv_layers[[i]](x)  # callable wrapper
            }
            # dynamisch die Flatten-Länge bestimmen
            n <- x$size(1)
            feat_dim <- x$size(2) * x$size(3)  # (Channels_out * T)
            x <- x$view(c(n, feat_dim))
            
            # lineare Head-Layer ggf. beim ersten Forward richtig „anschließen“
            if (self$dense1$in_features != feat_dim) {
              self$dense1 <- nn_linear(in_features = feat_dim,
                                       out_features = self$dense1$out_features)$to(device = x$device)
            }
            
            x <- self$dense1(x); x <- self$act1(x); x <- self$drop1(x)
            self$out(x)
          }
        )
        
        dl_temp_cnn_dynamic(
          settings = model_parameter,
          input_data_columns = input_data_columns,
          time_steps = time_steps,
          class_count = class_count
        )
      }
    ))
  }
)



#######################################
# --- MLP-Modelldefinition (Factory wie TempCNN) ---
#' MLP for classification
#'
#' @description
#' Model case for a Multi-Layer Perceptron (MLP) classifier.
#' Defines hidden dense layers incl. activation + dropout, and returns
#' a factory function to create the torch nn_module.
#'
#' @return list(parameters=..., create_model=function(...){ nn_module })
mlm_class_mlp <- Process$new(
  id = "mlm_class_mlp",
  description = "Model case for an MLP classifier (dense feedforward network).",
  categories = as.array("machine-learning"),
  summary = "Initialize an MLP classification model",
  parameters = list(
    Parameter$new(
      name = "hidden_layer_sizes",
      description = "Neurons per hidden layer",
      schema = list(
        type = "array",
        items = list(type = "integer", minimum = 1),
        minItems = 1,
        default = list(512L, 512L, 512L)
      )
    ),
    Parameter$new(
      name = "activation",
      description = "Activation for hidden layers",
      optional = TRUE,
      schema = list(type = "string",
                    enum = c("relu","tanh","logistic"),
                    default = "relu")
    ),
    Parameter$new(
      name = "dropout_rates",
      description = "Dropout per hidden layer (recycled if shorter)",
      schema = list(
        type = "array",
        items = list(type = "number", minimum = 0, maximum = 1),
        minItems = 1,
        default = list(0.4, 0.3, 0.2)
      )
    ),
    Parameter$new(
      name = "epochs",
      description = "Training epochs (for ml_fit)",
      optional = TRUE,
      schema = list(type = "integer", minimum = 1, default = 100L)
    ),
    Parameter$new(
      name = "batch_size",
      description = "Batch size (for ml_fit)",
      optional = TRUE,
      schema = list(type = "integer", minimum = 1, default = 64L)
    ),
    Parameter$new(
      name = "optimizer",
      description = "Optimizer name (handled by ml_fit)",
      optional = TRUE,
      schema = list(type = "string",
                    enum = c("adam","adabound","adabelief","madagrad","nadam","qhadam","radam","swats","yogi"),
                    default = "adam")
    ),
    Parameter$new(
      name = "learning_rate",
      description = "Learning rate (for ml_fit)",
      optional = TRUE,
      schema = list(type = "number", minimum = 0, default = 0.001)
    ),
    Parameter$new(
      name = "seed",
      description = "Random seed or NULL",
      optional = TRUE,
      schema = list(type = c("integer","null"), default = NULL)
    )
  ),
  returns = list(
    description = "Model parameters + factory for the MLP model",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(hidden_layer_sizes = list(512L,512L,512L),
                       activation = "relu",
                       dropout_rates = list(0.4,0.3,0.2),
                       epochs = 100L,
                       batch_size = 64L,
                       optimizer = "adam",
                       learning_rate = 0.001,
                       seed = NULL, job) {
    message("mlp starting...")
    
    params <- list(
      hidden_layer_sizes = hidden_layer_sizes,
      activation = activation,
      dropout_rates = dropout_rates,
      epochs = epochs,
      batch_size = batch_size,
      optimizer = optimizer,
      learning_rate = learning_rate,
      seed = seed
    )
    
    # Factory wie beim TempCNN: gibt ein nn_module-Objekt zurück
    create_model <- function(input_data_columns, time_steps, class_count) {
      library(torch)
      
      act_ctor <- switch(tolower(params$activation),
                         "relu"     = nn_relu,
                         "tanh"     = nn_tanh,
                         "logistic" = nn_sigmoid,
                         nn_relu)
      
      nn_module(
        "MLPModule",
        initialize = function(settings, input_cols, time_steps, class_count) {
          self$flatten <- nn_flatten()
          
          input_dim <- length(input_cols) * time_steps
          hls <- as.integer(unlist(settings$hidden_layer_sizes))
          dr  <- as.numeric(unlist(settings$dropout_rates))
          if (length(dr) < length(hls)) dr <- c(dr, rep(tail(dr,1), length(hls)-length(dr)))
          
          layers <- list()
          last <- input_dim
          for (i in seq_along(hls)) {
            layers <- append(layers, list(
              nn_linear(in_features = last, out_features = hls[[i]]),
              act_ctor(),
              nn_dropout(p = dr[[i]])
            ))
            last <- hls[[i]]
          }
          layers <- append(layers, list(
            nn_linear(in_features = last, out_features = class_count)
          ))
          
          self$mlp <- do.call(nn_sequential, layers)
        },
        forward = function(x) {
          x <- self$flatten(x)   # (N,C,T) -> (N, C*T)
          self$mlp(x)            # callable wrapper
        }
      )(
        settings   = params,
        input_cols = input_data_columns,
        time_steps = time_steps,
        class_count = class_count
      )
    }
    
    list(
      parameters = params,
      create_model = create_model,
      classification = TRUE,
      subtype = "mlm-model"
    )
  }
)




##########################################
# --- LightTAE-Modelldefinition (Factory) ---
#' LightTAE for classification (experimental)
#'
#' @description
#' Lightweight Temporal Self-Attention über die Zeitachse.
#' Eingabeform: (N, C, T). Pro Zeitstempel Linear-Projektion auf d_model,
#' dann scaled dot-product Self-Attention über T, Global-Pooling und
#' MLP-Head zur Klassifikation.
#'
mlm_class_lighttae <- Process$new(
  id = "mlm_class_lighttae",
  description = "Initialize a Lightweight Temporal Self-Attention (LightTAE) classification model.",
  categories = as.array("machine-learning"),
  summary = "Initialize a LightTAE classification model",
  parameters = list(
    Parameter$new(
      name = "epochs",
      description = "Number of training epochs.",
      optional = TRUE,
      schema = list(type = "integer", minimum = 1, default = 150L)
    ),
    Parameter$new(
      name = "batch_size",
      description = "Size of the training batches (currently unused, full-batch).",
      optional = TRUE,
      schema = list(type = "integer", minimum = 1, default = 128L)
    ),
    Parameter$new(
      name = "optimizer",
      description = "Optimizer name.",
      optional = TRUE,
      schema = list(type = "string",
                    enum = c("adam","adabound","adabelief","madagrad","nadam","qhadam","radam","swats","yogi"),
                    default = "adam")
    ),
    Parameter$new(
      name = "learning_rate",
      description = "Learning rate for the optimizer.",
      optional = TRUE,
      schema = list(type = "number", minimum = 0, default = 5e-4)
    ),
    Parameter$new(
      name = "epsilon",
      description = "Numerical stability eps for optimizer.",
      optional = TRUE,
      schema = list(type = "number", minimum = 0, default = 1e-8)
    ),
    Parameter$new(
      name = "weight_decay",
      description = "L2 weight decay.",
      optional = TRUE,
      schema = list(type = "number", minimum = 0, default = 7e-4)
    ),
    Parameter$new(
      name = "lr_decay_epochs",
      description = "Step-interval for LR decay (epochs).",
      optional = TRUE,
      schema = list(type = "integer", minimum = 1, default = 50L)
    ),
    Parameter$new(
      name = "lr_decay_rate",
      description = "Multiplicative LR decay factor (1 = off).",
      optional = TRUE,
      schema = list(type = "number", minimum = 0, default = 1.0)
    ),
    Parameter$new(
      name = "seed",
      description = "Random seed (or NULL).",
      optional = TRUE,
      schema = list(type = c("integer","null"), default = NULL)
    )
  ),
  returns = list(
    description = "Model object to be trained via ml_fit.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(epochs = 150L,
                       batch_size = 128L,
                       optimizer = "adam",
                       learning_rate = 5e-4,
                       epsilon = 1e-8,
                       weight_decay = 7e-4,
                       lr_decay_epochs = 50L,
                       lr_decay_rate = 1.0,
                       seed = NULL, job) {
    
    params <- list(
      epochs = epochs,
      batch_size = batch_size,
      optimizer = optimizer,
      learning_rate = learning_rate,
      epsilon = epsilon,
      weight_decay = weight_decay,
      lr_decay_epochs = lr_decay_epochs,
      lr_decay_rate = lr_decay_rate,
      seed = seed
    )
    
    # Factory: baut ein nn_module mit Temporal Self-Attention
    create_model <- function(input_data_columns, time_steps, class_count) {
      library(torch)
      
      LightTAE <- nn_module(
        "LightTAE",
        initialize = function(settings, input_cols, time_steps, class_count) {
          self$C <- length(input_cols)
          self$T <- time_steps
          
          # Heuristik für Einbettungsdimension (leicht & stabil)
          d_model <- max(64L, as.integer(8L * ceiling(self$C / 2)))
          d_model <- min(d_model, 256L)
          self$d_model <- d_model
          
          self$proj_in <- nn_linear(self$C, d_model)     # pro Zeitstempel C -> d_model
          self$q_proj  <- nn_linear(d_model, d_model)
          self$k_proj  <- nn_linear(d_model, d_model)
          self$v_proj  <- nn_linear(d_model, d_model)
          
          self$attn_drop <- nn_dropout(p = 0.1)
          
          # Feed-Forward mit Residual (Post-Attention)
          self$ffn <- nn_sequential(
            nn_linear(d_model, 2L * d_model),
            nn_relu(),
            nn_dropout(p = 0.2),
            nn_linear(2L * d_model, d_model)
          )
          
          self$norm1 <- nn_layer_norm(d_model)
          self$norm2 <- nn_layer_norm(d_model)
          
          self$head <- nn_sequential(
            nn_layer_norm(d_model),
            nn_linear(d_model, class_count)
          )
        },
        
        forward = function(x) {
          # x: (N, C, T) -> (N, T, C)
          x <- x$permute(c(1, 3, 2))
          # Einbettung pro Zeitstempel: (N, T, C) -> (N, T, d_model)
          h <- self$proj_in(x)
          
          # Q,K,V: (N, T, d_model)
          q <- self$q_proj(h)
          k <- self$k_proj(h)
          v <- self$v_proj(h)
          
          # Scaled Dot-Product Attention über T
          # scores: (N, T, T)
          scale <- sqrt(as.numeric(self$d_model))
          scores <- torch_matmul(q, k$transpose(2, 3)) / scale
          attn   <- nnf_softmax(scores, dim = 3)
          attn   <- self$attn_drop(attn)
          
          # Context: (N, T, d_model)
          ctx <- torch_matmul(attn, v)
          
          # Residual + Norm
          h2 <- self$norm1(h + ctx)
          # FFN + Residual + Norm
          h3 <- self$ffn(h2)
          h3 <- self$norm2(h2 + h3)
          
          # Global (Temporal) Pooling: Mittelwert über T -> (N, d_model)
          z <- torch_mean(h3, dim = 2)
          
          # Klassifikationskopf: (N, class_count) [Logits]
          self$head(z)
        }
      )
      
      LightTAE(
        settings    = params,
        input_cols  = input_data_columns,
        time_steps  = time_steps,
        class_count = class_count
      )
    }
    
    list(
      parameters    = params,
      create_model  = create_model,
      classification = TRUE,
      subtype       = "mlm-model"
    )
  }
)



mlm_class_stgf <- Process$new(
  id = "mlm_class_stgf",
  description = "Spectral-Temporal Gated Fusion (STGF): Depthwise T-Convs + T-Attention/Mamba + SE-Gating + soft attention pooling.",
  categories = as.array("machine-learning"),
  summary = "STGF: leichtgewichtiges (C,T)-Netz für Satellitenklassifikation",
  parameters = list(
    Parameter$new("d_model", "Hidden-Kanäle nach Projektion",
                  schema = list(type="integer", minimum=32, default=128)),
    Parameter$new("num_blocks", "Anzahl Fusionsblöcke",
                  schema = list(type="integer", minimum=1, default=3L)),
    Parameter$new("kernel_size", "Kernel der Temporal-Convs",
                  schema = list(type="integer", minimum=3, default=7L)),
    Parameter$new("dilations", "Dilations je Block (recycelt)",
                  schema = list(type="array", items=list(type="integer"), default=list(1L,2L,4L))),
    Parameter$new("dropout", "Dropout pro Block",
                  schema = list(type="number", minimum=0, maximum=1, default=0.2)),
    Parameter$new("time_mixer", "Zeitmischer ('attention' oder 'mamba')",
                  schema = list(type="string", enum=c("attention","mamba"), default="attention")),
    Parameter$new("epochs", "Trainingsepochen",
                  schema = list(type="integer", minimum=1, default=120L), optional=TRUE),
    Parameter$new("batch_size", "Batchgröße (falls mini-batch)",
                  schema = list(type="integer", minimum=1, default=128L), optional=TRUE),
    Parameter$new("optimizer", "Optimizer",
                  schema = list(type="string",
                                enum=c("adam","adabound","adabelief","madagrad","nadam","qhadam","radam","swats","yogi"),
                                default="adam"), optional=TRUE),
    Parameter$new("learning_rate", "LR",
                  schema = list(type="number", minimum=0, default=5e-4), optional=TRUE),
    Parameter$new("seed", "Seed oder NULL",
                  schema = list(type=c("integer","null"), default=NULL), optional=TRUE)
  ),
  returns = list(
    description = "STGF-Parameter + Factory",
    schema = list(type="object", subtype="mlm-model")
  ),
  operation = function(
    d_model = 128L, num_blocks = 3L, kernel_size = 7L,
    dilations = list(1L,2L,4L), dropout = 0.2, time_mixer = "attention",
    epochs = 120L, batch_size = 128L, optimizer = "adam",
    learning_rate = 5e-4, seed = NULL, job) {
    
    params <- list(d_model=d_model, num_blocks=num_blocks,
                   kernel_size=kernel_size, dilations=dilations,
                   dropout=dropout, time_mixer=time_mixer,
                   epochs=epochs, batch_size=batch_size,
                   optimizer=optimizer, learning_rate=learning_rate,
                   seed=seed)
    
    create_model <- function(input_data_columns, time_steps, class_count) {
      library(torch)
      
      ## ---------- Helpers: robuste Typ-Casts + Defaults + Logging ----------
      to_int1  <- function(x, name="") {
        if (inherits(x, "torch_tensor")) x <- as.numeric(torch::as_array(x)[1])
        if (is.list(x)) x <- unlist(x, use.names = FALSE)
        if (length(x) == 0) stop(sprintf("to_int1(%s): empty", name))
        if (is.character(x)) {
          y <- suppressWarnings(as.numeric(x))
          if (all(is.na(y))) {
            x2 <- gsub("[^0-9\\-]+", "", x)  # "7L"->"7", "NA"->""
            y  <- suppressWarnings(as.numeric(x2))
          }
        } else y <- as.numeric(x)
        y <- suppressWarnings(as.integer(y[1]))
        if (is.na(y)) { message(sprintf("[to_int1] BAD %s; str(x):", name)); str(x); stop(sprintf("to_int1(%s)=NA", name)) }
        y
      }
      
      to_num1 <- function(x, name="") {
        if (inherits(x, "torch_tensor")) x <- as.numeric(torch::as_array(x)[1])
        if (is.list(x)) x <- unlist(x, use.names = FALSE)
        if (length(x) == 0) stop(sprintf("to_num1(%s): empty", name))
        if (is.character(x)) {
          y <- suppressWarnings(as.numeric(x))
          if (all(!is.finite(y))) {
            x2 <- gsub("[^0-9eE\\+\\-\\.]+", "", x)
            y  <- suppressWarnings(as.numeric(x2))
          }
        } else y <- as.numeric(x)
        if (!is.finite(y[1])) { message(sprintf("[to_num1] BAD %s; str(x):", name)); str(x); stop(sprintf("to_num1(%s) not finite", name)) }
        y[1]
      }
      
      to_intv <- function(x, name="") {
        if (inherits(x, "torch_tensor")) x <- as.numeric(torch::as_array(x))
        if (is.character(x)) { x <- gsub("[^0-9\\-\\s,]+", "", x); x <- strsplit(x, "[,\\s]+")[[1]] }
        y <- as.integer(unlist(x, use.names = FALSE))
        if (length(y) == 0 || any(is.na(y))) { message(sprintf("[to_intv] BAD %s; str(x):", name)); str(x); stop(sprintf("to_intv(%s) bad", name)) }
        y
      }
      
      get_int_def <- function(x, default, name="") {
        val <- tryCatch(to_int1(x, name), error=function(e) NA_integer_)
        if (is.na(val) || val <= 0) { message(sprintf("[cfg] %s invalid -> default %d", name, as.integer(default))); val <- as.integer(default) }
        val
      }
      get_num_def <- function(x, default, name="") {
        val <- tryCatch(to_num1(x, name), error=function(e) NA_real_)
        if (!is.finite(val)) { message(sprintf("[cfg] %s invalid -> default %.4f", name, default)); val <- default }
        val
      }
      get_choice_def <- function(x, choices, default, name="") {
        s <- tolower(as.character(x))[1]
        if (!s %in% tolower(choices)) { message(sprintf("[cfg] %s invalid ('%s') -> default '%s'", name, s, default)); s <- default }
        s
      }
      log_shape <- function(tag, ten) {
        message(sprintf("[%s] size=(%s) dtype=%s",
                        tag, paste(as.integer(ten$size()), collapse="x"), ten$dtype))
      }
      
      ## ---------- STGF-Block ----------
      STGFBlock <- nn_module(
        "STGFBlock",
        initialize = function(in_ch, d_model, ksize, dilation, pdrop, time_mixer="attention") {
          in_ch   <- to_int1(in_ch, "block.in_ch")
          d_model <- to_int1(d_model, "block.d_model")
          ksize   <- to_int1(ksize, "block.kernel")
          dilation<- to_int1(dilation, "block.dilation")
          pdrop   <- to_num1(pdrop, "block.dropout")
          tmix    <- as.character(time_mixer)
          
          pad <- as.integer(((ksize - 1L) %/% 2L) * dilation)
          message(sprintf("[STGFBlock.init] in_ch=%d d=%d k=%d dil=%d pad=%d mixer=%s pdrop=%.3f",
                          in_ch, d_model, ksize, dilation, pad, tmix, pdrop))
          
          self$time_mixer <- tmix
          
          # Depthwise Temporal Conv -> (N, in_ch, T)
          self$dw <- nn_conv1d(in_channels=in_ch, out_channels=in_ch,
                               kernel_size=ksize, stride=1, padding=pad,
                               dilation=dilation, groups=in_ch, bias=TRUE)
          # Pointwise -> d_model
          self$pw <- nn_conv1d(in_channels=in_ch, out_channels=d_model,
                               kernel_size=1, bias=TRUE)
          
          if (self$time_mixer == "attention") {
            self$ln1 <- nn_layer_norm(d_model)
            self$q   <- nn_linear(d_model, d_model)
            self$k   <- nn_linear(d_model, d_model)
            self$v   <- nn_linear(d_model, d_model)
            self$attn_drop <- nn_dropout(p=pdrop)
          } else {
            # Mamba-artiger selektiver rekurrenter Mixer (O(T))
            self$ln1    <- nn_layer_norm(d_model)
            self$gate_u <- nn_linear(d_model, d_model)
            self$gate_f <- nn_linear(d_model, d_model)
            self$cand   <- nn_linear(d_model, d_model)
          }
          
          self$ff <- nn_sequential(
            nn_linear(d_model, 4L*d_model),
            nn_gelu(),
            nn_dropout(p=pdrop),
            nn_linear(4L*d_model, d_model)
          )
          self$ln2 <- nn_layer_norm(d_model)
          
          # Squeeze-Excitation (kanalweise)
          se_hidden <- max(8L, as.integer(d_model/4))
          self$se <- nn_sequential(
            nn_linear(d_model, se_hidden),
            nn_relu(),
            nn_linear(se_hidden, d_model),
            nn_sigmoid()
          )
        },
        forward = function(x) {
          message("[STGFBlock.fwd] ----")
          log_shape("x", x)
          
          h <- self$dw(x); log_shape("dw", h)
          h <- self$pw(h); log_shape("pw", h)
          
          h_t <- h$permute(c(1,3,2)); log_shape("permute(N,T,d)", h_t)
          
          if (self$time_mixer == "attention") {
            q <- self$q(h_t); k <- self$k(h_t); v <- self$v(h_t)
            log_shape("q", q); log_shape("k", k); log_shape("v", v)
            
            scale <- sqrt(as.numeric(q$size(3)))
            scores <- torch_matmul(q, k$transpose(2,3)) / scale  # (N,T,T)
            log_shape("scores", scores)
            
            a <- nnf_softmax(scores, dim=3)
            a <- self$attn_drop(a)
            ctx <- torch_matmul(a, v)                            # (N,T,d)
            log_shape("ctx(attn)", ctx)
            h1  <- self$ln1(h_t + ctx)
          } else {
            # Mamba-artig: s_t = f_t*s_{t-1} + (1-f_t)*g_t ; y_t = u_t*s_t
            u <- torch_sigmoid(self$gate_u(h_t))
            f <- torch_sigmoid(self$gate_f(h_t))
            g <- torch_tanh(self$cand(h_t))
            log_shape("u", u); log_shape("f", f); log_shape("g", g)
            
            Tlen <- as.integer(h_t$size(2))
            s <- torch_zeros_like(g[, 1, ])  # (N,d)
            s_list <- vector("list", Tlen)
            for (t in 1:Tlen) {
              gt <- g[, t, ]
              ft <- f[, t, ]
              s  <- ft * s + (1 - ft) * gt
              s_list[[t]] <- s$unsqueeze(2)  # (N,1,d)
            }
            S <- torch_cat(s_list, dim=2)    # (N,T,d)
            log_shape("S(state)", S)
            ctx <- u * S                     # (N,T,d)
            log_shape("ctx(mamba)", ctx)
            h1  <- self$ln1(h_t + ctx)
          }
          
          h2  <- self$ff(h1)
          h2  <- self$ln2(h1 + h2)                              # (N,T,d)
          log_shape("post-ffn", h2)
          
          # SE-Gate (kanalweise)
          z   <- torch_mean(h2, dim=2)                          # (N,d)
          gch <- self$se(z)$unsqueeze(2)                        # (N,1,d)
          h2g <- h2 * gch                                       # (N,T,d)
          log_shape("after-SE", h2g)
          
          y <- h2g$permute(c(1,3,2))                            # -> (N,d,T)
          log_shape("block-out(N,d,T)", y)
          y
        }
      )
      
      ## ---------- STGF-Top ----------
      STGF <- nn_module(
        "STGF",
        initialize = function(settings, C, T, class_count) {
          message("[STGF.init] raw types:")
          message(sprintf(" - kernel_size: %s", paste(capture.output(str(settings$kernel_size)), collapse=" ")))
          message(sprintf(" - num_blocks:  %s", paste(capture.output(str(settings$num_blocks)),  collapse=" ")))
          message(sprintf(" - dilations:   %s", paste(capture.output(str(settings$dilations)),  collapse=" ")))
          
          self$C <- get_int_def(C, default = 1L, name = "C")
          self$T <- get_int_def(T, default = 1L, name = "T")
          
          # d_model mit Fallback-Heuristik aus C
          d_raw <- settings$d_model
          d_parsed <- tryCatch(to_int1(d_raw, "d_model"), error=function(e) NA_integer_)
          if (is.na(d_parsed) || d_parsed <= 0) {
            d_fallback <- max(64L, as.integer(8L * ceiling(self$C / 2)))
            message(sprintf("[cfg] d_model invalid -> fallback %d", d_fallback))
            self$d <- d_fallback
          } else self$d <- d_parsed
          
          nb  <- get_int_def(settings$num_blocks,  default = 3L, name = "num_blocks")
          ksz <- get_int_def(settings$kernel_size, default = 7L, name = "kernel_size")
          if (ksz < 3L) { message("[cfg] kernel_size < 3 -> 3"); ksz <- 3L }
          if ((ksz %% 2L) == 0L) { ksz <- ksz + 1L; message(sprintf("[cfg] kernel_size forced odd -> %d", ksz)) }
          
          drp <- get_num_def(settings$dropout, default = 0.2, name = "dropout")
          drp <- max(0, min(1, drp))
          
          tmx <- get_choice_def(settings$time_mixer, c("attention","mamba"),
                                default = "attention", name = "time_mixer")
          
          dil <- tryCatch(to_intv(settings$dilations, "dilations"), error=function(e) c(1L,2L,4L))
          if (length(dil) < nb) dil <- c(dil, rep(tail(dil, 1), nb - length(dil)))
          
          message(sprintf("[STGF.init] C=%d T=%d d=%d nb=%d k=%d dilations=%s drop=%.3f mixer=%s",
                          self$C, self$T, self$d, nb, ksz, paste(dil, collapse=","),
                          drp, tmx))
          
          self$blocks <- nn_module_list()
          in_ch <- self$C
          for (i in seq_len(nb)) {
            self$blocks$append(
              STGFBlock(in_ch, self$d, ksz, dil[[i]], drp, tmx)
            )
            in_ch <- self$d
          }
          self$nb <- nb  # Anzahl der Blöcke merken
          
          # Soft-Attention-Pooling über T: Query-Parameter (d,)
          self$pool_query <- nn_parameter(torch_randn(self$d))
          
          self$head <- nn_sequential(
            nn_layer_norm(self$d),
            nn_linear(self$d, class_count)
          )
        },
        forward = function(x) {
          message("[STGF.fwd] =====")
          log_shape("x_in", x)
          
          h <- x
          # Index-basierte Iteration über ModuleList (R/torch)
          n_blocks <- if (!is.null(self$nb)) as.integer(self$nb) else length(self$blocks)
          if (is.na(n_blocks) || n_blocks < 1) stop("STGF has no blocks.")
          for (i in seq_len(n_blocks)) {
            h <- self$blocks[[i]](h)          # (N, d, T)
          }
          log_shape("h_after_blocks", h)
          
          # Soft-Attention-Pooling
          ht <- h$permute(c(1,3,2))           # (N, T, d)
          log_shape("ht(N,T,d)", ht)
          
          logits_t <- torch_matmul(ht, self$pool_query)$squeeze(-1)  # (N,T)
          w <- nnf_softmax(logits_t, dim=2)$unsqueeze(3)             # (N,T,1)
          
          lt_arr <- as.numeric(torch::as_array(logits_t))
          if (length(lt_arr) > 0) {
            message(sprintf("[pool] T=%d; logits min/max: %.4f / %.4f",
                            as.integer(ht$size(2)), min(lt_arr), max(lt_arr)))
          }
          
          z <- torch_sum(ht * w, dim=2)      # (N,d)
          log_shape("z(N,d)", z)
          
          y <- self$head(z)                  # (N, classes) [Logits]
          log_shape("head_out(N,classes)", y)
          y
        }
      )
      
      message(sprintf("[factory] input C=%d, T=%d, classes=%d",
                      length(input_data_columns), time_steps, class_count))
      STGF(settings=params, C=length(input_data_columns), T=time_steps, class_count=class_count)
    }
    
    list(
      parameters     = params,
      create_model   = create_model,
      classification = TRUE,
      subtype        = "mlm-model"
    )
  }
)



#######################################
#' Load an ML model from a URL (asynchronous subprocess)
#'
#' @description
#' Downloads a machine learning model file (`.rds` or `.onnx`) from the specified URL.
#' Supports internal URLs (e.g. `http://localhost:8000/download/…`) by loading directly
#' from the shared temp directory, and external URLs via HTTP(S). Google Drive links
#' are auto-converted to direct download URLs.
#'
#' @param url Character. URL from which to load the model file.
#'
#' @return
#' If the model is `.rds`, returns the loaded R object; if `.onnx`, returns a raw vector
#' of the ONNX file contents.
#'
load_ml_model <- Process$new(
  id = "load_ml_model",
  description = "Loads a machine learning model (.rds or .onnx) directly from a URL - asynchronously via subprocess.",
  categories = as.array("model-management", "data-loading"),
  summary = "Loads a model from a URL (only .rds or .onnx), even if it comes from the same server.",
  
  parameters = list(
    Parameter$new(
      name = "url",
      description = "The URL from which the model is to be loaded",
      schema = list(type = "string")
    )
  ),
  
  returns = ml_model,
  
  operation = function(url, job) {
    library(tools)
    library(httr)
    message("load_ml_model is started...")
    
    message("Receive model URL: ", url)
    if (grepl("^http://localhost:8000/download/", url)) {
      message("Internal URL recognized - file path is used directly")
      file_name <- basename(url)
      shared_dir <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      file_path <- file.path(shared_dir, file_name)
      
      if (!startsWith(normalizePath(file_path), normalizePath(shared_dir))) {
        stop("Access outside the permitted directory!")
      }
      if (!file.exists(file_path)) {
        stop("File was not found: ", file_path)
      }
      ext <- tolower(file_ext(file_path))
      if (ext == "rds") {
        return(file_path)
      } else if (ext == "onnx") {
        return(readBin(file_path, "raw", n = file.info(file_path)$size))
      } else {
        stop("Only .rds and .onnx are supported (currently: ", ext, ")")
      }
    }
    message("External URL recognized - try HTTP access")
    if (grepl("drive\\.google\\.com/file/d/", url)) {
      drive_match <- regmatches(url, regexec("drive\\.google\\.com/file/d/([^/?]+)", url))[[1]]
      if (length(drive_match) > 1) {
        file_id <- drive_match[2]
        url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
        message("Google Drive link recognized and converted: ", url)
      } else {
        stop("Could not extract a valid Google Drive file ID.")
      }
    }
    message("response is provided")
    response <- httr::GET(url)
    if (httr::status_code(response) != 200) {
      stop("Error retrieving the URL: ", httr::status_code(response))
    }
    content_disposition <- headers(response)[["content-disposition"]]
    file_name <- basename(url)
    if (!is.null(content_disposition)) {
      match <- regmatches(content_disposition, regexec('filename="?([^";]+)"?', content_disposition))[[1]]
      if (length(match) > 1) {
        file_name <- match[2]
      }
    }
    ext <- tolower(file_ext(file_name))
    raw_data <- content(response, "raw")
    if (ext == "rds") {
      tmp <- tempfile(fileext = ".rds")
      writeBin(raw_data, tmp)
      model <- readRDS(tmp)
      return(model)
      
    } else if (ext == "onnx") {
      return(raw_data)
      
    } else {
      stop("Only .rds and .onnx are supported (currently: ", ext, ")")
    }
  }
)
#######################################################
#' Save trained ML model as ONNX, JSON (MLM-STAC), and RDS
#'
#' @description
#' Saves a trained machine learning model in ONNX, JSON (MLM-STAC) and RDS formats.
#' For Torch (nn_module) models, also produces a TorchScript (.pt) version.
#' Handles caret `train` objects and torch `nn_module`s, enriches ONNX with metadata,
#' and returns download links for all artifacts.
#'
#' @param data Object of subtype "mlm-model". A trained ML model: a caret `train` object or a torch `nn_module`.
#' @param name Character. Unique identifier for the model; used as base filename for saved artifacts.
#' @param tasks List. The use case(s) of the model (e.g., `list("classification")` or `list("regression")`).
#' @param options List, optional. Additional key-value options for metadata (e.g., `list("mlm:pretrained" = TRUE)`).
#'
#' @return
#' A named list with file paths to the exported artifacts:
#' \describe{
#'   \item{onnx}{Path to the saved ONNX model file}
#'   \item{rds}{Path to the saved RDS (or raw RDS) file}
#'   \item{json}{Path to the saved MLM-STAC JSON metadata file}
#' }
save_ml_model <- Process$new(
  id = "save_ml_model",
  description = "Saves a trained machine learning model in ONNX, JSON (MLM-STAC) and RDS formats. For Torch models, a TorchScript version is also produced.",
  categories = as.array("cubes", "Machine Learning"),
  summary = "Save trained ML model as ONNX, JSON, and RDS",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A trained ML model.",
      schema = list(type = "object", subtype = "mlm-model")
    ),
    Parameter$new(
      name = "name",
      description = "The unique identifier for the model.",
      schema = list(type = "string")
    ),
    Parameter$new(
      name = "tasks",
      description = "The use case(s) of the model (e.g., classification or regression).",
      schema = list(type = "list")
    ),
    Parameter$new(
      name = "options",
      description = "Additional options as key-value pairs.",
      schema = list(type = "object"),
      optional = TRUE
    )
  ),
  returns = ml_model,
  operation = function(data, name, tasks, options, job) {
    message("Save model is started...")
    shared_dir <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
    message("shared_dir gesetzt: ", shared_dir)
    
    base_name <- name
    
    find_python_bin <- function() {
      bin_env <- Sys.getenv("PYTHON_BIN", Sys.getenv("RETICULATE_PYTHON", ""))
      if (nzchar(bin_env) && file.exists(bin_env)) return(bin_env)
      if (requireNamespace("reticulate", quietly = TRUE)) {
        pycfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
        if (!is.null(pycfg) && nzchar(pycfg$python) && file.exists(pycfg$python)) return(pycfg$python)
        try({
          envs <- reticulate::conda_list()
          for (nm in c("base", envs$name)) {
            py <- tryCatch(reticulate::conda_python(nm), error = function(e) "")
            if (nzchar(py) && file.exists(py)) return(py)
          }
        }, silent = TRUE)
      }
      py3 <- Sys.which("python3"); if (nzchar(py3)) return(py3)
      py  <- Sys.which("python");  if (nzchar(py))  return(py)
      stop("No suitable Python interpreter found!")
    }
    
  
    
    
    ensure_extension <- function(filename, ext) {
      if (!grepl(paste0("\\.", ext, "$"), filename, ignore.case = TRUE)) paste0(filename, ".", ext) else filename
    }
    
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    # --- helpers / classifiers ---
    is_caret_svm <- function(x) {
      inherits(x, "train") && inherits(x$finalModel, "ksvm")
    }
    
    detect_svm_kernel <- function(train_obj) {
      stopifnot(is_caret_svm(train_obj))
      kobj <- train_obj$finalModel@kernelf
      kcl  <- tolower(class(kobj))
      message("kernlab kernel classes: ", paste(kcl, collapse = ", "))
      if (any(kcl %in% c("vanilladot", "vanillakernel", "lineardot", "linear"))) return("linear")
      if (any(kcl %in% c("rbfdot", "rbfkernel", "gaussiandot", "gaussian")))     return("rbf")
      if (any(kcl %in% c("polydot", "polykernel", "poly")))                      return("poly")
      ktxt <- paste(kcl, collapse = " ")
      if (grepl("vanilla|linear", ktxt)) return("linear")
      if (grepl("rbf|gauss|radial", ktxt)) return("rbf")
      if (grepl("poly", ktxt)) return("poly")
      stop("Unsupported/unknown SVM kernel (kernlab). Class vector = ", paste(class(kobj), collapse="/"))
    }
    
    detect_model_type <- function(model) {
      fm <- model$finalModel
      
      # hilfreiches Debugging, das *wirklich* etwas ausgibt:
      kcls_vec <- try(class(fm@kernelf), silent = TRUE)

      if (inherits(fm, "ksvm")) {
        # 1) Primär über deine robuste Kernel-Detektion
        kern <- try(detect_svm_kernel(model), silent = TRUE)
        if (!inherits(kern, "try-error") && nzchar(kern)) {
          return(switch(tolower(kern),
                        linear = "svmLinear",
                        rbf    = "svmRadial",
                        poly   = "svmPoly",
                        "svmLinear"  # vorsichtiger Fallback
          ))
        }
        
        # 2) Fallbacks, falls detect_svm_kernel() scheitert
        kcls <- tolower(if (!inherits(kcls_vec, "try-error")) kcls_vec[1] else "")
        if (kcls %in% c("vanilladot","vanillakernel","lineardot","linear")) return("svmLinear")
        if (kcls %in% c("rbfdot","rbfkernel","gaussiandot","gaussian"))     return("svmRadial")
        if (kcls %in% c("polydot","polykernel","poly"))                      return("svmPoly")
        ktxt <- tolower(paste(kcls_vec, collapse = " "))
        if (grepl("vanilla|linear", ktxt)) return("svmLinear")
        if (grepl("rbf|gauss|radial", ktxt)) return("svmRadial")
        if (grepl("poly", ktxt)) return("svmPoly")
        
        warning("Unbekannter ksvm-Kernel (", ktxt, "); setze 'svmLinear' als Fallback.")
        return("svmLinear")
      }
      
      # Nicht-SVM
      safe_text <- paste(c(model$modelInfo$label %||% "", model$method %||% ""), collapse = " ")
      has <- function(pat) isTRUE(grepl(pat, safe_text, ignore.case = TRUE))
      if (has("random[ ]*forest|^rf$"))         return("random_forest")
      if (has("xgboost|xgb|gradient[ ]*boost")) return("xgbTree")
      
      stop("Model type could not be recognized (finalModel class: ",
           paste(class(fm), collapse = "/"),
           ", method: ", model$method %||% "?", ").")
    }
    
    
    
    

   
    
    # used in checks (linear)
    unscale_vec <- function(x, mu, sig) mu + x * sig
    pred_from_dec <- function(dec_sub, classes, posneg_pairs, tie_rule = c(">0",">=0")) {
      tie_rule <- match.arg(tie_rule)
      apply(dec_sub, 1L, function(z) {
        votes <- setNames(integer(length(classes)), classes)
        for (j in seq_along(posneg_pairs)) {
          ci <- posneg_pairs[[j]][1]; cj <- posneg_pairs[[j]][2]
          sel <- if (tie_rule == ">0") z[j] > 0 else z[j] >= 0
          if (sel) votes[ci] <- votes[ci] + 1L else votes[cj] <- votes[cj] + 1L
        }
        names(which.max(votes))
      })
    }
    
    # ---------- ONNX linear (OvO) ----------
    export_ksvm_linear_onnx <- function(train_obj, out_base,
                                        use_rule = "majority",
                                        primary_output = "idx1",
                                        dtype = "float32",
                                        do_checks = TRUE) {
      stopifnot(inherits(train_obj, "train"), inherits(train_obj$finalModel, "ksvm"))
      
      model_r <- train_obj
      model   <- model_r$finalModel
      
      classes <- as.character(model_r$levels)
      K       <- length(classes)                  # <- WICHTIG: fehlt bei dir
      feat    <- setdiff(colnames(model_r$trainingData), ".outcome")
      pp      <- model_r$preProcess
      means   <- as.numeric(pp$mean[feat]); names(means) <- feat
      sds     <- as.numeric(pp$std[feat]);  names(sds)   <- feat
     

      

      Xscaled <- as.matrix(model_r$trainingData[, feat, drop = FALSE])
      storage.mode(Xscaled) <- "double"
      y_true <- as.character(model_r$trainingData$.outcome)
      
      library(kernlab)
      get_decision_matrix <- function(model, X) {
        dec <- try(predict(model, X, type = "decision"), silent = TRUE)
        if (!inherits(dec, "try-error")) {
          dec <- as.matrix(dec); storage.mode(dec) <- "double"; return(dec)
        }
        dec <- kernlab::decision(model, X)
        dec <- as.matrix(dec); storage.mode(dec) <- "double"; dec
      }
      
      
      
      dec <- get_decision_matrix(model, Xscaled)


      C2 <- ncol(dec)
      expected_C2 <- K * (K - 1) / 2
      if (is.null(C2) || C2 != expected_C2) {
        stop(sprintf("OvO columns mismatch: NCOL(dec)=%s, expected %s (K=%s).",
                     as.character(C2), expected_C2, K))
      }
      # LS-Rekonstruktion je OvO-Spalte
      # ──────────────────────────────────────────────────────────────────────────────
      # 3) W,b direkt aus dec via Least-Squares + robuste Paarzuordnung (Assignment)
      # ──────────────────────────────────────────────────────────────────────────────
      # (a) W,b je Spalte j über LS
      X_aug <- cbind(Xscaled, 1.0)
      storage.mode(X_aug) <- "double"
      n_feat <- ncol(Xscaled)
      
      ls_fit <- function(y, X) as.numeric(qr.solve(X, y))
      weights_from_dec <- vector("list", C2)
      for (j in 1:C2) {
        th <- ls_fit(dec[, j], X_aug)
        weights_from_dec[[j]] <- list(W = th[1:n_feat], b = th[n_feat + 1])
      }
      
      # (b) Score-Matrix: |cor(dec[,j], t_ab)|, t_ab = +1 (Klasse a), -1 (Klasse b), 0 sonst
      all_pairs <- combn(classes, 2, simplify = FALSE)   # Länge = C2
      score_mat <- matrix(0, nrow = C2, ncol = C2)
      for (p in seq_along(all_pairs)) {
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        t_ab <- numeric(nrow(Xscaled))
        t_ab[y_true == a] <-  1
        t_ab[y_true == b] <- -1
        # Korrelationsvektor zu ALLEN dec-Spalten auf einmal
        sc <- suppressWarnings(apply(dec, 2, function(z) abs(cor(z, t_ab, use="complete.obs"))))
        sc[!is.finite(sc)] <- 0
        score_mat[, p] <- sc  # Reihen=Spalten j; Spalten=Pairs p
      }
      
      cost <- 1 - score_mat
      library(clue)        # solve_LSAP (Hungarian)
      assign_j_to_p <- as.integer(solve_LSAP(cost))   # für jede j → welcher pair-index p
      pair_name <- function(ci, cj) paste0(ci, "_vs_", cj)
      
      weights_aligned <- vector("list", C2)
      posneg_pairs    <- vector("list", C2)  # z>=0 → ci gewinnt
      for (j in 1:C2) {
        p <- assign_j_to_p[j]
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        m_a <- mean(dec[y_true == a, j], na.rm = TRUE)
        m_b <- mean(dec[y_true == b, j], na.rm = TRUE)
        if (m_a >= m_b) { ci <- a; cj <- b } else { ci <- b; cj <- a }
        wj <- weights_from_dec[[j]]
        weights_aligned[[j]] <- c(wj, list(class_pair = c(ci, cj), name = pair_name(ci, cj)))
        posneg_pairs[[j]]    <- c(ci, cj)
      }
      
      # (e) Konsistenz: Rekonstruktion muss perfekt zu dec passen (bis auf Rundung)
      Z_hat <- sapply(seq_len(C2), function(j) as.numeric(Xscaled %*% weights_aligned[[j]]$W + weights_aligned[[j]]$b))
      cors  <- diag(abs(cor(Z_hat, dec)))
      cat("Min/Median/Max |cor(Z_hat, dec)|: ",
          sprintf("%.6f / %.6f / %.6f\n", min(cors), median(cors), max(cors)))
      stopifnot(all(cors > 0.999))
      
      # ──────────────────────────────────────────────────────────────────────────────
      # 4) Regeln testen (welche passt zu caret?)
      # ──────────────────────────────────────────────────────────────────────────────
      pred_from_dec <- function(dec_rowmat, classes, posneg_pairs, rule = c("majority","margin","signedsum")) {
        rule <- match.arg(rule)
        K    <- length(classes); C2 <- length(posneg_pairs)
        apply(dec_rowmat, 1, function(zrow) {
          acc <- setNames(numeric(K), classes)
          for (j in 1:C2) {
            ci <- posneg_pairs[[j]][1]; cj <- posneg_pairs[[j]][2]
            z  <- zrow[j]
            if (rule == "majority") {
              if (z >= 0) acc[ci] <- acc[ci] + 1 else acc[cj] <- acc[cj] + 1
            } else if (rule == "margin") {
              if (z >= 0) acc[ci] <- acc[ci] + z else acc[cj] <- acc[cj] + (-z)
            } else { # signedsum
              acc[ci] <- acc[ci] + z
              acc[cj] <- acc[cj] - z
            }
          }
          names(which.max(acc))
        })
      }
      
      set.seed(1)
      rows <- sample(seq_len(nrow(Xscaled)), size = min(100, nrow(Xscaled)))
      pred_caret <- sapply(rows, function(r) {
        x_raw <- unscale_vec(as.numeric(Xscaled[r, ]), means, sds)
        df    <- as.data.frame(t(x_raw)); colnames(df) <- feat
        as.character(predict(model_r, newdata = df))
      })
      
      pred_major  <- pred_from_dec(dec[rows, , drop=FALSE], classes, posneg_pairs, "majority")
      pred_margin <- pred_from_dec(dec[rows, , drop=FALSE], classes, posneg_pairs, "margin")
      pred_signed <- pred_from_dec(dec[rows, , drop=FALSE], classes, posneg_pairs, "signedsum")
      
      cat("\n=== comparison of rules (", length(rows), " Samples) ===\n", sep = "")
      cat("caret vs majority : ", mean(pred_caret == pred_major),  "\n")
      cat("caret vs margin   : ", mean(pred_caret == pred_margin), "\n")
      cat("caret vs signedsum: ", mean(pred_caret == pred_signed), "\n")
      
      # >>> Regel wählen – bei dir war majority besser <<<
      use_rule <- "majority"
      
      # ONNX bauen
      onnx_path   <- paste0(out_base, "_svm_ovo_linear_", use_rule, ".onnx")
      labels_json <- sub("\\.onnx$", "_labels.json", onnx_path)

      build_onnx_for_rule(
        use_rule        = use_rule,
        weights_aligned = weights_aligned,
        feature_names   = feat,
        classes         = classes,
        means           = means,
        sds             = sds,
        out_path        = onnx_path
      )
      
      lbls <- setNames(as.list(classes), as.character(seq_along(classes)))
      jsonlite::write_json(lbls, labels_json, pretty = TRUE, auto_unbox = TRUE)
      
      list(onnx = onnx_path, labels_json = labels_json)
    }
    
    
    
    
  
    
    # --------- POLY ----------
    export_ksvm_poly_onnx <- function(train_obj, out_base,
                                      use_rule = "majority",
                                      primary_output = "idx1",
                                      dtype = "float32",
                                      do_checks = TRUE) {
      message("Export SVM Poly start...")
      model_r <- train_obj
      model_ksvm <- model_r$finalModel              # reines kernlab-Objekt
      
      classes       <- as.character(model_r$levels)
      feature_names <- setdiff(colnames(model_r$trainingData), ".outcome")
      
      # Preprocess-Objekt & Parameter (wie im Training)
      pp    <- model_r$preProcess
      means <- as.numeric(pp$mean[feature_names]); names(means) <- feature_names
      sds   <- as.numeric(pp$std[feature_names]);  names(sds)   <- feature_names
      
      # Rohwerte der Trainingsdaten -> caret-Skala wie im Training
      Xraw_train <- as.data.frame(model_r$trainingData[, feature_names, drop = FALSE])
      Xscaled    <- as.matrix(predict(pp, Xraw_train))
      storage.mode(Xscaled) <- "double"
      colnames(Xscaled) <- feature_names  # Fixierung der Feature-Reihenfolge
      
      K  <- length(classes)
      C2 <- K * (K - 1) / 2
      stopifnot(C2 >= 1)
      # Kernel-Parameter (polydot)
      library(kernlab)
      kp     <- kernlab::kpar(model_ksvm@kernelf)
      deg    <- as.numeric(kp$degree)
      scaleK <- as.numeric(kp$scale)
      offK   <- as.numeric(kp$offset)
      
      # ==============================================================================
      # 2) Decisions (N x C2) auf caret-Skala
      # ==============================================================================
      message("decsion started...")
      library(kernlab)
      get_decision_matrix <- function(model, Xscaled) {
        dec <- try(predict(model, Xscaled, type = "decision"), silent = TRUE)
        if (!inherits(dec, "try-error")) return(as.matrix(dec))
        as.matrix(kernlab::decision(model, Xscaled))
      }
      dec_raw <- get_decision_matrix(model_ksvm, Xscaled)
      stopifnot(ncol(dec_raw) == C2)
      message("decssion done")
      # ==============================================================================
      # 3) Votes aus kernlab (Referenz) + Sanity-Checks
      # ==============================================================================
      pv <- try(predict(model_ksvm, Xscaled, type = "votes"), silent = TRUE)
      if (inherits(pv, "try-error")) stop("Dieses ksvm liefert keine 'votes' – abbrechen.")
      rn <- rownames(pv); if (is.null(rn) || any(!nzchar(rn))) rn <- classes
      stopifnot(length(rn) == K)
      cat("C2 = ", C2, " | range(colSums(pv)) = ", paste(range(colSums(pv)), collapse=".."), "\n", sep="")
      
      # ==============================================================================
      # 4) k → (a,b) via Korrelation der Vorzeichensequenzen (Initial-Mapping)
      # ==============================================================================
      s_sign_strict <- function(z) ifelse(z > 0, 1L, -1L)
      all_pairs <- combn(rn, 2, simplify = FALSE)
      
      safe_cor <- function(x, y) {
        cr <- suppressWarnings(cor(x, y, use = "complete.obs"))
        if (!is.finite(cr)) 0 else cr
      }
      
      S <- matrix(0, nrow = C2, ncol = C2)
      for (p in seq_along(all_pairs)) {
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        d <- as.numeric(pv[match(a, rn), ] - pv[match(b, rn), ])
        for (k in 1:C2) S[k, p] <- abs(safe_cor(s_sign_strict(dec_raw[, k]), d))
      }
      pair_id_for_k <- as.integer(clue::solve_LSAP(1 - S))   # k -> pair (a,b)
      stopifnot(length(unique(pair_id_for_k)) == C2)
      
      # Orientierung je k
      pos_class_k <- neg_class_k <- character(C2)
      for (k in 1:C2) {
        p <- pair_id_for_k[k]
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        d <- as.numeric(pv[match(a, rn), ] - pv[match(b, rn), ])
        s <- s_sign_strict(dec_raw[, k])
        cr <- safe_cor(s, d)
        if (cr >= 0) { pos_class_k[k] <- a; neg_class_k[k] <- b } else { pos_class_k[k] <- b; neg_class_k[k] <- a }
      }
      
      # ==============================================================================
      # 5) Tie-Regel + Rekonstruktion (testen >0 vs >=0)
      # ==============================================================================
      votes_from_dec_rule_tie <- function(zrow, pair_id_for_k, pos_k, neg_k, cls, tie_rule = c(">0",">=0")){
        tie_rule <- match.arg(tie_rule)
        acc <- setNames(integer(length(cls)), cls)
        sel <- if (tie_rule == ">0") zrow > 0 else zrow >= 0
        for (k in seq_along(pair_id_for_k)) {
          if (sel[k]) acc[pos_k[k]] <- acc[pos_k[k]] + 1 else acc[neg_k[k]] <- acc[neg_k[k]] + 1
        }
        acc
      }
      build_votes_from_state_tie <- function(dec_raw, pair_id_for_k, pos_k, neg_k, rn, tie_rule = c(">0",">=0")){
        tie_rule <- match.arg(tie_rule)
        N <- nrow(dec_raw)
        out <- matrix(0L, nrow = length(rn), ncol = N, dimnames = list(rn, NULL))
        for (i in 1:N) out[, i] <- votes_from_dec_rule_tie(dec_raw[i, ], pair_id_for_k, pos_k, neg_k, rn, tie_rule)
        out
      }
      l1_state_tie <- function(dec_raw, pair_id_for_k, pos_k, neg_k, rn, pv, tie_rule){
        v <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_k, neg_k, rn, tie_rule)
        mean(colSums(abs(v - pv)))
      }
      
      # Teste beide Tie-Regeln
      our_votes_gt <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule=">0")
      our_votes_ge <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule=">=0")
      l1_gt <- mean(colSums(abs(our_votes_gt - pv)))
      l1_ge <- mean(colSums(abs(our_votes_ge - pv)))
      acc_gt <- mean(rn[apply(our_votes_gt, 2, which.max)] == rn[apply(pv, 2, which.max)])
      acc_ge <- mean(rn[apply(our_votes_ge, 2, which.max)] == rn[apply(pv, 2, which.max)])
      cat(sprintf("Tie '>0':   L1=%.12f | Top1-Acc=%.3f\n", l1_gt, acc_gt))
      cat(sprintf("Tie '>=0':  L1=%.12f | Top1-Acc=%.3f\n", l1_ge, acc_ge))
      
      # Wähle die Tie-Regel mit kleinerem L1 (bei Gleichstand >=0 bevorzugen)
      tie_rule <- if (l1_ge <= l1_gt) ">=0" else ">0"
      cat("→ Verwende Tie-Regel: ", tie_rule, "\n", sep="")
      
      # ==============================================================================
      # 6) Optimierung: Greedy-Flip (Orientierung) + 2-Swap (k↔k Zuordnung)
      # ==============================================================================
      l1_cur <- l1_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
      cat(sprintf("Greedy-Init: L1=%.6f\n", l1_cur))
      
      max_iter <- 10 * C2
      iter <- 0L
      repeat {
        iter <- iter + 1
        best_gain <- 0; best_k <- NA_integer_
        for (k in 1:C2) {
          pos_tmp <- pos_class_k; neg_tmp <- neg_class_k
          tmp <- pos_tmp[k]; pos_tmp[k] <- neg_tmp[k]; neg_tmp[k] <- tmp
          l1_new <- l1_state_tie(dec_raw, pair_id_for_k, pos_tmp, neg_tmp, rn, pv, tie_rule)
          gain <- l1_cur - l1_new
          if (gain > best_gain + 1e-12) { best_gain <- gain; best_k <- k }
        }
        if (is.na(best_k) || best_gain <= 0 || iter > max_iter) break
        tmp <- pos_class_k[best_k]; pos_class_k[best_k] <- neg_class_k[best_k]; neg_class_k[best_k] <- tmp
        l1_cur <- l1_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
        cat(sprintf("  Flip k=%d  → L1=%.6f\n", best_k, l1_cur))
        if (l1_cur < 1e-12) break
      }
      cat(sprintf("Greedy-Ende: L1=%.6f (Iterationen=%d)\n", l1_cur, iter))
      
      best_two_swap_once <- function(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule){
        C2 <- length(pair_id_for_k)
        l1_cur <- l1_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
        best <- list(gain=0, pid=pair_id_for_k, pos=pos_class_k, neg=neg_class_k, l1=l1_cur, k1=NA, k2=NA)
        for (k1 in 1:(C2-1)) for (k2 in (k1+1):C2) {
          pid <- pair_id_for_k
          p1 <- pid[k1]; p2 <- pid[k2]
          if (p1 == p2) next
          pid[k1] <- p2; pid[k2] <- p1
          for (o1 in 0:1) for (o2 in 0:1) {
            pos_tmp <- pos_class_k; neg_tmp <- neg_class_k
            a <- all_pairs[[pid[k1]]][1]; b <- all_pairs[[pid[k1]]][2]
            if (o1==0) { pos_tmp[k1] <- a; neg_tmp[k1] <- b } else { pos_tmp[k1] <- b; neg_tmp[k1] <- a }
            a <- all_pairs[[pid[k2]]][1]; b <- all_pairs[[pid[k2]]][2]
            if (o2==0) { pos_tmp[k2] <- a; neg_tmp[k2] <- b } else { pos_tmp[k2] <- b; neg_tmp[k2] <- a }
            l1_new <- l1_state_tie(dec_raw, pid, pos_tmp, neg_tmp, rn, pv, tie_rule)
            gain   <- l1_cur - l1_new
            if (gain > best$gain + 1e-12) best <- list(gain=gain, pid=pid, pos=pos_tmp, neg=neg_tmp, l1=l1_new, k1=k1, k2=k2)
          }
        }
        best
      }
      
      iter_sw <- 0L
      repeat {
        iter_sw <- iter_sw + 1L
        best <- best_two_swap_once(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
        if (best$gain <= 0) break
        pair_id_for_k <- best$pid
        pos_class_k   <- best$pos
        neg_class_k   <- best$neg
        l1_cur        <- best$l1
        cat(sprintf("Swap k=%d<->k=%d  → L1=%.12f\n", best$k1, best$k2, l1_cur))
        if (l1_cur < 1e-12 || iter_sw > 200) break
      }
      cat(sprintf("2-Swap Ende: L1=%.12f (Iterationen=%d)\n", l1_cur, iter_sw))
      
      # ==============================================================================
      # 7) Finaler Check (Votes)
      # ==============================================================================
      our_votes <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule)
      l1_final  <- mean(colSums(abs(our_votes - pv)))
      acc_final <- mean(rn[apply(our_votes, 2, which.max)] == rn[apply(pv, 2, which.max)])
      cat(sprintf("Votes-Check (Tie %s): L1=%.12f | Top1-Acc=%.3f\n", tie_rule, l1_final, acc_final))
      stopifnot(l1_final < 1e-12, acc_final >= 0.999)
      cat("✔ Votes exakt reproduziert (inkl. Optimierung & Tie-Regel).\n")
      
      # ==============================================================================
      # 8) Binärmodelle robust rekonstruieren via Least Squares + j↔k-Mapping
      #     -> nutzt NUR alphaindex-SV je j (caret-Skala!) und findet j→k per LS-Korrelation
      # ==============================================================================
      ai_list <- kernlab::alphaindex(model_ksvm)     # list(C2); Indizes in Xscaled!
      stopifnot(length(ai_list) == C2)
      
      # Precompute K-Matrizen je j
      Kmats <- vector("list", C2)
      SVs   <- vector("list", C2)
      for (j in 1:C2) {
        ai <- suppressWarnings(as.integer(ai_list[[j]]))
        stopifnot(length(ai) >= 1L,
                  all(is.finite(ai)),
                  all(ai >= 1L & ai <= nrow(Xscaled)))
        SVs[[j]]   <- Xscaled[ai, , drop = FALSE]
        Kmats[[j]] <- kernlab::kernelMatrix(model_ksvm@kernelf, Xscaled, SVs[[j]])  # [N x m_j]
      }
      
      # Fit pro (j,k) und sammle |cor|
      fit_one <- function(Kmat, y, tie_rule){
        Xls <- cbind(Kmat, 1.0)
        coefb <- tryCatch(qr.solve(Xls, y),
                          error = function(e) {
                            XtX <- crossprod(Xls)
                            lam <- 1e-10
                            solve(XtX + diag(lam, nrow(XtX)), crossprod(Xls, y))
                          })
        m <- ncol(Kmat)
        a <- as.numeric(coefb[seq_len(m)])
        b <- as.numeric(coefb[m + 1L])
        z <- as.numeric(Kmat %*% a + b)
        
        cr <- suppressWarnings(cor(z, y, use="complete.obs")); if (!is.finite(cr)) cr <- 0
        
        # ggf. flippen
        if (cr < 0) { a <- -a; b <- -b; z <- -z; cr <- -cr }
        
        # Intercept so justieren, dass Klassengrenze (0) mit y übereinstimmt
        y_pos <- if (tie_rule == ">=0") (y >= 0) else (y > 0)
        t_i   <- -z
        if (all(y_pos)) {
          delta <- max(t_i) + 1.0
        } else if (all(!y_pos)) {
          delta <- min(t_i) - 1.0
        } else {
          Tpos <- max(t_i[y_pos])
          Tneg <- min(t_i[!y_pos])
          if (Tpos <= Tneg) {
            delta <- (Tpos + Tneg)/2
          } else {
            # nicht trennbar per reinem Shift: nähere dich an
            delta <- mean(c(Tpos, Tneg))
          }
        }
        b2 <- b + delta
        z2 <- z + delta
        cr2 <- suppressWarnings(cor(z2, y, use="complete.obs")); if (!is.finite(cr2)) cr2 <- cr
        
        list(alpha=a, b=b2, z=z2, corr=abs(cr2))
      }
      
      corr_mat <- matrix(0, nrow=C2, ncol=C2)
      fits     <- vector("list", C2)     # fits[[j]][[k]] = Ergebnisliste
      for (j in 1:C2) {
        fits[[j]] <- vector("list", C2)
        for (k in 1:C2) {
          yk <- as.numeric(dec_raw[,k])
          fk <- fit_one(Kmats[[j]], yk, tie_rule)
          fits[[j]][[k]] <- fk
          corr_mat[j,k]  <- fk$corr
        }
      }
      
      assign_j_to_k <- as.integer(clue::solve_LSAP(1 - corr_mat))
      stopifnot(length(unique(assign_j_to_k)) == C2)
      
      # Binärmodelle in k-Ordnung aufbauen
      bin_models <- vector("list", C2)
      corr_used  <- numeric(C2)
      for (k in 1:C2) {
        j <- which(assign_j_to_k == k)
        fk <- fits[[j]][[k]]
        ci <- pos_class_k[k]; cj <- neg_class_k[k]
        bin_models[[k]] <- list(
          name = paste0(ci, "_vs_", cj),
          ci   = ci,
          cj   = cj,
          SV   = SVs[[j]],          # caret-Skala
          coef = fk$alpha,
          b    = fk$b
        )
        corr_used[k] <- fk$corr
      }
      cat(sprintf("LS corr(z_recon, dec_raw) pro k: min/median/max = %.6f / %.6f / %.6f\n",
                  min(corr_used), median(corr_used), max(corr_used)))
      
      # ------------------------------------------------------------------------------
      # Kontrolle: Stimmen aus bin_models vs. pv  (mit derselben Tie-Regel!)
      # ------------------------------------------------------------------------------
      z_from_models_scaled <- function(x_scaled_row, bm_list, kernelf) {
        vapply(bm_list, function(bm){
          ker <- kernlab::kernelMatrix(kernelf, matrix(x_scaled_row, nrow = 1L), bm$SV)
          as.numeric(ker %*% bm$coef) + bm$b
        }, numeric(1))
      }
      votes_from_models_scaled <- function(x_scaled_row, bm_list, classes, kernelf, tie_rule = c(">0", ">=0")){
        tie_rule <- match.arg(tie_rule)
        z <- z_from_models_scaled(x_scaled_row, bm_list, kernelf)
        acc <- setNames(integer(length(classes)), classes)
        if (tie_rule == ">=0") {
          for (j in seq_along(bm_list)) {
            if (z[j] >= 0) acc[bm_list[[j]]$ci] <- acc[bm_list[[j]]$ci] + 1
            else           acc[bm_list[[j]]$cj] <- acc[bm_list[[j]]$cj] + 1
          }
        } else {
          for (j in seq_along(bm_list)) {
            if (z[j] >  0) acc[bm_list[[j]]$ci] <- acc[bm_list[[j]]$ci] + 1
            else           acc[bm_list[[j]]$cj] <- acc[bm_list[[j]]$cj] + 1
          }
        }
        acc
      }
      l1_models_vs_pv <- 0
      for (i in seq_len(nrow(Xscaled))) {
        v_ref <- votes_from_models_scaled(Xscaled[i, , drop=FALSE],
                                          bin_models, rn, model_ksvm@kernelf,
                                          tie_rule = tie_rule)
        l1_models_vs_pv <- l1_models_vs_pv + sum(abs(as.numeric(v_ref) - as.numeric(pv[, i])))
      }
      l1_models_vs_pv <- l1_models_vs_pv / ncol(pv)
      cat(sprintf("Check (bin_models -> pv, tie %s): L1 = %.12e\n", tie_rule, l1_models_vs_pv))
      stopifnot(l1_models_vs_pv < 1e-9)
      cat("✔ Binärmodelle reproduzieren pv (Least-Squares, mit Intercept-Shift auf Tie-Regel).\n")
      
      # Für ONNX-Export: Klassenreihenfolge exakt wie in pv
      out_classes <- rn
      
      onnx_path   <- paste0(out_base, "_svm_ovo_linear_", use_rule, ".onnx")
      labels_json <- sub("\\.onnx$", "_labels.json", onnx_path)
      
      # externer Builder vorausgesetzt:
      build_onnx_poly_ovo(
        use_rule       = use_rule,
        bin_models     = bin_models,
        feature_names  = feature_names,
        classes        = out_classes,
        means          = means,
        sds            = sds,
        kpar           = list(degree = deg, scale = scaleK, offset = offK),
        out_path       = onnx_path,
        dtype          = "float32",
        reorder_idx    = NULL,
        clip_base      = NA_real_,
        add_debug      = TRUE,
        primary_output = "idx1",
        tie_rule       = tie_rule
      )
      
      lbls <- setNames(as.list(out_classes), as.character(seq_along(out_classes)))
      jsonlite::write_json(lbls, labels_json, pretty = TRUE, auto_unbox = TRUE)
      list(onnx = onnx_path, labels_json = labels_json)
    }
    
    # --------- RBF ----------
    export_ksvm_rbf_onnx <- function(train_obj, out_base,
                                     use_rule = "majority",
                                     primary_output = "idx1",
                                     dtype = "float32",
                                     do_checks = TRUE) {
      model_r <- train_obj
      model_ksvm <- model_r$finalModel
      
      classes       <- as.character(model_r$levels)
      feature_names <- setdiff(colnames(model_r$trainingData), ".outcome")
      
      # Preprocess (wie im Training, caret-Skala)
      pp    <- model_r$preProcess
      means <- as.numeric(pp$mean[feature_names]); names(means) <- feature_names
      sds   <- as.numeric(pp$std[feature_names]);  names(sds)   <- feature_names
      
      # Trainingsrohwerte -> caret-Skala
      Xraw_train <- as.data.frame(model_r$trainingData[, feature_names, drop = FALSE])
      Xscaled    <- as.matrix(predict(pp, Xraw_train))
      storage.mode(Xscaled) <- "double"
      colnames(Xscaled) <- feature_names
      
      K  <- length(classes)
      C2 <- K * (K - 1) / 2
      stopifnot(C2 >= 1)
      
      # Kernel-Parameter (rbfdot: sigma)
      kp    <- kernlab::kpar(model_ksvm@kernelf)
      sigma <- as.numeric(kp$sigma)  # entspricht "gamma" in vielen Libs
      
      # ==============================================================================
      # 2) Decisions (N x C2) auf caret-Skala
      # ==============================================================================
      library(kernlab)
      get_decision_matrix <- function(model, Xscaled) {
        dec <- try(predict(model, Xscaled, type = "decision"), silent = TRUE)
        if (!inherits(dec, "try-error")) return(as.matrix(dec))
        as.matrix(kernlab::decision(model, Xscaled))
      }
      dec_raw <- get_decision_matrix(model_ksvm, Xscaled)
      stopifnot(ncol(dec_raw) == C2)
      
      # ==============================================================================
      # 3) Votes-Referenz + Sanity-Checks
      # ==============================================================================
      pv <- try(predict(model_ksvm, Xscaled, type = "votes"), silent = TRUE)
      if (inherits(pv, "try-error")) stop("Dieses ksvm liefert keine 'votes' – abbrechen.")
      rn <- rownames(pv); if (is.null(rn) || any(!nzchar(rn))) rn <- classes
      stopifnot(length(rn) == K)
      cat("C2 = ", C2, " | range(colSums(pv)) = ", paste(range(colSums(pv)), collapse=".."), "\n", sep="")
      
      # ==============================================================================
      # 4) k → (a,b) Initial-Mapping über Korrelations-Vorzeichen
      # ==============================================================================
      s_sign_strict <- function(z) ifelse(z > 0, 1L, -1L)
      all_pairs <- combn(rn, 2, simplify = FALSE)
      safe_cor <- function(x, y) { cr <- suppressWarnings(cor(x, y, use = "complete.obs")); if (!is.finite(cr)) 0 else cr }
      
      S <- matrix(0, nrow = C2, ncol = C2)
      for (p in seq_along(all_pairs)) {
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        d <- as.numeric(pv[match(a, rn), ] - pv[match(b, rn), ])
        for (k in 1:C2) S[k, p] <- abs(safe_cor(s_sign_strict(dec_raw[, k]), d))
      }
      pair_id_for_k <- as.integer(clue::solve_LSAP(1 - S))
      stopifnot(length(unique(pair_id_for_k)) == C2)
      
      pos_class_k <- neg_class_k <- character(C2)
      for (k in 1:C2) {
        p <- pair_id_for_k[k]
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        d <- as.numeric(pv[match(a, rn), ] - pv[match(b, rn), ])
        s <- s_sign_strict(dec_raw[, k])
        cr <- safe_cor(s, d)
        if (cr >= 0) { pos_class_k[k] <- a; neg_class_k[k] <- b } else { pos_class_k[k] <- b; neg_class_k[k] <- a }
      }
      
      # ==============================================================================
      # 5) Tie-Regel + Rekonstruktion (>0 vs >=0)
      # ==============================================================================
      votes_from_dec_rule_tie <- function(zrow, pair_id_for_k, pos_k, neg_k, cls, tie_rule = c(">0",">=0")){
        tie_rule <- match.arg(tie_rule)
        acc <- setNames(integer(length(cls)), cls)
        sel <- if (tie_rule == ">0") zrow > 0 else zrow >= 0
        for (k in seq_along(pair_id_for_k)) {
          if (sel[k]) acc[pos_k[k]] <- acc[pos_k[k]] + 1 else acc[neg_k[k]] <- acc[neg_k[k]] + 1
        }
        acc
      }
      build_votes_from_state_tie <- function(dec_raw, pair_id_for_k, pos_k, neg_k, rn, tie_rule = c(">0",">=0")){
        tie_rule <- match.arg(tie_rule)
        N <- nrow(dec_raw)
        out <- matrix(0L, nrow = length(rn), ncol = N, dimnames = list(rn, NULL))
        for (i in 1:N) out[, i] <- votes_from_dec_rule_tie(dec_raw[i, ], pair_id_for_k, pos_k, neg_k, rn, tie_rule)
        out
      }
      l1_state_tie <- function(dec_raw, pair_id_for_k, pos_k, neg_k, rn, pv, tie_rule){
        v <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_k, neg_k, rn, tie_rule)
        mean(colSums(abs(v - pv)))
      }
      our_votes_gt <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule=">0")
      our_votes_ge <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule=">=0")
      l1_gt <- mean(colSums(abs(our_votes_gt - pv)))
      l1_ge <- mean(colSums(abs(our_votes_ge - pv)))
      acc_gt <- mean(rn[apply(our_votes_gt, 2, which.max)] == rn[apply(pv, 2, which.max)])
      acc_ge <- mean(rn[apply(our_votes_ge, 2, which.max)] == rn[apply(pv, 2, which.max)])
      cat(sprintf("Tie '>0':   L1=%.12f | Top1-Acc=%.3f\n", l1_gt, acc_gt))
      cat(sprintf("Tie '>=0':  L1=%.12f | Top1-Acc=%.3f\n", l1_ge, acc_ge))
      tie_rule <- if (l1_ge <= l1_gt) ">=0" else ">0"
      cat("→ Verwende Tie-Regel: ", tie_rule, "\n", sep="")
      
      # ==============================================================================
      # 6) Greedy-Flip + 2-Swap (Optimierung)
      # ==============================================================================
      l1_cur <- l1_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
      cat(sprintf("Greedy-Init: L1=%.6f\n", l1_cur))
      max_iter <- 10 * C2; iter <- 0L
      repeat {
        iter <- iter + 1
        best_gain <- 0; best_k <- NA_integer_
        for (k in 1:C2) {
          pos_tmp <- pos_class_k; neg_tmp <- neg_class_k
          tmp <- pos_tmp[k]; pos_tmp[k] <- neg_tmp[k]; neg_tmp[k] <- tmp
          l1_new <- l1_state_tie(dec_raw, pair_id_for_k, pos_tmp, neg_tmp, rn, pv, tie_rule)
          gain <- l1_cur - l1_new
          if (gain > best_gain + 1e-12) { best_gain <- gain; best_k <- k }
        }
        if (is.na(best_k) || best_gain <= 0 || iter > max_iter) break
        tmp <- pos_class_k[best_k]; pos_class_k[best_k] <- neg_class_k[best_k]; neg_class_k[best_k] <- tmp
        l1_cur <- l1_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
        cat(sprintf("  Flip k=%d  → L1=%.6f\n", best_k, l1_cur))
        if (l1_cur < 1e-12) break
      }
      cat(sprintf("Greedy-Ende: L1=%.6f\n", l1_cur))
      
      best_two_swap_once <- function(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule){
        C2 <- length(pair_id_for_k)
        l1_cur <- l1_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
        best <- list(gain=0, pid=pair_id_for_k, pos=pos_class_k, neg=neg_class_k, l1=l1_cur, k1=NA, k2=NA)
        for (k1 in 1:(C2-1)) for (k2 in (k1+1):C2) {
          pid <- pair_id_for_k
          p1 <- pid[k1]; p2 <- pid[k2]
          if (p1 == p2) next
          pid[k1] <- p2; pid[k2] <- p1
          for (o1 in 0:1) for (o2 in 0:1) {
            pos_tmp <- pos_class_k; neg_tmp <- neg_class_k
            a <- all_pairs[[pid[k1]]][1]; b <- all_pairs[[pid[k1]]][2]
            if (o1==0) { pos_tmp[k1] <- a; neg_tmp[k1] <- b } else { pos_tmp[k1] <- b; neg_tmp[k1] <- a }
            a <- all_pairs[[pid[k2]]][1]; b <- all_pairs[[pid[k2]]][2]
            if (o2==0) { pos_tmp[k2] <- a; neg_tmp[k2] <- b } else { pos_tmp[k2] <- b; neg_tmp[k2] <- a }
            l1_new <- l1_state_tie(dec_raw, pid, pos_tmp, neg_tmp, rn, pv, tie_rule)
            gain   <- l1_cur - l1_new
            if (gain > best$gain + 1e-12) best <- list(gain=gain, pid=pid, pos=pos_tmp, neg=neg_tmp, l1=l1_new, k1=k1, k2=k2)
          }
        }
        best
      }
      iter_sw <- 0L
      repeat {
        iter_sw <- iter_sw + 1L
        best <- best_two_swap_once(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, pv, tie_rule)
        if (best$gain <= 0) break
        pair_id_for_k <- best$pid
        pos_class_k   <- best$pos
        neg_class_k   <- best$neg
        l1_cur        <- best$l1
        cat(sprintf("Swap k=%d<->k=%d  → L1=%.12f\n", best$k1, best$k2, l1_cur))
        if (l1_cur < 1e-12 || iter_sw > 200) break
      }
      cat(sprintf("2-Swap Ende: L1=%.12f (Iterationen=%d)\n", l1_cur, iter_sw))
      
      # ==============================================================================
      # 7) Finaler Check (Votes)
      # ==============================================================================
      our_votes <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule)
      l1_final  <- mean(colSums(abs(our_votes - pv)))
      acc_final <- mean(rn[apply(our_votes, 2, which.max)] == rn[apply(pv, 2, which.max)])
      cat(sprintf("Votes-Check (Tie %s): L1=%.12f | Top1-Acc=%.3f\n", tie_rule, l1_final, acc_final))
      stopifnot(l1_final < 1e-12, acc_final >= 0.999)
      cat("✔ Votes exakt reproduziert (inkl. Optimierung & Tie-Regel).\n")
      
      # ==============================================================================
      # 8) Binärmodelle via Least Squares (kernel-agnostisch) + j↔k Zuordnung
      # ==============================================================================
      ai_list <- kernlab::alphaindex(model_ksvm)     # list(C2); Indizes in Xscaled!
      stopifnot(length(ai_list) == C2)
      
      Kmats <- vector("list", C2)
      SVs   <- vector("list", C2)
      for (j in 1:C2) {
        ai <- suppressWarnings(as.integer(ai_list[[j]]))
        stopifnot(length(ai) >= 1L,
                  all(is.finite(ai)),
                  all(ai >= 1L & ai <= nrow(Xscaled)))
        SVs[[j]]   <- Xscaled[ai, , drop = FALSE]
        Kmats[[j]] <- kernlab::kernelMatrix(model_ksvm@kernelf, Xscaled, SVs[[j]])  # [N x m_j]
      }
      
      fit_one <- function(Kmat, y, tie_rule){
        Xls <- cbind(Kmat, 1.0)
        coefb <- tryCatch(qr.solve(Xls, y),
                          error = function(e) {
                            XtX <- crossprod(Xls); lam <- 1e-10
                            solve(XtX + diag(lam, nrow(XtX)), crossprod(Xls, y))
                          })
        m <- ncol(Kmat)
        a <- as.numeric(coefb[seq_len(m)])
        b <- as.numeric(coefb[m + 1L])
        z <- as.numeric(Kmat %*% a + b)
        
        cr <- suppressWarnings(cor(z, y, use="complete.obs")); if (!is.finite(cr)) cr <- 0
        if (cr < 0) { a <- -a; b <- -b; z <- -z; cr <- -cr }
        
        y_pos <- if (tie_rule == ">=0") (y >= 0) else (y > 0)
        t_i   <- -z
        if (all(y_pos)) {
          delta <- max(t_i) + 1.0
        } else if (all(!y_pos)) {
          delta <- min(t_i) - 1.0
        } else {
          Tpos <- max(t_i[y_pos]); Tneg <- min(t_i[!y_pos])
          delta <- if (Tpos <= Tneg) (Tpos + Tneg)/2 else mean(c(Tpos,Tneg))
        }
        b2 <- b + delta
        z2 <- z + delta
        cr2 <- suppressWarnings(cor(z2, y, use="complete.obs")); if (!is.finite(cr2)) cr2 <- cr
        
        list(alpha=a, b=b2, z=z2, corr=abs(cr2))
      }
      
      corr_mat <- matrix(0, nrow=C2, ncol=C2)
      fits     <- vector("list", C2)
      for (j in 1:C2) {
        fits[[j]] <- vector("list", C2)
        for (k in 1:C2) {
          yk <- as.numeric(dec_raw[,k])
          fk <- fit_one(Kmats[[j]], yk, tie_rule)
          fits[[j]][[k]] <- fk
          corr_mat[j,k]  <- fk$corr
        }
      }
      assign_j_to_k <- as.integer(clue::solve_LSAP(1 - corr_mat))
      stopifnot(length(unique(assign_j_to_k)) == C2)
      
      bin_models <- vector("list", C2)
      corr_used  <- numeric(C2)
      for (k in 1:C2) {
        j <- which(assign_j_to_k == k)
        fk <- fits[[j]][[k]]
        ci <- pos_class_k[k]; cj <- neg_class_k[k]
        bin_models[[k]] <- list(
          name = paste0(ci, "_vs_", cj),
          ci   = ci,
          cj   = cj,
          SV   = SVs[[j]],     # caret-Skala
          coef = fk$alpha,
          b    = fk$b
        )
        corr_used[k] <- fk$corr
      }
      cat(sprintf("LS corr(z_recon, dec_raw) pro k: min/median/max = %.6f / %.6f / %.6f\n",
                  min(corr_used), median(corr_used), max(corr_used)))
      
      # Für ONNX-Export: Klassenreihenfolge exakt wie in pv
      out_classes <- rn
      
      onnx_path   <- paste0(out_base, "_svm_ovo_rbf_", use_rule, ".onnx")
      labels_json <- sub("\\.onnx$", "_labels.json", onnx_path)
      
      build_onnx_rbf_ovo(
        use_rule       = use_rule,
        bin_models, feature_names, classes, means, sds, sigma, onnx_path,
        dtype          = dtype,
        reorder_idx    = NULL,
        add_debug      = TRUE,
        primary_output = primary_output,
        tie_rule       = tie_rule
      )
      
      lbls <- setNames(as.list(out_classes), as.character(seq_along(out_classes)))
      jsonlite::write_json(lbls, labels_json, pretty = TRUE, auto_unbox = TRUE)
      list(onnx = onnx_path, labels_json = labels_json)
    }
    
    export_caret_ksvm_to_onnx <- function(train_obj, out_base,
                                          use_rule = "majority",
                                          primary_output = "idx1",
                                          dtype = "float32",
                                          do_checks = TRUE) {
      k <- detect_svm_kernel(train_obj)
      message("SVM kernel detected: ", k)
      switch(k,
             linear = { message("→ export_ksvm_linear_onnx()"); 
               export_ksvm_linear_onnx(train_obj, out_base, use_rule, primary_output, dtype, do_checks) },
             poly   = { message("→ export_ksvm_poly_onnx()");   
               export_ksvm_poly_onnx(  train_obj, out_base, use_rule, primary_output, dtype, do_checks) },
             rbf    = { message("→ export_ksvm_rbf_onnx()");    
               export_ksvm_rbf_onnx(   train_obj, out_base, use_rule, primary_output, dtype, do_checks) },
             stop("Unsupported kernel: ", k)
      )
    }
    
    sanitize <- function(s) {
      s2 <- gsub("[^A-Za-z0-9_]+", "_", s)
      if (grepl("^[0-9]", s2)) paste0("C_", s2) else s2
    }
    
    
    
    
    build_onnx_for_rule <- function(use_rule,
                                    weights_aligned, feature_names, classes,
                                    means, sds, out_path) {
      
      
      library(reticulate)
      py <- find_python_bin()
      
  
      
      onnx        <- reticulate::import("onnx",        convert = FALSE)
      helper      <- reticulate::import("onnx.helper", convert = FALSE)
      np          <- reticulate::import("numpy",       convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp          <- reticulate::tuple
      
      n_feat <- length(feature_names); K <- length(classes); C2 <- length(weights_aligned)
      inp <- helper$make_tensor_value_info("input_raw", TensorProto$FLOAT, list(1L, n_feat))
      out_idx   <- helper$make_tensor_value_info("idx1",         TensorProto$INT64, list(1L, 1L))
      out_score <- helper$make_tensor_value_info(
        if (use_rule=="margin") "scores_vector" else "votes_vector",
        TensorProto$FLOAT, list(1L, K)
      )
      
      init_common <- list(
        onnx$numpy_helper$from_array(np$array(means, dtype="float32")$reshape(tp(1L,n_feat)), "mean_vec"),
        onnx$numpy_helper$from_array(np$array(sds,   dtype="float32")$reshape(tp(1L,n_feat)), "std_vec"),
        onnx$numpy_helper$from_array(np$array(0.0,   dtype="float32"),                        "zero_f"),
        onnx$numpy_helper$from_array(np$array(1L,    dtype="int64"),                          "one_i")
      )
      
      nodes <- list(
        helper$make_node("Sub", list("input_raw","mean_vec"), list("centered"), name="Center"),
        helper$make_node("Div", list("centered","std_vec"),   list("scaled"),   name="Scale")
      )
      # One-Hots je Klasse
      oh_inits <- lapply(seq_along(classes), function(cix) {
        oh <- rep(0, K); oh[cix] <- 1
        onnx$numpy_helper$from_array(
          np$array(oh, dtype="float32")$reshape(tp(1L,K)),
          paste0("oh_", sanitize(classes[cix]))
        )
      })
      
      init_pair <- list(); acc_vecs <- character(0)
      for (j in seq_along(weights_aligned)) {
        w   <- weights_aligned[[j]]
        ci  <- sanitize(w$class_pair[1]); cj <- sanitize(w$class_pair[2])
        tag <- sanitize(w$name)
        sfx <- sprintf("_j%03d", j)
        
        init_pair <- c(init_pair,
                       onnx$numpy_helper$from_array(np$array(as.numeric(w$W), dtype="float32")$reshape(tp(n_feat,1L)), paste0("W_", tag, sfx)),
                       onnx$numpy_helper$from_array(np$array(as.numeric(w$b), dtype="float32")$reshape(tp(1L,1L)),     paste0("b_", tag, sfx))
        )
        
        raw <- paste0("raw_", tag, sfx)
        z   <- paste0("z_",   tag, sfx)
        
        if (use_rule == "majority") {
          mask <- paste0("mask_", tag, sfx)
          vote <- paste0("vote_", tag, sfx)
          nodes <- c(nodes,
                     helper$make_node("MatMul",  list("scaled", paste0("W_", tag, sfx)), list(raw), name=paste0("MatMul_", tag, sfx)),
                     helper$make_node("Add",     list(raw,      paste0("b_", tag, sfx)), list(z),   name=paste0("Bias_",   tag, sfx)),
                     helper$make_node("GreaterOrEqual", list(z, "zero_f"), list(mask),   name=paste0("Ge_",     tag, sfx)),
                     helper$make_node("Where",   list(mask, paste0("oh_", ci), paste0("oh_", cj)), list(vote), name=paste0("Vote_", tag, sfx))
          )
          acc_vecs <- c(acc_vecs, vote)
        } else {
          # Optional: Margin-Rule (hier nicht genutzt, aber vollständig)
          pos      <- paste0("pos_",        tag, sfx)
          neg_raw  <- paste0("neg_raw_",    tag, sfx)
          neg_relu <- paste0("neg_",        tag, sfx)
          posv     <- paste0("posv_",       tag, sfx)
          negv     <- paste0("negv_",       tag, sfx)
          score    <- paste0("score_",      tag, sfx)
          nodes <- c(nodes,
                     helper$make_node("MatMul", list("scaled", paste0("W_", tag, sfx)), list(raw),   name=paste0("MatMul_",   tag, sfx)),
                     helper$make_node("Add",    list(raw,      paste0("b_", tag, sfx)), list(z),     name=paste0("Bias_",     tag, sfx)),
                     helper$make_node("Relu",   list(z),                      list(pos),             name=paste0("ReluPos_",   tag, sfx)),
                     helper$make_node("Neg",    list(z),                      list(neg_raw),         name=paste0("Neg_",       tag, sfx)),
                     helper$make_node("Relu",   list(neg_raw),                list(neg_relu),        name=paste0("ReluNeg_",   tag, sfx)),
                     helper$make_node("Mul",    list(pos,     paste0("oh_", ci)), list(posv),        name=paste0("MulPos_",    tag, sfx)),
                     helper$make_node("Mul",    list(neg_relu,paste0("oh_", cj)), list(negv),        name=paste0("MulNeg_",    tag, sfx)),
                     helper$make_node("Add",    list(posv,    negv),             list(score),        name=paste0("AddScore_",  tag, sfx))
          )
          acc_vecs <- c(acc_vecs, score)
        }
      }
      
      # Summieren → [1,K]
      sum_name <- acc_vecs[1]
      if (length(acc_vecs) > 1) {
        for (i in 2:length(acc_vecs)) {
          new_sum <- paste0("sum_", sprintf("%03d", i))
          nodes <- c(nodes,
                     helper$make_node("Add", list(sum_name, acc_vecs[i]), list(new_sum),
                                      name=paste0("Sum_", sprintf("%03d", i))))
          sum_name <- new_sum
        }
      }
      
      nodes <- c(nodes,
                 helper$make_node("ArgMax", list(sum_name), list("idx_raw"), axis=1L, keepdims=1L, name="ArgMax"),
                 helper$make_node("Add",    list("idx_raw","one_i"), list("idx1"), name="Make1Idx"),
                 helper$make_node("Identity", list(sum_name),
                                  list(if (use_rule=="margin") "scores_vector" else "votes_vector"),
                                  name="OutVec")
      )
      
      graph <- helper$make_graph(
        nodes       = nodes,
        name        = paste0("svm_ovo_", use_rule, "_assigned"),
        inputs      = list(inp),
        outputs     = list(out_idx, out_score),
        initializer = c(init_common, oh_inits, init_pair)
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = paste0("ovo_", use_rule, "_export"),
        opset_imports = list(helper$make_operatorsetid("ai.onnx", 13L))
      )
      onnx$save(model_onnx, out_path)
      cat("ONNX (", use_rule, ") gespeichert: ", out_path, "\n", sep = "")
      
    }
    
    
    
    build_onnx_poly_ovo <- function(
    use_rule      = c("majority","margin"),
    bin_models, feature_names, classes, means, sds, kpar, out_path,
    dtype         = c("float64","float32"),
    reorder_idx   = NULL,            # 0-basig; NULL = keine Umordnung
    clip_base     = NA_real_,        # Clip *vor* Pow; NA = kein Clip
    add_debug     = TRUE,
    primary_output= c("idx1","votes_vector","scores_vector"),
    tie_rule      = c(">0",">=0")
    ){
      use_rule <- match.arg(use_rule)
      dtype    <- match.arg(dtype)
      primary_output <- match.arg(primary_output)
      tie_rule <- match.arg(tie_rule)
      
      onnx        <- reticulate::import("onnx",        convert = FALSE)
      helper      <- reticulate::import("onnx.helper", convert = FALSE)
      np          <- reticulate::import("numpy",       convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp          <- reticulate::tuple
      
      is_f64 <- (dtype == "float64")
      TNUM   <- if (is_f64) TensorProto$DOUBLE else TensorProto$FLOAT
      as_fp_vec <- function(x) np$array(as.numeric(x), dtype = if (is_f64) "float64" else "float32")
      as_fp_mat <- function(M){ if (is.vector(M)) M <- matrix(M, nrow=1L); np$array(M, dtype = if (is_f64) "float64" else "float32") }
      as_i64_vec <- function(x){ xv <- as.integer(x); np$array(xv, dtype = "int64")$reshape(tp(length(xv))) }
      `%||%` <- function(x,y) if (is.null(x) || !is.finite(x)) y else x
      
      K      <- length(classes)
      n_feat <- length(feature_names)
      deg_i  <- as.integer(round(kpar$degree %||% 3))
      
      # I/O
      inp    <- helper$make_tensor_value_info("input_raw", TensorProto$FLOAT, list(1L, n_feat))
      out_i  <- helper$make_tensor_value_info("idx1", TensorProto$INT64, list(1L,1L))
      out_vv <- helper$make_tensor_value_info("votes_vector", TNUM, list(1L,K))
      outs   <- list(out_i, out_vv)
      if (use_rule == "margin") {
        out_sv <- helper$make_tensor_value_info("scores_vector", TNUM, list(1L,K))
        outs   <- c(outs, list(out_sv))
      }
      if (isTRUE(add_debug)) {
        out_xs  <- helper$make_tensor_value_info("X_scaled_dbg",   TNUM, list(1L,n_feat))
        out_cen <- helper$make_tensor_value_info("centered_dbg",   TNUM, list(1L,n_feat))
        out_ss  <- helper$make_tensor_value_info("std_safe_dbg",   TNUM, list(1L,n_feat))
        out_z   <- helper$make_tensor_value_info("z_all",          TNUM, list(1L, length(bin_models)))
        out_m   <- helper$make_tensor_value_info("win_mask",       TensorProto$BOOL, list(1L, length(bin_models)))
        out_vbp <- helper$make_tensor_value_info("votes_by_pair",  TNUM, list(length(bin_models), K))
        out_tv  <- helper$make_tensor_value_info("total_votes",    TNUM, list(1L,1L))
        out_i0  <- helper$make_tensor_value_info("idx0",           TensorProto$INT64, list(1L,1L))
        outs    <- c(outs, list(out_xs, out_cen, out_ss, out_z, out_m, out_vbp, out_tv, out_i0))
      }
      
      # Primary output zuerst
      reorder_outs <- function(outs, names_vec, primary){
        idx <- match(primary, names_vec)
        if (is.na(idx) || idx == 1L) return(outs)
        c(outs[idx], outs[-idx])
      }
      out_name_vec <- c("idx1","votes_vector", if (use_rule=="margin") "scores_vector",
                        if (isTRUE(add_debug)) c("X_scaled_dbg","centered_dbg","std_safe_dbg","z_all","win_mask","votes_by_pair","total_votes","idx0"))
      outs <- reorder_outs(outs, out_name_vec, primary_output)
      
      # Initializer
      init_common <- list(
        onnx$numpy_helper$from_array(as_fp_mat(means),             "mean_vec"),
        onnx$numpy_helper$from_array(as_fp_mat(sds),               "std_vec"),
        onnx$numpy_helper$from_array(as_fp_vec(0.0),               "zero_f"),
        onnx$numpy_helper$from_array(as_fp_vec(kpar$scale %||% 1), "k_scale"),
        onnx$numpy_helper$from_array(as_fp_vec(kpar$offset%||% 1), "k_offset"),
        onnx$numpy_helper$from_array(as_fp_vec(deg_i),             "k_degree"),
        onnx$numpy_helper$from_array(as_fp_vec(1e-6),              "eps_f"),
        onnx$numpy_helper$from_array(as_i64_vec(c(1L, K)),         "shape_1K"),
        onnx$numpy_helper$from_array(as_i64_vec(1L),               "k1"),
        onnx$numpy_helper$from_array(as_i64_vec(1L),               "one_i")
      )
      if (!is.null(reorder_idx)) {
        stopifnot(length(reorder_idx) == n_feat)
        init_common <- c(init_common, onnx$numpy_helper$from_array(as_i64_vec(reorder_idx), "perm_idx"))
      }
      
      # One-Hot pro Klasse
      oh_inits <- lapply(seq_len(K), function(ci) {
        oh <- rep(0, K); oh[ci] <- 1
        onnx$numpy_helper$from_array(as_fp_mat(oh), paste0("oh_", ci))
      })
      
      nodes <- list()
      cur_in <- "input_raw"
      
      # optional Reorder
      if (!is.null(reorder_idx)) {
        nodes <- c(nodes, helper$make_node("Gather", list(cur_in,"perm_idx"), list("input_reordered"),
                                           axis=1L, name="Reorder"))
        cur_in <- "input_reordered"
      }
      
      # Center/Scale + Guards
      nodes <- c(nodes,
                 helper$make_node("Sub", list(cur_in, "mean_vec"),  list("centered"),   name="Center"),
                 helper$make_node("Abs", list("std_vec"),           list("std_abs"),    name="StdAbs"),
                 helper$make_node("Max", list("std_abs","eps_f"),   list("std_safe"),   name="StdSafe"),
                 helper$make_node("Div", list("centered","std_safe"), list("scaled_raw"), name="ScaleSafe"),
                 helper$make_node("IsNaN", list("scaled_raw"),      list("isnan_scaled"), name="IsNaN_scaled"),
                 helper$make_node("IsInf", list("scaled_raw"),      list("isinf_scaled"), name="IsInf_scaled"),
                 helper$make_node("Or",    list("isnan_scaled","isinf_scaled"), list("bad_scaled"), name="Bad_scaled"),
                 helper$make_node("Where", list("bad_scaled","zero_f","scaled_raw"),   list("scaled"), name="Scaled_Clean"))
      
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Identity", list("scaled"),   list("X_scaled_dbg"), name="DbgScaled"),
                   helper$make_node("Identity", list("centered"), list("centered_dbg"), name="DbgCentered"),
                   helper$make_node("Identity", list("std_safe"), list("std_safe_dbg"), name="DbgStdSafe"))
      }
      
      # OvO-Modelle
      acc_votes_vecs  <- character(0)
      acc_scores_vecs <- character(0)
      z_list    <- character(0)
      mask_list <- character(0)
      vbp_rows  <- character(0)
      
      ge_op <- if (tie_rule == ">=0") "GreaterOrEqual" else "Greater"
      
      for (j in seq_along(bin_models)) {
        bm  <- bin_models[[j]]
        SVT <- t(bm$SV)                               # [D, nSV] – caret-Skala
        coef<- matrix(bm$coef, nrow=length(bm$coef), ncol=1L)  # [nSV,1]
        b   <- matrix(bm$b,    nrow=1L, ncol=1L)
        
        SVt_name  <- paste0("SVt_",  j)
        coef_name <- paste0("coef_", j)
        b_name    <- paste0("b_",    j)
        
        init_common <- c(init_common,
                         onnx$numpy_helper$from_array(as_fp_mat(SVT),  SVt_name),
                         onnx$numpy_helper$from_array(as_fp_mat(coef), coef_name),
                         onnx$numpy_helper$from_array(as_fp_mat(b),    b_name))
        
        mm    <- paste0("mm_",   j)
        mm_s  <- paste0("mm_s_", j)
        base  <- paste0("base_", j)
        ker   <- paste0("ker_",  j)
        zlin  <- paste0("zlin_", j)
        z     <- paste0("z_",    j)
        ge    <- paste0("ge_",   j)
        ge2   <- paste0("ge2_",  j)
        votej <- paste0("vote_", j)
        pos   <- paste0("pos_",  j)
        neg0  <- paste0("neg0_", j)
        neg   <- paste0("neg_",  j)
        posv  <- paste0("posv_", j)
        negv  <- paste0("negv_", j)
        score <- paste0("score_",j)
        
        nodes <- c(nodes,
                   helper$make_node("MatMul", list("scaled", SVt_name), list(mm),   name=paste0("MM_", j)),
                   helper$make_node("Mul",    list(mm, "k_scale"),      list(mm_s), name=paste0("Scale_", j)),
                   helper$make_node("Add",    list(mm_s, "k_offset"),   list(base), name=paste0("Offset_", j)))
        if (is.finite(clip_base)) {
          nodes <- c(nodes, helper$make_node("Clip", list(base), list(base),
                                             min=-abs(clip_base), max=abs(clip_base),
                                             name=paste0("ClipBase_", j)))
        }
        nodes <- c(nodes,
                   helper$make_node("Pow",    list(base, "k_degree"), list(ker),   name=paste0("Pow_", j)),
                   helper$make_node("MatMul", list(ker, coef_name),   list(zlin),  name=paste0("MMcoef_", j)),
                   helper$make_node("Add",    list(zlin, b_name),     list(z),     name=paste0("Bias_",   j)),
                   helper$make_node(ge_op,    list(z, "zero_f"),      list(ge),    name=paste0("GE_",     j)),
                   helper$make_node("Expand", list(ge, "shape_1K"),   list(ge2),   name=paste0("ExpandGE_", j)),
                   helper$make_node("Where",
                                    list(ge2,
                                         paste0("oh_", match(bm$ci, classes)),
                                         paste0("oh_", match(bm$cj, classes))),
                                    list(votej), name=paste0("Vote_", j)))
        acc_votes_vecs <- c(acc_votes_vecs, votej)
        z_list    <- c(z_list, z)
        mask_list <- c(mask_list, ge)
        vbp_rows  <- c(vbp_rows, votej)
        
        if (use_rule == "margin") {
          nodes <- c(nodes,
                     helper$make_node("Relu", list(z),     list(pos),   name=paste0("ReluPos_", j)),
                     helper$make_node("Neg",  list(z),     list(neg0),  name=paste0("Neg_",     j)),
                     helper$make_node("Relu", list(neg0),  list(neg),   name=paste0("ReluNeg_", j)),
                     helper$make_node("Mul",  list(pos, paste0("oh_", match(bm$ci, classes))), list(posv)),
                     helper$make_node("Mul",  list(neg, paste0("oh_", match(bm$cj, classes))), list(negv)),
                     helper$make_node("Add",  list(posv, negv), list(score), name=paste0("Score_", j)))
          acc_scores_vecs <- c(acc_scores_vecs, score)
        }
      }
      
      # Summe Votes
      acc_votes <- acc_votes_vecs[1]
      if (length(acc_votes_vecs) > 1) {
        for (i in 2:length(acc_votes_vecs)) {
          ns <- paste0("sum_votes_", i)
          nodes <- c(nodes, helper$make_node("Add", list(acc_votes, acc_votes_vecs[i]), list(ns)))
          acc_votes <- ns
        }
      }
      # Summe Scores (nur margin)
      if (use_rule == "margin") {
        acc_scores <- acc_scores_vecs[1]
        if (length(acc_scores_vecs) > 1) {
          for (i in 2:length(acc_scores_vecs)) {
            ns <- paste0("sum_scores_", i)
            nodes <- c(nodes, helper$make_node("Add", list(acc_scores, acc_scores_vecs[i]), list(ns)))
            acc_scores <- ns
          }
        }
      }
      
      # Top1 nach Regel
      if (use_rule == "margin") {
        nodes <- c(nodes,
                   helper$make_node("TopK", list(acc_scores,"k1"), list("topv","topi"),
                                    axis=1L, largest=1L, sorted=1L, name="TopK1_Margin"))
      } else {
        nodes <- c(nodes,
                   helper$make_node("TopK", list(acc_votes,"k1"), list("topv","topi"),
                                    axis=1L, largest=1L, sorted=1L, name="TopK1_Majority"))
      }
      nodes <- c(nodes,
                 helper$make_node("Identity", list("topi"),         list("idx0"), name="Idx0"),
                 helper$make_node("Add",      list("topi","one_i"), list("idx1"), name="Idx1"))
      
      # Outputs
      nodes <- c(nodes, helper$make_node("Identity", list(acc_votes), list("votes_vector"), name="OutVotes"))
      if (use_rule == "margin") {
        nodes <- c(nodes, helper$make_node("Identity", list(acc_scores), list("scores_vector"), name="OutScores"))
      }
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Concat", z_list,    list("z_all"),         axis=1L, name="ConcatZ"),
                   helper$make_node("Concat", mask_list, list("win_mask"),      axis=1L, name="ConcatMask"),
                   helper$make_node("Concat", vbp_rows,  list("votes_by_pair"), axis=0L, name="ConcatVotesByPair"),
                   helper$make_node("ReduceSum", list("votes_vector"), list("total_votes"), keepdims=1L, name="TotalVotes"))
      }
      
      graph <- helper$make_graph(
        nodes       = nodes,
        name        = paste0("svmPoly_ovo_", use_rule, "_fp"),
        inputs      = list(inp),
        outputs     = outs,
        initializer = c(init_common, oh_inits)
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = paste0("ovo_poly_", use_rule, "_export"),
        opset_imports = list(helper$make_operatorsetid("", 13L))
      )
      onnx$save(model_onnx, out_path)
      cat("ONNX (", use_rule, ", dtype=", dtype,
          ", primary_output=", primary_output,
          ", tie_rule=", tie_rule,
          if (!is.na(clip_base)) paste0(", clip=", clip_base) else "",
          ") gespeichert: ", out_path, "\n", sep = "")
    }
    
    
    
    
    build_onnx_rbf_ovo <- function(
    use_rule      = c("majority","margin"),
    bin_models, feature_names, classes, means, sds, sigma, out_path,
    dtype         = c("float64","float32"),
    reorder_idx   = NULL,
    add_debug     = TRUE,
    primary_output= c("idx1","votes_vector","scores_vector"),
    tie_rule      = c(">0",">=0")
    ){
      use_rule <- match.arg(use_rule)
      dtype    <- match.arg(dtype)
      primary_output <- match.arg(primary_output)
      tie_rule <- match.arg(tie_rule)
      
      onnx        <- reticulate::import("onnx",        convert = FALSE)
      helper      <- reticulate::import("onnx.helper", convert = FALSE)
      np          <- reticulate::import("numpy",       convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp          <- reticulate::tuple
      
      is_f64 <- (dtype == "float64")
      TNUM   <- if (is_f64) TensorProto$DOUBLE else TensorProto$FLOAT
      as_fp_vec <- function(x) np$array(as.numeric(x), dtype = if (is_f64) "float64" else "float32")
      as_fp_mat <- function(M){ if (is.vector(M)) M <- matrix(M, nrow=1L); np$array(M, dtype = if (is_f64) "float64" else "float32") }
      as_i64_vec <- function(x){ xv <- as.integer(x); np$array(xv, dtype = "int64")$reshape(tp(length(xv))) }
      
      K      <- length(classes)
      n_feat <- length(feature_names)
      
      # I/O
      inp    <- helper$make_tensor_value_info("input_raw", TensorProto$FLOAT, list(1L, n_feat))
      out_i  <- helper$make_tensor_value_info("idx1", TensorProto$INT64, list(1L,1L))
      out_vv <- helper$make_tensor_value_info("votes_vector", TNUM, list(1L,K))
      outs   <- list(out_i, out_vv)
      if (use_rule == "margin") {
        out_sv <- helper$make_tensor_value_info("scores_vector", TNUM, list(1L,K))
        outs   <- c(outs, list(out_sv))
      }
      if (isTRUE(add_debug)) {
        out_xs  <- helper$make_tensor_value_info("X_scaled_dbg",   TNUM, list(1L,n_feat))
        out_cen <- helper$make_tensor_value_info("centered_dbg",   TNUM, list(1L,n_feat))
        out_ss  <- helper$make_tensor_value_info("std_safe_dbg",   TNUM, list(1L,n_feat))
        out_z   <- helper$make_tensor_value_info("z_all",          TNUM, list(1L, length(bin_models)))
        out_m   <- helper$make_tensor_value_info("win_mask",       TensorProto$BOOL, list(1L, length(bin_models)))
        out_vbp <- helper$make_tensor_value_info("votes_by_pair",  TNUM, list(length(bin_models), K))
        out_tv  <- helper$make_tensor_value_info("total_votes",    TNUM, list(1L,1L))
        out_i0  <- helper$make_tensor_value_info("idx0",           TensorProto$INT64, list(1L,1L))
        outs    <- c(outs, list(out_xs, out_cen, out_ss, out_z, out_m, out_vbp, out_tv, out_i0))
      }
      
      # Primary output zuerst
      reorder_outs <- function(outs, names_vec, primary){
        idx <- match(primary, names_vec)
        if (is.na(idx) || idx == 1L) return(outs)
        c(outs[idx], outs[-idx])
      }
      out_name_vec <- c("idx1","votes_vector", if (use_rule=="margin") "scores_vector",
                        if (isTRUE(add_debug)) c("X_scaled_dbg","centered_dbg","std_safe_dbg","z_all","win_mask","votes_by_pair","total_votes","idx0"))
      outs <- reorder_outs(outs, out_name_vec, primary_output)
      
      # Initializer (global)
      init_common <- list(
        onnx$numpy_helper$from_array(as_fp_mat(means),   "mean_vec"),
        onnx$numpy_helper$from_array(as_fp_mat(sds),     "std_vec"),
        onnx$numpy_helper$from_array(as_fp_vec(0.0),     "zero_f"),
        onnx$numpy_helper$from_array(as_fp_vec(-2.0),    "neg2_f"),
        onnx$numpy_helper$from_array(as_fp_vec(-sigma),  "neg_sigma_f"),
        onnx$numpy_helper$from_array(as_fp_vec(1e-6),    "eps_f"),
        onnx$numpy_helper$from_array(as_i64_vec(c(1L,K)),"shape_1K"),
        onnx$numpy_helper$from_array(as_i64_vec(1L),     "k1"),
        onnx$numpy_helper$from_array(as_i64_vec(1L),     "one_i")
      )
      if (!is.null(reorder_idx)) {
        stopifnot(length(reorder_idx) == n_feat)
        init_common <- c(init_common, onnx$numpy_helper$from_array(as_i64_vec(reorder_idx), "perm_idx"))
      }
      
      # One-Hot pro Klasse
      oh_inits <- lapply(seq_len(K), function(ci) {
        oh <- rep(0, K); oh[ci] <- 1
        onnx$numpy_helper$from_array(as_fp_mat(oh), paste0("oh_", ci))
      })
      
      nodes <- list()
      cur_in <- "input_raw"
      
      # optional Feature-Reorder
      if (!is.null(reorder_idx)) {
        nodes <- c(nodes, helper$make_node("Gather", list(cur_in,"perm_idx"), list("input_reordered"),
                                           axis=1L, name="Reorder"))
        cur_in <- "input_reordered"
      }
      
      # Center/Scale + Guards
      nodes <- c(nodes,
                 helper$make_node("Sub", list(cur_in, "mean_vec"),  list("centered"),   name="Center"),
                 helper$make_node("Abs", list("std_vec"),           list("std_abs"),    name="StdAbs"),
                 helper$make_node("Max", list("std_abs","eps_f"),   list("std_safe"),   name="StdSafe"),
                 helper$make_node("Div", list("centered","std_safe"), list("scaled_raw"), name="ScaleSafe"),
                 helper$make_node("IsNaN", list("scaled_raw"),      list("isnan_scaled"), name="IsNaN_scaled"),
                 helper$make_node("IsInf", list("scaled_raw"),      list("isinf_scaled"), name="IsInf_scaled"),
                 helper$make_node("Or",    list("isnan_scaled","isinf_scaled"), list("bad_scaled"), name="Bad_scaled"),
                 helper$make_node("Where", list("bad_scaled","zero_f","scaled_raw"),   list("scaled"), name="Scaled_Clean"))
      
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Identity", list("scaled"),   list("X_scaled_dbg"), name="DbgScaled"),
                   helper$make_node("Identity", list("centered"), list("centered_dbg"), name="DbgCentered"),
                   helper$make_node("Identity", list("std_safe"), list("std_safe_dbg"), name="DbgStdSafe"))
      }
      
      # RBF-Modelle
      acc_votes_vecs  <- character(0)
      acc_scores_vecs <- character(0)
      z_list    <- character(0)
      mask_list <- character(0)
      vbp_rows  <- character(0)
      
      ge_op <- if (tie_rule == ">=0") "GreaterOrEqual" else "Greater"
      
      for (j in seq_along(bin_models)) {
        bm  <- bin_models[[j]]
        SVT <- t(bm$SV)                                # [D, nSV] (caret)
        coef<- matrix(bm$coef, nrow=length(bm$coef), ncol=1L)
        b   <- matrix(bm$b,    nrow=1L, ncol=1L)
        
        sv_norm2 <- matrix(colSums(SVT*SVT), nrow=1L)
        
        SVt_name   <- paste0("SVt_",  j)
        coef_name  <- paste0("coef_", j)
        b_name     <- paste0("b_",    j)
        svn_name   <- paste0("svnorm2_", j)
        shape1M    <- paste0("shape_1m_", j)
        axes1_name <- paste0("axes1_", j)  # <-- NEU: axes als Input
        
        init_common <- c(init_common,
                         onnx$numpy_helper$from_array(as_fp_mat(SVT),      SVt_name),
                         onnx$numpy_helper$from_array(as_fp_mat(coef),     coef_name),
                         onnx$numpy_helper$from_array(as_fp_mat(b),        b_name),
                         onnx$numpy_helper$from_array(as_fp_mat(sv_norm2), svn_name),
                         onnx$numpy_helper$from_array(as_i64_vec(c(1L, ncol(SVT))), shape1M),
                         onnx$numpy_helper$from_array(as_i64_vec(1L),      axes1_name))  # <-- axes=(1,) als Tensor
        
        mm     <- paste0("mm_", j)
        xn2    <- paste0("xn2_", j)
        xn2e   <- paste0("xn2e_", j)
        mmt    <- paste0("mmt_", j)
        dist2  <- paste0("dist2_", j)
        gprod  <- paste0("gprod_", j)
        ker    <- paste0("ker_", j)
        zlin   <- paste0("zlin_", j)
        z      <- paste0("z_", j)
        ge     <- paste0("ge_", j)
        ge2    <- paste0("ge2_", j)
        votej  <- paste0("vote_", j)
        pos    <- paste0("pos_", j)
        neg0   <- paste0("neg0_", j)
        neg    <- paste0("neg_", j)
        posv   <- paste0("posv_", j)
        negv   <- paste0("negv_", j)
        score  <- paste0("score_", j)
        
        # RBF: ||x - sv||^2 = ||x||^2 + ||sv||^2 - 2·(x·sv)
        nodes <- c(nodes,
                   helper$make_node("MatMul", list("scaled", SVt_name), list(mm),   name=paste0("MM_", j)),
                   helper$make_node("Mul",    list(mm, "neg2_f"),       list(mmt),  name=paste0("MulNeg2_", j)),
                   helper$make_node("Mul",    list("scaled","scaled"),  list(paste0("x2_",j)), name=paste0("SquareX_", j)),
                   # *** FIX: ReduceSum axes als 2. Input, nicht als Attribut ***
                   helper$make_node("ReduceSum", list(paste0("x2_",j), axes1_name), list(xn2),
                                    keepdims=1L, name=paste0("SumX2_", j)),
                   helper$make_node("Expand", list(xn2, shape1M),       list(xn2e), name=paste0("ExpandX2_", j)),
                   helper$make_node("Add",    list(xn2e, svn_name),     list(paste0("tmp_",j)), name=paste0("AddXnSv_", j)),
                   helper$make_node("Add",    list(paste0("tmp_",j), mmt), list(dist2), name=paste0("Dist2_", j)),
                   helper$make_node("Mul",    list(dist2, "neg_sigma_f"), list(gprod), name=paste0("MulGamma_", j)),
                   helper$make_node("Exp",    list(gprod),              list(ker),   name=paste0("Exp_", j)),
                   helper$make_node("MatMul", list(ker, coef_name),     list(zlin),  name=paste0("MMcoef_", j)),
                   helper$make_node("Add",    list(zlin, b_name),       list(z),     name=paste0("Bias_", j)),
                   helper$make_node(ge_op,    list(z, "zero_f"),        list(ge),    name=paste0("GE_", j)),
                   helper$make_node("Expand", list(ge, "shape_1K"),     list(ge2),   name=paste0("ExpandGE_", j)),
                   helper$make_node("Where",
                                    list(ge2,
                                         paste0("oh_", match(bm$ci, classes)),
                                         paste0("oh_", match(bm$cj, classes))),
                                    list(votej), name=paste0("Vote_", j)))
        
        acc_votes_vecs <- c(acc_votes_vecs, votej)
        z_list    <- c(z_list, z)
        mask_list <- c(mask_list, ge)
        vbp_rows  <- c(vbp_rows, votej)
        
        if (use_rule == "margin") {
          nodes <- c(nodes,
                     helper$make_node("Relu", list(z),     list(pos),   name=paste0("ReluPos_", j)),
                     helper$make_node("Neg",  list(z),     list(neg0),  name=paste0("Neg_",     j)),
                     helper$make_node("Relu", list(neg0),  list(neg),   name=paste0("ReluNeg_", j)),
                     helper$make_node("Mul",  list(pos, paste0("oh_", match(bm$ci, classes))), list(posv)),
                     helper$make_node("Mul",  list(neg, paste0("oh_", match(bm$cj, classes))), list(negv)),
                     helper$make_node("Add",  list(posv, negv), list(score), name=paste0("Score_", j)))
          acc_scores_vecs <- c(acc_scores_vecs, score)
        }
      }
      
      # Summe Votes
      acc_votes <- acc_votes_vecs[1]
      if (length(acc_votes_vecs) > 1) {
        for (i in 2:length(acc_votes_vecs)) {
          ns <- paste0("sum_votes_", i)
          nodes <- c(nodes, helper$make_node("Add", list(acc_votes, acc_votes_vecs[i]), list(ns)))
          acc_votes <- ns
        }
      }
      # Summe Scores (nur margin)
      if (use_rule == "margin") {
        acc_scores <- acc_scores_vecs[1]
        if (length(acc_scores_vecs) > 1) {
          for (i in 2:length(acc_scores_vecs)) {
            ns <- paste0("sum_scores_", i)
            nodes <- c(nodes, helper$make_node("Add", list(acc_scores, acc_scores_vecs[i]), list(ns)))
            acc_scores <- ns
          }
        }
      }
      
      # Top1
      if (use_rule == "margin") {
        nodes <- c(nodes, helper$make_node("TopK", list(acc_scores,"k1"), list("topv","topi"),
                                           axis=1L, largest=1L, sorted=1L, name="TopK1_Margin"))
      } else {
        nodes <- c(nodes, helper$make_node("TopK", list(acc_votes,"k1"), list("topv","topi"),
                                           axis=1L, largest=1L, sorted=1L, name="TopK1_Majority"))
      }
      nodes <- c(nodes,
                 helper$make_node("Identity", list("topi"),         list("idx0"), name="Idx0"),
                 helper$make_node("Add",      list("topi","one_i"), list("idx1"), name="Idx1"))
      
      nodes <- c(nodes, helper$make_node("Identity", list(acc_votes), list("votes_vector"), name="OutVotes"))
      if (use_rule == "margin") {
        nodes <- c(nodes, helper$make_node("Identity", list(acc_scores), list("scores_vector"), name="OutScores"))
      }
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Concat", z_list,    list("z_all"),         axis=1L, name="ConcatZ"),
                   helper$make_node("Concat", mask_list, list("win_mask"),      axis=1L, name="ConcatMask"),
                   helper$make_node("Concat", vbp_rows,  list("votes_by_pair"), axis=0L, name="ConcatVotesByPair"),
                   helper$make_node("ReduceSum", list("votes_vector"), list("total_votes"), keepdims=1L, name="TotalVotes"))
      }
      
      graph <- helper$make_graph(
        nodes       = nodes,
        name        = paste0("svmRBF_ovo_", use_rule, "_fp"),
        inputs      = list(inp),
        outputs     = outs,
        initializer = c(init_common, oh_inits)
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = paste0("ovo_rbf_", use_rule, "_export"),
        opset_imports = list(helper$make_operatorsetid("", 13L))
      )
      onnx$save(model_onnx, out_path)
      cat("ONNX (RBF, ", use_rule, ", dtype=", dtype,
          ", primary_output=", primary_output,
          ", tie_rule=", tie_rule, ") gespeichert: ", out_path, "\n", sep = "")
    }
    
    
    
    
    
    
    
    
    
    
    save_torch_model <- function(model, filepath,
                                 input_channels = NULL,
                                 time_steps     = NULL) {
      `%||%` <- function(a,b) if (!is.null(a)) a else b
      # --- 1) TempCNN-Spezialpfad ---
      has_conv <- tryCatch(!is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
                             length(model$conv_layers[[1]]) >= 1 &&
                             !is.null(model$conv_layers[[1]][[1]]$in_channels),
                           error = function(e) FALSE)
      
      if (has_conv) {
        first_conv     <- model$conv_layers[[1]][[1]]
        input_channels <- as.integer(first_conv$in_channels)
        time_steps     <- as.integer(model$time_steps %||% stop("'time_steps' fehlt am TempCNN-Modell"))

        B     <- 1L
        dummy <- torch::torch_randn(c(B, input_channels, time_steps))
        model$eval()
        script_model <- torch::jit_trace(model, dummy)
        script_model$eval()
        torch::jit_save(script_model, filepath)
        return(list(pt_path = filepath,
                    input_chan = input_channels,
                    time_steps = time_steps,
                    input_layout = "NCT"))
      }
      
      ic_from_model <- tryCatch(model$input_channels,     error = function(e) NULL)
      ts_from_model <- tryCatch(model$time_steps,         error = function(e) NULL)
      il_from_model <- tryCatch(model$input_layout,       error = function(e) NULL)
      cols_from_mod <- tryCatch(model$input_data_columns, error = function(e) NULL)

      
      input_channels <- input_channels %||% ic_from_model %||%
        (if (!is.null(cols_from_mod)) length(cols_from_mod) else NULL)
      time_steps   <- time_steps   %||% ts_from_model    %||% 1L
      
      # >>> FIX hier: keine Selbst-Referenz
      input_layout <- if (!is.null(il_from_model)) il_from_model else "NCT"

      if (is.null(input_channels) || input_channels < 1L)
        stop("Für Torch-Export fehlt 'input_channels'. Hänge es beim Training ans Modell oder übergib es als Argument.")
      if (is.null(time_steps) || time_steps < 1L)
        stop("Für Torch-Export fehlt 'time_steps'. Hänge es beim Training ans Modell oder übergib es als Argument.")
      
      # Scripted-Modell? Dann nicht nochmal tracen – nur speichern
      if (inherits(model, "jit_script_module")) {
        torch::jit_save(model, filepath)
        return(list(pt_path = filepath,
                    input_chan = as.integer(input_channels),
                    time_steps = as.integer(time_steps),
                    input_layout = toupper(input_layout)))
      }
      
      # Dummy je nach Layout
      B <- 1L
      if (toupper(input_layout) == "NC") {
        dummy <- torch::torch_randn(c(B, as.integer(input_channels)))
      } else {
        dummy <- torch::torch_randn(c(B, as.integer(input_channels), as.integer(time_steps)))
      }
      
      model$eval()
      script_model <- torch::jit_trace(model, dummy)
      script_model$eval()
      torch::jit_save(script_model, filepath)

      list(pt_path = filepath,
           input_chan = as.integer(input_channels),
           time_steps = as.integer(time_steps),
           input_layout = toupper(input_layout))
    }
    
    
    
    convert_torch_to_onnx_from_pt <- function(script_pt, input_chan, time_steps, base_name, output_dir) {
      message("Start ONNX conversion...")
      onnx_path <- ensure_extension(file.path(output_dir, base_name), "onnx")
      py_file <- tempfile(fileext = ".py")
      writeLines(sprintf("
import torch, warnings
warnings.filterwarnings('ignore', category=UserWarning)
mod = torch.jit.load(r'%s')
mod.eval()
dummy = torch.randn(1, %d, %d)
torch.onnx.export(
    mod, dummy, r'%s',
    input_names=['input'], output_names=['output'],
    dynamic_axes={'input': {0: 'batch_size', 2: 'time_steps'}, 'output': {0: 'batch_size'}},
    opset_version=14
)
print('ONNX successfully saved: %s')
", script_pt, input_chan, time_steps, onnx_path, onnx_path), con = py_file)
python_bin <- find_python_bin()
res <- system2(python_bin, py_file, stdout = TRUE, stderr = TRUE)
cat(res, sep = "\n")
unlink(py_file)
if (!file.exists(onnx_path)) stop("ONNX export failed:\n", paste(res, collapse = "\n"))
message("ONNX stored under: ", onnx_path)
return(onnx_path)
    }
    
    convert_model_to_pkl <- function(model, model_type, filepath) {
      library(reticulate)
      joblib <- reticulate::import("joblib"); np <- reticulate::import("numpy")
      sklearn <- reticulate::import("sklearn.ensemble"); sklearn_svm <- reticulate::import("sklearn.svm"); xgboost <- reticulate::import("xgboost")
      
      if (!("train" %in% class(model))) stop("Please pass a caret train object")
      if (!("trainingData" %in% names(model))) stop("The caret model does not contain any trainingData. Please save the model with trainingData")
      
      train_data <- model$trainingData
      if (!(".outcome" %in% colnames(train_data))) stop("The trainingData does not contain an '.outcome' column")
      
      predictors <- setdiff(colnames(train_data), ".outcome")
      target_column <- ".outcome"
      final <- model$finalModel
      train_data_clean <- train_data[, predictors, drop = FALSE]
      if (is.factor(train_data[[target_column]]) || is.character(train_data[[target_column]])) {
        train_data[[target_column]] <- as.integer(as.factor(train_data[[target_column]])) - 1
      }
      x_train <- np$array(as.matrix(train_data_clean))
      y_train <- np$array(as.numeric(train_data[[target_column]]))
      
      if (model_type == "random_forest") {
        if (!inherits(final, "randomForest")) stop("Error: The model is not a RandomForest model")
        rf_py_model <- sklearn$RandomForestClassifier(
          n_estimators = as.integer(final$ntree),
          max_features = as.integer(final$mtry)
        )
        rf_py_model$fit(x_train, y_train)
        joblib$dump(rf_py_model, paste0(filepath, ".pkl"))
      } else if (model_type == "xgbTree") {
        if (!inherits(final, "xgb.Booster")) stop("Error: The model is not an XGBoost model")
        xgboost::xgb.save(final, paste0(filepath, ".bin"))
      } else {
        stop("Model type is not supported!")
      }
    }
    
    add_metadata_to_onnx <- function(onnx_path, options) {
      message("add metadata to onnx")
      onnx <- reticulate::import("onnx")
      model <- onnx$load_model(onnx_path)
      for (key in names(options)) {
        value <- paste(as.character(options[[key]]), collapse = ", ")
        meta_prop <- onnx$StringStringEntryProto(key = key, value = value)
        model$metadata_props$append(meta_prop)
      }
      onnx$save_model(model, onnx_path)
      message("Metadata has been added to the ONNX model.")
    }
    
    save_ml_model_as_onnx <- function(model_type, filepath) {
      library(reticulate)
      tryCatch({
        onnxmltools   <- reticulate::import("onnxmltools")
        skl2onnx      <- reticulate::import("skl2onnx")
        onnx          <- reticulate::import("onnx")
        joblib        <- reticulate::import("joblib")
        FloatTensorType <- reticulate::import("skl2onnx.common.data_types")$FloatTensorType
        
        n_features <- NULL; model_py <- NULL
        if (model_type == "xgbTree") {
          xgb_mod <- reticulate::import("xgboost")
          booster_py <- xgb_mod$Booster()
          booster_py$load_model(paste0(filepath, ".bin"))
          model_py   <- booster_py
          n_features <- booster_py$num_features()
        } else if (model_type == "random_forest") {
          rf_model_py <- joblib$load(paste0(filepath, ".pkl"))
          n_features  <- rf_model_py$n_features_in_
          model_py    <- rf_model_py
        } else {
          stop("Model type is not supported!")
        }
        
        if (is.null(n_features)) stop("n_features_in_ could not be determined.")
        
        initial_type <- list(list("float_input", FloatTensorType(list(NULL, as.integer(n_features)))))
        
        if (model_type == "xgbTree") onnx_model <- onnxmltools$convert_xgboost(model_py, initial_types = initial_type)
        else onnx_model <- skl2onnx$convert_sklearn(model_py, initial_types = initial_type)
        
        onnx_file <- ensure_extension(filepath, "onnx")
        onnx$save_model(onnx_model, onnx_file)
        message("Model successfully saved as ONNX under: ", onnx_file)
        return(onnx_file)
      }, error = function(e) {
        message("Error in save_ml_model_as_onnx:", e$message); stop(e)
      })
    }
    
    save_model_as_mlm_stac_json <- function(model, filepath, tasks = list("classification"), options = list()) {
      mlm_stac_item <- list(
        type = "Feature",
        stac_version = "1.0.0",
        id = basename(sub("\\.json$", "", filepath)),
        properties = list(datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
        geometry = NULL, bbox = NULL,
        stac_extensions = list("https://stac-extensions.github.io/mlm/v1.0.0/schema.json"),
        assets = list()
      )
      model_info <- list()
      if ("train" %in% class(model)) {
        model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
        model_info$"mlm:architecture" <- model$modelInfo$label
        model_info$"mlm:tasks" <- tasks
        model_info$"mlm:framework" <- paste("R", paste(model$modelInfo$library, collapse = "/"), sep = " (")
        model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
        model_type <- detect_model_type(model)
        if (model_type == "random_forest") {
          model_info$"mlm:total_parameters" <- model$finalModel$ntree * length(model$finalModel$forest$nodestatus)
          hyperparameters <- list()
          if (!is.null(model$finalModel$mtry)) hyperparameters$mtry <- model$finalModel$mtry
          if (!is.null(model$finalModel$ntree)) hyperparameters$ntree <- model$finalModel$ntree
          if (!is.null(model$finalModel$max_depth)) hyperparameters$max_depth <- model$finalModel$max_depth
        } else if (model_type %in% c("svmLinear","svmRadial","svmPoly")) {
          cost <- tryCatch(model$finalModel@kernelf@kpar$C, error = function(e) 1.0)
          nSV <- model$finalModel@nSV
          model_info$"mlm:total_parameters" <- nSV
          hyperparameters <- list(cost = cost, nSV = nSV,
                                  kernel = if (model_type == "svmLinear") "linear" else if (model_type == "svmRadial") "radial" else "poly")
        } else if (model_type == "xgbTree") {
          nrounds <- model$finalModel$niter
          model_info$"mlm:total_parameters" <- nrounds
          hyperparameters <- list(
            max_depth = model$finalModel$tuneValue$max_depth,
            nrounds   = nrounds,
            eta       = model$finalModel$tuneValue$eta,
            gamma     = model$finalModel$tuneValue$gamma
          )
        } else hyperparameters <- list()
        if (length(hyperparameters) > 0) model_info$"mlm:hyperparameters" <- hyperparameters
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- NULL
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        
        predictors <- if (inherits(model$finalModel, "ksvm"))
          names(model$trainingData)[!names(model$trainingData) %in% ".outcome"]
        else model$finalModel$xNames
        
        model_info$"mlm:input" <- list(
          list(
            name = "Features",
            bands = predictors,
            input = list(shape = list(1, length(predictors)),
                         dim_order = list("batch", "features"),
                         data_type = "float32"),
            description = "Input features for classification",
            pre_processing_function = NULL
          )
        )
        classes <- model$levels
        class_objects <- lapply(seq_along(classes), function(i) {
          list(value = i - 1, name = classes[i], description = paste("Class:", classes[i]))
        })
        model_info$"mlm:output" <- list(
          list(
            name = "CLASSIFICATION",
            tasks = tasks,
            result = list(shape = list(1, length(classes)),
                          dim_order = list("batch", "classes"),
                          data_type = "float32"),
            description = "Predicted probabilities for classification",
            "classification:classes" = class_objects,
            post_processing_function = NULL
          )
        )
        if (length(options) > 0) {
          for (key in names(options)) if (grepl("^mlm:", key)) model_info[[key]] <- options[[key]]
        }
      } else stop("Unknown model type: Please check the model!")
      mlm_stac_item$properties <- c(mlm_stac_item$properties, model_info)
      rds_path <- ensure_extension(sub("\\.json$", "", filepath), "rds")
      saveRDS(model, file = rds_path)
      mlm_stac_item$assets <- list(
        model = list(
          href = rds_path, type = "application/octet-stream",
          title = paste(model_info$"mlm:architecture", "Model"),
          "mlm:artifact_type" = "R (RDS)", roles = list("mlm:model")
        )
      )
      jsonlite::write_json(mlm_stac_item, path = filepath, auto_unbox = TRUE, pretty = TRUE)
      message("Model was saved as MLM-STAC-JSON under: ", filepath)
      return(filepath)
    }
    
    save_model_as_mlm_stac_json_dl <- function(model, filepath, tasks = list("classification"), options = list()) {
      message("save mlm-stac-dl...")
      mlm_stac_item <- list(
        type = "Feature",
        stac_version = "1.0.0",
        id = basename(sub("\\.json$", "", filepath)),
        properties = list(datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
        geometry = NULL, bbox = NULL,
        stac_extensions = list("https://stac-extensions.github.io/mlm/v1.0.0/schema.json"),
        assets = list()
      )
      
      message("Nun sind wir hier")
      model_info <- list()
      
      # Erkennen, ob es wirklich ein TempCNN ist (conv_layers vorhanden)
      is_tempcnn <- tryCatch(
        !is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
          length(model$conv_layers[[1]]) >= 1 &&
          !is.null(model$conv_layers[[1]][[1]]$in_channels),
        error = function(e) FALSE
      )
      
      if (is_tempcnn) {
        model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
        model_info$"mlm:architecture" <- "TempCNN"
        model_info$"mlm:tasks" <- tasks
        model_info$"mlm:framework" <- "R (torch)"
        model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
        
        total_params <- tryCatch(sum(unlist(lapply(model$parameters, function(p) prod(dim(p))))), error = function(e) NA_integer_)
        model_info$"mlm:total_parameters" <- if (is.finite(total_params)) as.integer(total_params) else NULL
        model_info$"mlm:hyperparameters" <- list(conv_layers = length(model$conv_layers), dense_layers = length(model$dense))
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- "gpu"
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        
        input_channels <- model$conv_layers[[1]][[1]]$in_channels
        time_steps     <- model$time_steps
        bands          <- tryCatch(model$input_data_columns, error = function(e) NULL)
        if (is.null(input_channels)) stop("Could not extract input_channels from conv_layers!")
        if (is.null(time_steps))     stop("time_steps is missing in the model!")
        model_info$"mlm:input" <- list(
          list(
            name  = "Temporal CNN Input",
            bands = if (!is.null(bands)) as.list(bands) else list("unknown"),
            input = list(
              shape     = list(1, as.integer(input_channels), as.integer(time_steps)),
              dim_order = list("batch", "channels", "time_steps"),
              data_type = "float32"
            ),
            description = "Temporal input for CNN",
            pre_processing_function = NULL
          )
        )
        
        # Outputgröße ermitteln (Dense → Weights)
        output_size <- tryCatch({
          if (!is.null(model$dense) && length(model$dense) >= 1L) {
            last_dense <- model$dense[[length(model$dense)]]
            os <- tryCatch(as.integer(last_dense$out_features), error = function(e) NA_integer_)
            if (is.finite(os) && os > 0) return(os)
            if (!is.null(last_dense$weight)) {
              sz <- tryCatch(as.integer(last_dense$weight$size()), error = function(e) integer())
              if (length(sz) >= 1L && is.finite(sz[1]) && sz[1] > 0) return(sz[1])
            }
          }
          NA_integer_
        }, error = function(e) NA_integer_)
        if (!is.finite(output_size) || is.na(output_size) || output_size < 1L)
          stop("Could not extract output_size from dense layer")
        
        model_info$"mlm:output" <- list(
          list(
            name  = "CNN Output",
            tasks = tasks,
            result = list(
              shape     = list(1, as.integer(output_size)),
              dim_order = list("batch", "features"),
              data_type = "float32"
            ),
            description = "Output features from TempCNN",
            "classification:classes" = if (tasks[[1]] == "classification") {
              lapply(0:(output_size - 1), function(i) list(value = i, name = paste("class", i), description = paste("Class", i)))
            } else NULL,
            post_processing_function = NULL
          )
        )
        
      } else if ("nn_module" %in% class(model)) {
        # Generischer DL-Zweig: LightTAE / MLP / STGF
        arch_name <- tryCatch({
          cls <- class(model)
          # Nimm den spez. Klassennamen, falls vorhanden (z.B. "LightTAE")
          pick <- cls[!cls %in% c("nn_module", "R6", "R6Class")]
          if (length(pick) >= 1) pick[1] else "DLModel"
        }, error = function(e) "DLModel")
        
        model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
        model_info$"mlm:architecture" <- arch_name
        model_info$"mlm:tasks" <- tasks
        model_info$"mlm:framework" <- "R (torch)"
        model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
        
        total_params <- tryCatch(sum(unlist(lapply(model$parameters, function(p) prod(dim(p))))), error = function(e) NA_integer_)
        model_info$"mlm:total_parameters" <- if (is.finite(total_params)) as.integer(total_params) else NULL
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- "gpu"
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        
        # Input ableiten
        input_channels <- tryCatch(model$input_channels, error = function(e) NULL)
        time_steps     <- tryCatch(model$time_steps,     error = function(e) NULL)
        bands          <- tryCatch(model$input_data_columns, error = function(e) NULL)
        layout         <- toupper(tryCatch(model$input_layout, error = function(e) NULL) %||% "NCT")
        
        if (is.null(input_channels) && !is.null(bands)) input_channels <- length(bands)
        if (is.null(input_channels) || input_channels < 1L)
          stop("Could not infer input_channels for generic DL model.")
        if (is.null(time_steps) || time_steps < 1L) time_steps <- 1L  # MLP → 1
        
        # Shape + Dim-Order je nach Layout
        if (layout == "NC") {
          shape <- list(1, as.integer(input_channels))
          dim_order <- list("batch", "channels")
          desc <- "Static (non-temporal) input"
        } else {
          shape <- list(1, as.integer(input_channels), as.integer(time_steps))
          dim_order <- list("batch", "channels", "time_steps")
          desc <- "Temporal input"
        }
        
        model_info$"mlm:input" <- list(
          list(
            name  = paste(arch_name, "Input"),
            bands = if (!is.null(bands)) as.list(bands) else list("unknown"),
            input = list(shape = shape, dim_order = dim_order, data_type = "float32"),
            description = desc,
            pre_processing_function = NULL
          )
        )
        
        # Outputgröße robust ermitteln (Forward → class_count → classes)
        output_size <- tryCatch({
          B <- 1L
          dummy <- if (layout == "NC") {
            torch::torch_zeros(c(B, as.integer(input_channels)), dtype = torch_float())
          } else {
            torch::torch_zeros(c(B, as.integer(input_channels), as.integer(time_steps)), dtype = torch_float())
          }
          model$eval()
          out <- model(dummy)
          sz  <- tryCatch(as.integer(out$size()), error = function(e) integer())
          if (length(sz) >= 2L && is.finite(sz[2]) && sz[2] > 0) return(sz[2])
          
          cc <- tryCatch(as.integer(model$class_count), error = function(e) NA_integer_)
          if (is.finite(cc) && cc > 0) return(cc)
          cls <- tryCatch(length(model$classes), error = function(e) NA_integer_)
          if (is.finite(cls) && cls > 0) return(as.integer(cls))
          
          NA_integer_
        }, error = function(e) NA_integer_)
        
        if (!is.finite(output_size) || is.na(output_size) || output_size < 1L)
          stop("Could not infer output_size for generic DL model.")
        
        model_info$"mlm:output" <- list(
          list(
            name  = paste(arch_name, "Output"),
            tasks = tasks,
            result = list(
              shape     = list(1, as.integer(output_size)),
              dim_order = list("batch", "features"),
              data_type = "float32"
            ),
            description = paste("Output features from", arch_name),
            "classification:classes" = if (tasks[[1]] == "classification") {
              lapply(0:(output_size - 1), function(i)
                list(value = i, name = paste("class", i), description = paste("Class", i)))
            } else NULL,
            post_processing_function = NULL
          )
        )
        
        # optionale mlm:* Zusatzfelder übernehmen
        if (length(options) > 0) for (key in names(options)) if (grepl("^mlm:", key)) model_info[[key]] <- options[[key]]
        
      } else {
        stop("Unknown model type: Please check the model!")
      }
      
      mlm_stac_item$properties <- c(mlm_stac_item$properties, model_info)
      
      rds_path <- ensure_extension(sub("\\.json$", "", filepath), "rds")
      con <- rawConnection(raw(0), "wb"); torch::torch_save(model, con)
      raw_model <- rawConnectionValue(con); close(con)
      saveRDS(raw_model, file = rds_path)
      
      mlm_stac_item$assets <- list(
        model = list(
          href = rds_path, type = "application/octet-stream", title = paste0(model_info$`mlm:architecture`, " Model"),
          "mlm:artifact_type" = "R (Raw RDS)", roles = list("mlm:model")
        )
      )
      jsonlite::write_json(mlm_stac_item, path = filepath, auto_unbox = TRUE, pretty = TRUE)
      message("Model was saved as MLM-STAC-JSON under: ", filepath)
      return(filepath)
    }
    
    
    # ================== OPERATION BODY ==================

    # robustes options/tasks-Merging (ohne %||% auf NULL)
    if (missing(options) || is.null(options)) options <- list() else options <- as.list(options)
    if (!missing(tasks) && length(tasks) > 0) options[["mlm:tasks"]] <- as.list(tasks)
    
    result <- list()
    tmp <- shared_dir
    
    message("data class: ", paste(class(data), collapse = ", "))
    
    if (is.character(data) && length(data) == 1 && file.exists(data) && grepl("\\.pt$", data, ignore.case = TRUE)) {
      `%||%` <- function(a,b) if (!is.null(a) && length(a) > 0) a else b
      nz1    <- function(x) if (is.null(x) || length(x) == 0) NULL else x
      is_bad_dim <- function(x) { is.null(x) || length(x) != 1L || !is.finite(x) || x < 1 }
      
      model <- torch::torch_load(data)

      # --- EARLY: TempCNN-Erkennung (bevor wir model$input_channels etc. anfassen) ---
      has_conv <- tryCatch(
        !is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
          length(model$conv_layers[[1]]) >= 1 &&
          !is.null(model$conv_layers[[1]][[1]]$in_channels),
        error = function(e) FALSE
      )
      if (has_conv) {
        first_conv     <- model$conv_layers[[1]][[1]]
        input_channels <- first_conv$in_channels
        time_steps     <- model$time_steps
        message(" → input_channels = ", input_channels,
                ", time_steps = ", time_steps)
        
       
        pt_meta <- save_torch_model(
          model,
          file.path(shared_dir, paste0(base_name, ".pt")),
          input_channels = input_channels,
          time_steps     = time_steps
        )
      }
      else {
        # <<< NEU: LightTAE/sonstige DL-Modelle hier sauber behandeln
        pt_meta <- save_torch_model(
          model,
          file.path(shared_dir, paste0(base_name, ".pt"))
        )
      }
      
      # 3) ONNX-Export (Layout durchreichen)
      result$onnx <- convert_torch_to_onnx_from_pt(
        script_pt    = pt_meta$pt_path,
        input_chan   = pt_meta$input_chan,
        time_steps   = pt_meta$time_steps,
        base_name    = base_name,
        output_dir   = shared_dir
        )
      
      json_file <- file.path(shared_dir, ensure_extension(base_name, "json"))
      save_model_as_mlm_stac_json_dl(model, json_file, tasks, options)
      result$json <- json_file
      
      
    } 
    
    else if (inherits(data, "train")) {
      # Caret-Modelle
      message("Machine model detected...")
      if (inherits(data$finalModel, "ksvm")) {
        message("Detected SVM (kernlab::ksvm) → custom ONNX path.")
        ex <- export_caret_ksvm_to_onnx(
          train_obj      = data,
          out_base       = file.path(tmp, base_name),
          use_rule       = "majority",
          primary_output = "idx1",
          dtype          = "float32",
          do_checks      = TRUE
        )
        result$onnx <- ex$onnx
      } else {
        model_type <- detect_model_type(data)
        message("Detected model type: ", model_type)
        convert_model_to_pkl(data, model_type, file.path(tmp, base_name))
        message("Convert done!")
        onnx_path <- save_ml_model_as_onnx(model_type, file.path(tmp, base_name))
        result$onnx <- onnx_path
      }
      json_file <- file.path(tmp, ensure_extension(base_name, "json"))
      save_model_as_mlm_stac_json(data, json_file, tasks, options)
      result$json <- json_file
      
    } else {
      stop("Unknown model type: must be 'nn_module' (Torch) or 'train' (Caret).")
    }
    
    message("rds path")
    rds_path <- file.path(tmp, ensure_extension(base_name, "rds"))
    message("rds_path: ", rds_path)
    result$rds <- rds_path
    
    if (length(options) > 0 && !is.null(result$onnx) && file.exists(result$onnx)) {
      add_metadata_to_onnx(result$onnx, options)
    }
    
    download_base <- Sys.getenv("DOWNLOAD_BASE_URL", "http://localhost:8000/download/")
    download_links <- list(
      onnx  = sprintf("%s%s", download_base, basename(result$onnx)),
      json  = sprintf("%s%s", download_base, basename(result$json)),
      rds   = sprintf("%s%s", download_base, basename(result$rds))
    )
    if (!is.null(result$torch)) {
      download_links$torch <- sprintf("%s%s", download_base, basename(result$torch))
    }
    message("Model exported successfully (Download-Links):")
    message("- ONNX: ", download_links$onnx)
    message("- JSON (MLM-STAC): ", download_links$json)
    message("- RDS: ", download_links$rds)
    if (!is.null(download_links$torch)) message("- TorchScript: ", download_links$torch)
    
    return(result)
  }
)


#######################################################
#' Download a file from the shared temporary directory
#'
#' @description
#' Serves a file stored in the shared temporary directory as an HTTP download.
#' Checks that the filename is provided and exists, sets appropriate headers
#' (including Content-Type and Content-Disposition), and returns the file's binary
#' content or an error status.
#'
#' @param filename Character. Name of the file to download (relative to `SHARED_TEMP_DIR`).
#' @param res Response. Plumber response object that will be modified with status,
#'            headers, and body.
#'
#' @return
#' The modified `res` object containing either:
#' - On success: HTTP 200 status, download headers, and raw binary body of the file.
#' - On error: HTTP 400 or 404 status and a list with an `error` message.
#'
download <- function(filename, res) {
  if (is.null(filename) || filename == "") {
    res$status <- 400
    return(list(error = "No file name specified"))
  }
  
  shared_dir <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
  file_path <- file.path(shared_dir, filename)
  
  if (!file.exists(file_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  ext <- tools::file_ext(filename)
  content_type <- switch(
    ext,
    "json" = "application/json",
    "rds" = "application/octet-stream",
    "onnx" = "application/octet-stream",
    "pt"   = "application/octet-stream",
    "pkl"  = "application/octet-stream",
    "bin"  = "application/octet-stream",
    "txt"  = "text/plain",
    "application/octet-stream"
  )
  
  res$setHeader("Content-Type", content_type)
  res$setHeader("Content-Disposition", sprintf('attachment; filename="%s"', filename))
  
  file_content <- readBin(file_path, "raw", n = file.info(file_path)$size)
  res$body <- file_content
  
  return(res)
}
