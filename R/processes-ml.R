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
#' @param target_column Character. Name of the column in `training_set` that contains the target variable.
#'
#' @return
#' A trained model object of subtype "mlm-model", ready for predictions.
#'
ml_fit <- Process$new(
  id = "ml_fit",
  description = "Trains a Machine Learning or Deep Learning model based on input training data.",
  categories = as.array("machine-learning", "model-training"),
  summary = "Train an ML/DL model on extracted features.",
  
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
      name = "target_column",
      description = "The column containing the target variable",
      schema = list(type = "string")
    )
  ),
  
  returns = list(
    description = "Trained model",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(model, training_set, target_column, job) {
    
    ############# help functions ##############
    extract_time_series_features <- function(training_set, features_data, time_steps) {
      if (time_steps > 1) {
        message("multi time steps")
        features <- array(
          data = as.matrix(training_set[, grep("_T\\d+$", colnames(training_set))]),
          dim = c(nrow(training_set), length(features_data), time_steps)
        )
      } else {
        message("one time step")
        features <- array(
          data = as.matrix(training_set[, features_data]),
          dim = c(nrow(training_set), length(features_data), time_steps)
        )
      }
      return(features)
    }
    
    
    
    
    identify_predictors <- function(training_set, pattern = "^(B\\d+|(?i)NDVI(_T\\d+)?)$") {
      predictor_names <- colnames(training_set)
      predictor_names <- predictor_names[
        grepl(pattern, predictor_names) &
          sapply(training_set[, predictor_names, drop = FALSE], is.numeric)
      ]
      if (length(predictor_names) == 0) {
        stop("No valid predictors detected. Please check.")
      }
      return(predictor_names)
    }
    
    
    convert_to_wide_format <- function(train_data, target_column) {
      library(tidyr)
      library(dplyr)
      library(sf)
      
      band_names <- grep("^B0?\\d{1,2}$", names(train_data), value = TRUE)
      has_ndvi <- "NDVI" %in% colnames(train_data)
      bands_to_use <- c(band_names, if (has_ndvi) "NDVI")
      message("Found bands: ", paste(bands_to_use, collapse = ", "))
      
      # Drop Geometry + erzwinge reines DataFrame
      train_data <- train_data %>%
        sf::st_drop_geometry() %>%
        dplyr::as_tibble()
      
      train_data_wide <- train_data %>%
        select(fid, time, all_of(bands_to_use)) %>%
        pivot_wider(
          names_from = time,
          values_from = all_of(bands_to_use),
          names_glue = "{.value}_T{match(time, sort(unique(time)))}"
        )
      
      time_order <- sort(unique(train_data$time))
      cols_sorted <- unlist(lapply(seq_along(time_order), function(i) {
        paste0(bands_to_use, "_T", i)
      }))
      cols_sorted <- c("fid", cols_sorted)
      train_data_wide <- train_data_wide[, cols_sorted]
      
      train_data_clean <- train_data_wide %>%
        filter(complete.cases(.))
      
      target_data <- train_data %>%
        select(fid, !!sym(target_column)) %>%
        distinct(fid, .keep_all = TRUE)
      
      train_data_clean <- left_join(train_data_clean, target_data, by = "fid")
      
      message("Wide format done")
      return(train_data_clean)
    }
    
    
    
    
    
    ##################################################################################
    message("ml_fit is being prepared...")
    
    if (!is.null(model$parameters$cnn_layer)) {
      message("Deep learning model recognized. Start DL calculation...")
      
      time_steps <- length(unique(training_set$time))
      if (any(grepl("^X\\d+\\.", names(training_set)))) {
        names(training_set) <- gsub("^X\\d+\\.", "", names(training_set))
      }
      message("Detection of multiple time steps: ",time_steps)
      
      if (!"fid" %in% colnames(training_set)) {
        stop("The column 'fid' is missing in training_set. Please check your input data or preprocessing.")
      }
      
      fid_counts <- table(training_set$fid)
      n_samples <- length(unique(training_set$fid))
      message(n_samples)
      valid_fids <- as.integer(names(fid_counts[fid_counts == time_steps]))
      
      training_set <- training_set[training_set$fid %in% valid_fids, ]
      
      training_set <- training_set[order(training_set$fid, training_set$time), ]
      
      features_data <- grep("^B0?\\d{1,2}$", names(training_set), value = TRUE)
      if ("NDVI" %in% names(training_set)) {
        features_data <- c(features_data, "NDVI")
      }
      message(features_data)
      training_data <- convert_to_wide_format(train_data = training_set, target_column = target_column)
      features <- extract_time_series_features(training_set = training_data,features_data = features_data,time_steps = time_steps  )
      


      labels <- as.numeric(as.factor(training_data[[target_column]]))
      
      library(torch)
      x_train <- torch::torch_tensor(features, dtype = torch_float())
      y_train <- torch::torch_tensor(labels, dtype = torch_long())
        
      class_count <- length(unique(labels))
      message("Anzahl Klassen: ", class_count)
      dl_model <- model$create_model(
        input_data_columns = features_data,
        time_steps = time_steps,
        class_count = class_count
      )
        
      optimizer <- optim_adam(
        dl_model$parameters,
        lr = model$parameters$learning_rate,
        weight_decay = model$parameters$weight_decay
      )
      loss_fn <- nn_cross_entropy_loss()
        
      for (epoch in 1:model$parameters$epochs) {
        dl_model$train()
        optimizer$zero_grad()
        predictions <- dl_model(x_train)
        loss <- loss_fn(predictions, y_train)
        loss$backward()
        optimizer$step()
        print(sprintf("Epoch: %d, Loss: %.4f", epoch, loss$item()))
      }
        
      dl_model$eval()
      with_no_grad({
        predictions <- dl_model(x_train)
        predicted_classes <- torch_argmax(predictions, dim = 2)
        accuracy <- mean(as.numeric(predicted_classes == y_train))
        message(sprintf("Accuracy: %.2f%%", accuracy * 100))
      })
        
      confusion_matrix <- table(
        Predicted = as.numeric(predicted_classes),
        Actual = as.numeric(y_train)
      )
      message("Confusion Matrix:")
      print(confusion_matrix)
      model_file <- tempfile(fileext = ".pt")
      torch_save(dl_model, model_file)
      message("Model saved in Torch file: ", model_file)
        
      return(model_file)
      }
    
    
    message("Machine learning model recognized. Start ML calculation...")
    
    if (any(grepl("^X\\d+\\.", names(training_set)))) {
      names(training_set) <- gsub("^X\\d+\\.", "", names(training_set))
    }
    
    #### wide format########
    time_steps<- length(unique(training_set$time))
    if(time_steps > 1){
      message("Detection of multiple time steps: ", time_steps)
      
      training_set <- convert_to_wide_format(train_data = training_set, target_column = target_column)
      
    }else{
      message("Recognizing a time step")
      band_names <- grep("^B0?\\d{1,2}$", names(training_set), value = TRUE)
      has_ndvi <- "NDVI" %in% colnames(training_set)
      predictors_name <- c(band_names, if (has_ndvi) "NDVI")
      
      
      train_ids <- caret::createDataPartition(training_set$fid, p = 1.0, list = FALSE)
      train_data <- training_set[train_ids, ]
      train_data <- train_data %>% sf::st_drop_geometry()
      
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
    
    y <- training_set[[target_column]]
    is_classification <- !is.null(model$classification) && isTRUE(model$classification)
    
    if (is_classification) {
      if (!is.factor(y)) {
        message("Classification is carried out...")
        message("Numerical target variable is converted into a factor.")
        training_set[[target_column]] <- as.factor(y)
      } else {
        message("Classification is carried out...")
      }
    } else {
      if (!is.numeric(y)) {
        message("Regression is carried out...")
        message("Factor target variable is converted into numeric.")
        training_set[[target_column]] <- as.numeric(y)
      } else {
        message("Regression is carried out...")
      }
    }
    
    
    
    ##############################
    
    predictor_names <- identify_predictors(training_set)
    
    
    if (length(predictor_names) == 0) {
      stop("No valid predictors detected. Please check.")
    }
    message("Automatically recognized predictors: ", paste(predictor_names, collapse = ", "))
    
    x <- as.data.frame(lapply(training_set[, predictor_names, drop = FALSE], as.numeric))
    
    if (ncol(x) == 0) {
      stop("No predictors detected.")
    }
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
      } else if (model$method %in% c("svmRadial", "svmLinear", "svmPoly")) {
        if (is.null(model$tuneGrid)) {
          stop("SVM models require a defined tuneGrid")
        }
        model$preProcess <- c("center", "scale")
      } else if (model$method == "xgbTree") {
        if (is.null(model$tuneGrid)) {
          stop("XGBoost model requires a defined tuneGrid")
        }
        if (is.null(model$trControl)) {
          model$trControl <- caret::trainControl(method = "cv", number = 5, search = "grid")
        }
      } else {
        stop("Undetected method! You can only work with: 'rf', 'svmRadial', 'svmLinear', 'svmPoly', 'xgbTree'.")
      }
      
      model <- caret::train(
        x = x,
        y = y,
        method = model$method,
        tuneGrid = model$tuneGrid,
        trControl = model$trControl,
        ntree = if (model$method == "rf") model$ntree else NULL,
        preProcess = if (model$method %in% c("svmRadial", "svmLinear", "svmPoly")) model$preProcess else NULL
      )
    }, error = function(e) {
      message("An error has occurred: ", e$message)
      traceback()  
    })
    
    
    if (!is.numeric(y)) {
      if ("Accuracy" %in% colnames(model$results)) {
        accuracy <- max(model$results$Accuracy, na.rm = TRUE)
        message("Accuracy: ", round(accuracy * 100, 2), "%")
      }
    }else {
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
      name = "cube",
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
  operation = function(cube, model, job) {
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
    
    
    time_steps_query <- function(cube){
      time_steps <- gdalcubes::dimension_values(cube)
      time_steps <- time_steps$t
      return(time_steps)
    }
    
    #######################################
    time <- time_steps_query(cube)
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
    
    band_info <- gdalcubes::bands(cube)
    band_names <- band_info$name
    cube_dimensions <- gdalcubes::dimensions(cube)
    time_count <- cube_dimensions$t$count
    multi_timesteps <- time_count > 1
    input_channels <- length(band_names)
    
    if (is.character(model) && endsWith(model, ".onnx")) {
      if (multi_timesteps) {
        message("ONNX model detected – multi time steps")
        return(mlm_multi_onnx(cube, model))
      } else {
        message("ONNX model detected – single time step")
        return(mlm_single_onnx(cube, model))
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
      prediction <- mlm_multi(cube, model)
      message("prediction successful")
      return(prediction)
    } else {
      message("Only one time step")
      prediction <- mlm_single(cube, model)
      return(prediction)
    }
  }
)

########################################################
##### mlm_svm_process #####
#' Support Vector Machine for classification or regression
#'
#' @description
#' Model case for support vector machines. Creates a parameter list and trainControl
#' settings for SVM models (classification or regression) with specified kernel and hyperparameters.
#'
#' @param kernel Character. Kernel type to use: `'radial'`, `'linear'`, or `'polynomial'`.
#' @param C Numeric. Regularization parameter (cost).
#' @param sigma Numeric, optional. Kernel coefficient (σ) for the radial basis function kernel.
#' @param gamma Numeric, optional. Kernel coefficient (γ) for the polynomial kernel (also scales the polynomial).
#' @param degree Integer, optional. Degree of the polynomial kernel (default is 3).
#' @param coef0 Numeric, optional. Independent term in polynomial kernel (default is 0).
#' @param random_state Integer, optional. Random seed for reproducibility.
#' @param classification Logical. If `TRUE`, sets up SVM for classification; if `FALSE`, for regression.
#'
#' @return
#' A list of model parameters, including:
#' - `method`: the caret model string (`"svmRadial"`, `"svmLinear"`, or `"svmPoly"`)
#' - `tuneGrid`: a data.frame with the tuning grid
#' - `trControl`: a `trainControl` object with 5-fold cross-validation
#' - `seed`: the random seed (if provided)
#'
#' @examples
#' # Classification with radial kernel
#' params <- mlm_class_svm(kernel = "radial", C = 1, sigma = 0.1)
#'
#' # Regression with polynomial kernel
#' params <- mlm_regr_svm_(kernel = "polynomial", C = 0.5, degree = 4, gamma = 0.01, coef0 = 1)
#'

#### help-function
mlm_svm_envelope <- function(kernel,
                             C,
                             sigma = NULL,
                             gamma = NULL,
                             degree = 3,
                             coef0 = 0,
                             random_state = NULL,
                             classification) {
  
  if (kernel == "radial") {
    tuneGrid <- expand.grid(C = C, sigma = sigma)
    method <- "svmRadial"
  } else if (kernel == "linear") {
    tuneGrid <- expand.grid(C = C)
    method <- "svmLinear"
  } else if (kernel == "polynomial") {
    tuneGrid <- expand.grid(C = C, degree = degree, scale = gamma, coef0 = coef0)
    method <- "svmPoly"
  } else {
    stop("Unsupported kernel type.")
  }
  
  return(list(
    method = method,
    tuneGrid = tuneGrid,
    trControl = caret::trainControl(method = "cv", number = 5),
    seed = random_state,
    classification = classification
  ))
}


mlm_class_svm <- Process$new(
  id = "mlm_class_svm",
  description = "Support Vector Machine for classification.",
  categories = c("machine-learning"),
  summary = "Support Vector Machine for classification with radial, linear, or polynomial kernel.",
  parameters = list(
    Parameter$new("kernel", "Kernel type ('radial', 'linear', 'polynomial')", list(type = "string")),
    Parameter$new("C", "Regularization parameter", list(type = "number")),
    Parameter$new("sigma", "Kernel coefficient for radial basis function", list(type = "number"), optional = TRUE),
    Parameter$new("gamma", "Kernel coefficient for polynomial kernel", list(type = "number"), optional = TRUE),
    Parameter$new("degree", "Degree for polynomial kernel", list(type = "integer"), optional = TRUE),
    Parameter$new("coef0", "Independent term in polynomial kernel", list(type = "number"), optional = TRUE),
    Parameter$new("random_state", "Random seed for reproducibility", list(type = "integer"), optional = TRUE)
  ),
  returns = list(
    description = "Model parameters for SVM classification",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(kernel, C, sigma = NULL, gamma = NULL, degree = 3,
                       coef0 = 0, random_state = NULL, job) {
    message("Creating SVM classification model.")
    mlm_svm_envelope(
      kernel = kernel,
      C = C,
      sigma = sigma,
      gamma = gamma,
      degree = degree,
      coef0 = coef0,
      random_state = random_state,
      classification = TRUE
    )
  }
)

mlm_regr_svm <- Process$new(
  id = "mlm_regr_svm",
  description = "Support Vector Machine for regression.",
  categories = c("machine-learning"),
  summary = "Support Vector Machine for regression with radial, linear, or polynomial kernel.",
  parameters = list(
    Parameter$new("kernel", "Kernel type ('radial', 'linear', 'polynomial')", list(type = "string")),
    Parameter$new("C", "Regularization parameter", list(type = "number")),
    Parameter$new("sigma", "Kernel coefficient for radial basis function", list(type = "number"), optional = TRUE),
    Parameter$new("gamma", "Kernel coefficient for polynomial kernel", list(type = "number"), optional = TRUE),
    Parameter$new("degree", "Degree for polynomial kernel", list(type = "integer"), optional = TRUE),
    Parameter$new("coef0", "Independent term in polynomial kernel", list(type = "number"), optional = TRUE),
    Parameter$new("random_state", "Random seed for reproducibility", list(type = "integer"), optional = TRUE)
  ),
  returns = list(
    description = "Model parameters for SVM regression",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(kernel, C, sigma = NULL, gamma = NULL, degree = 3,
                       coef0 = 0, random_state = NULL, job) {
    message("Creating SVM regression model.")
    mlm_svm_envelope(
      kernel = kernel,
      C = C,
      sigma = sigma,
      gamma = gamma,
      degree = degree,
      coef0 = coef0,
      random_state = random_state,
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
#' @param num_trees Integer. Number of trees in the forest.
#' @param min_samples_split Integer. Minimum number of observations required to split an internal node.
#' @param min_samples_leaf Integer. Minimum number of observations required to be at a leaf node.
#' @param max_features String. Number of features to consider when looking for the best split (e.g., `"sqrt"` or a numeric value).
#' @param random_state Integer, optional. Random seed for reproducibility.
#' @param classification Logical. If `TRUE`, sets up Random Forest for classification; if `FALSE`, for regression.
#'
#' @return
#' A list of model parameters for caret, including:
#' - `method`: `"rf"`
#' - `tuneGrid`: a `data.frame` with the `mtry` value
#' - `trControl`: a `trainControl` object with 5-fold cross-validation
#' - `ntree`: number of trees
#' - `seed`: the random seed (if provided)
#' - `min_samples_split` and `min_samples_leaf`: node splitting criteria
#'
#' @examples
#' # Classification with 100 trees, sqrt(max_features)
#' params <- mlm_class_random_forest(num_trees = 100,
#'                             min_samples_split = 2,
#'                             min_samples_leaf = 1,
#'                             max_features = "sqrt",
#'                             classification = TRUE)
#'
#' # Regression with 200 trees, 5 features per split
#' params <- mlm_regr_random_forest(num_trees = 200,
#'                             min_samples_split = 5,
#'                             min_samples_leaf = 2,
#'                             max_features = "5",
#'                             random_state = 42,
#'                             classification = FALSE)
#'

### help function ###
mlm_random_forest_envelope <- function(num_trees,
                                       min_samples_split,
                                       min_samples_leaf,
                                       max_features,
                                       random_state = NULL,
                                       classification) {
  
  mtry_value <- if (max_features == "sqrt") NA else as.numeric(max_features)
  
  model_params <- list(
    method = "rf",
    tuneGrid = expand.grid(mtry = mtry_value),
    trControl = caret::trainControl(method = "cv", number = 5),
    ntree = num_trees,
    seed = random_state,
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
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
    Parameter$new("num_trees", "Number of trees in the forest.", list(type = "integer")),
    Parameter$new("min_samples_split", "Minimum samples required to split an internal node.", list(type = "integer")),
    Parameter$new("min_samples_leaf", "Minimum samples required to be at a leaf node.", list(type = "integer")),
    Parameter$new("max_features", "Features to consider when looking for best split ('sqrt' or a number).", list(type = "string")),
    Parameter$new("random_state", "Random seed for reproducibility.", list(type = "integer"), optional = TRUE)
  ),
  returns = list(
    description = "Model parameters for Random Forest classification.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(num_trees,
                       min_samples_split,
                       min_samples_leaf,
                       max_features,
                       random_state = NULL,
                       job) {
    message("Creating Random Forest classification model.")
    mlm_random_forest_envelope(
      num_trees = num_trees,
      min_samples_split = min_samples_split,
      min_samples_leaf = min_samples_leaf,
      max_features = max_features,
      random_state = random_state,
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
    Parameter$new("num_trees", "Number of trees in the forest.", list(type = "integer")),
    Parameter$new("min_samples_split", "Minimum samples required to split an internal node.", list(type = "integer")),
    Parameter$new("min_samples_leaf", "Minimum samples required to be at a leaf node.", list(type = "integer")),
    Parameter$new("max_features", "Features to consider when looking for best split ('sqrt' or a number).", list(type = "string")),
    Parameter$new("random_state", "Random seed for reproducibility.", list(type = "integer"), optional = TRUE)
  ),
  returns = list(
    description = "Model parameters for Random Forest regression.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(num_trees,
                       min_samples_split,
                       min_samples_leaf,
                       max_features,
                       random_state = NULL,
                       job) {
    message("Creating Random Forest regression model.")
    mlm_random_forest_envelope(
      num_trees = num_trees,
      min_samples_split = min_samples_split,
      min_samples_leaf = min_samples_leaf,
      max_features = max_features,
      random_state = random_state,
      classification = FALSE
    )
  }
)


########################################################
#' Extreme Gradient Boosting for classification or regression
#'
#' @description
#' Model case for XGBoost. Creates a parameter list and trainControl settings
#' for XGBoost models (classification or regression) with specified hyperparameters.
#'
#' @param learning_rate Numeric. Step size shrinkage (eta) to prevent overfitting.
#' @param max_depth Integer. Maximum depth of each tree.
#' @param min_child_weight Numeric. Minimum sum of instance weight (hessian) needed in a child.
#' @param subsample Numeric. Subsample ratio of the training instances.
#' @param colsample_bytree Numeric. Subsample ratio of columns when constructing each tree.
#' @param gamma Numeric. Minimum loss reduction required to make a further partition.
#' @param nrounds Integer. Number of boosting iterations.
#' @param random_state Integer, optional. Random seed for reproducibility.
#' @param classification Logical. If `TRUE`, sets up XGBoost for classification; if `FALSE`, for regression.
#'
#' @return
#' A list of model parameters for caret's `xgbTree` method, including:
#' - `method`: `"xgbTree"`
#' - `tuneGrid`: a `data.frame` of hyperparameters (`nrounds`, `max_depth`, `eta`, `gamma`, `colsample_bytree`, `min_child_weight`, `subsample`)
#' - `trControl`: a `trainControl` object with 5-fold cross-validation and grid search
#' - `random_state`: the random seed (if provided)
#'
#' @examples
#' # Classification example
#' params <- mlm_class_xgboost(
#'   learning_rate    = 0.1,
#'   max_depth        = 6,
#'   min_child_weight = 1,
#'   subsample        = 0.8,
#'   colsample_bytree = 0.8,
#'   gamma            = 0,
#'   nrounds          = 100,
#'   classification   = TRUE
#' )
#'
#' # Regression example
#' params <- mlm_regr_xgboost(
#'   learning_rate    = 0.05,
#'   max_depth        = 4,
#'   min_child_weight = 3,
#'   subsample        = 0.7,
#'   colsample_bytree = 0.7,
#'   gamma            = 1,
#'   nrounds          = 200,
#'   random_state     = 42,
#'   classification   = FALSE
#' )
#' 

### help function ###
mlm_xgboost_envelope <- function(learning_rate,
                                 max_depth,
                                 min_child_weight,
                                 subsample,
                                 colsample_bytree,
                                 gamma,
                                 nrounds,
                                 random_state = NULL,
                                 classification) {
  list(
    method = "xgbTree",
    tuneGrid = expand.grid(
      nrounds = nrounds,
      max_depth = max_depth,
      eta = learning_rate,
      gamma = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample = subsample
    ),
    trControl = caret::trainControl(
      method = "cv",
      number = 5,
      search = "grid"
    ),
    random_state = random_state,
    classification = classification
  )
}
#### Process

mlm_class_xgboost <- Process$new(
  id = "mlm_class_xgboost",
  description = "XGBoost for classification.",
  categories = c("machine-learning"),
  summary = "Extreme Gradient Boosting for classification with specified hyperparameters.",
  parameters = list(
    Parameter$new("learning_rate", "Step size shrinkage (eta) to prevent overfitting.", list(type = "number")),
    Parameter$new("max_depth", "Maximum depth of each tree.", list(type = "integer")),
    Parameter$new("min_child_weight", "Minimum sum of instance weight needed in a child.", list(type = "number")),
    Parameter$new("subsample", "Subsample ratio of training instances.", list(type = "number")),
    Parameter$new("colsample_bytree", "Subsample ratio of columns when constructing each tree.", list(type = "number")),
    Parameter$new("gamma", "Minimum loss reduction required to make a further partition.", list(type = "number")),
    Parameter$new("nrounds", "Number of boosting iterations.", list(type = "integer")),
    Parameter$new("random_state", "Random seed for reproducibility.", list(type = "integer"), optional = TRUE)
  ),
  returns = list(
    description = "Model parameters for XGBoost classification.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(learning_rate,
                       max_depth,
                       min_child_weight,
                       subsample,
                       colsample_bytree,
                       gamma,
                       nrounds,
                       random_state = NULL,
                       job) {
    message("Creating XGBoost classification model.")
    mlm_xgboost_envelope(
      learning_rate = learning_rate,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      gamma = gamma,
      nrounds = nrounds,
      random_state = random_state,
      classification = TRUE
    )
  }
)

mlm_regr_xgboost <- Process$new(
  id = "mlm_regr_xgboost",
  description = "XGBoost for regression.",
  categories = c("machine-learning"),
  summary = "Extreme Gradient Boosting for regression with specified hyperparameters.",
  parameters = list(
    Parameter$new("learning_rate", "Step size shrinkage (eta) to prevent overfitting.", list(type = "number")),
    Parameter$new("max_depth", "Maximum depth of each tree.", list(type = "integer")),
    Parameter$new("min_child_weight", "Minimum sum of instance weight needed in a child.", list(type = "number")),
    Parameter$new("subsample", "Subsample ratio of training instances.", list(type = "number")),
    Parameter$new("colsample_bytree", "Subsample ratio of columns when constructing each tree.", list(type = "number")),
    Parameter$new("gamma", "Minimum loss reduction required to make a further partition.", list(type = "number")),
    Parameter$new("nrounds", "Number of boosting iterations.", list(type = "integer")),
    Parameter$new("random_state", "Random seed for reproducibility.", list(type = "integer"), optional = TRUE)
  ),
  returns = list(
    description = "Model parameters for XGBoost regression.",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(learning_rate,
                       max_depth,
                       min_child_weight,
                       subsample,
                       colsample_bytree,
                       gamma,
                       nrounds,
                       random_state = NULL,
                       job) {
    message("Creating XGBoost regression model.")
    mlm_xgboost_envelope(
      learning_rate = learning_rate,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      gamma = gamma,
      nrounds = nrounds,
      random_state = random_state,
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
#' @param cnn_layer Integer vector. Number of filters in each convolutional layer.
#' @param cnn_kernels Integer vector. Kernel sizes for each convolutional layer.
#' @param cnn_dropout_rates Numeric vector. Dropout rates for each convolutional layer.
#' @param dense_layer_nodes Integer. Number of neurons in the dense (fully-connected) layer.
#' @param dense_layer_dropout_rate Numeric. Dropout rate for the dense layer.
#' @param optimizer Character. Name of the optimizer (e.g., `"adam"`).
#' @param learning_rate Numeric. Learning rate for the optimizer.
#' @param epsilon Numeric. Epsilon value for numerical stability in the optimizer.
#' @param weight_decay Numeric. Weight decay (L2 regularization) coefficient.
#' @param lr_decay_epochs Integer. Number of epochs after which to decay the learning rate.
#' @param lr_decay_rate Numeric. Factor by which to decay the learning rate.
#' @param epochs Integer. Total number of training epochs.
#' @param batch_size Integer. Batch size for training.
#' @param random_state Integer, optional. Random seed for reproducibility.
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
      name = "cnn_layer",
      description = "Array: Number of filters per convolutional layer",
      schema = list(type = "array", items = list(type = "integer"))
    ),
    Parameter$new(
      name = "cnn_kernels",
      description = "Array: Kernel sizes for the convolutional layers",
      schema = list(type = "array", items = list(type = "integer"))
    ),
    Parameter$new(
      name = "cnn_dropout_rates",
      description = "Array: Dropout rates for the convolutional layers",
      schema = list(type = "array", items = list(type = "number"))
    ),
    Parameter$new(
      name = "dense_layer_nodes",
      description = "Number of neurons in the dense layer",
      schema = list(type = "integer")
    ),
    Parameter$new(
      name = "dense_layer_dropout_rate",
      description = "Dropout rate for the dense layer",
      schema = list(type = "number")
    ),
    Parameter$new(
      name = "optimizer",
      description = "Optimizer name (e.g. 'adam')",
      schema = list(type = "string")
    ),
    Parameter$new(
      name = "learning_rate",
      description = "Learning rate for the optimizer",
      schema = list(type = "number")
    ),
    Parameter$new(
      name = "epsilon",
      description = "Epsilon value for the optimizer",
      schema = list(type = "number")
    ),
    Parameter$new(
      name = "weight_decay",
      description = "Weight loss (regularization)",
      schema = list(type = "number")
    ),
    Parameter$new(
      name = "lr_decay_epochs",
      description = "Number of epochs after which the learning rate decreases",
      schema = list(type = "integer")
    ),
    Parameter$new(
      name = "lr_decay_rate",
      description = "Rate of learning rate decay",
      schema = list(type = "number")
    ),
    Parameter$new(
      name = "epochs",
      description = "Number of training epochs",
      schema = list(type = "integer")
    ),
    Parameter$new(
      name = "batch_size",
      description = "Batch size for training",
      schema = list(type = "integer")
    ),
    Parameter$new(
      name = "random_state",
      description = "Random generator for reproducibility",
      schema = list(type = "integer"),
      optional = TRUE
    ), 
    Parameter$new(
      name = "Classification",
      description = "TRUE for classification; FALSE for regression",
      schema = list(type = "boolean")
    )
  ),
  returns = list(
    description = "Model parameters as a list for the TempCNN model",
    schema = list(type = "object", subtype = "mlm-model")
  ),
  operation = function(cnn_layer = cnn_layer,
                       cnn_kernels = cnn_kernels,
                       cnn_dropout_rates = cnn_dropout_rates,
                       dense_layer_nodes = dense_layer_nodes,
                       dense_layer_dropout_rate = dense_layer_dropout_rate,
                       optimizer = optimizer,
                       learning_rate = learning_rate,
                       epsilon = epsilon,
                       weight_decay = weight_decay,
                       lr_decay_epochs = lr_decay_epochs,
                       lr_decay_rate = lr_decay_rate,
                       epochs = epochs,
                       batch_size = batch_size,
                       random_state = random_state, job) {
    
    
    model_parameter <- list(
      cnn_layer = cnn_layer,
      cnn_kernels = cnn_kernels,
      cnn_dropout_rates = cnn_dropout_rates,
      dense_layer_nodes = dense_layer_nodes,
      dense_layer_dropout_rate = dense_layer_dropout_rate,
      optimizer = optimizer,
      learning_rate = learning_rate,
      epsilon = epsilon,
      weight_decay = weight_decay,
      lr_decay_epochs = lr_decay_epochs,
      lr_decay_rate = lr_decay_rate,
      epochs = epochs,
      batch_size = batch_size,
      random_state = random_state
    )
    
    return(list(
      parameters = model_parameter,
      create_model = function(input_data_columns, time_steps, class_count) {
        dl_temp_cnn_dynamic <- nn_module(
          initialize = function(settings, input_data_columns, time_steps, class_count) {
            self$conv_layers <- nn_module_list()
            self$input_data_columns <- input_data_columns
            self$original_time_steps <- time_steps
            
            reduced_time_steps <- time_steps
            for (i in seq_along(settings$cnn_layer)) {
              input_channels <- ifelse(
                i == 1,
                length(self$input_data_columns),
                settings$cnn_layer[[i - 1]]
              )
              
              output_channels <- settings$cnn_layer[[i]]
              kernel_size <- settings$cnn_kernels[[i]]
              dropout_rate <- settings$cnn_dropout_rates[[i]]
              
              self$conv_layers$append(
                nn_sequential(
                  nn_conv1d(
                    in_channels = input_channels,
                    out_channels = output_channels,
                    kernel_size = kernel_size,
                    stride = 1,
                    padding = kernel_size %/% 2
                  ),
                  nn_batch_norm1d(output_channels),
                  nn_relu(),
                  nn_dropout(p = dropout_rate)
                )
              )
              
              reduced_time_steps <- (reduced_time_steps + 2 * (kernel_size %/% 2) - kernel_size) + 1
            }
            
            self$time_steps <- reduced_time_steps
            self$flatten <- nn_flatten()
            self$dense <- nn_sequential(
              nn_linear(settings$cnn_layer[[length(settings$cnn_layer)]] * self$time_steps, settings$dense_layer_nodes),
              nn_relu(),
              nn_dropout(p = settings$dense_layer_dropout_rate),
              nn_linear(settings$dense_layer_nodes, class_count)
            )
          },
          
          forward = function(x) {
            for (i in seq_along(self$conv_layers)) {
              x <- self$conv_layers[[i]](x)
            }
            x <- self$flatten(x)
            x <- self$dense(x)
            return(x)
          }
        )
        return(dl_temp_cnn_dynamic(
          settings = model_parameter,
          input_data_columns = input_data_columns,
          time_steps = time_steps,
          class_count = class_count
        ))
      }
    ))
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
    # Ensure Python environment for ONNX export etc.
    # ensure_python_env(required_modules = c("torch", "onnxruntime", "numpy", "joblib", "scikit-learn", "xgboost", "onnxmltools", "skl2onnx"))
    message("Save model is started...")
    
    shared_dir <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
    message("shared_dir gesetzt: ", shared_dir)
    
    base_name <- name
    
    find_python_bin <- function() {
      bin_env <- Sys.getenv("PYTHON_BIN", Sys.getenv("RETICULATE_PYTHON", ""))
      if (nzchar(bin_env) && file.exists(bin_env)) {
        return(bin_env)
      }
      
      if (requireNamespace("reticulate", quietly = TRUE)) {
        pycfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
        if (!is.null(pycfg) && nzchar(pycfg$python) && file.exists(pycfg$python)) {
          return(pycfg$python)
        }
      }
      
      if (requireNamespace("reticulate", quietly = TRUE)) {
        try({
          envs <- reticulate::conda_list()
          for (nm in c("base", envs$name)) {
            py <- tryCatch(reticulate::conda_python(nm), error = function(e) "")
            if (nzchar(py) && file.exists(py)) return(py)
          }
        }, silent = TRUE)
      }
      
      py3 <- Sys.which("python3")
      if (nzchar(py3)) return(py3)
      py  <- Sys.which("python")
      if (nzchar(py))  return(py)
      
      stop("No suitable Python interpreter found!")
    }
    
    
    # Helper: add extension if missing
    ensure_extension <- function(filename, ext) {
      if (!grepl(paste0("\\.", ext, "$"), filename, ignore.case = TRUE)) {
        paste0(filename, ".", ext)
      } else {
        filename
      }
    }
    
    # Detect caret model type
    detect_model_type <- function(model) {
      if ("train" %in% class(model)) {
        label <- model$modelInfo$label
        # classify by label
        if (grepl("Random Forest", label, ignore.case = TRUE)) {
          "random_forest"
        } else if (grepl("Support Vector Machines", label, ignore.case = TRUE)) {
          if (grepl("Linear", label, ignore.case = TRUE)) "svmLinear" else if (grepl("Radial", label, ignore.case = TRUE)) "svmRadial" else if (grepl("Poly", label, ignore.case = TRUE)) "svmPoly" else "svmLinear"
        } else if (grepl("xgboost|Gradient Boosting|Xtreme", label, ignore.case = TRUE)) {
          "xgbTree"
        } else {
          stop("Model type could not be recognized from model$modelInfo$label: ", label)
        }
      } else {
        NULL
      }
    }
    
    
    save_torch_model <- function(model, filepath) {
      first_conv     <- model$conv_layers[[1]][[1]]
      input_channels <- first_conv$in_channels
      time_steps     <- model$time_steps
      message(" → input_channels = ", input_channels,
              ", time_steps = ", time_steps)
      
      B     <- 8L
      dummy <- torch::torch_randn(c(B, input_channels, time_steps))
      
      model$eval()
      script_model <- torch::jit_trace(model, dummy)
      script_model$eval()
      
      torch::jit_save(script_model, filepath)
      
      list(
        pt_path    = filepath,
        input_chan = input_channels,
        time_steps = time_steps
      )
    }
    
    
    convert_torch_to_onnx_from_pt <- function(
    script_pt,
    input_chan,
    time_steps,
    base_name,
    output_dir
    ) {
      message("Start ONNX conversion...")
      message(script_pt)
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
    dynamic_axes={
      'input' : {0: 'batch_size', 2: 'time_steps'},
      'output': {0: 'batch_size'}
    },
    opset_version=14
)
print('ONNX successfully saved: %s')
", script_pt, input_chan, time_steps, onnx_path, onnx_path),
      con = py_file
      )

python_bin <- find_python_bin()
res <- system2(python_bin, py_file, stdout = TRUE, stderr = TRUE)
cat(res, sep = "\n")
unlink(py_file)

if (!file.exists(onnx_path)) {
  stop("ONNX export failed:\n", paste(res, collapse = "\n"))
}
message("ONNX stored under: ", onnx_path)
return(onnx_path)
    }
    
    
    
    convert_model_to_pkl <- function(model, model_type, filepath) {
      library(reticulate)
      joblib <- import("joblib")
      np <- import("numpy")
      sklearn <- import("sklearn.ensemble")
      sklearn_svm <- import("sklearn.svm")
      xgboost <- import("xgboost")
      
      
      if (!("train" %in% class(model))) {
        stop("Please pass a caret train object")
      }
      
      if (!("trainingData" %in% names(model))) {
        stop("The caret model does not contain any trainingData. Please save the model with trainingData")
      }
      train_data <- model$trainingData
      
      if (!(".outcome" %in% colnames(train_data))) {
        stop("The trainingData does not contain an '.outcome' column")
      }
      predictors <- setdiff(colnames(train_data), ".outcome")
      target_column <- ".outcome"
      model <- model$finalModel
      train_data_clean <- train_data[, predictors, drop = FALSE]
      if (is.factor(train_data[[target_column]]) || is.character(train_data[[target_column]])) {
        train_data[[target_column]] <- as.integer(as.factor(train_data[[target_column]])) - 1
      }
      
      tryCatch({
        x_train <- np$array(as.matrix(train_data_clean))
        y_train <- np$array(as.numeric(train_data[[target_column]]))
      }, error = function(e) {
        message("Error during numpy array conversion: ", e$message)
        stop(e)
      })
      
      
      if (model_type == "random_forest") {
        if (!inherits(model, "randomForest")) {
          stop("Error: The model is not a RandomForest model")
        }
        rf_py_model <- sklearn$RandomForestClassifier(
          n_estimators = as.integer(model$ntree),
          max_features = as.integer(model$mtry)
        )
        rf_py_model$fit(x_train, y_train)
        joblib$dump(rf_py_model, paste0(filepath, ".pkl"))
      } else if (model_type %in% c("svmLinear", "svmRadial", "svmPoly")) {
        message("svm")
        if (!inherits(model, "ksvm") && !("svm" %in% class(model))) {
          stop("Error: The model is not an SVM model")
        }
        C_value <- tryCatch({
          if (!is.null(model@kernelf@kpar$C)) model@kernelf@kpar$C else 1.0
        }, error = function(e) {
          1.0
        })
        kernel <- if (model_type == "svmLinear") "linear" else "rbf"
        svm_py_model <- sklearn_svm$SVC(
          kernel = kernel,
          C = as.numeric(C_value)
        )
        svm_py_model$fit(x_train, y_train)
        joblib$dump(svm_py_model, paste0(filepath, ".pkl"))
      } else if (model_type == "xgbTree") {
        if (!inherits(model, "xgb.Booster")) {
          stop("Error: The model is not an XGBoost model")
        }
        xgboost::xgb.save(model, paste0(filepath, ".bin"))
      } else {
        stop("Model type is not supported!")
      }
    }
    
    add_metadata_to_onnx <- function(onnx_path, options) {
      onnx <- reticulate::import("onnx")
      model <- onnx$load_model(onnx_path)
      for (key in names(options)) {
        value <- as.character(options[[key]])
        meta_prop <- onnx$StringStringEntryProto(key = key, value = value)
        model$metadata_props$append(meta_prop)
      }
      onnx$save_model(model, onnx_path)
      message("Metadata has been added to the ONNX model.")
    }
    
    save_ml_model_as_onnx <- function(model_type, filepath) {
      library(reticulate)
      
      tryCatch({
        onnxmltools   <- import("onnxmltools")
        skl2onnx      <- import("skl2onnx")
        xgboost       <- import("xgboost")
        sklearn       <- import("sklearn.ensemble")
        sklearn_svm   <- import("sklearn.svm")
        onnx          <- import("onnx")
        joblib        <- import("joblib")
        FloatTensorType <- import("skl2onnx.common.data_types")$FloatTensorType
        
        n_features <- NULL
        
        if (model_type == "xgbTree") {
          xgb_mod <- import("xgboost")
          booster_py <- xgb_mod$Booster()
          booster_py$load_model(paste0(filepath, ".bin"))
          model_py   <- booster_py
          n_features <- booster_py$num_features()
        } else if (model_type == "random_forest") {
          rf_model_py <- joblib$load(paste0(filepath, ".pkl"))
          n_features <- rf_model_py$n_features_in_
          model_py <- rf_model_py
        } else if (model_type %in% c("svmLinear", "svmRadial", "svmPoly")) {
          svm_model_py <- joblib$load(paste0(filepath, ".pkl"))
          n_features <- tryCatch({
            svm_model_py$n_features_in_
          }, error = function(e) {
            message("Access to n_features_in_ failed:", e$message)
            NULL
          })
          model_py <- svm_model_py
        } else {
          stop("Model type is not supported!")
        }
        
        if (is.null(n_features)) {
          stop("n_features_in_ could not be determined. Please define initial_type manually!")
        }
        
        initial_type <- list(list("float_input", FloatTensorType(list(NULL, as.integer(n_features)))))
        
        if (model_type == "xgbTree") {
          onnx_model <- onnxmltools$convert_xgboost(model_py, initial_types = initial_type)
        } else {
          onnx_model <- skl2onnx$convert_sklearn(model_py, initial_types = initial_type)
        }
        
        onnx_file <- ensure_extension(filepath, "onnx")
        onnx$save_model(onnx_model, onnx_file)
        message("Model successfully saved as ONNX under: ", onnx_file)
        return(onnx_file)
        
      }, error = function(e) {
        message("Error in save_ml_model_as_onnx:", e$message)
        stop(e)
      })
    }
    
    
    save_model_as_mlm_stac_json <- function(model, filepath, tasks = list("classification"), options = list()) {
      mlm_stac_item <- list(
        type = "Feature",
        stac_version = "1.0.0",
        id = basename(sub("\\.json$", "", filepath)),
        properties = list(
          datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        ),
        geometry = NULL,
        bbox = NULL,
        stac_extensions = list(
          "https://stac-extensions.github.io/mlm/v1.0.0/schema.json"
        ),
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
        } else if (model_type %in% c("svmLinear", "svmRadial", "svmPoly")) {
          cost <- tryCatch(model$finalModel@kernelf@kpar$C, error = function(e) 1.0)
          nSV <- model$finalModel@nSV
          model_info$"mlm:total_parameters" <- nSV
          hyperparameters <- list(
            cost = cost,
            nSV = nSV,
            kernel = if (model_type == "svmLinear") "linear" else if (model_type == "svmRadial") "radial" else "poly"
          )
        } else if (model_type == "xgbTree") {
          nrounds <- model$finalModel$niter
          model_info$"mlm:total_parameters" <- nrounds
          hyperparameters <- list(
            max_depth = model$finalModel$tuneValue$max_depth,
            nrounds = nrounds,
            eta = model$finalModel$tuneValue$eta,
            gamma = model$finalModel$tuneValue$gamma
          )
        } else {
          hyperparameters <- list()
        }
        if (length(hyperparameters) > 0) model_info$"mlm:hyperparameters" <- hyperparameters
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- NULL
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        if (inherits(model$finalModel, "ksvm")) {
          predictors <- names(model$trainingData)[!names(model$trainingData) %in% ".outcome"]
        } else {
          predictors <- model$finalModel$xNames
        }
        model_info$"mlm:input" <- list(
          list(
            name = "Features",
            bands = predictors,
            input = list(
              shape = list(1, length(predictors)),
              dim_order = list("batch", "features"),
              data_type = "float32"
            ),
            description = "Input features for classification",
            pre_processing_function = NULL
          )
        )
        classes <- model$levels
        class_objects <- lapply(seq_along(classes), function(i) {
          list(
            value = i - 1,
            name = classes[i],
            description = paste("Class:", classes[i])
          )
        })
        model_info$"mlm:output" <- list(
          list(
            name = "CLASSIFICATION",
            tasks = tasks,
            result = list(
              shape = list(1, length(classes)),
              dim_order = list("batch", "classes"),
              data_type = "float32"
            ),
            description = "Predicted probabilities for classification",
            "classification:classes" = class_objects,
            post_processing_function = NULL
          )
        )
        if (length(options) > 0) {
          for (key in names(options)) {
            if (grepl("^mlm:", key)) {
              model_info[[key]] <- options[[key]]
            }
          }
        }
      } else {
        stop("Unknown model type: Please check the model!")
      }
      mlm_stac_item$properties <- c(mlm_stac_item$properties, model_info)
      rds_path <- ensure_extension(sub("\\.json$", "", filepath), "rds")
      saveRDS(model, file = rds_path)
      mlm_stac_item$assets <- list(
        model = list(
          href = rds_path,
          type = "application/octet-stream",
          title = paste(model_info$"mlm:architecture", "Model"),
          "mlm:artifact_type" = "R (RDS)",
          roles = list("mlm:model")
        )
      )
      jsonlite::write_json(mlm_stac_item, path = filepath, auto_unbox = TRUE, pretty = TRUE)
      message("Model was saved as MLM-STAC-JSON under: ", filepath)
      return(filepath)
    }
    
    
    save_model_as_mlm_stac_json_dl <- function(model, filepath, tasks = list("classification"), options = list()) {
      mlm_stac_item <- list(
        type = "Feature",
        stac_version = "1.0.0",
        id = basename(sub("\\.json$", "", filepath)),
        properties = list(
          datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        ),
        geometry = NULL,
        bbox = NULL,
        stac_extensions = list(
          "https://stac-extensions.github.io/mlm/v1.0.0/schema.json"
        ),
        assets = list()
      )
      model_info <- list()
      if ("nn_module" %in% class(model) || !is.null(model$conv_layers)) {
        model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
        model_info$"mlm:architecture" <- "TempCNN"
        model_info$"mlm:tasks" <- tasks
        model_info$"mlm:framework" <- "R (torch)"
        model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
        total_params <- sum(unlist(lapply(model$parameters, function(p) prod(dim(p)))))
        model_info$"mlm:total_parameters" <- as.integer(total_params)
        model_info$"mlm:hyperparameters" <- list(
          conv_layers = length(model$conv_layers),
          dense_layers = length(model$dense)
        )
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- "gpu"
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        input_channels <- if (!is.null(model$conv_layers[[1]])) model$conv_layers[[1]][[1]]$in_channels else NULL
        time_steps <- model$time_steps
        bands <- if (!is.null(model$input_data_columns)) model$input_data_columns else NULL
        if (is.null(input_channels)) stop("Could not extract input_channels from conv_layers!")
        if (is.null(time_steps)) stop("time_steps is missing in the model!")
        model_info$"mlm:input" <- list(
          list(
            name = "Temporal CNN Input",
            bands = if (!is.null(bands)) as.list(bands) else list("unknown"),
            input = list(
              shape = list(1, as.integer(input_channels), as.integer(time_steps)),
              dim_order = list("batch", "channels", "time_steps"),
              data_type = "float32"
            ),
            description = "Temporal input for CNN",
            pre_processing_function = NULL
          )
        )
        output_size <- if (!is.null(model$dense[[length(model$dense)]])) {
          model$dense[[length(model$dense)]]$out_features
        } else {
          NULL
        }
        if (is.null(output_size)) stop("Could not extract output_size from dense layer")
        model_info$"mlm:output" <- list(
          list(
            name = "CNN Output",
            tasks = tasks,
            result = list(
              shape = list(1, as.integer(output_size)),
              dim_order = list("batch", "features"),
              data_type = "float32"
            ),
            description = "Output features from TempCNN",
            "classification:classes" = if (tasks[[1]] == "classification") {
              lapply(0:(output_size - 1), function(i) list(
                value = i,
                name = paste("class", i),
                description = paste("Class", i)
              ))
            } else NULL,
            post_processing_function = NULL
          )
        )
        if (length(options) > 0) {
          for (key in names(options)) {
            if (grepl("^mlm:", key)) {
              model_info[[key]] <- options[[key]]
            }
          }
        }
      } else {
        stop("Unknown model type: Please check the model!")
      }
      mlm_stac_item$properties <- c(mlm_stac_item$properties, model_info)
      
      rds_path <- ensure_extension(sub("\\.json$", "", filepath), "rds")
      con <- rawConnection(raw(0), "wb")
      torch::torch_save(model, con)
      raw_model <- rawConnectionValue(con)
      close(con)
      saveRDS(raw_model, file = rds_path)
      
      mlm_stac_item$assets <- list(
        model = list(
          href = rds_path,
          type = "application/octet-stream",
          title = "TempCNN Model",
          "mlm:artifact_type" = "R (Raw RDS)",
          roles = list("mlm:model")
        )
      )
      jsonlite::write_json(mlm_stac_item, path = filepath, auto_unbox = TRUE, pretty = TRUE)
      message("Model was saved as MLM-STAC-JSON under: ", filepath)
      return(filepath)
      
      
    }
    
    #######################################
    if (!is.null(tasks) && length(tasks) > 0) {
      options <- list()
      options[["mlm:tasks"]] <- tasks
    }
    result <- list()
    tmp <- shared_dir
    
    
    
    if (is.character(data) && length(data)==1 && file.exists(data) && grepl("\\.pt$", data, ignore.case=TRUE)) {
      
      model <- torch::torch_load(data)
      pt_meta <- save_torch_model(
        model, 
        file.path(shared_dir, paste0(base_name, ".pt"))
      )
      
      result$onnx <- convert_torch_to_onnx_from_pt(
        script_pt   = pt_meta$pt_path,
        input_chan  = pt_meta$input_chan,
        time_steps  = pt_meta$time_steps,
        base_name   = base_name,
        output_dir  = shared_dir
      )
      
      json_file <- file.path(shared_dir, ensure_extension(base_name, "json"))
      save_model_as_mlm_stac_json_dl(
        model    = model,
        filepath = json_file,
        tasks    = tasks,
        options  = options
      )
      result$json <- json_file
      
    } else if ("train" %in% class(data)) {
      message("Machine model detected...")
      model_type <- detect_model_type(data)
      message("Detected model type: ", model_type)
      convert_model_to_pkl(data, model_type, file.path(tmp, base_name))
      message("Convert done!")
      onnx_path <- save_ml_model_as_onnx(model_type, file.path(tmp, base_name))
      result$onnx <- onnx_path
      json_file <- file.path(tmp, ensure_extension(base_name, "json"))
      save_model_as_mlm_stac_json(data, json_file, tasks, options)
      result$json <- json_file
    } else {
      stop("Unknown model type: must be 'nn_module' (Torch) or 'train' (Caret).")
    }
    
    rds_path <- file.path(tmp, ensure_extension(base_name, "rds"))
    result$rds <- rds_path
    
    if (length(options) > 0) {
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
    if (!is.null(download_links$torch)) {
      message("- TorchScript: ", download_links$torch)
    }
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
