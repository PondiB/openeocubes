



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
#' @import tools
#' @import tidyr
#' @import rlang
#' @import httr2
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
    
    
    extract_time_series_features <- function(training_set, features_data, time_steps) {
      if (time_steps >= 1) {
        message("multi time steps")
        features <- array(
          data = as.matrix(training_set[, grep("_T\\d+$", colnames(training_set))]),
          dim = c(nrow(training_set), length(features_data), time_steps)
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
      band_names <- grep("^B0?\\d{1,2}$", names(train_data), value = TRUE)
      has_ndvi <- "NDVI" %in% colnames(train_data)
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
      
      time_order <- sort(unique(train_data$time))
      cols_sorted <- unlist(lapply(seq_along(time_order), function(i) paste0(bands_to_use, "_T", i)))
      cols_sorted <- c("fid", cols_sorted)
      train_data_wide <- train_data_wide[, cols_sorted]
      
      train_data_clean <- train_data_wide %>% dplyr::filter(complete.cases(.))
      
      target_data <- train_data %>%
        dplyr::select(fid, !!rlang::sym(target)) %>%
        dplyr::distinct(fid, .keep_all = TRUE)
      
      train_data_clean <- dplyr::left_join(train_data_clean, target_data, by = "fid")
      train_data_clean
    }
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    message("ml_fit is being prepared...")
    
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
  
      seed <- model$parameters$seed
      message("Vor dem seed", seed)

      tryCatch({
        if (!is.null(seed) && length(seed) == 1 && is.finite(seed)) {
          torch::torch_manual_seed(as.integer(seed))
        }
      }, error = function(e) {
        message("Error in setting torch manual seed: ", e$message)
      })

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
      message("Number of classes detected: ", class_count)
      dl_model <- model$create_model(
        input_data_columns = features_data,
        time_steps = time_steps,
        class_count = class_count
      )
      
      try({
        dl_model$eval()
        take <- as.integer(min(4, x_train$size(1)))
        xin <- x_train$narrow(1, 1, take)$contiguous()
        out <- dl_model(xin)
        arr <- torch::as_array(out)
      }, silent = FALSE)
      
      opt_name <- tolower(model$parameters$optimizer %||% "adam")
      optimizer <- switch(
        opt_name,
        adam = optim_adam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3),
        radam = optim_radam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3),
        nadam = optim_nadam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3),
        optim_adam(dl_model$parameters, lr = model$parameters$learning_rate %||% 1e-3)
      )
      loss_fn <- nn_cross_entropy_loss()
      
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
        Actual = as.integer(torch::as_array(y_train))
      )
      message("Confusion Matrix:"); print(confusion_matrix)
      
      dl_model$time_steps <- as.integer(time_steps)
      dl_model$input_channels <- as.integer(length(features_data))
      dl_model$input_data_columns <- features_data
      dl_model$input_layout <- "NCT"
      
      
      model_file <- tempfile(fileext = ".pt")
      torch_save(dl_model, model_file)

      message("Model saved in Torch file: ", model_file)
      
      return(model_file)
    }
    
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


    if (!is.null(model$method) && identical(model$method, "xgbTree")) {
      message("Training with xgboost::xgb.train (direct engine).")

      if (!is.null(model$seed) && length(model$seed) > 0) {
        set.seed(as.integer(model$seed[1]))
        message("Seed: ", as.integer(model$seed[1]))
      } else {
        message("Seed: <NULL>")
      }

      train_data <- as.matrix(x)
      message("X dims: ", paste(dim(train_data), collapse = " x "))

      labels <- NULL
      if (is_classification) {
        y_fac <- as.factor(training_set[[target]])
        labels <- levels(y_fac)
        y_num <- as.integer(y_fac) - 1L
        message("Task: classification")
        message("Class counts:")
        print(table(y_fac))
      } else {
        y_num <- as.numeric(training_set[[target]])
        message("Task: regression")
        message("y summary:")
        print(summary(y_num))
      }

      params <- model$params
      message("Base params (from model$params):")
      print(params)

      if (is_classification) {
        if (length(labels) > 2) {
          params$objective <- "multi:softprob"
          params$num_class <- length(labels)
          params$eval_metric <- "mlogloss"
        } else {
          params$objective <- "binary:logistic"
          params$eval_metric <- "logloss"
        }
      } else {
        params$objective <- "reg:squarederror"
        params$eval_metric <- "rmse"
      }

      dtrain <- xgboost::xgb.DMatrix(train_data, label = y_num)

      message("Starting cross-validation to determine best nrounds...")
      cv <- xgboost::xgb.cv(
        params = params,
        data = dtrain,
        nrounds = 2000,
        nfold = 5,
        stratified = is_classification,
        early_stopping_rounds = 30,
        maximize = FALSE,
        verbose = 0
      )
      ev <- cv$evaluation_log

      best_iter <- cv$best_iteration
      if (is.null(best_iter) || length(best_iter) == 0) {

        test_mean_col <- grep("^test_.*_mean$", names(ev), value = TRUE)[1]
        if (length(test_mean_col) == 0) {
          message("No test_*_mean column found, fallback to last iteration.")
          best_iter <- nrow(ev)
        } else {
          message("Using metric column for best iteration: ", test_mean_col)
          metric <- ev[[test_mean_col]]
          best_iter <- which.min(metric)
          message("Best iteration by which.min(", test_mean_col, "): ", best_iter,
                  " (metric=", metric[best_iter], ")")
        }
      } else {
        message("Using cv$best_iteration: ", best_iter)
      }

      n_rounds <- as.integer(best_iter)[1]
      if (is.na(n_rounds) || n_rounds < 1L) n_rounds <- 100L

      split_idx <- function(y, p = 0.8) {
        n <- length(y)
        if (is.factor(y)) {
          idx <- integer(0)
          for (lv in levels(y)) {
            ii <- which(y == lv)
            if (length(ii) == 0) next
            k  <- max(1L, floor(length(ii) * p))
            idx <- c(idx, sample(ii, k))
          }
          sort(unique(idx))
        } else {
          sample.int(n, size = max(1L, floor(n * p)))
        }
      }

      train_idx <- split_idx(if (is_classification) y_fac else y_num, p = 0.8)
      valid_idx <- setdiff(seq_len(nrow(train_data)), train_idx)

      message("n train: ", length(train_idx), " | n valid: ", length(valid_idx))
      if (is_classification) {
        message("Holdout class counts:")
        print(table(y_fac[valid_idx]))
        baseline <- max(prop.table(table(y_fac[valid_idx])))
      }

      dtr <- xgboost::xgb.DMatrix(train_data[train_idx, , drop = FALSE], label = y_num[train_idx])
      dva <- xgboost::xgb.DMatrix(train_data[valid_idx, , drop = FALSE], label = y_num[valid_idx])

      booster <- xgboost::xgb.train(
        params = params,
        data = dtr,
        nrounds = n_rounds,
        evals = list(train = dtr, valid = dva),
        early_stopping_rounds = 30,
        verbose = 1
      )

      pred_valid <- predict(booster, train_data[valid_idx, , drop = FALSE])

      if (is_classification) {
        if (length(labels) > 2) {
          K <- length(labels)
          p_mat <- matrix(pred_valid, ncol = K, byrow = TRUE)
          pred_idx <- max.col(p_mat) - 1L
          acc <- mean(pred_idx == y_num[valid_idx])
          message("Accuracy (holdout): ", round(acc * 100, 2), "%")

          pred_lab <- factor(labels[pred_idx + 1L], levels = labels)
          true_lab <- factor(y_fac[valid_idx], levels = labels)
          message("Confusion matrix (holdout):")
          print(table(Pred = pred_lab, True = true_lab))
        } else {
          pred_cls <- ifelse(pred_valid >= 0.5, 1L, 0L)
          acc <- mean(pred_cls == y_num[valid_idx])
          message("Accuracy (holdout): ", round(acc * 100, 2), "%")
        }
      } else {
        rmse <- sqrt(mean((pred_valid - y_num[valid_idx])^2))
        message("RMSE (holdout): ", round(rmse, 4))
      }

      dfull <- xgboost::xgb.DMatrix(train_data, label = y_num)
      model <- xgboost::xgb.train(
        params = params,
        data = dfull,
        nrounds = n_rounds,
        verbose = 0
      )

      attr(model, "model") <- "xgboost"
      attr(model, "classification") <- is_classification
      attr(model, "class_levels") <- labels
      attr(model, "predictor_names") <- predictor_names
      attr(model, "params") <- params
      attr(model, "nrounds") <- n_rounds

      return(model)
    }

    tryCatch({
      if (model$method == "rf") {
        if (!is.null(model$seed)) set.seed(as.integer(model$seed))
        p <- ncol(x)
        mm <- model$mtry_mode
        mtry <- NULL
        
        if (!is.null(mm)) {
          if (is.character(mm)) {
            mtry <- switch(tolower(mm),
                           "sqrt" = max(1L, floor(sqrt(p))),
                           "log2" = max(1L, floor(log2(p))),
                           "onethird" = max(1L, floor(p/3)),
                           "all" = p,
                           suppressWarnings(as.integer(mm))
            )
          } else if (is.numeric(mm)) {
            mtry <- as.integer(mm)
          }
        }
        if (is.null(mtry) || is.na(mtry) || mtry < 1L || mtry > p) {
          mtry <- max(1L, floor(sqrt(p)))  
        }
        
        model$tuneGrid <- expand.grid(mtry = mtry)
        if (is.null(model$trControl)) {
          model$trControl <- caret::trainControl(method = "cv", number = 5, sampling = "up")
        }
        message("Random Forest mtry = ", mtry, " (p = ", p, ")")
      } else if (model$method %in% c("svmRadial","svmLinear","svmPoly")) {
        if (is.null(model$tuneGrid)) stop("SVM models require a defined tuneGrid")
        model$preProcess <- c("center","scale")
      } else {
        stop("Undetected method! Allowed: 'rf', 'svmRadial', 'svmLinear', 'svmPoly'")
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
  categories = as.array(c("machine-learning", "prediction")),
  summary = "Predicts the AOI with the model (ONNX/RDS/PT/TerraTorch-CKPT)",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "The Area of interest cube, which we want to predict",
      schema = list(type = "objects", subtype = "raster-cube")
    ),
    Parameter$new(
      name = "model",
      description = "The trained machine learning model (ONNX, RDS, PT, or TerraTorch descriptor)",
      schema = list(type = "object", subtype ="mlm-model")
    )
  ),
  returns = eo_datacube,
  operation = function(data, model, job) {
    

    # ---------- Helpers ----------
    is_torch_model <- function(model) {
      inherits(model, "nn_module") ||
        "nn_module" %in% class(model) ||
        (!is.null(model$conv_layers) && !is.null(model$dense))
    }
    sanitize_band_names <- function(nms) {
      nms <- as.character(nms)
      if (length(nms) == 1L) {
        s <- gsub("X[0-9]+[\\._]", "", nms, perl = TRUE)
        parts <- unlist(regmatches(s, gregexpr("[A-Za-z][A-Za-z0-9_]+", s, perl = TRUE)))
        parts <- parts[nzchar(parts)]
        return(parts)
      } else {
        out <- gsub("^X[0-9]+[\\._]", "", nms, perl = TRUE)
        out <- sub("^[\\._]+", "", out, perl = TRUE)
        return(out)
      }
    }
    is_terratorch_desc <- function(m) {
      is.list(m) && !is.null(m$type) && m$type %in% c("terratorch-ckpt","terratorch-backbone")
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
          message("Classic ML model saved to RDS single: ", model_file)
        }
      }
      Sys.setenv(MODEL_FILE = model_file)
      
      raw_band_names <- gdalcubes::bands(data_cube)$name
      cube_band_names <- sanitize_band_names(raw_band_names)
      message("Bands: ", paste(cube_band_names, collapse = ", "))
      
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
        
        if (!is.matrix(x)) x <- matrix(x, nrow = 1)
        
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        if (is.null(colnames(x))) colnames(x) <- local_bands
        
        pixel_df <- as.data.frame(x)
        
        library(torch)
        if (endsWith(model_file, ".pt")) {
          local_model <- torch::torch_load(model_file)
        } else {
          local_model <- readRDS(model_file)
        }

        infer_torch_layout <- function(model, n_bands, nsteps) {
          candidates <- list(
            NCT = torch::torch_randn(1, n_bands, nsteps),
            NTC = torch::torch_randn(1, nsteps, n_bands)
          )
          for (k in names(candidates)) {
            ok <- tryCatch({ model(candidates[[k]]); TRUE }, error = function(e) FALSE)
            if (ok) return(list(kind="torch", layout=k))
          }
          stop("Torch model accepts neither NCT nor NTC with given dims.")
        }


        if (inherits(local_model, "xgb.Booster")) {
          is_class <- attr(local_model, "classification")
          labels <- attr(local_model, "class_levels")
          
          dmat <- xgboost::xgb.DMatrix(as.matrix(pixel_df))
          raw_pred <- predict(local_model, dmat)
          
          if (isTRUE(is_class) && length(labels) > 2) {
            p_mat <- matrix(raw_pred, ncol = length(labels), byrow = TRUE)
            pred_value <- max.col(p_mat)  
          } else if (isTRUE(is_class)) {
            pred_value <- ifelse(raw_pred >= 0.5, 2L, 1L) 
          } else {
            pred_value <- raw_pred
          }
          return(as.numeric(pred_value))
        } else if (is_torch_model(local_model)) {
          message("Deep Learning model detected in predict_pixel_fun")
          pixel_matrix <- as.matrix(pixel_df)
          n_channels <- length(local_bands)
          layout_info <- infer_torch_layout(local_model, n_bands = n_channels, nsteps = 1)
          pixel_tensor <- torch::torch_tensor(pixel_matrix, dtype = torch_float())

          if (layout_info$layout == "NCT") {
              message("Using NCT layout for prediction")
            pixel_tensor <- pixel_tensor$view(c(1, n_channels, 1))
          } else if (layout_info$layout == "NTC") {
            message("Using NTC layout for prediction")
            pixel_tensor <- pixel_tensor$transpose(1,2)$view(c(1, 1, n_channels))
          } else {
            stop("Unknown torch layout: ", layout_info$layout)
          }

          message("Shape of pixel_tensor: ", paste(dim(pixel_tensor), collapse = " x "))
          local_model$eval()
          with_no_grad({ preds <- local_model(pixel_tensor) })
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
      tmp <- Sys.getenv("SHARED_TEMP_DIR", unset = tempdir())
      
      if (is.character(model)) {
        model_file <- model
      } else {
        if (is_torch_model(model)) {
          model_file <- tempfile(fileext = ".pt")
          torch::torch_save(model, model_file)
        } else {
          model_file <- file.path(tmp, "model.rds")
          saveRDS(model, model_file)
          message("Classic ML model saved to RDS multi: ", model_file)
        }
      }
      
      Sys.setenv(MODEL_FILE = model_file)
      
      raw_band_names <- gdalcubes::bands(data_cube)$name
      cube_band_names <- sanitize_band_names(raw_band_names)
      message("Bands santalize: ", paste(cube_band_names, collapse = ", "))
      
      band_names_file <- file.path(tmp, "band_names.rds")
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
      message("temp", tmp)
      
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

        infer_torch_layout <- function(model, n_bands, nsteps) {
          candidates <- list(
            NCT = torch::torch_randn(1, n_bands, nsteps),
            NTC = torch::torch_randn(1, nsteps, n_bands)
          )
          for (k in names(candidates)) {
            ok <- tryCatch({ model(candidates[[k]]); TRUE }, error = function(e) FALSE)
            if (ok) return(list(kind="torch", layout=k))
          }
          stop("Torch model accepts neither NCT nor NTC with given dims.")
        }


        if (inherits(local_model, "xgb.Booster")) {
          is_class <- attr(local_model, "classification")
          labels <- attr(local_model, "class_levels")

          wide_vec <- as.vector(x)
          wide_mat <- matrix(wide_vec, nrow = 1)
          wide_names <- unlist(lapply(seq_len(local_nsteps), function(i) paste0(local_bands, "_T", i)))
          message("Wide names: ", paste(wide_names, collapse = ", "))
          if (length(wide_vec) != length(wide_names)) {
            stop("Mismatch: vector length is ", length(wide_vec), " but expected ", length(wide_names))
          }
          colnames(wide_mat) <- wide_names
          pixel_df <- as.data.frame(wide_mat)
          
          dmat <- xgboost::xgb.DMatrix(as.matrix(pixel_df))
          raw_pred <- predict(local_model, dmat)
          
          if (isTRUE(is_class) && length(labels) > 2) {
            p_mat <- matrix(raw_pred, ncol = length(labels), byrow = TRUE)
            pred_value <- max.col(p_mat)  
          } else if (isTRUE(is_class)) {
            pred_value <- ifelse(raw_pred >= 0.5, 2L, 1L) 
          } else {
            pred_value <- raw_pred
          }
          result_matrix <- matrix(rep(pred_value, local_nsteps), nrow = 1)
          return(result_matrix)
        } else if (is_torch_model(local_model)){
          message("Deep Learning model detected")
          library(torch)
          layout_info <- infer_torch_layout(local_model, n_bands = n_bands, nsteps = local_nsteps)
          pixel_tensor <- torch::torch_tensor(x, dtype = torch_float())

          if (layout_info$layout == "NCT") {
              message("Using NCT layout for prediction")
            pixel_tensor <- pixel_tensor$view(c(1, n_bands, local_nsteps))
          } else if (layout_info$layout == "NTC") {
            message("Using NTC layout for prediction")
            pixel_tensor <- pixel_tensor$transpose(1,2)$view(c(1, local_nsteps, n_bands))
          } else {
            stop("Unknown torch layout: ", layout_info$layout)
          }

          local_model$eval()
          with_no_grad({ preds <- local_model(pixel_tensor) })
          pred_class_tensor <- torch::torch_argmax(preds, dim = 2)
          pred_class <- as.numeric(torch::as_array(pred_class_tensor))
          return(matrix(rep(pred_class, local_nsteps), nrow = 1))
        } else {
          message("Classic machine learning detected")
          wide_vec <- as.vector(x)
          wide_mat <- matrix(wide_vec, nrow = 1)
          wide_names <- unlist(lapply(seq_len(local_nsteps), function(i) paste0(local_bands, "_T", i)))
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
    
    
    detected_model_type <- function(model_path) {
      stopifnot(file.exists(model_path))
      onnxruntime <- reticulate::import("onnxruntime", delay_load = TRUE)
      session <- tryCatch(
        onnxruntime$InferenceSession(model_path),
        error = function(e) stop("ONNXRuntime InferenceSession failed: ", conditionMessage(e))
      )
      
      inputs <- session$get_inputs()
      if (is.null(inputs) || length(inputs) < 1) {
        stop("ONNX session has no inputs (get_inputs() empty). Model likely invalid or not loaded correctly.")
      }
      
      input_details <- inputs[[1]]
      shp <- input_details$shape
      
      if (is.null(shp)) {
        stop("ONNX input shape is NULL. input_details: ", paste(names(input_details), collapse=", "))
      }
      
      shp_len <- length(shp)
      if (shp_len == 3) {
        Sys.setenv(ONNX_DL_FLAG = "TRUE")
        message("ONNX model detected as Deep Learning (3D input).")
      } else {
        Sys.setenv(ONNX_DL_FLAG = "FALSE")
        message("ONNX model detected as Classic ML (2D input).")
      }
      
     return(list(
       session = session,
       input_name = input_details$name,
       input_shape = shp,
       is_onnx = TRUE
     ))
    }
    
    
    
    
    mlm_single_onnx <- function(data_cube, model) {
      message("Preparing ONNX prediction (single time step) using apply_pixel()...")
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      
      if (is.character(model)) {
        message("Model is a file path: ", model)
        model_file <- model
      } else {
        stop("For ONNX predictions, model must be a path.")
      }
      Sys.setenv(MODEL_FILE = model_file)
      
      raw_band_names <- gdalcubes::bands(data_cube)$name
      cube_band_names <- sanitize_band_names(raw_band_names)
      message("Bands: ", paste(cube_band_names, collapse = ", "))
      
      band_names_file <- file.path(tmp, "band_names.rds")
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
      
      model <- detected_model_type(model_file)
      
      predict_pixel_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        nsteps <- as.numeric(Sys.getenv("NSTEPS"))
        model_file <- Sys.getenv("MODEL_FILE")
        
        if (!is.matrix(x)) x <- matrix(x, nrow = 1)
        
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        if (is.null(colnames(x))) colnames(x) <- local_bands
        n_bands <- length(local_bands)
        nsteps_local <- nsteps

        infer_onnx_dl_layout <- function(onnx_path, n_bands, nsteps) {
          ort <- reticulate::import("onnxruntime")
          sess <- ort$InferenceSession(onnx_path)
          inp <- sess$get_inputs()[[1]]
          input_name <- inp$name
          candidates <- list(
            NCT = array(rnorm(1 * n_bands * nsteps), dim = c(1, n_bands, nsteps)),
            NTC = array(rnorm(1 * nsteps * n_bands), dim = c(1, nsteps, n_bands))
          )
          for (k in names(candidates)) {
            ok <- tryCatch({
              x_np <- reticulate::np_array(candidates[[k]], dtype = "float32")
              sess$run(NULL, setNames(list(x_np), input_name))
              TRUE
            }, error = function(e) FALSE)

            if (ok) return(list(kind = "onnx", layout = k, input_name = input_name))
          }
          stop("ONNX model accepts neither NCT nor NTC with given dims.")
        }
        
        onnx_dl_flag <- Sys.getenv("ONNX_DL_FLAG")
        if (onnx_dl_flag == "TRUE") {
           layout_info <- infer_onnx_dl_layout(model_file, n_bands = n_bands, nsteps = nsteps_local)
            if (layout_info$layout == "NCT") {
              message("Using ONNX NCT layout for prediction")
              x <- array(x, dim = c(1, n_bands, nsteps_local))
            } else if (layout_info$layout == "NTC") {
              message("Using ONNX NTC layout for prediction")
              x <- array(t(x), dim = c(1, nsteps_local, n_bands))  
            } else {
              stop("Unknown ONNX layout: ", layout_info$layout)
            }
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
        #np <- reticulate::import("numpy")
        onnxruntime <- reticulate::import("onnxruntime")
        
        session <- onnxruntime$InferenceSession(Sys.getenv("MODEL_FILE"))
        input_details <- session$get_inputs()[[1]]
        input_name <- input_details$name
        
        np_x <- reticulate::np_array(x, dtype = "float32")
        pred <- session$run(output_names = NULL, input_feed = setNames(list(np_x), input_name))[[1]]
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
      tmp <- Sys.getenv("SHARED_TEMP_DIR", unset = tempdir())
      
      if (is.character(model)) {
        model_file <- model
      } else {
        stop("For ONNX predictions, model must be a path")
      }
      Sys.setenv(MODEL_FILE = model_file)
      
      raw_band_names <- gdalcubes::bands(data_cube)$name
      cube_band_names <- sanitize_band_names(raw_band_names)
      message("Bands: ", paste(cube_band_names, collapse = ", "))
      
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
        if (is.null(colnames(x))) colnames(x) <- local_bands

        infer_onnx_dl_layout <- function(onnx_path, n_bands, nsteps) {
          ort <- reticulate::import("onnxruntime")
          sess <- ort$InferenceSession(onnx_path)
          inp <- sess$get_inputs()[[1]]
          input_name <- inp$name
          candidates <- list(
            NCT = array(rnorm(1 * n_bands * nsteps), dim = c(1, n_bands, nsteps)),
            NTC = array(rnorm(1 * nsteps * n_bands), dim = c(1, nsteps, n_bands))
          )
          for (k in names(candidates)) {
            ok <- tryCatch({
              x_np <- reticulate::np_array(candidates[[k]], dtype = "float32")
              sess$run(NULL, setNames(list(x_np), input_name))
              TRUE
            }, error = function(e) FALSE)

            if (ok) return(list(kind = "onnx", layout = k, input_name = input_name))
          }
          stop("ONNX model accepts neither NCT nor NTC with given dims.")
        }

        
        onnx_dl_flag <- Sys.getenv("ONNX_DL_FLAG")
        if (onnx_dl_flag == "TRUE") {
           layout_info <- infer_onnx_dl_layout(model_file, n_bands = n_bands, nsteps = nsteps_local)
            if (layout_info$layout == "NCT") {
              message("Using ONNX NCT layout for prediction")
              x <- array(x, dim = c(1, n_bands, nsteps_local))
            } else if (layout_info$layout == "NTC") {
              message("Using ONNX NTC layout for prediction")
              x <- array(t(x), dim = c(1, nsteps_local, n_bands))  
            } else {
              stop("Unknown ONNX layout: ", layout_info$layout)
            }
        } else {
          wide_vec <- as.vector(x)
          wide_mat <- matrix(wide_vec, nrow = 1, ncol = n_bands * nsteps_local)
          wide_names <- unlist(lapply(seq_len(nsteps_local), function(i) paste0(local_bands, "_T", i)))
          if (ncol(wide_mat) != length(wide_names)) {
            stop("Mismatch: number of columns is ", ncol(wide_mat), " but expected ", length(wide_names))
          }
          colnames(wide_mat) <- wide_names
          x <- wide_mat
        }
        if (!reticulate::py_module_available("numpy")) reticulate::py_install("numpy", pip = TRUE)
        if (!reticulate::py_module_available("onnxruntime")) reticulate::py_install("onnxruntime", pip = TRUE)
        #np <- reticulate::import("numpy")
        onnxruntime <- reticulate::import("onnxruntime")
        session <- onnxruntime$InferenceSession(Sys.getenv("MODEL_FILE"))
        input_name <- session$get_inputs()[[1]]$name
        np_x <- reticulate::np_array(x, dtype = "float32")
        
        pred <- session$run(output_names = NULL, input_feed = setNames(list(np_x), input_name))[[1]]
        if (onnx_dl_flag == "TRUE") {
          pred_class <- as.numeric(apply(pred, 1, which.max))
        } else {
          pred_class <- as.numeric(pred)
        }
        result <- matrix(rep(pred_class, nsteps_local), nrow = 1)
        message("Prediction: ", paste(result, collapse = ", "))
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
    
    mlm_single_terratorch <- function(data_cube, desc) {
      message("TerraTorch single-timestep via apply_pixel() [pred_class only]")
      
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      Sys.setenv(TMPDIRPATH = tmp)
      Sys.setenv(WANDB_DISABLED = "true")  
      
      raw_band_names <- gdalcubes::bands(data_cube)$name
      cube_band_names <- sanitize_band_names(raw_band_names)
      saveRDS(cube_band_names, file.path(tmp, "band_names.rds"))
      
      tt_desc <- list(
        expected_bands = if (!is.null(desc$bands)) desc$bands else c("B02","B03","B04","B8A","B11","B12"),
        num_frames = if (!is.null(desc$num_frames)) as.integer(desc$num_frames) else 3L,
        backbone = if (!is.null(desc$backbone)) desc$backbone else "terratorch_prithvi_eo_v2_100_tl",
        ckpt = if (!is.null(desc$ckpt)) desc$ckpt else NULL,
        backbone_pt = if (!is.null(desc$backbone_pt)) desc$backbone_pt else NULL,
        num_classes = if (!is.null(desc$num_classes)) as.integer(desc$num_classes) else 13L,
        backbone_bands = if (!is.null(desc$backbone_bands)) as.character(desc$backbone_bands) else
          c("BLUE","GREEN","RED","NIR_NARROW","SWIR_1","SWIR_2"),
        neck_indices = if (!is.null(desc$neck_indices)) as.integer(desc$neck_indices) else c(2L,5L,8L,11L),
        decoder_name = if (!is.null(desc$decoder)) desc$decoder else "UNetDecoder",
        decoder_channels = if (!is.null(desc$decoder_channels)) as.integer(desc$decoder_channels) else c(512L,256L,128L,64L),
        head_dropout = if (!is.null(desc$head_dropout)) as.numeric(desc$head_dropout) else 0.1,
        model_factory = if (!is.null(desc$factory)) desc$factory else "EncoderDecoderFactory",
        loss_name = if (!is.null(desc$loss)) desc$loss else "ce",
        optimizer_name = if (!is.null(desc$optimizer)) desc$optimizer else "AdamW",
        freeze_backbone = if (!is.null(desc$freeze_backbone)) isTRUE(desc$freeze_backbone) else TRUE,
        freeze_decoder = if (!is.null(desc$freeze_decoder)) isTRUE(desc$freeze_decoder) else FALSE
      )
      desc_file <- file.path(tmp, "terratorch_desc.rds")
      saveRDS(tt_desc, desc_file)
      Sys.setenv(TT_DESC_FILE = desc_file)
      
      predict_pixel_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        desc_file <- Sys.getenv("TT_DESC_FILE")
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        tt <- readRDS(desc_file)
        
        if (!is.matrix(x)) x <- matrix(x, nrow = length(local_bands))
        
        resolve_tt_band_indices <- function(expected_bands, cube_bands) {
          expected_bands <- as.character(expected_bands)
          cube_bands <- as.character(cube_bands)
          
          s2_to_hls_all <- c(
            B01 = "COASTAL",
            B02 = "BLUE",
            B03 = "GREEN",
            B04 = "RED",
            B05 = "RED_EDGE_1",
            B06 = "RED_EDGE_2",
            B07 = "RED_EDGE_3",
            B08 = "NIR",
            B8A = "NIR_NARROW",
            B09 = "WATER_VAPOR",
            B10 = "CIRRUS",
            B11 = "SWIR_1",
            B12 = "SWIR_2"
          )
          hls_to_s2_all <- setNames(names(s2_to_hls_all), s2_to_hls_all)
          
          known_hls <- names(hls_to_s2_all)
          known_s2  <- names(s2_to_hls_all)
          
          if (all(expected_bands %in% cube_bands)) {
            idx <- match(expected_bands, cube_bands)
            return(list(idx = idx, effective_expected = expected_bands))
          }
          
          if (any(expected_bands %in% known_hls)) {
            mapped_s2 <- unname(hls_to_s2_all[expected_bands])
            if (!all(is.na(mapped_s2)) && all(mapped_s2 %in% cube_bands)) {
              idx <- match(mapped_s2, cube_bands)
              return(list(idx = idx, effective_expected = mapped_s2))
            }
          }
          
          if (any(expected_bands %in% known_s2)) {
            mapped_hls <- unname(s2_to_hls_all[expected_bands])
            if (!all(is.na(mapped_hls)) && all(mapped_hls %in% cube_bands)) {
              idx <- match(mapped_hls, cube_bands)
              return(list(idx = idx, effective_expected = mapped_hls))
            }
          }
          
          mapped_from_s2 <- ifelse(expected_bands %in% known_s2, unname(s2_to_hls_all[expected_bands]), NA_character_)
          mapped_from_hls <- ifelse(expected_bands %in% known_hls, unname(hls_to_s2_all[expected_bands]), NA_character_)
          
          msg_extra <- paste0(
            "S2->HLS: ", paste(expected_bands, "→", mapped_from_s2, collapse = ", "), " | ",
            "HLS->S2: ", paste(expected_bands, "→", mapped_from_hls, collapse = ", ")
          )
          
          stop(
            "Bands mismatch: expected (descriptor/YAML) ", paste(expected_bands, collapse = ", "),
            " vs cube bands ", paste(cube_bands, collapse = ", "),
            ". Konnte kein konsistentes Mapping finden. Details: ", msg_extra
          )
        }
        
        
        band_res <- resolve_tt_band_indices(tt$expected_bands, local_bands)
        idx <- band_res$idx
        x <- x[idx, , drop = FALSE]

        K <- tt$num_frames
        Tnow <- ncol(x)
        if (Tnow < K) {
          pad <- matrix(x[, Tnow, drop = FALSE], nrow = nrow(x), ncol = K - Tnow)
          x <- cbind(x, pad)
        } else if (Tnow > K) {
          x <- x[, (Tnow - K + 1):Tnow, drop = FALSE]
        }
        
        if (!requireNamespace("reticulate", quietly = TRUE)) stop("reticulate required")
        cache_name <- "..mlp_tt_cache"
        if (!exists(cache_name, envir = .GlobalEnv, inherits = FALSE)) {
          assign(cache_name, new.env(parent = emptyenv()), envir = .GlobalEnv)
        }
        cache <- get(cache_name, envir = .GlobalEnv, inherits = FALSE)
        
        if (!isTRUE(get0("py_loaded", envir = cache, inherits = FALSE))) {
          cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
          if (!is.null(cfg)) message("[worker] reticulate python: ", cfg$python)
          ok <- TRUE
          tryCatch(reticulate::import("terratorch"), error = function(e) { ok <<- FALSE })
          if (!ok) stop("TerraTorch Python package not found in worker interpreter: ",
                        if (!is.null(cfg)) cfg$python else "(unknown)")
          
          reticulate::py_run_string("
import torch
import torch.nn.functional as F
from collections.abc import Mapping
import numpy as np

def _to_tensor(out):
    if isinstance(out, torch.Tensor):
        return out
    if isinstance(out, (list, tuple)):
        for it in out:
            try:
                return _to_tensor(it)
            except Exception:
                pass
        raise TypeError('No tensor found in list/tuple output')
    if isinstance(out, Mapping):
        for k in ('logits','seg','out','pred','y','y_hat','masks'):
            if k in out:
                try: return _to_tensor(out[k])
                except Exception: pass
        for v in out.values():
            try: return _to_tensor(v)
            except Exception: pass
        raise TypeError('No tensor found in mapping output')
    try:
        from transformers.utils import ModelOutput as HFModelOutput
        if isinstance(out, HFModelOutput):
            for k in ('logits','seg','out','pred','y','y_hat','masks'):
                v = getattr(out, k, None)
                if v is not None:
                    try: return _to_tensor(v)
                    except Exception: pass
            tup = out.to_tuple()
            for it in tup:
                try: return _to_tensor(it)
                except Exception: pass
            raise TypeError('No tensor in HF ModelOutput')
    except Exception:
        pass
    if hasattr(out, '__dict__'):
        for v in vars(out).values():
            try: return _to_tensor(v)
            except Exception: pass
    for k in ('logits','seg','out','pred','y','y_hat','masks'):
        if hasattr(out, k):
            try: return _to_tensor(getattr(out, k))
            except Exception: pass
    raise TypeError(f'Unsupported model output type: {type(out)}')


@torch.inference_mode()
def run_forward(backbone_name,
                num_frames,
                band_names,
                ckpt_path=None,
                backbone_pt=None,
                num_classes=13,
                neck_indices=None,
                decoder_name='UNetDecoder',
                decoder_channels=None,
                head_dropout=0.1,
                model_factory='EncoderDecoderFactory',
                loss_name='ce',
                optimizer_name='AdamW',
                freeze_backbone=True,
                freeze_decoder=False):
    from terratorch.tasks import SemanticSegmentationTask

    if neck_indices is None:
        neck_indices = [2, 5, 8, 11]
    else:
        neck_indices = [int(i) for i in neck_indices]

    if decoder_channels is None:
        decoder_channels = [512, 256, 128, 64]
    else:
        decoder_channels = [int(c) for c in decoder_channels]

    if band_names is None:
        band_names = []
    else:
        band_names = list(band_names)

    model = SemanticSegmentationTask(
        model_factory=model_factory,
        model_args={
            'backbone': backbone_name,
            'backbone_pretrained': True,
            'backbone_num_frames': int(num_frames),
            'backbone_bands': band_names,
            'backbone_coords_encoding': [],
            'necks': [
                {'name': 'SelectIndices', 'indices': neck_indices},
                {'name': 'ReshapeTokensToImage', 'effective_time_dim': int(num_frames)},
                {'name': 'LearnedInterpolateToPyramidal'},
            ],
            'decoder': decoder_name,
            'decoder_channels': decoder_channels,
            'head_dropout': float(head_dropout),
            'num_classes': int(num_classes),
        },
        loss=loss_name,
        lr=1e-4,
        optimizer=optimizer_name,
        ignore_index=-1,
        freeze_backbone=bool(freeze_backbone),
        freeze_decoder=bool(freeze_decoder),
        plot_on_val=False,
    )
    if backbone_pt:
        try:
            sd = torch.load(backbone_pt, map_location='cpu')
            if isinstance(sd, dict) and 'state_dict' in sd and isinstance(sd['state_dict'], dict):
                sd = sd['state_dict']
            enc = model.model.encoder
            enc.load_state_dict(sd, strict=False)
        except Exception as e:
            print('[Warn] could not load backbone_pt:', e)
    if ckpt_path:
        ck = torch.load(ckpt_path, map_location='cpu')
        sd = ck.get('state_dict', ck)
        model.load_state_dict(sd, strict=False)
    model.eval()
    return model


@torch.inference_mode()
def infer_pixel(model, x_np, min_multiple=32):
    C, T = x_np.shape
    H = W = int(min_multiple)
    x_tile = np.tile(x_np[:, :, None, None], (1, 1, H, W)).astype('float32')
    xt = torch.from_numpy(x_tile).unsqueeze(0)  # (1,C,T,H,W)
    try:
        y = model.model(xt)
    except Exception:
        y = model(xt)
    logits = _to_tensor(y)
    if hasattr(logits, 'dim'):
        if logits.dim() == 5 and logits.shape[0] == 1:
            logits = logits[0]
        if logits.dim() == 4 and logits.shape[0] == 1:
            logits = logits[0]
    logp = F.log_softmax(logits, dim=0)
    hh, ww = H // 2, W // 2
    cls0 = int(torch.argmax(logp[:, hh, ww]).item())
    return cls0
")
          assign("py_loaded", TRUE, envir = cache)
        }
        
        model_obj <- get0("model_obj", envir = cache, inherits = FALSE)
        if (is.null(model_obj)) {
          model_obj <- reticulate::py$run_forward(
            tt$backbone,
            as.integer(tt$num_frames),
            tt$backbone_bands,
            tt$ckpt,
            tt$backbone_pt,
            as.integer(tt$num_classes),
            as.integer(tt$neck_indices),
            tt$decoder_name,
            as.integer(tt$decoder_channels),
            as.numeric(tt$head_dropout),
            tt$model_factory,
            tt$loss_name,
            tt$optimizer_name,
            tt$freeze_backbone,
            tt$freeze_decoder
          )
          assign("model_obj", model_obj, envir = cache)
        }
        
        cls0 <- tryCatch({
          reticulate::py$infer_pixel(model_obj, x)
        }, error = function(e) {
          pe <- tryCatch(reticulate::py_last_error(), error = function(.) NULL)
          if (!is.null(pe)) message("PyErr: ", pe$type, " | ", pe$value)
          stop(e)
        })
        
        cls1 <- as.numeric(cls0) + 1 
        return(cls1)
      }
      
      gdalcubes::apply_pixel(
        data_cube,
        names = "pred_class",
        keep_bands = FALSE,
        FUN = predict_pixel_fun
      )
    }
    
    mlm_multi_terratorch <- function(data_cube, desc) {
      message("TerraTorch multi-timestep via apply_time() [pred_class only + broadcast]")
      
      tmp <- Sys.getenv("SHARED_TEMP_DIR", tempdir())
      Sys.setenv(TMPDIRPATH = tmp)
      Sys.setenv(WANDB_DISABLED = "true")
      
      raw_band_names  <- gdalcubes::bands(data_cube)$name
      cube_band_names <- sanitize_band_names(raw_band_names)
      saveRDS(cube_band_names, file.path(tmp, "band_names.rds"))
      
      tt_desc <- list(
        expected_bands = if (!is.null(desc$bands)) desc$bands else c("B02","B03","B04","B8A","B11","B12"),
        num_frames = if (!is.null(desc$num_frames)) as.integer(desc$num_frames) else 3L,
        backbone = if (!is.null(desc$backbone)) desc$backbone else "terratorch_prithvi_eo_v2_100_tl",
        ckpt = if (!is.null(desc$ckpt)) desc$ckpt else NULL,
        backbone_pt = if (!is.null(desc$backbone_pt)) desc$backbone_pt else NULL,
        num_classes = if (!is.null(desc$num_classes)) as.integer(desc$num_classes) else 13L,
        backbone_bands = if (!is.null(desc$backbone_bands)) as.character(desc$backbone_bands) else
          c("BLUE","GREEN","RED","NIR_NARROW","SWIR_1","SWIR_2"),
        neck_indices = if (!is.null(desc$neck_indices)) as.integer(desc$neck_indices) else c(2L,5L,8L,11L),
        decoder_name = if (!is.null(desc$decoder)) desc$decoder else "UNetDecoder",
        decoder_channels = if (!is.null(desc$decoder_channels)) as.integer(desc$decoder_channels) else c(512L,256L,128L,64L),
        head_dropout = if (!is.null(desc$head_dropout)) as.numeric(desc$head_dropout) else 0.1,
        model_factory = if (!is.null(desc$factory)) desc$factory else "EncoderDecoderFactory",
        loss_name = if (!is.null(desc$loss)) desc$loss else "ce",
        optimizer_name = if (!is.null(desc$optimizer)) desc$optimizer else "AdamW",
        freeze_backbone = if (!is.null(desc$freeze_backbone)) isTRUE(desc$freeze_backbone) else TRUE,
        freeze_decoder = if (!is.null(desc$freeze_decoder)) isTRUE(desc$freeze_decoder) else FALSE
      )
      desc_file <- file.path(tmp, "terratorch_desc.rds")
      saveRDS(tt_desc, desc_file)
      Sys.setenv(TT_DESC_FILE = desc_file)
      
      nsteps <- length(gdalcubes::dimension_values(data_cube)$t)
      Sys.setenv(NSTEPS = nsteps)
      
      predict_time_fun <- function(x) {
        local_tmp <- Sys.getenv("TMPDIRPATH")
        desc_file <- Sys.getenv("TT_DESC_FILE")
        nsteps_loc <- as.numeric(Sys.getenv("NSTEPS"))
        local_bands <- readRDS(file.path(local_tmp, "band_names.rds"))
        tt <- readRDS(desc_file)
        
        if (!is.matrix(x)) x <- matrix(x, nrow = length(local_bands))
        
        resolve_tt_band_indices <- function(expected_bands, cube_bands) {
          expected_bands <- as.character(expected_bands)
          cube_bands <- as.character(cube_bands)
          
          s2_to_hls_all <- c(
            B01 = "COASTAL",
            B02 = "BLUE",
            B03 = "GREEN",
            B04 = "RED",
            B05 = "RED_EDGE_1",
            B06 = "RED_EDGE_2",
            B07 = "RED_EDGE_3",
            B08 = "NIR",
            B8A = "NIR_NARROW",
            B09 = "WATER_VAPOR",
            B10 = "CIRRUS",
            B11 = "SWIR_1",
            B12 = "SWIR_2"
          )
          hls_to_s2_all <- setNames(names(s2_to_hls_all), s2_to_hls_all)
          
          known_hls <- names(hls_to_s2_all)
          known_s2 <- names(s2_to_hls_all)
          
          if (all(expected_bands %in% cube_bands)) {
            idx <- match(expected_bands, cube_bands)
            return(list(idx = idx, effective_expected = expected_bands))
          }
          
          if (any(expected_bands %in% known_hls)) {
            mapped_s2 <- unname(hls_to_s2_all[expected_bands])
            if (!all(is.na(mapped_s2)) && all(mapped_s2 %in% cube_bands)) {
              idx <- match(mapped_s2, cube_bands)
              return(list(idx = idx, effective_expected = mapped_s2))
            }
          }
          
          if (any(expected_bands %in% known_s2)) {
            mapped_hls <- unname(s2_to_hls_all[expected_bands])
            if (!all(is.na(mapped_hls)) && all(mapped_hls %in% cube_bands)) {
              idx <- match(mapped_hls, cube_bands)
              return(list(idx = idx, effective_expected = mapped_hls))
            }
          }
          
          mapped_from_s2 <- ifelse(expected_bands %in% known_s2,
                                   unname(s2_to_hls_all[expected_bands]),
                                   NA_character_)
          mapped_from_hls <- ifelse(expected_bands %in% known_hls,
                                    unname(hls_to_s2_all[expected_bands]),
                                    NA_character_)
          
          msg_extra <- paste0(
            "S2->HLS: ", paste(expected_bands, "→", mapped_from_s2, collapse = ", "), " | ",
            "HLS->S2: ", paste(expected_bands, "→", mapped_from_hls, collapse = ", ")
          )
          
          stop(
            "Bands mismatch: expected (descriptor/YAML) ", paste(expected_bands, collapse = ", "),
            " vs cube bands ", paste(cube_bands, collapse = ", "),
            ". Konnte kein konsistentes Mapping finden. Details: ", msg_extra
          )
        }
        
        band_res <- resolve_tt_band_indices(tt$expected_bands, local_bands)
        idx <- band_res$idx
        x <- x[idx, , drop = FALSE]
        
        
        
        K <- tt$num_frames
        Tnow <- ncol(x)
        if (Tnow < K) {
          pad <- matrix(x[, Tnow, drop = FALSE], nrow = nrow(x), ncol = K - Tnow)
          x <- cbind(x, pad)
        } else if (Tnow > K) {
          x <- x[, (Tnow - K + 1):Tnow, drop = FALSE]
        }
        
        if (!requireNamespace("reticulate", quietly = TRUE)) stop("reticulate required")
        cache_name <- "..mlp_tt_cache"
        if (!exists(cache_name, envir = .GlobalEnv, inherits = FALSE)) {
          assign(cache_name, new.env(parent = emptyenv()), envir = .GlobalEnv)
        }
        cache <- get(cache_name, envir = .GlobalEnv, inherits = FALSE)
        
        if (!isTRUE(get0("py_loaded", envir = cache, inherits = FALSE))) {
          cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
          if (!is.null(cfg)) message("[worker] reticulate python: ", cfg$python)
          ok <- TRUE
          tryCatch(reticulate::import("terratorch"), error = function(e) { ok <<- FALSE })
          if (!ok) stop("TerraTorch Python package not found in worker interpreter: ",
                        if (!is.null(cfg)) cfg$python else "(unknown)")
          
          reticulate::py_run_string("
import torch
import torch.nn.functional as F
from collections.abc import Mapping
import numpy as np

def _to_tensor(out):
    if isinstance(out, torch.Tensor):
        return out
    if isinstance(out, (list, tuple)):
        for it in out:
            try:
                return _to_tensor(it)
            except Exception:
                pass
        raise TypeError('No tensor found in list/tuple output')
    if isinstance(out, Mapping):
        for k in ('logits','seg','out','pred','y','y_hat','masks'):
            if k in out:
                try: return _to_tensor(out[k])
                except Exception: pass
        for v in out.values():
            try: return _to_tensor(v)
            except Exception: pass
        raise TypeError('No tensor found in mapping output')
    try:
        from transformers.utils import ModelOutput as HFModelOutput
        if isinstance(out, HFModelOutput):
            for k in ('logits','seg','out','pred','y','y_hat','masks'):
                v = getattr(out, k, None)
                if v is not None:
                    try: return _to_tensor(v)
                    except Exception: pass
            tup = out.to_tuple()
            for it in tup:
                try: return _to_tensor(it)
                except Exception: pass
            raise TypeError('No tensor in HF ModelOutput')
    except Exception:
        pass
    if hasattr(out, '__dict__'):
        for v in vars(out).values():
            try: return _to_tensor(v)
            except Exception: pass
    for k in ('logits','seg','out','pred','y','y_hat','masks'):
        if hasattr(out, k):
            try: return _to_tensor(getattr(out, k))
            except Exception: pass
    raise TypeError(f'Unsupported model output type: {type(out)}')


@torch.inference_mode()
def run_forward(backbone_name,
                num_frames,
                band_names,
                ckpt_path=None,
                backbone_pt=None,
                num_classes=13,
                neck_indices=None,
                decoder_name='UNetDecoder',
                decoder_channels=None,
                head_dropout=0.1,
                model_factory='EncoderDecoderFactory',
                loss_name='ce',
                optimizer_name='AdamW',
                freeze_backbone=True,
                freeze_decoder=False):
    from terratorch.tasks import SemanticSegmentationTask

    if neck_indices is None:
        neck_indices = [2, 5, 8, 11]
    else:
        neck_indices = [int(i) for i in neck_indices]

    if decoder_channels is None:
        decoder_channels = [512, 256, 128, 64]
    else:
        decoder_channels = [int(c) for c in decoder_channels]

    if band_names is None:
        band_names = []
    else:
        band_names = list(band_names)

    model = SemanticSegmentationTask(
        model_factory=model_factory,
        model_args={
            'backbone': backbone_name,
            'backbone_pretrained': True,
            'backbone_num_frames': int(num_frames),
            'backbone_bands': band_names,
            'backbone_coords_encoding': [],
            'necks': [
                {'name': 'SelectIndices', 'indices': neck_indices},
                {'name': 'ReshapeTokensToImage', 'effective_time_dim': int(num_frames)},
                {'name': 'LearnedInterpolateToPyramidal'},
            ],
            'decoder': decoder_name,
            'decoder_channels': decoder_channels,
            'head_dropout': float(head_dropout),
            'num_classes': int(num_classes),
        },
        loss=loss_name,
        lr=1e-4,
        optimizer=optimizer_name,
        ignore_index=-1,
        freeze_backbone=bool(freeze_backbone),
        freeze_decoder=bool(freeze_decoder),
        plot_on_val=False,
    )
    if backbone_pt:
        try:
            sd = torch.load(backbone_pt, map_location='cpu')
            if isinstance(sd, dict) and 'state_dict' in sd and isinstance(sd['state_dict'], dict):
                sd = sd['state_dict']
            enc = model.model.encoder
            enc.load_state_dict(sd, strict=False)
        except Exception as e:
            print('[Warn] could not load backbone_pt:', e)
    if ckpt_path:
        ck = torch.load(ckpt_path, map_location='cpu')
        sd = ck.get('state_dict', ck)
        model.load_state_dict(sd, strict=False)
    model.eval()
    return model


@torch.inference_mode()
def infer_pixel(model, x_np, min_multiple=32):
    C, T = x_np.shape
    H = W = int(min_multiple)
    x_tile = np.tile(x_np[:, :, None, None], (1, 1, H, W)).astype('float32')
    xt = torch.from_numpy(x_tile).unsqueeze(0)  # (1,C,T,H,W)
    try:
        y = model.model(xt)
    except Exception:
        y = model(xt)
    logits = _to_tensor(y)
    if hasattr(logits, 'dim'):
        if logits.dim() == 5 and logits.shape[0] == 1:
            logits = logits[0]
        if logits.dim() == 4 and logits.shape[0] == 1:
            logits = logits[0]
    logp = F.log_softmax(logits, dim=0)
    hh, ww = H // 2, W // 2
    cls0 = int(torch.argmax(logp[:, hh, ww]).item())
    return cls0
")
          
          assign("py_loaded", TRUE, envir = cache)
        }
        
        model_obj <- get0("model_obj", envir = cache, inherits = FALSE)
        if (is.null(model_obj)) {
          model_obj <- reticulate::py$run_forward(
            tt$backbone,
            as.integer(tt$num_frames),
            tt$backbone_bands,
            tt$ckpt,
            tt$backbone_pt,
            as.integer(tt$num_classes),
            as.integer(tt$neck_indices),
            tt$decoder_name,
            as.integer(tt$decoder_channels),
            as.numeric(tt$head_dropout),
            tt$model_factory,
            tt$loss_name,
            tt$optimizer_name,
            tt$freeze_backbone,
            tt$freeze_decoder
          )
          assign("model_obj", model_obj, envir = cache)
        }
        
        cls0 <- tryCatch({
          reticulate::py$infer_pixel(model_obj, x)
        }, error = function(e) {
          pe <- tryCatch(reticulate::py_last_error(), error = function(.) NULL)
          if (!is.null(pe)) message("PyErr: ", pe$type, " | ", pe$value)
          stop(e)
        })
        
        cls1 <- as.numeric(cls0) + 1
        matrix(rep(cls1, nsteps_loc), nrow = 1)
      }
      
      gdalcubes::apply_time(
        data_cube,
        names = "pred_class",
        keep_bands = FALSE,
        FUN = predict_time_fun
      )
    }
    
    
    time_count <- gdalcubes::dimensions(data)$t$count
    multi_timesteps <- time_count > 1
    
    if (is.raw(model)) {
      tmp_file <- tempfile(fileext = ".onnx"); writeBin(model, tmp_file)
      message("RAW model treated as ONNX at: ", tmp_file)
      model <- tmp_file
    }
    if (is.list(model) && !is.null(model$onnx) && endsWith(tolower(model$onnx), ".onnx")) {
      message("Model provided as list – using ONNX: ", model$onnx)
      model <- model$onnx
    }
    
    if (is_terratorch_desc(model)) {
      
      if (multi_timesteps) {
        prediction <-  mlm_multi_terratorch(data, model)
        return(prediction)
      }else{
        prediction <- mlm_single_terratorch(data, model)
        return(prediction)
        
      }
    }

    stac_mlm_identification <- function(model) {
      if (is.list(model) && model$subtype == "stac-mlm-model") {
        input_index <- model$input_index
        output_index <- model$output_index
        input_spec <- model$input_spec
        output_spec <- model$output_spec
        shape <- model$output_spec$result$shape
        input_shape <- model$input_spec$input$shape
        message("nun hier")
        bands_in <- model$input_spec$bands
        model <- model$path
        return(list(model = model, input_index = input_index, output_index = output_index, input_spec = input_spec, output_spec = output_spec, shape = shape, input_shape = input_shape, bands_in = bands_in))
      }
      return(NULL)
    }



    message("external model loading...")
    if (is.list(model) && identical(model$subtype, "stac-mlm-model")) {
      message("STAC MLM model detected.")
      stac_info <- stac_mlm_identification(model)
      if (!is.null(stac_info)) {
        message("STAC MLM model detected.")
        model <- stac_info$model
        input_index <- stac_info$input_index
        output_index <- stac_info$output_index
        inout_spec <- stac_info$input_spec
        output_spec <- stac_info$output_spec
        shape <- stac_info$shape
        output_shape <- shape[[2]]
        input_shape <- stac_info$input_shape
        bands_in <- input_shape[[2]]
        timesteps_in <- input_shape[[3]]
        bands_in <- stac_info$bands_in
        message("Model path: ", model)
        message("Input index: ", input_index)
        message("Output index: ", output_index)
        message("Input spec: ", inout_spec)
        message("Output spec: ", output_spec)
        message("shape: ", paste(shape, collapse = ", "))
        message("output shape:", output_shape)
        message("input shape:", paste(input_shape, collapse = ", "))
        message("bands in:", bands_in)
        message("timesteps in:", timesteps_in) 
      }
    }
  
    compare_input_stac_cube <- function(data, bands_in, timesteps_in, shape, input_shape) {
      cube_bands <- gdalcubes::bands(data)$name
      cube_bands <- sanitize_band_names(cube_bands)
      time_count <- gdalcubes::dimensions(data)$t$count
      message("shape from stac model: ", paste(shape, collapse = ", "))
      message("bands from data cube: ", paste(cube_bands, collapse = ", "))
      message("timesteps from data cube: ", time_count)
      message("input shape from stac model: ", paste(input_shape, collapse = ", "))
      
      if (length(bands_in) != length(cube_bands)) {
        stop("Band count mismatch between STAC model and data cube.")
      }
      if (!all(bands_in %in% cube_bands)) {
        stop("Band names mismatch between STAC model and data cube.")
      }
      if (timesteps_in != time_count) {
        stop("Timestep count mismatch between STAC model and data cube.")
      }
      message("STAC model input matches data cube.")
    }

    detect_shape <- function(input_shape, timesteps_in, bands_in){
      shape <- input_shape
      if(length(shape) == 3 ) {
        if(shape[2] == length(bands_in) && shape[3] == timesteps_in){
          return("NDT")
        }
        if(shape[2] == timesteps_in && shape[3] == length(bands_in)){
          return("NTD")
        }
      }
      if(length(shape) == 2 ) {
        if(shape[1] == length(bands_in) && shape[2] == timesteps_in){
          return("DT")
        }
        if(shape[1] == timesteps_in && shape[2] == length(bands_in)){
          return("TD")
        }
      }
    stop("Cannot infer shape format from input shape.", paste("Input shape:", paste(input_shape, collapse = ", ")), "Bands: ", length(bands_in), "Timesteps: ", timesteps_in)

    }

    reshape_input_stac_cube <- function(input_shape, bands_in, timesteps_in){
      shape <- detect_shape(input_shape, timesteps_in, bands_in)
      message("Inferred shape format: ", shape)

      shape_info <- list(
        shape = shape, 
        bands_in = bands_in,
        timesteps_in = timesteps_in,
        batches = if(length(input_shape) == 3) input_shape[1] else 10L,
        target_shape = "NDT"
      )

      return(shape_info)
      
    }


    if (exists("bands_in") && exists("timesteps_in") && exists("shape")) {
      compare_input_stac_cube(data, bands_in, timesteps_in, shape)
    }

    if (is.character(model) && endsWith(tolower(model), ".onnx")) {
      if (multi_timesteps) {
        prediction <- mlm_multi_onnx(data, model)
        return(prediction)
      }else{
        prediction <- mlm_single_onnx(data, model)
        return(prediction)
      } 
    }

    if (is.character(model)) {
      message("Loading external model...")
      if (endsWith(tolower(model), ".pt")) {
        library(torch)
        if (!file.exists(model)) stop("Model file does not exist: ", model)
        model <- torch::torch_load(model)
        message("Torch model successfully loaded.")
      } else if (endsWith(tolower(model), ".rds")) {
        model_obj <- readRDS(model)
        message("RDS model loaded successfully.")
        if (is.raw(model_obj)) {
          message("raw RDS detected; trying torch_load from raw")
          con_rds <- rawConnection(model_obj, "rb")
          model <- torch::torch_load(con_rds)
        } else if (inherits(model_obj, "train")) {
          message("Caret model recognized")
          model <- model_obj
        } else {
          stop("Unknown model type in .rds - no nn_module, no caret model, no Torch state_dict")
        }
      } else {
        stop("Unsupported model format: ", model)
      }
    }
    
    if (multi_timesteps) {
      prediction <- mlm_multi(data, model)
      return(prediction)
    } else {
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
  
  tuneGrid <- switch(kernel,
                     rbf = expand.grid(C = C, sigma = gamma),                 
                     linear = expand.grid(C = C),                               
                     poly = expand.grid(C = C, degree = degree, scale = gamma) 
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
  
  mtry_grid <- if (is.character(max_variables)) {
    if (max_variables %in% c("sqrt", "log2", "onethird")) {
      data.frame(mtry = NA)  
    } else if (max_variables == "all") {
      data.frame(mtry = NA)  
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
#'   learning_rate = 0.1,
#'   max_depth = 6,
#'   min_child_weight = 1,
#'   subsample = 0.8,
#'   min_split_loss = 0,
#'   seed = 123
#' )
#'
#' # Regression example
#' params <- mlm_regr_xgboost(
#'   learning_rate = 0.05,
#'   max_depth = 4,
#'   min_child_weight = 3,
#'   subsample = 0.7,
#'   min_split_loss = 1,
#'   seed = 42
#' )


mlm_xgboost_envelope <- function(learning_rate = 0.15,
                                        max_depth = 5,
                                        min_child_weight = 1,
                                        subsample = 0.8,
                                        min_split_loss = 1,
                                        seed = NULL,
                                        classification) {
  params <- list(
    eta = learning_rate,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    gamma = min_split_loss,
    colsample_bytree = 1
  )
  if(!is.null(seed)){
    params$seed <- seed
  }

  list(
    method = "xgbTree",
    params = params,
    nrounds = 100,
    classification = classification,
    seed = seed
  )
}



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
    
    return(list(
      kind = "tempcnn",                  
      classification = TRUE,             
      subtype = "mlm-model",             
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
              in_ch <- if (i == 1) length(self$input_data_columns) else settings$cnn_layers[[i - 1]]
              out_ch <- settings$cnn_layers[[i]]
              k <- settings$cnn_kernels[[i]]
              drop <- settings$cnn_dropout_rates[[i]]
              
              self$conv_layers$append(
                nn_sequential(
                  nn_conv1d(in_channels = in_ch, out_channels = out_ch, kernel_size = k, stride = 1, padding = k %/% 2),
                  nn_batch_norm1d(out_ch),
                  nn_relu(),
                  nn_dropout(p = drop)
                )
              )
              
            }
            
            self$flatten <- nn_flatten()
            self$dense1 <- nn_linear(in_features = 1L, out_features = settings$dense_layer_nodes) 
            self$act1 <- nn_relu()
            self$drop1 <- nn_dropout(p = settings$dense_layer_dropout_rate)
            self$out <- nn_linear(in_features = settings$dense_layer_nodes, out_features = class_count)
          },
          forward = function(x) {
            for (i in seq_along(self$conv_layers)) {
              x <- self$conv_layers[[i]](x)  
            }
            n <- x$size(1)
            feat_dim <- x$size(2) * x$size(3) 
            x <- x$view(c(n, feat_dim))
            
            if (self$dense1$in_features != feat_dim) {
              self$dense1 <- nn_linear(in_features = feat_dim, out_features = self$dense1$out_features)$to(device = x$device)
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
    
    create_model <- function(input_data_columns, time_steps, class_count) {
      library(torch)
      
      act_ctor <- switch(tolower(params$activation),
                         "relu" = nn_relu,
                         "tanh" = nn_tanh,
                         "logistic" = nn_sigmoid,
                         nn_relu)
      
      nn_module(
        "MLPModule",
        initialize = function(settings, input_cols, time_steps, class_count) {
          self$flatten <- nn_flatten()
          input_dim <- length(input_cols) * time_steps
          hls <- as.integer(unlist(settings$hidden_layer_sizes))
          dr <- as.numeric(unlist(settings$dropout_rates))
          if (length(dr) < length(hls)) dr <- c(dr, rep(tail(dr,1), length(hls)-length(dr)))

          layers <- list()
          last <- input_dim
          for (i in seq_along(hls)) {
            layers <- append(layers, list(nn_linear(in_features = last, out_features = hls[[i]]), act_ctor(), nn_dropout(p = dr[[i]])))
            last <- hls[[i]]
          }
          layers <- append(layers, list(
            nn_linear(in_features = last, out_features = class_count)
          ))

          self$mlp <- do.call(nn_sequential, layers)
        },
        forward = function(x) {
          x <- self$flatten(x)  
          self$mlp(x)            
        }
      )(
        settings = params,
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
    
    create_model <- function(input_data_columns, time_steps, class_count) {
      library(torch)
      
      LightTAE <- nn_module(
        "LightTAE",
        initialize = function(settings, input_cols, time_steps, class_count) {
          self$C <- length(input_cols)
          self$T <- time_steps
          
          d_model <- max(64L, as.integer(8L * ceiling(self$C / 2)))
          d_model <- min(d_model, 256L)
          self$d_model <- d_model
          
          self$proj_in <- nn_linear(self$C, d_model)     
          self$q_proj <- nn_linear(d_model, d_model)
          self$k_proj <- nn_linear(d_model, d_model)
          self$v_proj <- nn_linear(d_model, d_model)
          
          self$attn_drop <- nn_dropout(p = 0.1)
          
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
          x <- x$permute(c(1, 3, 2))
          h <- self$proj_in(x)
          
          q <- self$q_proj(h)
          k <- self$k_proj(h)
          v <- self$v_proj(h)
          
          scale <- sqrt(as.numeric(self$d_model))
          scores <- torch_matmul(q, k$transpose(2, 3)) / scale
          attn <- nnf_softmax(scores, dim = 3)
          attn <- self$attn_drop(attn)
          
          ctx <- torch_matmul(attn, v)
          
          h2 <- self$norm1(h + ctx)
          h3 <- self$ffn(h2)
          h3 <- self$norm2(h2 + h3)
          
          z <- torch_mean(h3, dim = 2)
          
          self$head(z)
        }
      )
      
      LightTAE(
        settings = params,
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
      
      to_int1  <- function(x, name="") {
        if (inherits(x, "torch_tensor")) x <- as.numeric(torch::as_array(x)[1])
        if (is.list(x)) x <- unlist(x, use.names = FALSE)
        if (length(x) == 0) stop(sprintf("to_int1(%s): empty", name))
        if (is.character(x)) {
          y <- suppressWarnings(as.numeric(x))
          if (all(is.na(y))) {
            x2 <- gsub("[^0-9\\-]+", "", x)  
            y <- suppressWarnings(as.numeric(x2))
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
        message(sprintf("[%s] size=(%s) dtype=%s", tag, paste(as.integer(ten$size()), collapse="x"), ten$dtype))
      }
      
      STGFBlock <- nn_module(
        "STGFBlock",
        initialize = function(in_ch, d_model, ksize, dilation, pdrop, time_mixer="attention") {
          in_ch <- to_int1(in_ch, "block.in_ch")
          d_model <- to_int1(d_model, "block.d_model")
          ksize <- to_int1(ksize, "block.kernel")
          dilation<- to_int1(dilation, "block.dilation")
          pdrop <- to_num1(pdrop, "block.dropout")
          tmix <- as.character(time_mixer)
          
          pad <- as.integer(((ksize - 1L) %/% 2L) * dilation)
          message(sprintf("[STGFBlock.init] in_ch=%d d=%d k=%d dil=%d pad=%d mixer=%s pdrop=%.3f", in_ch, d_model, ksize, dilation, pad, tmix, pdrop))
          
          self$time_mixer <- tmix
          
          self$dw <- nn_conv1d(in_channels=in_ch, out_channels=in_ch, kernel_size=ksize, stride=1, padding=pad, dilation=dilation, groups=in_ch, bias=TRUE)
          self$pw <- nn_conv1d(in_channels=in_ch, out_channels=d_model, kernel_size=1, bias=TRUE)
          
          if (self$time_mixer == "attention") {
            self$ln1 <- nn_layer_norm(d_model)
            self$q <- nn_linear(d_model, d_model)
            self$k <- nn_linear(d_model, d_model)
            self$v <- nn_linear(d_model, d_model)
            self$attn_drop <- nn_dropout(p=pdrop)
          } else {
            self$ln1 <- nn_layer_norm(d_model)
            self$gate_u <- nn_linear(d_model, d_model)
            self$gate_f <- nn_linear(d_model, d_model)
            self$cand <- nn_linear(d_model, d_model)
          }
          
          self$ff <- nn_sequential(
            nn_linear(d_model, 4L*d_model),
            nn_gelu(),
            nn_dropout(p=pdrop),
            nn_linear(4L*d_model, d_model)
          )
          self$ln2 <- nn_layer_norm(d_model)
          
          se_hidden <- max(8L, as.integer(d_model/4))
          self$se <- nn_sequential(
            nn_linear(d_model, se_hidden),
            nn_relu(),
            nn_linear(se_hidden, d_model),
            nn_sigmoid()
          )
        },
        forward = function(x) {
          log_shape("x", x)
          
          h <- self$dw(x); log_shape("dw", h)
          h <- self$pw(h); log_shape("pw", h)
          
          h_t <- h$permute(c(1,3,2)); log_shape("permute(N,T,d)", h_t)
          
          if (self$time_mixer == "attention") {
            q <- self$q(h_t); k <- self$k(h_t); v <- self$v(h_t)
            log_shape("q", q); log_shape("k", k); log_shape("v", v)
            
            scale <- sqrt(as.numeric(q$size(3)))
            scores <- torch_matmul(q, k$transpose(2,3)) / scale 
            log_shape("scores", scores)
            
            a <- nnf_softmax(scores, dim=3)
            a <- self$attn_drop(a)
            ctx <- torch_matmul(a, v)                           
            log_shape("ctx(attn)", ctx)
            h1  <- self$ln1(h_t + ctx)
          } else {
            u <- torch_sigmoid(self$gate_u(h_t))
            f <- torch_sigmoid(self$gate_f(h_t))
            g <- torch_tanh(self$cand(h_t))
            log_shape("u", u); log_shape("f", f); log_shape("g", g)
            
            Tlen <- as.integer(h_t$size(2))
            s <- torch_zeros_like(g[, 1, ])  
            s_list <- vector("list", Tlen)
            for (t in 1:Tlen) {
              gt <- g[, t, ]
              ft <- f[, t, ]
              s  <- ft * s + (1 - ft) * gt
              s_list[[t]] <- s$unsqueeze(2)  
            }
            S <- torch_cat(s_list, dim=2)    
            log_shape("S(state)", S)
            ctx <- u * S                     
            log_shape("ctx(mamba)", ctx)
            h1 <- self$ln1(h_t + ctx)
          }
          
          h2 <- self$ff(h1)
          h2 <- self$ln2(h1 + h2)                              
          log_shape("post-ffn", h2)
          
          z <- torch_mean(h2, dim=2)                         
          gch <- self$se(z)$unsqueeze(2)                        
          h2g <- h2 * gch                                       
          log_shape("after-SE", h2g)
          
          y <- h2g$permute(c(1,3,2))                            
          log_shape("block-out(N,d,T)", y)
          y
        }
      )
      
      STGF <- nn_module(
        "STGF",
        initialize = function(settings, C, T, class_count) {
          message("[STGF.init] raw types:")
          message(sprintf(" - kernel_size: %s", paste(capture.output(str(settings$kernel_size)), collapse=" ")))
          message(sprintf(" - num_blocks:  %s", paste(capture.output(str(settings$num_blocks)),  collapse=" ")))
          message(sprintf(" - dilations:   %s", paste(capture.output(str(settings$dilations)),  collapse=" ")))
          
          self$C <- get_int_def(C, default = 1L, name = "C")
          self$T <- get_int_def(T, default = 1L, name = "T")
          
          d_raw <- settings$d_model
          d_parsed <- tryCatch(to_int1(d_raw, "d_model"), error=function(e) NA_integer_)
          if (is.na(d_parsed) || d_parsed <= 0) {
            d_fallback <- max(64L, as.integer(8L * ceiling(self$C / 2)))
            message(sprintf("[cfg] d_model invalid -> fallback %d", d_fallback))
            self$d <- d_fallback
          } else self$d <- d_parsed
          
          nb <- get_int_def(settings$num_blocks,  default = 3L, name = "num_blocks")
          ksz <- get_int_def(settings$kernel_size, default = 7L, name = "kernel_size")
          if (ksz < 3L) { message("[cfg] kernel_size < 3 -> 3"); ksz <- 3L }
          if ((ksz %% 2L) == 0L) { ksz <- ksz + 1L; message(sprintf("[cfg] kernel_size forced odd -> %d", ksz)) }
          
          drp <- get_num_def(settings$dropout, default = 0.2, name = "dropout")
          drp <- max(0, min(1, drp))
          
          tmx <- get_choice_def(settings$time_mixer, c("attention","mamba"), default = "attention", name = "time_mixer")
          
          dil <- tryCatch(to_intv(settings$dilations, "dilations"), error=function(e) c(1L,2L,4L))
          if (length(dil) < nb) dil <- c(dil, rep(tail(dil, 1), nb - length(dil)))
          
          message(sprintf("[STGF.init] C=%d T=%d d=%d nb=%d k=%d dilations=%s drop=%.3f mixer=%s", self$C, self$T, self$d, nb, ksz, paste(dil, collapse=","), drp, tmx))
          
          self$blocks <- nn_module_list()
          in_ch <- self$C
          for (i in seq_len(nb)) {
            self$blocks$append(
              STGFBlock(in_ch, self$d, ksz, dil[[i]], drp, tmx)
            )
            in_ch <- self$d
          }
          self$nb <- nb  
          
          self$pool_query <- nn_parameter(torch_randn(self$d))
          
          self$head <- nn_sequential(
            nn_layer_norm(self$d),
            nn_linear(self$d, class_count)
          )
        },
        forward = function(x) {
          log_shape("x_in", x)
          
          h <- x
          n_blocks <- if (!is.null(self$nb)) as.integer(self$nb) else length(self$blocks)
          if (is.na(n_blocks) || n_blocks < 1) stop("STGF has no blocks.")
          for (i in seq_len(n_blocks)) {
            h <- self$blocks[[i]](h)          
          }
          log_shape("h_after_blocks", h)
          
          ht <- h$permute(c(1,3,2))           
          log_shape("ht(N,T,d)", ht)
          
          logits_t <- torch_matmul(ht, self$pool_query)$squeeze(-1)  
          w <- nnf_softmax(logits_t, dim=2)$unsqueeze(3)             
          
          lt_arr <- as.numeric(torch::as_array(logits_t))
          if (length(lt_arr) > 0) {
            message(sprintf("[pool] T=%d; logits min/max: %.4f / %.4f", as.integer(ht$size(2)), min(lt_arr), max(lt_arr)))
          }
          
          z <- torch_sum(ht * w, dim=2)      
          log_shape("z(N,d)", z)
          
          y <- self$head(z)                  
          log_shape("head_out(N,classes)", y)
          y
        }
      )
      
      message(sprintf("[factory] input C=%d, T=%d, classes=%d", length(input_data_columns), time_steps, class_count))
      STGF(settings=params, C=length(input_data_columns), T=time_steps, class_count=class_count)
    }
    
    list(
      parameters = params,
      create_model = create_model,
      classification = TRUE,
      subtype = "mlm-model"
    )
  }
)

#'
#'load_stac_ml 
#' 

load_stac_ml <- Process$new(
  id = "load_stac_ml",
  description = paste(
    "Loads a machine learning model from a STAC Item.",
    "Such a model could be trained and saved as part of a previous batch job with processes such as  ``ml_fit()`` and ``save_ml_model()`` or  externally hosted models."
  ),
  categories = as.array(c("machine learning", "import")),
  summary = "Load a ML model from a STAC Item",
  
  parameters = list(
    Parameter$new(
      name = "uri",
      description = 
        "The STAC Item to load the machine learning model from. The STAC Item must implement the [`mlm`](https://github.com/stac-extensions/mlm) extension. This parameter can point to a remote STAC Item via ``URL`` or a local JSON file.",
      schema = list(
        list(
          title = "URL",
          type = "string",
          format = "uri",
          subtype = "uri",
          pattern = "^https?://"
        ),
        list(
          title = "User-uploaded File",
          type = "string",
          subtype = "file-path",
          pattern = "^[^\r\n\\:'\"]+$"
        )
      )
      
    ), 
    Parameter$new(
      name = "model_asset", 
      description = paste(
        "The Asset name of the given STAC Item which represents the actual ML model.",
        "The asset must list ``mlm:model`` as its role. If only one asset lists ``mlm:model`` as its role, this parameter is optional as this asset will be used by default. If multiple assets list ``mlm:model`` as their role, this parameter is required to determine which asset to use."
      ),
      schema = list(type = "string", default = NULL), 
      optional = TRUE
    ),
    Parameter$new(
      name = "input_index", 
      description = "STAC:MLM items supports multiple ML model input specification. This parameter specifies the index of the input specification in the ``mlm:input`` array to use for prediction or training. As ``mlm:input`` is an array, the first input in the array has index 0.",
      schema = list(type = "integer", default = 0),
      optional = TRUE 
    ),
    Parameter$new(
      name = "output_index", 
      description = "STAC:MLM items supports multiple ML model output specification. This parameter specifies the index of the output specification in the ``mlm:output`` array to use for prediction or training. As ``mlm:output`` is an array, the first output in the array has index 0.",
      schema = list(type = "integer", default = 0),
      optional = TRUE
    )
    
    
  ),
  
  returns = list(
    description = "A machine learning model to be used with machine learning processes such as ``ml_predict()``.", 
    schema = list(type = "object", subtype = "ml-model")
  ),
  
  operation = function(uri, model_asset = NULL, input_index = 0, output_index = 0, job) {
    
    read_stac_item <- function(uri_or_json) {
      if (is.character(uri_or_json) && length(uri_or_json) == 1) {
        trimmed <- trimws(uri_or_json)
        if (startsWith(trimmed, "{")) {
          return(jsonlite::fromJSON(trimmed, simplifyVector = FALSE))
        }
      }
      jsonlite::fromJSON(uri_or_json, simplifyVector = FALSE)
    }
    
    pick_mlm_model_asset <- function(item, model_asset = NULL) {
      message("sind in der pick_mlm_assets")
      assets <- item$`assets`
      message("assets json: ", jsonlite::toJSON(assets, auto_unbox = TRUE))
      message("assets sind hier: ", assets)
      if (!is.list(assets) || length(assets) == 0)
        {stop("STAC Item has no assets.")} 
      
      if (!is.null(model_asset)) {
        model_a <- assets[[model_asset]]
        if (is.null(model_a)) stop(sprintf("model_asset '%s' not found in assets.", model_asset))
        if (is.null(model_a$roles) || !("mlm:model" %in% model_a$roles)) {
          stop(sprintf("Asset '%s' does not have role 'mlm:model'.", model_asset))
        }
        return(list(name = model_asset, asset = model_a))
      }
      
      candidates <- names(Filter(function(model_a) !is.null(model_a$roles) && ("mlm:model" %in% model_a$roles), assets))
      if (length(candidates) == 0) stop("No asset with role 'mlm:model' found.")
      if (length(candidates) > 1){stop("Multiple assets with role 'mlm:model' found. Please set model_asset.")
      }
      
      list(name = candidates[[1]], asset = assets[[candidates[[1]]]])
    }
    
    is_url <- function(x) is.character(x) && length(x) == 1 && grepl("^https?://", x)
    
    extract_gdrive_id <- function(url) {
      m <- regmatches(url, regexec("/file/d/([^/]+)", url))[[1]]
      if (length(m) >= 2) return(m[2])
      
      m <- regmatches(url, regexec("[?&]id=([^&]+)", url))[[1]]
      if (length(m) >= 2) return(m[2])
      
      m <- regmatches(url, regexec("/uc\\?.*id=([^&]+)", url))[[1]]
      if (length(m) >= 2) return(m[2])
      
      NULL
    }
    
    gdrive_direct_url <- function(url) {
      id <- extract_gdrive_id(url)
      if (is.null(id)) return(url)
      paste0("https://drive.google.com/uc?export=download&id=", id)
    }
    
    download_file_http <- function(url, destfile) {
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      
      resp1 <- httr::GET(url, httr::write_disk(destfile, overwrite = TRUE))
      httr::stop_for_status(resp1)
      
      ct <- httr::headers(resp1)[["content-type"]]
      if (!is.null(ct) && grepl("text/html", ct, ignore.case = TRUE)) {
        html <- paste(readLines(destfile, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        m <- regmatches(html, regexec("confirm=([0-9A-Za-z_\\-]+)", html))[[1]]
        if (length(m) >= 2) {
          token <- m[2]
          url2 <- paste0(url, "&confirm=", token)
          resp2 <- httr::GET(url2, httr::write_disk(destfile, overwrite = TRUE))
          httr::stop_for_status(resp2)
        } else {
          stop("Downloaded HTML instead of model file. Google Drive link may not be publicly accessible or needs confirmation token.")
        }
      }
      
      return(destfile)
    }

    
    materialize_model_asset <- function(href, out_dir, filename = NULL) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      if (is.null(filename) || !nzchar(filename)) {
        clean_href <- sub("\\?.*$", "", href)     
        filename <- basename(clean_href)
      }

      if (!nzchar(filename)) {
        stop("Cannot derive a filename from href. Please provide a filename or ensure href ends with a filename.")
      }

      ext <- tools::file_ext(filename)
      if (!nzchar(ext)) {
        stop(sprintf(
          "Asset href does not contain a file extension ('%s'). Please provide an explicit filename with extension in the STAC asset href (or add metadata so it can be derived).",
          href
        ))
      }

      dest <- file.path(out_dir, filename)
      
      if (is_url(href)) {
        if (grepl("drive\\.google\\.com", href)) {
          href <- gdrive_direct_url(href)
        }
        return(download_file_http(href, dest))
      }
      
      if (grepl("^file://", href)) href <- sub("^file://", "", href)
      if (!file.exists(href)) stop(sprintf("Local asset file not found: %s", href))
      file.copy(href, dest, overwrite = TRUE)
      dest
    }
    
    
    shared_dir <- Sys.getenv("SHARED_TEMP_DIR", "")
    if (shared_dir == "") {
      shared_dir <- file.path(getwd(), "shared_temp")
    }
    if (!dir.exists(shared_dir)) {
      dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
    }

    make_filename <- function(asset) {
      base <- if (!is.null(asset$title) && nzchar(asset$title)) {
        gsub("[^A-Za-z0-9._-]+", "_", asset$title)
      } else {
        "model"
      }

      art <- asset$`mlm:artifact_type`
      if (!is.null(art) && grepl("RDS", art, ignore.case = TRUE)) return(paste0(base, ".rds"))
      if (!is.null(art) && grepl("ONNX", art, ignore.case = TRUE)) return(paste0(base, ".onnx"))

      stop("Cannot determine model file extension. Provide an href with extension or add file name metadata.")
    }
    
    item <- read_stac_item(uri)
    properties <- item$properties
    message(item)
    message("input = ", jsonlite::toJSON(properties$`mlm:input`, auto_unbox = TRUE))
    message("output = ", jsonlite::toJSON(properties$`mlm:output`, auto_unbox = TRUE))
    
    
    if (is.null(properties$`mlm:input`) || is.null(properties$`mlm:output`)) { stop("STAC Item missing 'mlm:input' or 'mlm:output' (MLM extension not present?).") }
    
    
    if(!is.null(model_asset)){
      sel <- pick_mlm_model_asset(item, model_asset)
    }else{
      sel <- pick_mlm_model_asset(item)
    }
    
    asset_name <- sel$name
    message(asset_name)
    asset <- sel$asset
    if (is.null(asset$href)) stop(sprintf("Asset '%s' has no href.", asset_name))
    model_id <- if (!is.null(item$id) && nzchar(item$id)) item$id else "stac_model"
    model_dir <- file.path(shared_dir, "ml_models", model_id)
   
    filename <- make_filename(asset)
    model_path <- materialize_model_asset(asset$href, model_dir, filename = filename)
    message("model path: ", model_path)


    get_indexed <- function(x, idx, what) {
      if (!is.list(x)) stop(sprintf("'%s' must be an array/list.", what))
      i <- as.integer(idx) + 1L
      if (i < 1L || i > length(x)) stop(sprintf("'%s' index %d out of range.", what, idx))
      x[[i]]
    }
    input_spec <- get_indexed(properties$`mlm:input`,  input_index,  "mlm:input")
    output_spec <- get_indexed(properties$`mlm:output`, output_index, "mlm:output")
    
    message("input_spec: ", jsonlite::toJSON(input_spec, auto_unbox = TRUE))
    message("output_spec: ", jsonlite::toJSON(output_spec, auto_unbox = TRUE))
    
    
    list(
      subtype = "stac-mlm-model",
      stac_item = properties,
      model_asset = asset_name,
      path = model_path,
      input_index = input_index,
      output_index = output_index,
      input_spec = input_spec,
      output_spec = output_spec
    )
    
    
  })

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
#' Load an ML model or TerraTorch config (asynchronous subprocess)
#'
#' @description
#' Lädt ein ML/DL-Modell aus
#' - URL oder lokalem Pfad (.rds, .onnx, .pt) oder
#' - einer YAML-Konfiguration für TerraTorch (Pfad zu .yaml oder YAML-Text).
#'
#' Regeln:
#' - .rds -> R-Objekt (classic / torch / caret)
#' - .onnx -> raw bytes (wird in ml_predict() zu Datei geschrieben)
#' - .pt -> Pfad zur Torch-Datei (ml_predict lädt selbst)
#' - .ckpt -> **nicht erlaubt** (Fehler; bitte YAML mit paths.ckpt_pfad verwenden)
#' - YAML -> TerraTorch-Descriptor mit CKPT
#'     * wenn paths.ckpt_pfad != null: CKPT wird aus URL/Pfad nach SHARED_TEMP_DIR geladen
#'     * wenn paths.ckpt_pfad == null: generic_terra.py trainiert CKPT mit paths.data_pfad
#'
load_ml_model <- Process$new(
  id = "load_ml_model",
  description = paste(
    "Lädt ein ML-/DL-Modell aus URL/Pfad, eine TerraTorch-YAML-Konfiguration",
    "oder eine STAC-MLM-GeoJSON-Konfiguration.",
    "Unterstützt: .rds, .onnx, .pt, YAML (mt_crop/burnscars/etc.) und STAC-MLM.",
    ".ckpt darf NICHT direkt übergeben werden – dafür YAML oder STAC-MLM verwenden."
  ),
  categories = as.array(c("model-management", "data-loading")),
  summary = "Lädt ein Modell oder eine TerraTorch-/STAC-MLM-Beschreibung (inkl. Training bei Bedarf).",
  
  parameters = list(
    Parameter$new(
      name = "url",
      description = paste(
        "String, der entweder ist:",
        "- eine HTTP/HTTPS-URL oder ein lokaler Pfad zu .rds/.onnx/.pt, oder",
        "- eine YAML-Konfiguration (Pfad zu .yaml oder YAML-Text wie config_mt_crop_*.yaml), oder",
        "- eine STAC-MLM-GeoJSON-Datei (lokal oder per HTTP/HTTPS).",
        "Direkte .ckpt-Pfade/-URLs sind verboten – dafür bitte YAML oder STAC-MLM verwenden."
      ),
      schema = list(type = "string")
    )
    
    
  ),
  
  returns = list(
    description = "A machine learning model to be used with machine learning processes such as ``ml_predict()``.", 
    schema = list(type = "object", subtype = "ml-model")
  ),
  
  operation = function(url, job) {
    library(tools)
    library(httr2)
    library(yaml)
    library(jsonlite)  
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    norm  <- function(x) trimws(as.character(x))
    is_http <- function(x) is.character(x) && grepl("^https?://", x, ignore.case = TRUE)
    is_file_url <- function(x) is.character(x) && grepl("^file://", x, ignore.case = TRUE)
    looks_like_path <- function(x) is.character(x) && !is_http(x) && !is_file_url(x)
    
    shared_dir <- Sys.getenv("SHARED_TEMP_DIR", "")
    if (shared_dir == "") {
      shared_dir <- file.path(getwd(), "shared_temp")
    }
    if (!dir.exists(shared_dir)) {
      dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    map_internal_download <- function(u) {
      if (is.character(u) && grepl("^http://localhost:8000/download/", u)) {
        file_name <- basename(u)
        file_path <- file.path(shared_dir, file_name)
        if (!startsWith(normalizePath(file_path), normalizePath(shared_dir))) {
          stop("Access outside the permitted directory!")
        }
        if (!file.exists(file_path)) stop("File not found in SHARED_TEMP_DIR: ", file_path)
        return(file_path)
      }
      NULL
    }
    
    normalize_gdrive <- function(u) {
      if (!grepl("drive\\.google\\.com", u)) return(u)
      m <- regmatches(u, regexec("drive\\.google\\.com/file/d/([^/?]+)", u))[[1]]
      if (length(m) > 1) {
        u2 <- paste0("https://drive.google.com/uc?export=download&id=", m[2])
        message("Google Drive normalized: ", u2)
        return(u2)
      }
      stop("Could not extract a valid Google Drive file ID.")
    }
    
    download_to_shared <- function(u) {
      if (is_file_url(u)) {
        p <- sub("^file://", "", u)
        if (!file.exists(p)) stop("file:// path not found: ", p)
        return(normalizePath(p))
      }
      
      p_int <- map_internal_download(u)
      if (!is.null(p_int)) return(normalizePath(p_int))
      
      u2 <- normalize_gdrive(u)
      
      if (grepl("dropbox.com", u2, ignore.case = TRUE)) {
        url_parts <- url_parse(u2)
        url_parts$query$dl <- "1"
        u2 <- url_build(url_parts)
        message("Dropbox-URL normalisiert: ", u2)
      }
      
      resp <- request(u2) |> req_perform()
      
      if (resp_status(resp) != 200) {
        stop("HTTP error: ", resp_status(resp))
      }
      
      hdr <- resp_headers(resp)
      cd  <- hdr[["content-disposition"]]
      
      file_name <- basename(u2)
      if (!is.null(cd)) {
        m <- regmatches(cd, regexec('filename="?([^";]+)"?', cd))[[1]]
        if (length(m) > 1) file_name <- m[2]
      }
      
      file_name <- sub("\\?.*$", "", file_name)
      
      out <- file.path(shared_dir, file_name)
      writeBin(resp$body, out)
      
      normalizePath(out)
    }
    
    
    download_ckpt <- function(u) {
      if (looks_like_path(u) && file.exists(u)) {
        p <- normalizePath(u)
        if (!endsWith(tolower(p), ".ckpt"))
          stop("paths.ckpt_path must end with .ckpt: ", p)
        return(p)
      }
      
      if (is_file_url(u)) {
        p <- sub("^file://", "", u)
        if (!file.exists(p)) stop("file:// path not found: ", p)
        if (!endsWith(tolower(p), ".ckpt"))
          stop("file:// .ckpt path invalid: ", p)
        return(normalizePath(p))
      }
      
      p_int <- map_internal_download(u)
      if (!is.null(p_int)) {
        p_int <- normalizePath(p_int)
        if (!endsWith(tolower(p_int), ".ckpt"))
          stop("Internal CKPT path does not end with .ckpt")
        return(p_int)
      }
      
      if (!is_http(u)) {
        stop("paths.ckpt_path must be a local path, file:// or HTTP/HTTPS: ", u)
      }

      u2 <- normalize_gdrive(u)
      
      if (grepl("dropbox.com", u2, ignore.case = TRUE)) {
        url_parts <- url_parse(u2)
        url_parts$query$dl <- "1"
        u2 <- url_build(url_parts)
        message(" Dropbox-CKPT-URL normalisiert: ", u2)
      }
      
      resp <- request(u2) |> req_perform()
      
      if (resp_status(resp) != 200)
        stop("HTTP error during CKPT download: ", resp_status(resp))
      
      hdr <- resp_headers(resp)
      cd  <- hdr[["content-disposition"]]
      
      file_name <- NULL
      if (!is.null(cd)) {
        m <- regmatches(cd, regexec('filename="?([^";]+)"?', cd))[[1]]
        if (length(m) > 1) file_name <- m[2]
      }
      
      if (is.null(file_name)) file_name <- basename(u2)
      file_name <- sub("\\?.*$", "", file_name)
      
      if (!grepl("\\.ckpt$", tolower(file_name)))
        file_name <- paste0(file_name, ".ckpt")
      
      out <- file.path(shared_dir, file_name)
      writeBin(resp$body, out)
      
      if (!endsWith(tolower(out), ".ckpt"))
        stop("Saved CKPT file does not end with .ckpt")
      
      normalizePath(out)
    }
    
    
    
    detect_type_from_ext <- function(ext) {
      ext <- tolower(ext)
      if (ext %in% "rds")  return("rds")
      if (ext %in% "onnx") return("onnx")
      if (ext %in% "pt")   return("pt")
      if (ext %in% "ckpt") return("ckpt")
      if (ext %in% c("yaml","yml")) return("yaml")
      "unknown"
    }
    
    is_yaml_text <- function(x) {
      if (!is.character(x) || length(x) != 1L) return(FALSE)
      if (is_http(x) || is_file_url(x)) return(FALSE)
      if (file.exists(x)) return(FALSE)
      if (!grepl(":", x)) return(FALSE)
      if (!grepl("experiment_name\\s*:", x) && !grepl("paths\\s*:", x)) return(FALSE)
      out <- tryCatch(yaml::yaml.load(x), error = function(e) NULL)
      is.list(out) && !is.null(out)
    }
    
    load_yaml_cfg <- function(id) {
      if (file.exists(id) && grepl("\\.ya?ml$", id, ignore.case = TRUE)) {
        cfg <- yaml::read_yaml(id)
        cfg_path <- normalizePath(id)
      } else {
        cfg <- yaml::yaml.load(id)
        cfg_path <- tempfile(fileext = ".yaml")
        writeLines(id, cfg_path)
      }
      list(cfg = cfg, cfg_path = cfg_path)
    }
    
    make_terratorch_descriptor <- function(cfg, cfg_path = NA_character_, ckpt_local = NULL) {
      `%||%` <- function(x, y) if (is.null(x)) y else x
      
      dcfg <- cfg$dataset %||% list()
      mcfg <- cfg$model   %||% list()
      
      bands_yaml <- dcfg$bands
      if (is.null(bands_yaml) || length(bands_yaml) == 0L) {
        stop("dataset$bands must be set in the configuration (e.g., [BLUE, GREEN, RED, ...] or [B02, B03, ...]).")
      }
      
      expected_bands <- as.character(bands_yaml)
      backbone_bands <- expected_bands
      
      structure(list(
        type = "terratorch-ckpt",
        backbone = mcfg$backbone %||% "terratorch_prithvi_eo_v2_100_tl",
        num_frames = as.integer(dcfg$num_frames %||% 1L),
        bands = expected_bands,
        backbone_bands = backbone_bands,
        num_classes = as.integer(mcfg$num_classes %||% 13L),
        ckpt = ckpt_local,
        backbone_pt = NULL,
        factory = mcfg$factory %||% "EncoderDecoderFactory",
        decoder = mcfg$decoder %||% "UNetDecoder",
        neck_indices = mcfg$neck_indices %||% c(2L, 5L, 8L, 11L),
        decoder_channels= mcfg$decoder_channels %||% c(512L, 256L, 128L, 64L),
        head_dropout = mcfg$head_dropout %||% 0.1,
        loss = mcfg$loss %||% "ce",
        optimizer = mcfg$optimizer %||% "AdamW",
        freeze_backbone = mcfg$freeze_backbone %||% TRUE,
        freeze_decoder = mcfg$freeze_decoder  %||% FALSE,
        config = cfg,
        config_path = cfg_path
      ), class = "mlm-model")
    }
    
    
    is_stac_mlm_file <- function(path) {
      if (!file.exists(path)) return(FALSE)
      if (!grepl("\\.geo?json$", path, ignore.case = TRUE) &&
          !grepl("\\.json$", path, ignore.case = TRUE)) {
        return(FALSE)
      }
      x <- tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(x) || is.null(x$properties)) return(FALSE)
      props <- x$properties
      any(startsWith(names(props), "mlm:"))
    }
    
    stac_mlm_to_cfg <- function(path) {
      x <- jsonlite::read_json(path, simplifyVector = TRUE)
      props <- x$properties %||% list()
      
      cfg <- list(
        experiment_name = props[["mlm:experiment_name"]] %||% x$id %||% "stac_mlm_experiment",
        paths = props[["mlm:paths"]] %||% list(),
        dataset = props[["mlm:dataset"]] %||% list(),
        model = props[["mlm:model"]] %||% list(),
        train = props[["mlm:train"]] %||% list(),
        inference = props[["mlm:inference"]] %||% list()
      )
      
      cfg
    }
    
    url <- norm(url)
    if (is.na(url) || url == "") {
      stop("The ‘url’ parameter must not be empty.")
    }
    
    
    use_yaml <- FALSE
    use_stac <- FALSE
    cfg <- NULL
    cfg_path <- NA_character_
    
    if (file.exists(url) && is_stac_mlm_file(url)) {
      use_stac <- TRUE
      cfg <- stac_mlm_to_cfg(url)
      cfg_path <- normalizePath(url)
      message("Detected STAC-MLM configuration (lokale Datei) – TerraTorch-Pipeline.")
    }
    
    if (!use_stac && (is_http(url) || is_file_url(url))) {
      tmp <- tryCatch(download_to_shared(url), error = function(e) NULL)
      if (!is.null(tmp) && is_stac_mlm_file(tmp)) {
        use_stac <- TRUE
        cfg <- stac_mlm_to_cfg(tmp)
        cfg_path <- tmp
        message("Detected STAC-MLM configuration (Remote-URL) – TerraTorch-Pipeline.")
      }
    }
    
    if (!use_stac) {
      if (file.exists(url) && grepl("\\.ya?ml$", url, ignore.case = TRUE)) {
        use_yaml <- TRUE
      } else if (is_yaml_text(url)) {
        use_yaml <- TRUE
      }
      
      destfile
    }
    
    if (use_yaml) {
      message("Detected YAML configuration – TerraTorch-Pipeline.")
      y <- load_yaml_cfg(url)
      cfg <- y$cfg
      cfg_path <- y$cfg_path
    }
    
    if (use_yaml || use_stac) {
      exp_name <- cfg$experiment_name %||% "experiment"
      paths_cfg <- cfg$paths %||% list()
      icfg <- cfg$inference %||% list()
      
      terra_base <- file.path(shared_dir, "terra_work")
      if (!dir.exists(terra_base)) {
        dir.create(terra_base, recursive = TRUE, showWarnings = FALSE)
      }
      work_root <- file.path(terra_base, exp_name)
      ckpt_dir <- file.path(work_root, "output", "checkpoints")
      if (!dir.exists(ckpt_dir)) {
        dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      ckpt_pfad  <- paths_cfg$ckpt_pfad %||% NULL
      
      if (!is.null(ckpt_pfad)) {
        msg_prefix <- if (use_yaml) "[YAML]" else "[STAC-MLM]"
        message(msg_prefix, " paths.ckpt_path specified: ", ckpt_pfad)
        ckpt_local <- download_ckpt(ckpt_pfad)
        if (!endsWith(tolower(ckpt_local), ".ckpt")) {
          stop("paths.ckpt_path must end with .ckpt: ", ckpt_local)
        }
        desc <- make_terratorch_descriptor(cfg, cfg_path, ckpt_local)
        message(msg_prefix, " Using external CKPT: ", ckpt_local)
        return(desc)
      }
      
      data_pfad <- paths_cfg$data_pfad %||% NULL
      if (is.null(data_pfad)) {
        stop(if (use_yaml) {
          "YAML does not contain paths.ckpt_path or paths.data_path – at least data_path is required for training."
        } else {
          "STAC-MLM does not contain paths.ckpt_path or paths.data_path – at least data_path is required for training."
        })
      }
      
      terra_script <- Sys.getenv(
        "TERRATORCH_SCRIPT",
        file.path("Python", "terra_for_R.py")
      )
      
      terra_python <- Sys.getenv("TERRATORCH_PYTHON", "python")
      
      message("[TerraTorch] Use Python-Binary: ", terra_python)
      message("[TerraTorch] Use TerraTorch-Skript: ", normalizePath(terra_script))

      py_which <- Sys.which(terra_python)
      message("[TerraTorch] Sys.which('", terra_python, "') = ", ifelse(py_which == "", "<nicht gefunden>", py_which))
      
      py_info <- tryCatch(
        system2(terra_python, args = "--version", stdout = TRUE, stderr = TRUE),
        error = function(e) paste("Error when querying Python --version: ", e$message)
      )
      
      message("[TerraTorch] Python --version:\n", paste(py_info, collapse = "\n"))
      
      if (!file.exists(terra_script)) {
        stop("generic_terra.py not found. Please set ENV TERRATORCH_SCRIPT or adjust the path in the code:", terra_script)
      }
      
      msg_prefix <- if (use_yaml) "[YAML]" else "[STAC-MLM]"
      message(msg_prefix, " No CKPT specified – start Python training with generic_terra.py ...")
      cmd_args <- c(
        shQuote(terra_script),
        "--config", shQuote(cfg_path),
        "--work-dir", shQuote(terra_base)
      )
      status <- system2("python", args = cmd_args, stdout = "", stderr = "")
      if (!identical(status, 0L)) {
        stop("generic_terra.py returned with status ", status, ".")
      }
      
      ckpt_name  <- icfg$ckpt_filename %||% "best.ckpt"
      ckpt_local <- file.path(ckpt_dir, ckpt_name)
      if (!file.exists(ckpt_local)) {
        candidates <- list.files(ckpt_dir, pattern = "\\.ckpt$", full.names = TRUE)
        if (!length(candidates)) {
          stop("No CKPT found in ", ckpt_dir, " after training.")
        }
        ckpt_local <- candidates[which.max(file.info(candidates)$mtime)]
      }
      
      message(msg_prefix, " Training ready, CKPT: ", ckpt_local)
      
      desc <- make_terratorch_descriptor(cfg, cfg_path, ckpt_local)
      return(desc)
    }
    
    local_file <- NULL
    if (looks_like_path(url) && file.exists(url)) {
      local_file <- normalizePath(url)
    } else if (is_http(url) || is_file_url(url)) {
      local_file <- download_to_shared(url)
    } else {
      if (looks_like_path(url)) stop("Path does not exist: ", url)
      stop("Unsupported URL/ID: ", url)
    }
    
    ext <- tolower(file_ext(local_file))
    mtype <- detect_type_from_ext(ext)
    message("Detected file: ", local_file, " (type=", mtype, ")")
    
    if (identical(mtype, "ckpt")) {
      stop("Direct .ckpt files are not allowed. Please use a YAML or STAC-MLM configuration with paths.ckpt_path or paths.data_path.")
    }
    
    if (identical(mtype, "rds")) {
      model <- readRDS(local_file)
      message("RDS model loaded (class: ", paste(class(model), collapse = ","), ").")
      return(model)
    }
    
    if (identical(mtype, "onnx")) {
      raw_data <- readBin(local_file, "raw", n = file.info(local_file)$size)
      message("ONNX model loaded as raw bytes (", length(raw_data), " bytes).")
      return(raw_data)
    }
    
    if (identical(mtype, "pt")) {
      message("Torch .pt file – returning path for ml_predict(): ", local_file)
      return(local_file)
    }
    
    stop("Unknown or unsupported file type: ", ext, " (", local_file, "). ", "Allowed are .rds, .onnx, .pt, YAML (Path/Text) or STAC-MLM-GeoJSON.")
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
      py <- Sys.which("python");  if (nzchar(py))  return(py)
      stop("No suitable Python interpreter found!")
    }
    
    ensure_extension <- function(filename, ext) {
      if (!grepl(paste0("\\.", ext, "$"), filename, ignore.case = TRUE)) paste0(filename, ".", ext) else filename
    }
    
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    is_caret_svm <- function(x) {
      inherits(x, "train") && inherits(x$finalModel, "ksvm")
    }
    
    detect_svm_kernel <- function(train_obj) {
      stopifnot(is_caret_svm(train_obj))
      kobj <- train_obj$finalModel@kernelf
      kcl <- tolower(class(kobj))
      message("kernlab kernel classes: ", paste(kcl, collapse = ", "))
      if (any(kcl %in% c("vanilladot", "vanillakernel", "lineardot", "linear"))) return("linear")
      if (any(kcl %in% c("rbfdot", "rbfkernel", "gaussiandot", "gaussian"))) return("rbf")
      if (any(kcl %in% c("polydot", "polykernel", "poly"))) return("poly")
      ktxt <- paste(kcl, collapse = " ")
      if (grepl("vanilla|linear", ktxt)) return("linear")
      if (grepl("rbf|gauss|radial", ktxt)) return("rbf")
      if (grepl("poly", ktxt)) return("poly")
      stop("Unsupported/unknown SVM kernel (kernlab). Class vector = ", paste(class(kobj), collapse="/"))
    }
    



    detect_model_type <- function(model) {
      fm <- model$finalModel
      
      kcls_vec <- try(class(fm@kernelf), silent = TRUE)
      
      if (inherits(fm, "ksvm")) {
        kern <- try(detect_svm_kernel(model), silent = TRUE)
        if (!inherits(kern, "try-error") && nzchar(kern)) {
          return(switch(tolower(kern),
                        linear = "svmLinear",
                        rbf = "svmRadial",
                        poly = "svmPoly",
                        "svmLinear"  
          ))
        }
        
        kcls <- tolower(if (!inherits(kcls_vec, "try-error")) kcls_vec[1] else "")
        if (kcls %in% c("vanilladot","vanillakernel","lineardot","linear")) return("svmLinear")
        if (kcls %in% c("rbfdot","rbfkernel","gaussiandot","gaussian")) return("svmRadial")
        if (kcls %in% c("polydot","polykernel","poly")) return("svmPoly")
        ktxt <- tolower(paste(kcls_vec, collapse = " "))
        if (grepl("vanilla|linear", ktxt)) return("svmLinear")
        if (grepl("rbf|gauss|radial", ktxt)) return("svmRadial")
        if (grepl("poly", ktxt)) return("svmPoly")
        warning("Unknown ksvm kernel (", ktxt, "); setting 'svmLinear' as fallback.")
        return("svmLinear")
      }
      
      safe_text <- paste(c(model$modelInfo$label %||% "", model$method %||% ""), collapse = " ")
      has <- function(pat) isTRUE(grepl(pat, safe_text, ignore.case = TRUE))
      if (has("random[ ]*forest|^rf$")) return("random_forest")
      if (has("xgboost|xgb.Booster|xgb|gradient[ ]*boost")) return("xgbTree")
      
      stop("Model type could not be recognized (finalModel class: ",
           paste(class(fm), collapse = "/"),
           ", method: ", model$method %||% "?", ").")
    }
    
    
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
    export_ksvm_linear_onnx <- function(train_obj, out_base, use_rule = "majority", primary_output = "idx1", dtype = "float32", do_checks = TRUE) {
      stopifnot(inherits(train_obj, "train"), inherits(train_obj$finalModel, "ksvm"))
      
      model_r <- train_obj
      model <- model_r$finalModel
      
      classes <- as.character(model_r$levels)
      K <- length(classes)                  
      feat <- setdiff(colnames(model_r$trainingData), ".outcome")
      pp <- model_r$preProcess
      means <- as.numeric(pp$mean[feat])
      names(means) <- feat
      sds <- as.numeric(pp$std[feat])
      names(sds) <- feat
      
      Xscaled <- as.matrix(model_r$trainingData[, feat, drop = FALSE])
      storage.mode(Xscaled) <- "double"
      y_true <- as.character(model_r$trainingData$.outcome)
      #library(kernlab)

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
      X_aug <- cbind(Xscaled, 1.0)
      storage.mode(X_aug) <- "double"
      n_feat <- ncol(Xscaled)

      ls_fit <- function(y, X) as.numeric(qr.solve(X, y))
      weights_from_dec <- vector("list", C2)
      for (j in 1:C2) {
        th <- ls_fit(dec[, j], X_aug)
        weights_from_dec[[j]] <- list(W = th[1:n_feat], b = th[n_feat + 1])
      }
      all_pairs <- combn(classes, 2, simplify = FALSE)   
      score_mat <- matrix(0, nrow = C2, ncol = C2)
      for (p in seq_along(all_pairs)) {
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        t_ab <- numeric(nrow(Xscaled))
        t_ab[y_true == a] <-  1
        t_ab[y_true == b] <- -1
        sc <- suppressWarnings(apply(dec, 2, function(z) abs(cor(z, t_ab, use="complete.obs"))))
        sc[!is.finite(sc)] <- 0
        score_mat[, p] <- sc  
      }
      cost <- 1 - score_mat
      if(!is.matrix(cost)) stop("cost is not a matrix")
      if(length(dim(cost)) != 2) stop("cost is not 2-dimensional")
      if(nrow(cost) != C2 || ncol(cost) != C2) stop("cost matrix has incorrect dimensions")
      if (nrow(cost) != ncol(cost)) stop("cost must be square", call.=FALSE)


      assign_j_to_p <- tryCatch(
        as.integer(clue::solve_LSAP(cost)),
        error = function(e) {
          msg <- paste0(
            "solve_LSAP failed for assigning decision functions to class pairs\n",
            "cost dim = ", paste(dim(cost), collapse = "x"), "\n",
            "underlying error = ", conditionMessage(e)
          )
          message(msg)
          stop(msg, call. = FALSE)
        }
      )

      pair_name <- function(ci, cj) paste0(ci, "_vs_", cj)

      weights_aligned <- vector("list", C2)
      posneg_pairs <- vector("list", C2)  
      for (j in 1:C2) {
        p <- assign_j_to_p[j]
        a <- all_pairs[[p]][1]; b <- all_pairs[[p]][2]
        m_a <- mean(dec[y_true == a, j], na.rm = TRUE)
        m_b <- mean(dec[y_true == b, j], na.rm = TRUE)
        if (m_a >= m_b) { ci <- a; cj <- b } else { ci <- b; cj <- a }
        wj <- weights_from_dec[[j]]
        weights_aligned[[j]] <- c(wj, list(class_pair = c(ci, cj), name = pair_name(ci, cj)))
        posneg_pairs[[j]] <- c(ci, cj)
      }

      Z_hat <- sapply(seq_len(C2), function(j) as.numeric(Xscaled %*% weights_aligned[[j]]$W + weights_aligned[[j]]$b))
      cors  <- diag(abs(cor(Z_hat, dec)))
      cat("Min/Median/Max |cor(Z_hat, dec)|: ",
          sprintf("%.6f / %.6f / %.6f\n", min(cors), median(cors), max(cors)))
      stopifnot(all(cors > 0.999))
      
      
      pred_from_dec <- function(dec_rowmat, classes, posneg_pairs, rule = c("majority","margin","signedsum")) {
        rule <- match.arg(rule)
        K <- length(classes); C2 <- length(posneg_pairs)
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
        df <- as.data.frame(t(x_raw)); colnames(df) <- feat
        as.character(predict(model_r, newdata = df))
      })
      
      pred_major <- pred_from_dec(dec[rows, , drop=FALSE], classes, posneg_pairs, "majority")
      pred_margin <- pred_from_dec(dec[rows, , drop=FALSE], classes, posneg_pairs, "margin")
      pred_signed <- pred_from_dec(dec[rows, , drop=FALSE], classes, posneg_pairs, "signedsum")
      
      cat("\n=== comparison of rules (", length(rows), " Samples) ===\n", sep = "")
      cat("caret vs majority : ", mean(pred_caret == pred_major),  "\n")
      cat("caret vs margin   : ", mean(pred_caret == pred_margin), "\n")
      cat("caret vs signedsum: ", mean(pred_caret == pred_signed), "\n")
      
      use_rule <- "majority"
      
      onnx_path <- paste0(out_base, "_svm_ovo_linear_", use_rule, ".onnx")
      labels_json <- sub("\\.onnx$", "_labels.json", onnx_path)
      
      build_onnx_for_rule(
        use_rule = use_rule,
        weights_aligned = weights_aligned,
        feature_names = feat,
        classes = classes,
        means = means,
        sds = sds,
        out_path = onnx_path
      )
      
      lbls <- setNames(as.list(classes), as.character(seq_along(classes)))
      jsonlite::write_json(lbls, labels_json, pretty = TRUE, auto_unbox = TRUE)
      
      list(onnx = onnx_path, labels_json = labels_json)
    }
    
    
    
    
    
    
    # --------- POLY ----------
    export_ksvm_poly_onnx <- function(train_obj, out_base, use_rule = "majority", primary_output = "idx1", dtype = "float32", do_checks = TRUE) {
      message("Export SVM Poly start...")
      model_r <- train_obj
      model_ksvm <- model_r$finalModel              
      
      classes <- as.character(model_r$levels)
      feature_names <- setdiff(colnames(model_r$trainingData), ".outcome")
      
      pp <- model_r$preProcess
      means <- as.numeric(pp$mean[feature_names]); names(means) <- feature_names
      sds <- as.numeric(pp$std[feature_names]);  names(sds) <- feature_names
      
      Xraw_train <- as.data.frame(model_r$trainingData[, feature_names, drop = FALSE])
      Xscaled <- as.matrix(predict(pp, Xraw_train))
      storage.mode(Xscaled) <- "double"
      colnames(Xscaled) <- feature_names 
      
      K <- length(classes)
      C2 <- K * (K - 1) / 2
      stopifnot(C2 >= 1)
      library(kernlab)
      kp <- kernlab::kpar(model_ksvm@kernelf)
      deg <- as.numeric(kp$degree)
      scaleK <- as.numeric(kp$scale)
      offK <- as.numeric(kp$offset)
      
      #library(kernlab)
      get_decision_matrix <- function(model, Xscaled) {
        dec <- try(predict(model, Xscaled, type = "decision"), silent = TRUE)
        if (!inherits(dec, "try-error")) return(as.matrix(dec))
        as.matrix(kernlab::decision(model, Xscaled))
      }
      dec_raw <- get_decision_matrix(model_ksvm, Xscaled)
      stopifnot(ncol(dec_raw) == C2)
      
      pv <- try(predict(model_ksvm, Xscaled, type = "votes"), silent = TRUE)
      if (inherits(pv, "try-error")) stop("This ksvm does not provide any ‘votes’ – cancel.")
      rn <- rownames(pv); if (is.null(rn) || any(!nzchar(rn))) rn <- classes
      stopifnot(length(rn) == K)
      cat("C2 = ", C2, " | range(colSums(pv)) = ", paste(range(colSums(pv)), collapse=".."), "\n", sep="")
      
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
      cat("→ Use tie role: ", tie_rule, "\n", sep="")
      
      
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
      cat(sprintf("Greedy-ends: L1=%.6f (Iterationen=%d)\n", l1_cur, iter))
      
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
        pos_class_k <- best$pos
        neg_class_k <- best$neg
        l1_cur <- best$l1
        cat(sprintf("Swap k=%d<->k=%d  → L1=%.12f\n", best$k1, best$k2, l1_cur))
        if (l1_cur < 1e-12 || iter_sw > 200) break
      }
      cat(sprintf("2-Swap ends: L1=%.12f (Iterationen=%d)\n", l1_cur, iter_sw))
      
      
      our_votes <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule)
      l1_final <- mean(colSums(abs(our_votes - pv)))
      acc_final <- mean(rn[apply(our_votes, 2, which.max)] == rn[apply(pv, 2, which.max)])
      cat(sprintf("Votes-Check (Tie %s): L1=%.12f | Top1-Acc=%.3f\n", tie_rule, l1_final, acc_final))
      stopifnot(l1_final < 1e-12, acc_final >= 0.999)
      
      
      ai_list <- kernlab::alphaindex(model_ksvm)     
      stopifnot(length(ai_list) == C2)
      
      Kmats <- vector("list", C2)
      SVs <- vector("list", C2)
      for (j in 1:C2) {
        ai <- suppressWarnings(as.integer(ai_list[[j]]))
        stopifnot(length(ai) >= 1L,
                  all(is.finite(ai)),
                  all(ai >= 1L & ai <= nrow(Xscaled)))
        SVs[[j]] <- Xscaled[ai, , drop = FALSE]
        Kmats[[j]] <- kernlab::kernelMatrix(model_ksvm@kernelf, Xscaled, SVs[[j]])  # [N x m_j]
      }
      
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
        
        if (cr < 0) { a <- -a; b <- -b; z <- -z; cr <- -cr }
        
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
            delta <- mean(c(Tpos, Tneg))
          }
        }
        b2 <- b + delta
        z2 <- z + delta
        cr2 <- suppressWarnings(cor(z2, y, use="complete.obs")); if (!is.finite(cr2)) cr2 <- cr
        
        list(alpha=a, b=b2, z=z2, corr=abs(cr2))
      }
      
      corr_mat <- matrix(0, nrow=C2, ncol=C2)
      fits <- vector("list", C2)     
      for (j in 1:C2) {
        fits[[j]] <- vector("list", C2)
        for (k in 1:C2) {
          yk <- as.numeric(dec_raw[,k])
          fk <- fit_one(Kmats[[j]], yk, tie_rule)
          fits[[j]][[k]] <- fk
          corr_mat[j,k] <- fk$corr
        }
      }
      
      assign_j_to_k <- as.integer(clue::solve_LSAP(1 - corr_mat))
      stopifnot(length(unique(assign_j_to_k)) == C2)
      
      bin_models <- vector("list", C2)
      corr_used <- numeric(C2)
      for (k in 1:C2) {
        j <- which(assign_j_to_k == k)
        fk <- fits[[j]][[k]]
        ci <- pos_class_k[k]; cj <- neg_class_k[k]
        bin_models[[k]] <- list(
          name = paste0(ci, "_vs_", cj),
          ci = ci,
          cj = cj,
          SV = SVs[[j]],          
          coef = fk$alpha,
          b = fk$b
        )
        corr_used[k] <- fk$corr
      }
      cat(sprintf("LS corr(z_recon, dec_raw) pro k: min/median/max = %.6f / %.6f / %.6f\n",
                  min(corr_used), median(corr_used), max(corr_used)))
      
      
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
            else acc[bm_list[[j]]$cj] <- acc[bm_list[[j]]$cj] + 1
          }
        } else {
          for (j in seq_along(bm_list)) {
            if (z[j] >  0) acc[bm_list[[j]]$ci] <- acc[bm_list[[j]]$ci] + 1
            else acc[bm_list[[j]]$cj] <- acc[bm_list[[j]]$cj] + 1
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
      
      out_classes <- rn
      
      onnx_path <- paste0(out_base, "_svm_ovo_linear_", use_rule, ".onnx")
      labels_json <- sub("\\.onnx$", "_labels.json", onnx_path)
      
      build_onnx_poly_ovo(
        use_rule = use_rule,
        bin_models = bin_models,
        feature_names  = feature_names,
        classes = out_classes,
        means = means,
        sds = sds,
        kpar = list(degree = deg, scale = scaleK, offset = offK),
        out_path = onnx_path,
        dtype = "float32",
        reorder_idx = NULL,
        clip_base = NA_real_,
        add_debug = TRUE,
        primary_output = "idx1",
        tie_rule = tie_rule
      )
      
      lbls <- setNames(as.list(out_classes), as.character(seq_along(out_classes)))
      jsonlite::write_json(lbls, labels_json, pretty = TRUE, auto_unbox = TRUE)
      list(onnx = onnx_path, labels_json = labels_json)
    }
    
    # --------- RBF ----------
    export_ksvm_rbf_onnx <- function(train_obj, out_base, use_rule = "majority", primary_output = "idx1", dtype = "float32", do_checks = TRUE) {
      model_r <- train_obj
      model_ksvm <- model_r$finalModel
      
      classes <- as.character(model_r$levels)
      feature_names <- setdiff(colnames(model_r$trainingData), ".outcome")
      
      pp <- model_r$preProcess
      means <- as.numeric(pp$mean[feature_names]); names(means) <- feature_names
      sds <- as.numeric(pp$std[feature_names]);  names(sds)   <- feature_names
      
      Xraw_train <- as.data.frame(model_r$trainingData[, feature_names, drop = FALSE])
      Xscaled <- as.matrix(predict(pp, Xraw_train))
      storage.mode(Xscaled) <- "double"
      colnames(Xscaled) <- feature_names
      
      K  <- length(classes)
      C2 <- K * (K - 1) / 2
      stopifnot(C2 >= 1)
      
      kp <- kernlab::kpar(model_ksvm@kernelf)
      sigma <- as.numeric(kp$sigma) 
      
      
      library(kernlab)
      get_decision_matrix <- function(model, Xscaled) {
        dec <- try(predict(model, Xscaled, type = "decision"), silent = TRUE)
        if (!inherits(dec, "try-error")) return(as.matrix(dec))
        as.matrix(kernlab::decision(model, Xscaled))
      }
      dec_raw <- get_decision_matrix(model_ksvm, Xscaled)
      stopifnot(ncol(dec_raw) == C2)
      
      
      pv <- try(predict(model_ksvm, Xscaled, type = "votes"), silent = TRUE)
      if (inherits(pv, "try-error")) stop("This ksvm does not provide any votes – cancel.")
      rn <- rownames(pv); if (is.null(rn) || any(!nzchar(rn))) rn <- classes
      stopifnot(length(rn) == K)
      cat("C2 = ", C2, " | range(colSums(pv)) = ", paste(range(colSums(pv)), collapse=".."), "\n", sep="")
      
      
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
            gain <- l1_cur - l1_new
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
        pos_class_k <- best$pos
        neg_class_k <- best$neg
        l1_cur <- best$l1
        cat(sprintf("Swap k=%d<->k=%d  → L1=%.12f\n", best$k1, best$k2, l1_cur))
        if (l1_cur < 1e-12 || iter_sw > 200) break
      }
      cat(sprintf("2-Swap Ende: L1=%.12f (Iterationen=%d)\n", l1_cur, iter_sw))
      
      
      our_votes <- build_votes_from_state_tie(dec_raw, pair_id_for_k, pos_class_k, neg_class_k, rn, tie_rule)
      l1_final <- mean(colSums(abs(our_votes - pv)))
      acc_final <- mean(rn[apply(our_votes, 2, which.max)] == rn[apply(pv, 2, which.max)])
      cat(sprintf("Votes-Check (Tie %s): L1=%.12f | Top1-Acc=%.3f\n", tie_rule, l1_final, acc_final))
      stopifnot(l1_final < 1e-12, acc_final >= 0.999)
      
      ai_list <- kernlab::alphaindex(model_ksvm)     
      stopifnot(length(ai_list) == C2)
      
      Kmats <- vector("list", C2)
      SVs <- vector("list", C2)
      for (j in 1:C2) {
        ai <- suppressWarnings(as.integer(ai_list[[j]]))
        stopifnot(length(ai) >= 1L,
                  all(is.finite(ai)),
                  all(ai >= 1L & ai <= nrow(Xscaled)))
        SVs[[j]] <- Xscaled[ai, , drop = FALSE]
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
        t_i <- -z
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
      fits <- vector("list", C2)
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
          ci = ci,
          cj = cj,
          SV = SVs[[j]],     
          coef = fk$alpha,
          b = fk$b
        )
        corr_used[k] <- fk$corr
      }
      cat(sprintf("LS corr(z_recon, dec_raw) pro k: min/median/max = %.6f / %.6f / %.6f\n", min(corr_used), median(corr_used), max(corr_used)))
      
      out_classes <- rn
      
      onnx_path <- paste0(out_base, "_svm_ovo_rbf_", use_rule, ".onnx")
      labels_json <- sub("\\.onnx$", "_labels.json", onnx_path)
      
      build_onnx_rbf_ovo(
        use_rule = use_rule,
        bin_models, feature_names, classes, means, sds, sigma, onnx_path,
        dtype = dtype,
        reorder_idx = NULL,
        add_debug = TRUE,
        primary_output = primary_output,
        tie_rule = tie_rule
      )
      
      lbls <- setNames(as.list(out_classes), as.character(seq_along(out_classes)))
      jsonlite::write_json(lbls, labels_json, pretty = TRUE, auto_unbox = TRUE)
      list(onnx = onnx_path, labels_json = labels_json)
    }
    
    
    
    
    export_caret_ksvm_to_onnx <- function(train_obj, out_base, use_rule = "majority", primary_output = "idx1", dtype = "float32", do_checks = TRUE) {
      k <- detect_svm_kernel(train_obj)
      message("SVM kernel detected: ", k)
      switch(k,
             linear = { message("export_ksvm_linear_onnx()"); 
               export_ksvm_linear_onnx(train_obj, out_base, use_rule, primary_output, dtype, do_checks) },
             poly = { message("export_ksvm_poly_onnx()");   
               export_ksvm_poly_onnx(  train_obj, out_base, use_rule, primary_output, dtype, do_checks) },
             rbf = { message("export_ksvm_rbf_onnx()");    
               export_ksvm_rbf_onnx(   train_obj, out_base, use_rule, primary_output, dtype, do_checks) },
             stop("Unsupported kernel: ", k)
      )
    }
    
    sanitize <- function(s) {
      s2 <- gsub("[^A-Za-z0-9_]+", "_", s)
      if (grepl("^[0-9]", s2)) paste0("C_", s2) else s2
    }
    
    canon <- function(x) make.names(as.character(x), unique = FALSE)
    
    coerce_to_rf_numeric <- function(X, rf, feature_names) {
      Xc <- X
      xlv <- if (!is.null(rf$forest) && !is.null(rf$forest$xlevels)) rf$forest$xlevels else NULL
      for (nm in feature_names) {
        if (is.null(Xc[[nm]])) next
        if (is.factor(Xc[[nm]]) || is.character(Xc[[nm]])) {
          lev <- NULL
          if (!is.null(xlv) && !is.null(xlv[[nm]])) lev <- xlv[[nm]]
          if (is.null(lev) && is.factor(Xc[[nm]]))   lev <- levels(Xc[[nm]])
          Xc[[nm]] <- as.numeric(factor(as.character(Xc[[nm]]), levels = lev))
        } else {
          Xc[[nm]] <- suppressWarnings(as.numeric(Xc[[nm]]))
        }
      }
      Xc
    }
    
    leaf_ids_via_traversal <- function(rf, X, feature_names) {
      stopifnot(inherits(rf, "randomForest"))
      Xn <- coerce_to_rf_numeric(X, rf, feature_names)
      n <- nrow(Xn); Tt <- rf$ntree
      out <- matrix(NA_integer_, nrow = n, ncol = Tt)
      for (t in seq_len(Tt)) {
        tr <- randomForest::getTree(rf, k = t, labelVar = FALSE)
        cn <- colnames(tr)
        pick <- function(name) which(tolower(cn) == tolower(name))[1]
        i_left <- pick("left daughter"); i_right <- pick("right daughter")
        i_svar <- pick("split var");     i_spt   <- pick("split point")
        i_stat <- pick("status")
        left <- suppressWarnings(as.integer(tr[, i_left ]))
        right <- suppressWarnings(as.integer(tr[, i_right]))
        svar <- suppressWarnings(as.integer(tr[, i_svar ]))
        spt <- suppressWarnings(as.numeric(tr[, i_spt ]))
        status <- if (is.finite(i_stat)) suppressWarnings(as.integer(tr[, i_stat])) else rep(NA_integer_, nrow(tr))
        n_local <- nrow(tr)
        is_leaf_row <- (left == 0L & right == 0L) | (svar == 0L) | (!is.na(status) & status == -1L)
        is_leaf_row[is.na(is_leaf_row)] <- FALSE
        for (r in seq_len(n)) {
          node <- 1L; steps <- 0L
          repeat {
            steps <- steps + 1L
            if (!is.finite(node) || node < 1L || node > n_local || steps > (n_local + 5L)) { out[r, t] <- NA_integer_; break }
            if (isTRUE(is_leaf_row[node])) { out[r, t] <- node; break }
            fid  <- svar[node]
            xval <- if (is.finite(fid) && fid >= 1L && fid <= ncol(Xn)) Xn[r, fid] else NA_real_
            go_left <- !is.finite(xval) || !is.finite(spt[node]) || (xval <= spt[node])
            next_node <- if (isTRUE(go_left)) left[node] else right[node]
            if (!is.finite(next_node) || next_node <= 0L || next_node > n_local) { out[r, t] <- node; break }
            node <- next_node
          }
        }
      }
      colnames(out) <- paste0("Tree", seq_len(Tt))
      out
    }
    
    extract_arrays_randomForest_TE <- function(rf, feature_names, out_classes, train_X=NULL, train_y=NULL) {
      stopifnot(inherits(rf, "randomForest"))
      n_trees <- rf$ntree
      pick_col <- function(cn, patterns) {
        for (p in patterns) {
          hit <- which(tolower(cn) == tolower(p))
          if (length(hit)) return(hit[1])
        }
        integer(0)
      }
      nodes_treeids <- integer(0); nodes_nodeids <- integer(0)
      nodes_featureids <- integer(0); nodes_values <- numeric(0)
      nodes_truenodeids <- integer(0); nodes_falsenodeids <- integer(0)
      nodes_modes <- character(0); nodes_missing_value_tracks_true <- integer(0)
      leaf_treeids <- integer(0); leaf_nodeids <- integer(0)
      leaf_targets <- integer(0); leaf_weights <- numeric(0)
      per_tree_nnodes <- integer(n_trees)
      
      nodes_mat <- if (!is.null(train_X)) leaf_ids_via_traversal(rf, train_X, feature_names) else NULL
      library(randomForest)
      for (t in seq_len(n_trees)) {
        tr_idx <- randomForest::getTree(rf, k = t, labelVar = FALSE)
        tr_lab <- tryCatch(randomForest::getTree(rf, k = t, labelVar = TRUE), error = function(e) NULL)
        cn <- colnames(tr_idx)
        i_left <- pick_col(cn, c("left daughter","left_daughter","left"))
        i_right <- pick_col(cn, c("right daughter","right_daughter","right"))
        i_svar <- pick_col(cn, c("split var","split_var","splitvar"))
        i_spt <- pick_col(cn, c("split point","split_point","splitpoint","cutoff"))
        i_stat <- pick_col(cn, c("status"))
        i_pred <- pick_col(cn, c("prediction","pred"))
        left <- suppressWarnings(as.integer(tr_idx[, i_left ]))
        right <- suppressWarnings(as.integer(tr_idx[, i_right]))
        svar <- suppressWarnings(as.integer(tr_idx[, i_svar ]))
        spt <- suppressWarnings(as.numeric(tr_idx[, i_spt ]))
        status <- if (is.finite(i_stat)) suppressWarnings(as.integer(tr_idx[, i_stat])) else rep(NA_integer_, nrow(tr_idx))
        pred_i <- tr_idx[, i_pred]
        
        n_local <- nrow(tr_idx); per_tree_nnodes[t] <- n_local
        nodeid0 <- as.integer(seq_len(n_local) - 1L)
        is_leaf <- (left == 0L & right == 0L) | (svar == 0L) | (!is.na(status) & status == -1L)
        is_leaf[is.na(is_leaf)] <- FALSE
        
        br_idx <- which(!is_leaf & left > 0L & right > 0L)
        if (length(br_idx)) {
          nodes_treeids <- c(nodes_treeids, rep(as.integer(t - 1L), length(br_idx)))
          nodes_nodeids <- c(nodes_nodeids, nodeid0[br_idx])
          nodes_featureids <- c(nodes_featureids, as.integer(pmax(1L, svar[br_idx]) - 1L))
          nodes_values <- c(nodes_values, spt[br_idx])
          nodes_truenodeids  <- c(nodes_truenodeids,  left[br_idx]  - 1L)
          nodes_falsenodeids <- c(nodes_falsenodeids, right[br_idx] - 1L)
          nodes_modes <- c(nodes_modes, rep("BRANCH_LEQ", length(br_idx)))
          nodes_missing_value_tracks_true <- c(nodes_missing_value_tracks_true, rep(1L, length(br_idx)))
        }
        
        lf_idx <- which(is_leaf)
        if (length(lf_idx)) {
          leaf_treeids <- c(leaf_treeids, rep(as.integer(t - 1L), length(lf_idx)))
          leaf_nodeids <- c(leaf_nodeids, nodeid0[lf_idx])
          
          pred_lab <- if (!is.null(tr_lab)) as.character(tr_lab[, i_pred][lf_idx]) else NULL
          cid <- rep(NA_integer_, length(lf_idx))
          
          if (!is.null(nodes_mat) && !is.null(train_y)) {
            for (j in seq_along(lf_idx)) {
              nid1 <- as.integer(lf_idx[j])
              idx_samp <- which(nodes_mat[, t] == nid1)
              if (length(idx_samp)) {
                maj <- names(which.max(table(train_y[idx_samp])))
                maj_id <- match(maj, out_classes)
                if (!is.na(maj_id)) cid[j] <- maj_id
              }
            }
          }
          if (any(is.na(cid)) && !is.null(pred_lab)) {
            m <- match(pred_lab[is.na(cid)], out_classes)
            cid[is.na(cid)] <- m
          }
          if (any(is.na(cid))) {
            pn <- suppressWarnings(as.integer(as.character(pred_i[lf_idx])))
            ok1 <- which(is.na(cid) & is.finite(pn) & pn >= 1L & pn <= length(out_classes)); if (length(ok1)) cid[ok1] <- pn[ok1]
            ok0 <- which(is.na(cid) & is.finite(pn) & pn >= 0L & pn <= (length(out_classes)-1L)); if (length(ok0)) cid[ok0] <- pn[ok0] + 1L
          }
          if (any(is.na(cid))) {
            bad <- lf_idx[is.na(cid)]
            stop("Leaf prediction could not be mapped (randomForest). Examples: ", paste(head(bad, 3), collapse = ", "))
          }
          
          leaf_targets <- c(leaf_targets, as.integer(cid - 1L))
          leaf_weights <- c(leaf_weights, rep(1.0, length(lf_idx)))
        }
      }
      
      if (!length(leaf_treeids)) stop("0 leaves extracted – check getTree()/leaf detection.")
      
      if (length(leaf_treeids)) {
        df_leaf <- unique(data.frame(tid = as.integer(leaf_treeids),
                                     nid = as.integer(leaf_nodeids)))
        nL <- nrow(df_leaf)
        if (nL > 0) {
          nodes_treeids <- c(nodes_treeids, df_leaf$tid)
          nodes_nodeids <- c(nodes_nodeids, df_leaf$nid)
          nodes_featureids <- c(nodes_featureids, rep(0L, nL))
          nodes_values <- c(nodes_values, rep(0.0, nL))
          nodes_truenodeids <- c(nodes_truenodeids,  rep(-1L, nL))
          nodes_falsenodeids <- c(nodes_falsenodeids, rep(-1L, nL))
          nodes_modes <- c(nodes_modes, rep("LEAF", nL))
          nodes_missing_value_tracks_true <- c(nodes_missing_value_tracks_true, rep(0L, nL))
        }
      }
      
      list(
        nodes_treeids = as.integer(nodes_treeids),
        nodes_nodeids = as.integer(nodes_nodeids),
        nodes_featureids = as.integer(nodes_featureids),
        nodes_values = as.numeric(nodes_values),
        nodes_truenodeids  = as.integer(nodes_truenodeids),
        nodes_falsenodeids = as.integer(nodes_falsenodeids),
        nodes_modes = as.character(nodes_modes),
        nodes_missing_value_tracks_true = as.integer(nodes_missing_value_tracks_true),
        leaf_treeids = as.integer(leaf_treeids),
        leaf_nodeids = as.integer(leaf_nodeids),
        leaf_targets = as.integer(leaf_targets),
        leaf_weights = as.numeric(leaf_weights)
      )
    }
    
    ####
    export_rf_onnx <- function(train_obj, out_base, dtype = "float32", primary_output = "idx1", do_checks = TRUE, tie_eps = 1e-6) {
      model_r <- train_obj
      rf_mod  <- if (inherits(model_r, "train")) model_r$finalModel else model_r
      if (!(inherits(rf_mod, "randomForest") || inherits(rf_mod, "ranger"))) {
        stop("Expect randomForest or ranger classification model (if applicable in caret::train).")
      }
      
      out_classes <- if (!is.null(model_r$levels)) {
        as.character(model_r$levels)
      } else if (inherits(rf_mod, "randomForest") && !is.null(rf_mod$classes)) {
        as.character(rf_mod$classes)
      } else if (inherits(rf_mod, "ranger") && !is.null(rf_mod$forest$levels)) {
        as.character(rf_mod$forest$levels)
      } else stop("Could not determine class labels with certainty")
      K <- length(out_classes); stopifnot(K >= 2)
      
      feature_names <- NULL
      if (!is.null(model_r$trainingData)) {
        feature_names <- setdiff(colnames(model_r$trainingData), ".outcome")
      } else if (!is.null(model_r$xNames)) {
        feature_names <- model_r$xNames
      } else if (inherits(rf_mod, "ranger") && !is.null(rf_mod$forest$independent.variable.names)) {
        feature_names <- rf_mod$forest$independent.variable.names
      }
      stopifnot(length(feature_names) >= 1)
      
      pp <- model_r$preProcess
      means <- sds <- NULL
      if (!is.null(pp) && !is.null(pp$mean) && !is.null(pp$std)) {
        if (all(feature_names %in% names(pp$mean)) && all(feature_names %in% names(pp$std))) {
          means <- as.numeric(pp$mean[feature_names]); names(means) <- feature_names
          sds  <- as.numeric(pp$std[feature_names]);  names(sds)   <- feature_names
          if (any(!is.finite(sds)) || any(sds == 0)) means <- sds <- NULL
        }
      }
      
      train_X <- NULL; train_y <- NULL
      if (!is.null(model_r$trainingData)) {
        train_X <- as.data.frame(model_r$trainingData[, feature_names, drop = FALSE])
        train_y <- factor(model_r$trainingData$.outcome, levels = out_classes)
      }
      
      arr <- extract_arrays_randomForest_TE(rf_mod, feature_names, out_classes, train_X, train_y)
      
      if (isTRUE(do_checks)) {
        true_branches <- sum(arr$nodes_modes == "BRANCH_LEQ")
        cat(sprintf("Forest summary: %d branch nodes, %d leaves, trees_seen=%d\n",
                    true_branches, length(arr$leaf_treeids),
                    if (length(c(arr$nodes_treeids, arr$leaf_treeids))) max(c(arr$nodes_treeids, arr$leaf_treeids))+1L else 0L))
      }
      
      onnx_path <- paste0(out_base, "_rf_teclassifier.onnx")
      message("feature names", feature_names)

      build_rf_teclassifier_onnx(
        arr = arr,
        feature_names  = feature_names,
        classes = out_classes,
        out_path = onnx_path,
        means = means,
        sds = sds,
        dtype = dtype,
        primary_output = primary_output,
        tie_eps = tie_eps
      )
      
      meta_opts <- list(
        feature_names = paste(feature_names, collapse = ", "),
        class_labels  = paste(out_classes,  collapse = ", "),
        zero_based = "true" 
      )
      
      add_metadata_to_onnx(onnx_path, meta_opts)
      
      if (isTRUE(do_checks) && !is.null(train_X)) {
        ort <- reticulate::import("onnxruntime", delay_load = TRUE)
        np  <- reticulate::import("numpy", convert = FALSE)
        sess <- ort$InferenceSession(onnx_path)
        ins_r  <- reticulate::py_to_r(sess$get_inputs())
        input_name <- as.character(ins_r[[1]]$name)
        is_f64  <- identical(dtype, "float64")
        np_dtype <- if (is_f64) "float64" else "float32"
        
        X_ref <- train_X
        if (!is.null(means) && !is.null(sds)) {
          X_ref <- sweep(X_ref, 2, sds, "*")
          X_ref <- sweep(X_ref, 2, means, "+")
        }
        
        onnx_predict_idx1_batch <- function(sess, input_name, X_raw) {
          X <- if (is.vector(X_raw)) matrix(X_raw, nrow=1L) else as.matrix(X_raw)
          colnames(X) <- colnames(X_raw)
          xnp <- np$array(X, dtype = np_dtype)
          res <- reticulate::py_to_r(sess$run(NULL, setNames(list(xnp), input_name)))
          as.integer(res[[1]])
        }
        
        rf_predict_idx1 <- function(model_r_full, X_raw, out_classes) {
          X <- if (is.vector(X_raw)) matrix(X_raw, nrow=1L) else as.matrix(X_raw)
          colnames(X) <- colnames(X_raw)
          pr <- try(predict(model_r_full, newdata = as.data.frame(X)), silent = TRUE)
          if (inherits(pr, "try-error")) pr <- predict(rf_mod, newdata = as.data.frame(X))
          match(as.character(pr), out_classes)
        }
        
        set.seed(1)
        N <- min(100L, nrow(X_ref))
        rows <- sample(seq_len(nrow(X_ref)), N)
        X_raw_N <- X_ref[rows, , drop=FALSE]; colnames(X_raw_N) <- feature_names
        
        onnx_idx1 <- onnx_predict_idx1_batch(sess, input_name, X_raw_N)
        caret_idx1 <- rf_predict_idx1(model_r, X_raw_N, out_classes)
        
        cat(sprintf("\n--- Batch check on %d training rows ---\n", N))
        cat("Mismatch-Rate:", mean(onnx_idx1 != caret_idx1), "\n")
        cat("ONNX class distribution:\n");  print(sort(table(onnx_idx1)))
        cat("Caret class distribution:\n"); print(sort(table(caret_idx1)))
      }
      
      invisible(list(
        onnx_path = onnx_path,
        classes   = out_classes,
        features  = feature_names
      ))
    }
    
    
    build_onnx_for_rule <- function(use_rule, weights_aligned, feature_names, classes, means, sds, out_path) {
      
      library(reticulate)
      py <- find_python_bin()
      
      onnx <- reticulate::import("onnx", convert = FALSE)
      helper <- reticulate::import("onnx.helper", convert = FALSE)
      np <- reticulate::import("numpy", convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp <- reticulate::tuple
      
      n_feat <- length(feature_names); K <- length(classes); C2 <- length(weights_aligned)
      inp <- helper$make_tensor_value_info("input_raw", TensorProto$FLOAT, list(1L, n_feat))
      out_idx   <- helper$make_tensor_value_info("idx1", TensorProto$INT64, list(1L, 1L))
      out_score <- helper$make_tensor_value_info(
        if (use_rule=="margin") "scores_vector" else "votes_vector",
        TensorProto$FLOAT, list(1L, K)
      )
      
      init_common <- list(
        onnx$numpy_helper$from_array(np$array(means, dtype="float32")$reshape(tp(1L,n_feat)), "mean_vec"),
        onnx$numpy_helper$from_array(np$array(sds, dtype="float32")$reshape(tp(1L,n_feat)), "std_vec"),
        onnx$numpy_helper$from_array(np$array(0.0, dtype="float32"), "zero_f"),
        onnx$numpy_helper$from_array(np$array(1L, dtype="int64"), "one_i")
      )
      
      nodes <- list(
        helper$make_node("Sub", list("input_raw","mean_vec"), list("centered"), name="Center"),
        helper$make_node("Div", list("centered","std_vec"), list("scaled"), name="Scale")
      )
      oh_inits <- lapply(seq_along(classes), function(cix) {
        oh <- rep(0, K); oh[cix] <- 1
        onnx$numpy_helper$from_array(
          np$array(oh, dtype="float32")$reshape(tp(1L,K)),
          paste0("oh_", sanitize(classes[cix]))
        )
      })
      
      init_pair <- list(); acc_vecs <- character(0)
      for (j in seq_along(weights_aligned)) {
        w <- weights_aligned[[j]]
        ci <- sanitize(w$class_pair[1]); cj <- sanitize(w$class_pair[2])
        tag <- sanitize(w$name)
        sfx <- sprintf("_j%03d", j)
        
        init_pair <- c(init_pair,
                       onnx$numpy_helper$from_array(np$array(as.numeric(w$W), dtype="float32")$reshape(tp(n_feat,1L)), paste0("W_", tag, sfx)),
                       onnx$numpy_helper$from_array(np$array(as.numeric(w$b), dtype="float32")$reshape(tp(1L,1L)), paste0("b_", tag, sfx)))
        
        raw <- paste0("raw_", tag, sfx)
        z <- paste0("z_", tag, sfx)
        
        if (use_rule == "majority") {
          mask <- paste0("mask_", tag, sfx)
          vote <- paste0("vote_", tag, sfx)
          nodes <- c(nodes,
                     helper$make_node("MatMul", list("scaled", paste0("W_", tag, sfx)), list(raw), name=paste0("MatMul_", tag, sfx)),
                     helper$make_node("Add", list(raw, paste0("b_", tag, sfx)), list(z),   name=paste0("Bias_", tag, sfx)),
                     helper$make_node("GreaterOrEqual", list(z, "zero_f"), list(mask), name=paste0("Ge_", tag, sfx)),
                     helper$make_node("Where", list(mask, paste0("oh_", ci), paste0("oh_", cj)), list(vote), name=paste0("Vote_", tag, sfx))
          )
          acc_vecs <- c(acc_vecs, vote)
        } else {
          pos <- paste0("pos_", tag, sfx)
          neg_raw  <- paste0("neg_raw_", tag, sfx)
          neg_relu <- paste0("neg_", tag, sfx)
          posv <- paste0("posv_", tag, sfx)
          negv <- paste0("negv_", tag, sfx)
          score <- paste0("score_", tag, sfx)
          nodes <- c(nodes,
                     helper$make_node("MatMul", list("scaled", paste0("W_", tag, sfx)), list(raw), name=paste0("MatMul_", tag, sfx)),
                     helper$make_node("Add", list(raw, paste0("b_", tag, sfx)), list(z), name=paste0("Bias_", tag, sfx)),
                     helper$make_node("Relu", list(z), list(pos), name=paste0("ReluPos_", tag, sfx)),
                     helper$make_node("Neg", list(z), list(neg_raw), name=paste0("Neg_", tag, sfx)),
                     helper$make_node("Relu", list(neg_raw), list(neg_relu), name=paste0("ReluNeg_", tag, sfx)),
                     helper$make_node("Mul", list(pos, paste0("oh_", ci)), list(posv), name=paste0("MulPos_", tag, sfx)),
                     helper$make_node("Mul", list(neg_relu,paste0("oh_", cj)), list(negv), name=paste0("MulNeg_", tag, sfx)),
                     helper$make_node("Add", list(posv, negv), list(score), name=paste0("AddScore_", tag, sfx))
          )
          acc_vecs <- c(acc_vecs, score)
        }
      }
      
      sum_name <- acc_vecs[1]
      if (length(acc_vecs) > 1) {
        for (i in 2:length(acc_vecs)) {
          new_sum <- paste0("sum_", sprintf("%03d", i))
          nodes <- c(nodes,
                     helper$make_node("Add", list(sum_name, acc_vecs[i]), list(new_sum), name=paste0("Sum_", sprintf("%03d", i))))
          sum_name <- new_sum
        }
      }
      
      nodes <- c(nodes,
                 helper$make_node("ArgMax", list(sum_name), list("idx_raw"), axis=1L, keepdims=1L, name="ArgMax"),
                 helper$make_node("Add", list("idx_raw","one_i"), list("idx1"), name="Make1Idx"),
                 helper$make_node("Identity", list(sum_name), list(if (use_rule=="margin") "scores_vector" else "votes_vector"), name="OutVec")
      )
      
      graph <- helper$make_graph(
        nodes = nodes,
        name = paste0("svm_ovo_", use_rule, "_assigned"),
        inputs = list(inp),
        outputs = list(out_idx, out_score),
        initializer = c(init_common, oh_inits, init_pair)
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = paste0("ovo_", use_rule, "_export"),
        opset_imports = list(helper$make_operatorsetid("ai.onnx", 13L))
      )
      model_onnx$ir_version <- 10L
      onnx$save(model_onnx, out_path)
      cat("ONNX (", use_rule, ") gespeichert: ", out_path, "\n", sep = "")
      
    }
    
    
    
    build_onnx_poly_ovo <- function(use_rule = c("majority","margin"), bin_models, feature_names, classes, means, sds, kpar, out_path, dtype = c("float64","float32"), reorder_idx  = NULL, clip_base = NA_real_, add_debug = TRUE, primary_output= c("idx1","votes_vector","scores_vector"), tie_rule = c(">0",">=0")){
      
      
      use_rule <- match.arg(use_rule)
      dtype <- match.arg(dtype)
      primary_output <- match.arg(primary_output)
      tie_rule <- match.arg(tie_rule)
      
      onnx <- reticulate::import("onnx", convert = FALSE)
      helper <- reticulate::import("onnx.helper", convert = FALSE)
      np <- reticulate::import("numpy", convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp <- reticulate::tuple
      
      is_f64 <- (dtype == "float64")
      TNUM  <- if (is_f64) TensorProto$DOUBLE else TensorProto$FLOAT
      as_fp_vec <- function(x) np$array(as.numeric(x), dtype = if (is_f64) "float64" else "float32")
      as_fp_mat <- function(M){ if (is.vector(M)) M <- matrix(M, nrow=1L); np$array(M, dtype = if (is_f64) "float64" else "float32") }
      as_i64_vec <- function(x){ xv <- as.integer(x); np$array(xv, dtype = "int64")$reshape(tp(length(xv))) }
      `%||%` <- function(x,y) if (is.null(x) || !is.finite(x)) y else x
      
      K <- length(classes)
      n_feat <- length(feature_names)
      deg_i <- as.integer(round(kpar$degree %||% 3))
      
      inp <- helper$make_tensor_value_info("input_raw", TensorProto$FLOAT, list(1L, n_feat))
      out_i <- helper$make_tensor_value_info("idx1", TensorProto$INT64, list(1L,1L))
      out_vv <- helper$make_tensor_value_info("votes_vector", TNUM, list(1L,K))
      outs <- list(out_i, out_vv)
      if (use_rule == "margin") {
        out_sv <- helper$make_tensor_value_info("scores_vector", TNUM, list(1L,K))
        outs <- c(outs, list(out_sv))
      }
      if (isTRUE(add_debug)) {
        out_xs <- helper$make_tensor_value_info("X_scaled_dbg", TNUM, list(1L,n_feat))
        out_cen <- helper$make_tensor_value_info("centered_dbg", TNUM, list(1L,n_feat))
        out_ss <- helper$make_tensor_value_info("std_safe_dbg", TNUM, list(1L,n_feat))
        out_z <- helper$make_tensor_value_info("z_all", TNUM, list(1L, length(bin_models)))
        out_m <- helper$make_tensor_value_info("win_mask", TensorProto$BOOL, list(1L, length(bin_models)))
        out_vbp <- helper$make_tensor_value_info("votes_by_pair", TNUM, list(length(bin_models), K))
        out_tv <- helper$make_tensor_value_info("total_votes", TNUM, list(1L,1L))
        out_i0 <- helper$make_tensor_value_info("idx0", TensorProto$INT64, list(1L,1L))
        outs <- c(outs, list(out_xs, out_cen, out_ss, out_z, out_m, out_vbp, out_tv, out_i0))
      }
      
      reorder_outs <- function(outs, names_vec, primary){
        idx <- match(primary, names_vec)
        if (is.na(idx) || idx == 1L) return(outs)
        c(outs[idx], outs[-idx])
      }
      out_name_vec <- c("idx1","votes_vector", if (use_rule=="margin") "scores_vector",
                        if (isTRUE(add_debug)) c("X_scaled_dbg","centered_dbg","std_safe_dbg","z_all","win_mask","votes_by_pair","total_votes","idx0"))
      outs <- reorder_outs(outs, out_name_vec, primary_output)
      
      init_common <- list(
        onnx$numpy_helper$from_array(as_fp_mat(means),"mean_vec"),
        onnx$numpy_helper$from_array(as_fp_mat(sds), "std_vec"),
        onnx$numpy_helper$from_array(as_fp_vec(0.0), "zero_f"),
        onnx$numpy_helper$from_array(as_fp_vec(kpar$scale %||% 1), "k_scale"),
        onnx$numpy_helper$from_array(as_fp_vec(kpar$offset%||% 1), "k_offset"),
        onnx$numpy_helper$from_array(as_fp_vec(deg_i),"k_degree"),
        onnx$numpy_helper$from_array(as_fp_vec(1e-6), "eps_f"),
        onnx$numpy_helper$from_array(as_i64_vec(c(1L, K)), "shape_1K"),
        onnx$numpy_helper$from_array(as_i64_vec(1L), "k1"),
        onnx$numpy_helper$from_array(as_i64_vec(1L), "one_i")
      )
      if (!is.null(reorder_idx)) {
        stopifnot(length(reorder_idx) == n_feat)
        init_common <- c(init_common, onnx$numpy_helper$from_array(as_i64_vec(reorder_idx), "perm_idx"))
      }
      
      oh_inits <- lapply(seq_len(K), function(ci) {
        oh <- rep(0, K); oh[ci] <- 1
        onnx$numpy_helper$from_array(as_fp_mat(oh), paste0("oh_", ci))
      })
      
      nodes <- list()
      cur_in <- "input_raw"
      
      if (!is.null(reorder_idx)) {
        nodes <- c(nodes, helper$make_node("Gather", list(cur_in,"perm_idx"), list("input_reordered"), axis=1L, name="Reorder"))
        cur_in <- "input_reordered"
      }
      
      nodes <- c(nodes,
                 helper$make_node("Sub", list(cur_in, "mean_vec"),  list("centered"), name="Center"),
                 helper$make_node("Abs", list("std_vec"), list("std_abs"), name="StdAbs"),
                 helper$make_node("Max", list("std_abs","eps_f"), list("std_safe"), name="StdSafe"),
                 helper$make_node("Div", list("centered","std_safe"), list("scaled_raw"), name="ScaleSafe"),
                 helper$make_node("IsNaN", list("scaled_raw"), list("isnan_scaled"), name="IsNaN_scaled"),
                 helper$make_node("IsInf", list("scaled_raw"), list("isinf_scaled"), name="IsInf_scaled"),
                 helper$make_node("Or", list("isnan_scaled","isinf_scaled"), list("bad_scaled"), name="Bad_scaled"),
                 helper$make_node("Where", list("bad_scaled","zero_f","scaled_raw"),   list("scaled"), name="Scaled_Clean"))
      
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Identity", list("scaled"), list("X_scaled_dbg"), name="DbgScaled"),
                   helper$make_node("Identity", list("centered"),list("centered_dbg"), name="DbgCentered"),
                   helper$make_node("Identity", list("std_safe"), list("std_safe_dbg"), name="DbgStdSafe"))
      }
      
      acc_votes_vecs <- character(0)
      acc_scores_vecs <- character(0)
      z_list <- character(0)
      mask_list <- character(0)
      vbp_rows <- character(0)
      
      ge_op <- if (tie_rule == ">=0") "GreaterOrEqual" else "Greater"
      
      for (j in seq_along(bin_models)) {
        bm <- bin_models[[j]]
        SVT <- t(bm$SV)                             
        coef<- matrix(bm$coef, nrow=length(bm$coef), ncol=1L)  
        b <- matrix(bm$b,    nrow=1L, ncol=1L)
        
        SVt_name  <- paste0("SVt_", j)
        coef_name <- paste0("coef_", j)
        b_name    <- paste0("b_", j)
        
        init_common <- c(init_common,
                         onnx$numpy_helper$from_array(as_fp_mat(SVT), SVt_name),
                         onnx$numpy_helper$from_array(as_fp_mat(coef), coef_name),
                         onnx$numpy_helper$from_array(as_fp_mat(b), b_name))
        
        mm <- paste0("mm_", j)
        mm_s <- paste0("mm_s_", j)
        base <- paste0("base_", j)
        ker <- paste0("ker_", j)
        zlin <- paste0("zlin_", j)
        z <- paste0("z_", j)
        ge <- paste0("ge_", j)
        ge2 <- paste0("ge2_", j)
        votej <- paste0("vote_", j)
        pos <- paste0("pos_", j)
        neg0 <- paste0("neg0_", j)
        neg <- paste0("neg_", j)
        posv <- paste0("posv_", j)
        negv  <- paste0("negv_", j)
        score <- paste0("score_",j)
        
        nodes <- c(nodes,
                   helper$make_node("MatMul", list("scaled", SVt_name), list(mm), name=paste0("MM_", j)),
                   helper$make_node("Mul", list(mm, "k_scale"), list(mm_s), name=paste0("Scale_", j)),
                   helper$make_node("Add", list(mm_s, "k_offset"), list(base), name=paste0("Offset_", j)))
        if (is.finite(clip_base)) {
          nodes <- c(nodes, helper$make_node("Clip", list(base), list(base), min=-abs(clip_base), max=abs(clip_base), name=paste0("ClipBase_", j)))
        }
        
        nodes <- c(nodes,
                   helper$make_node("Pow", list(base, "k_degree"), list(ker), name=paste0("Pow_", j)),
                   helper$make_node("MatMul", list(ker, coef_name), list(zlin), name=paste0("MMcoef_", j)),
                   helper$make_node("Add", list(zlin, b_name), list(z), name=paste0("Bias_",   j)),
                   helper$make_node(ge_op, list(z, "zero_f"), list(ge), name=paste0("GE_",     j)),
                   helper$make_node("Expand", list(ge, "shape_1K"), list(ge2), name=paste0("ExpandGE_", j)),
                   helper$make_node("Where",
                                    list(ge2,
                                         paste0("oh_", match(bm$ci, classes)),
                                         paste0("oh_", match(bm$cj, classes))),
                                    list(votej), name=paste0("Vote_", j)))
        acc_votes_vecs <- c(acc_votes_vecs, votej)
        z_list <- c(z_list, z)
        mask_list <- c(mask_list, ge)
        vbp_rows <- c(vbp_rows, votej)
        
        if (use_rule == "margin") {
          nodes <- c(nodes,
                     helper$make_node("Relu", list(z), list(pos), name=paste0("ReluPos_", j)),
                     helper$make_node("Neg", list(z), list(neg0), name=paste0("Neg_", j)),
                     helper$make_node("Relu", list(neg0), list(neg), name=paste0("ReluNeg_", j)),
                     helper$make_node("Mul", list(pos, paste0("oh_", match(bm$ci, classes))), list(posv)),
                     helper$make_node("Mul", list(neg, paste0("oh_", match(bm$cj, classes))), list(negv)),
                     helper$make_node("Add", list(posv, negv), list(score), name=paste0("Score_", j)))
          acc_scores_vecs <- c(acc_scores_vecs, score)
        }
      }
      
      acc_votes <- acc_votes_vecs[1]
      if (length(acc_votes_vecs) > 1) {
        for (i in 2:length(acc_votes_vecs)) {
          ns <- paste0("sum_votes_", i)
          nodes <- c(nodes, helper$make_node("Add", list(acc_votes, acc_votes_vecs[i]), list(ns)))
          acc_votes <- ns
        }
      }
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
      
      if (use_rule == "margin") {
        nodes <- c(nodes,
                   helper$make_node("TopK", list(acc_scores,"k1"), list("topv","topi"), axis=1L, largest=1L, sorted=1L, name="TopK1_Margin"))
      } else {
        nodes <- c(nodes,
                   helper$make_node("TopK", list(acc_votes,"k1"), list("topv","topi"), axis=1L, largest=1L, sorted=1L, name="TopK1_Majority"))
      }
      nodes <- c(nodes,
                 helper$make_node("Identity", list("topi"), list("idx0"), name="Idx0"),
                 helper$make_node("Add", list("topi","one_i"), list("idx1"), name="Idx1"))
      
      nodes <- c(nodes, helper$make_node("Identity", list(acc_votes), list("votes_vector"), name="OutVotes"))
      if (use_rule == "margin") {
        nodes <- c(nodes, helper$make_node("Identity", list(acc_scores), list("scores_vector"), name="OutScores"))
      }
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Concat", z_list, list("z_all"), axis=1L, name="ConcatZ"),
                   helper$make_node("Concat", mask_list, list("win_mask"), axis=1L, name="ConcatMask"),
                   helper$make_node("Concat", vbp_rows, list("votes_by_pair"), axis=0L, name="ConcatVotesByPair"),
                   helper$make_node("ReduceSum", list("votes_vector"), list("total_votes"), keepdims=1L, name="TotalVotes"))
      }
      
      graph <- helper$make_graph(
        nodes = nodes,
        name = paste0("svmPoly_ovo_", use_rule, "_fp"),
        inputs = list(inp),
        outputs = outs,
        initializer = c(init_common, oh_inits)
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = paste0("ovo_poly_", use_rule, "_export"),
        opset_imports = list(helper$make_operatorsetid("", 13L))
      )
      model_onnx$ir_version <- 10L
      onnx$save(model_onnx, out_path)
      
      
      cat("ONNX (", use_rule, ", dtype=", dtype,
          ", primary_output=", primary_output,
          ", tie_rule=", tie_rule,
          if (!is.na(clip_base)) paste0(", clip=", clip_base) else "",
          ") stored: ", out_path, "\n", sep = "")
    }
    
    
    
    
    build_onnx_rbf_ovo <- function(use_rule = c("majority","margin"), bin_models, feature_names, classes, means, sds, sigma, out_path, dtype = c("float64","float32"), reorder_idx = NULL, add_debug  = TRUE, primary_output= c("idx1","votes_vector","scores_vector"), tie_rule = c(">0",">=0")){
      
      
      use_rule <- match.arg(use_rule)
      dtype <- match.arg(dtype)
      primary_output <- match.arg(primary_output)
      tie_rule <- match.arg(tie_rule)
      
      onnx <- reticulate::import("onnx", convert = FALSE)
      helper <- reticulate::import("onnx.helper", convert = FALSE)
      np <- reticulate::import("numpy", convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp <- reticulate::tuple
      
      is_f64 <- (dtype == "float64")
      TNUM <- if (is_f64) TensorProto$DOUBLE else TensorProto$FLOAT
      as_fp_vec <- function(x) np$array(as.numeric(x), dtype = if (is_f64) "float64" else "float32")
      as_fp_mat <- function(M){ if (is.vector(M)) M <- matrix(M, nrow=1L); np$array(M, dtype = if (is_f64) "float64" else "float32") }
      as_i64_vec <- function(x){ xv <- as.integer(x); np$array(xv, dtype = "int64")$reshape(tp(length(xv))) }
      
      K <- length(classes)
      n_feat <- length(feature_names)
      
      inp <- helper$make_tensor_value_info("input_raw", TensorProto$FLOAT, list(1L, n_feat))
      out_i <- helper$make_tensor_value_info("idx1", TensorProto$INT64, list(1L,1L))
      out_vv <- helper$make_tensor_value_info("votes_vector", TNUM, list(1L,K))
      outs  <- list(out_i, out_vv)
      if (use_rule == "margin") {
        out_sv <- helper$make_tensor_value_info("scores_vector", TNUM, list(1L,K))
        outs <- c(outs, list(out_sv))
      }
      if (isTRUE(add_debug)) {
        out_xs <- helper$make_tensor_value_info("X_scaled_dbg", TNUM, list(1L,n_feat))
        out_cen <- helper$make_tensor_value_info("centered_dbg", TNUM, list(1L,n_feat))
        out_ss <- helper$make_tensor_value_info("std_safe_dbg", TNUM, list(1L,n_feat))
        out_z <- helper$make_tensor_value_info("z_all", TNUM, list(1L, length(bin_models)))
        out_m <- helper$make_tensor_value_info("win_mask", TensorProto$BOOL, list(1L, length(bin_models)))
        out_vbp <- helper$make_tensor_value_info("votes_by_pair", TNUM, list(length(bin_models), K))
        out_tv <- helper$make_tensor_value_info("total_votes", TNUM, list(1L,1L))
        out_i0 <- helper$make_tensor_value_info("idx0", TensorProto$INT64, list(1L,1L))
        outs <- c(outs, list(out_xs, out_cen, out_ss, out_z, out_m, out_vbp, out_tv, out_i0))
      }
      
      reorder_outs <- function(outs, names_vec, primary){
        idx <- match(primary, names_vec)
        if (is.na(idx) || idx == 1L) return(outs)
        c(outs[idx], outs[-idx])
      }
      out_name_vec <- c("idx1","votes_vector", if (use_rule=="margin") "scores_vector",
                        if (isTRUE(add_debug)) c("X_scaled_dbg","centered_dbg","std_safe_dbg","z_all","win_mask","votes_by_pair","total_votes","idx0"))
      outs <- reorder_outs(outs, out_name_vec, primary_output)
      
      init_common <- list(
        onnx$numpy_helper$from_array(as_fp_mat(means), "mean_vec"),
        onnx$numpy_helper$from_array(as_fp_mat(sds), "std_vec"),
        onnx$numpy_helper$from_array(as_fp_vec(0.0), "zero_f"),
        onnx$numpy_helper$from_array(as_fp_vec(-2.0), "neg2_f"),
        onnx$numpy_helper$from_array(as_fp_vec(-sigma), "neg_sigma_f"),
        onnx$numpy_helper$from_array(as_fp_vec(1e-6), "eps_f"),
        onnx$numpy_helper$from_array(as_i64_vec(c(1L,K)),"shape_1K"),
        onnx$numpy_helper$from_array(as_i64_vec(1L), "k1"),
        onnx$numpy_helper$from_array(as_i64_vec(1L), "one_i")
      )
      if (!is.null(reorder_idx)) {
        stopifnot(length(reorder_idx) == n_feat)
        init_common <- c(init_common, onnx$numpy_helper$from_array(as_i64_vec(reorder_idx), "perm_idx"))
      }
      
      oh_inits <- lapply(seq_len(K), function(ci) {
        oh <- rep(0, K); oh[ci] <- 1
        onnx$numpy_helper$from_array(as_fp_mat(oh), paste0("oh_", ci))
      })
      
      nodes <- list()
      cur_in <- "input_raw"
      
      if (!is.null(reorder_idx)) {
        nodes <- c(nodes, helper$make_node("Gather", list(cur_in,"perm_idx"), list("input_reordered"), axis=1L, name="Reorder"))
        cur_in <- "input_reordered"
      }
      
      nodes <- c(nodes,
                 helper$make_node("Sub", list(cur_in, "mean_vec"),  list("centered"), name="Center"),
                 helper$make_node("Abs", list("std_vec"), list("std_abs"), name="StdAbs"),
                 helper$make_node("Max", list("std_abs","eps_f"), list("std_safe"), name="StdSafe"),
                 helper$make_node("Div", list("centered","std_safe"), list("scaled_raw"), name="ScaleSafe"),
                 helper$make_node("IsNaN", list("scaled_raw"), list("isnan_scaled"), name="IsNaN_scaled"),
                 helper$make_node("IsInf", list("scaled_raw"), list("isinf_scaled"), name="IsInf_scaled"),
                 helper$make_node("Or",  list("isnan_scaled","isinf_scaled"), list("bad_scaled"), name="Bad_scaled"),
                 helper$make_node("Where", list("bad_scaled","zero_f","scaled_raw"),   list("scaled"), name="Scaled_Clean"))
      
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Identity", list("scaled"), list("X_scaled_dbg"), name="DbgScaled"),
                   helper$make_node("Identity", list("centered"), list("centered_dbg"), name="DbgCentered"),
                   helper$make_node("Identity", list("std_safe"), list("std_safe_dbg"), name="DbgStdSafe"))
      }
      
      acc_votes_vecs <- character(0)
      acc_scores_vecs <- character(0)
      z_list <- character(0)
      mask_list <- character(0)
      vbp_rows <- character(0)
      
      ge_op <- if (tie_rule == ">=0") "GreaterOrEqual" else "Greater"
      
      for (j in seq_along(bin_models)) {
        bm  <- bin_models[[j]]
        SVT <- t(bm$SV)                                
        coef<- matrix(bm$coef, nrow=length(bm$coef), ncol=1L)
        b   <- matrix(bm$b,    nrow=1L, ncol=1L)
        
        sv_norm2 <- matrix(colSums(SVT*SVT), nrow=1L)
        
        SVt_name <- paste0("SVt_", j)
        coef_name <- paste0("coef_", j)
        b_name <- paste0("b_", j)
        svn_name <- paste0("svnorm2_", j)
        shape1M <- paste0("shape_1m_", j)
        axes1_name <- paste0("axes1_", j)  
        
        init_common <- c(init_common,
                         onnx$numpy_helper$from_array(as_fp_mat(SVT), SVt_name),
                         onnx$numpy_helper$from_array(as_fp_mat(coef), coef_name),
                         onnx$numpy_helper$from_array(as_fp_mat(b), b_name),
                         onnx$numpy_helper$from_array(as_fp_mat(sv_norm2), svn_name),
                         onnx$numpy_helper$from_array(as_i64_vec(c(1L, ncol(SVT))), shape1M),
                         onnx$numpy_helper$from_array(as_i64_vec(1L), axes1_name))  
        
        mm <- paste0("mm_", j)
        xn2 <- paste0("xn2_", j)
        xn2e <- paste0("xn2e_", j)
        mmt <- paste0("mmt_", j)
        dist2 <- paste0("dist2_", j)
        gprod <- paste0("gprod_", j)
        ker <- paste0("ker_", j)
        zlin <- paste0("zlin_", j)
        z <- paste0("z_", j)
        ge <- paste0("ge_", j)
        ge2 <- paste0("ge2_", j)
        votej <- paste0("vote_", j)
        pos <- paste0("pos_", j)
        neg0 <- paste0("neg0_", j)
        neg <- paste0("neg_", j)
        posv <- paste0("posv_", j)
        negv <- paste0("negv_", j)
        score <- paste0("score_", j)
        
        nodes <- c(nodes,
                   helper$make_node("MatMul", list("scaled", SVt_name), list(mm), name=paste0("MM_", j)),
                   helper$make_node("Mul", list(mm, "neg2_f"), list(mmt), name=paste0("MulNeg2_", j)),
                   helper$make_node("Mul", list("scaled","scaled"), list(paste0("x2_",j)), name=paste0("SquareX_", j)),
                   helper$make_node("ReduceSum", list(paste0("x2_",j), axes1_name), list(xn2), keepdims=1L, name=paste0("SumX2_", j)),
                   helper$make_node("Expand", list(xn2, shape1M), list(xn2e), name=paste0("ExpandX2_", j)),
                   helper$make_node("Add", list(xn2e, svn_name), list(paste0("tmp_",j)), name=paste0("AddXnSv_", j)),
                   helper$make_node("Add",list(paste0("tmp_",j), mmt), list(dist2), name=paste0("Dist2_", j)),
                   helper$make_node("Mul", list(dist2, "neg_sigma_f"), list(gprod), name=paste0("MulGamma_", j)),
                   helper$make_node("Exp", list(gprod), list(ker),   name=paste0("Exp_", j)),
                   helper$make_node("MatMul", list(ker, coef_name), list(zlin),  name=paste0("MMcoef_", j)),
                   helper$make_node("Add", list(zlin, b_name), list(z),     name=paste0("Bias_", j)),
                   helper$make_node(ge_op, list(z, "zero_f"), list(ge),    name=paste0("GE_", j)),
                   helper$make_node("Expand", list(ge, "shape_1K"), list(ge2),   name=paste0("ExpandGE_", j)),
                   helper$make_node("Where",
                                    list(ge2,
                                         paste0("oh_", match(bm$ci, classes)),
                                         paste0("oh_", match(bm$cj, classes))),
                                    list(votej), name=paste0("Vote_", j)))
        
        acc_votes_vecs <- c(acc_votes_vecs, votej)
        z_list <- c(z_list, z)
        mask_list <- c(mask_list, ge)
        vbp_rows  <- c(vbp_rows, votej)
        
        if (use_rule == "margin") {
          nodes <- c(nodes,
                     helper$make_node("Relu", list(z), list(pos), name=paste0("ReluPos_", j)),
                     helper$make_node("Neg", list(z), list(neg0), name=paste0("Neg_", j)),
                     helper$make_node("Relu", list(neg0), list(neg), name=paste0("ReluNeg_", j)),
                     helper$make_node("Mul", list(pos, paste0("oh_", match(bm$ci, classes))), list(posv)),
                     helper$make_node("Mul", list(neg, paste0("oh_", match(bm$cj, classes))), list(negv)),
                     helper$make_node("Add", list(posv, negv), list(score), name=paste0("Score_", j)))
          acc_scores_vecs <- c(acc_scores_vecs, score)
        }
      }
      
      acc_votes <- acc_votes_vecs[1]
      if (length(acc_votes_vecs) > 1) {
        for (i in 2:length(acc_votes_vecs)) {
          ns <- paste0("sum_votes_", i)
          nodes <- c(nodes, helper$make_node("Add", list(acc_votes, acc_votes_vecs[i]), list(ns)))
          acc_votes <- ns
        }
      }
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
      
      
      if (use_rule == "margin") {
        nodes <- c(nodes, helper$make_node("TopK", list(acc_scores,"k1"), list("topv","topi"), axis=1L, largest=1L, sorted=1L, name="TopK1_Margin"))
      } else {
        nodes <- c(nodes, helper$make_node("TopK", list(acc_votes,"k1"), list("topv","topi"), axis=1L, largest=1L, sorted=1L, name="TopK1_Majority"))
      }
      nodes <- c(nodes,
                 helper$make_node("Identity", list("topi"), list("idx0"), name="Idx0"),
                 helper$make_node("Add", list("topi","one_i"), list("idx1"), name="Idx1"))
      
      nodes <- c(nodes, helper$make_node("Identity", list(acc_votes), list("votes_vector"), name="OutVotes"))
      if (use_rule == "margin") {
        nodes <- c(nodes, helper$make_node("Identity", list(acc_scores), list("scores_vector"), name="OutScores"))
      }
      if (isTRUE(add_debug)) {
        nodes <- c(nodes,
                   helper$make_node("Concat", z_list, list("z_all"), axis=1L, name="ConcatZ"),
                   helper$make_node("Concat", mask_list, list("win_mask"), axis=1L, name="ConcatMask"),
                   helper$make_node("Concat", vbp_rows, list("votes_by_pair"), axis=0L, name="ConcatVotesByPair"),
                   helper$make_node("ReduceSum", list("votes_vector"), list("total_votes"), keepdims=1L, name="TotalVotes"))
      }
      
      graph <- helper$make_graph(
        nodes = nodes,
        name = paste0("svmRBF_ovo_", use_rule, "_fp"),
        inputs = list(inp),
        outputs = outs,
        initializer = c(init_common, oh_inits)
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = paste0("ovo_rbf_", use_rule, "_export"),
        opset_imports = list(helper$make_operatorsetid("", 13L))
      )
      model_onnx$ir_version <- 10L
      onnx$save(model_onnx, out_path)
      cat("ONNX (RBF, ", use_rule, ", dtype=", dtype,
          ", primary_output=", primary_output,
          ", tie_rule=", tie_rule, ") gespeichert: ", out_path, "\n", sep = "")
    }
    
    
    
    build_rf_teclassifier_onnx <- function(arr, feature_names, classes, out_path, means = NULL, sds = NULL, dtype = c("float32","float64"), primary_output = c("idx1","scores"), tie_eps = 1e-6) {
      
      dtype <- match.arg(dtype)
      primary_output <- match.arg(primary_output)
      
      onnx <- reticulate::import("onnx", convert = FALSE)
      helper <- reticulate::import("onnx.helper", convert = FALSE)
      np <- reticulate::import("numpy", convert = FALSE)
      TensorProto <- onnx$TensorProto
      tp <- reticulate::tuple
      
      is_f64 <- identical(dtype, "float64")
      TNUM <- if (is_f64) TensorProto$DOUBLE else TensorProto$FLOAT
      as_fp_vec <- function(x) np$array(as.numeric(x), dtype = if (is_f64) "float64" else "float32")
      as_fp_mat <- function(M){
        if (is.vector(M)) M <- matrix(M, nrow=1L)
        np$array(as.numeric(M), dtype = if (is_f64) "float64" else "float32")
      }
      
      K <- length(classes)
      n_feat <- length(feature_names)
      
      inp <- helper$make_tensor_value_info("input_raw", TNUM, list(-1L, as.integer(n_feat)))
      out_i <- helper$make_tensor_value_info("idx1", TensorProto$INT64, list(-1L, 1L))
      out_s <- helper$make_tensor_value_info("scores", TNUM, list(-1L, as.integer(K)))
      outs  <- list(out_i, out_s)
      reorder_outs <- function(outs, names_vec, primary){
        idx <- match(primary, names_vec)
        if (is.na(idx) || idx == 1L) return(outs)
        c(outs[idx], outs[-idx])
      }
      outs <- reorder_outs(outs, c("idx1","scores"), primary_output)
      
      
      initializers <- list(
        onnx$numpy_helper$from_array(np$array(1L, dtype="int64"), "one_i"), 
        onnx$numpy_helper$from_array(as_fp_mat((K - seq_len(K)) * tie_eps), "tie_bias"),
        onnx$numpy_helper$from_array(as_fp_vec(0.0), "zero_f") 
      )
      
      nodes <- list()
      cur_in <- "input_raw"
      
      
      if (!is.null(means) && !is.null(sds)) {
        stopifnot(length(means) == n_feat, length(sds) == n_feat)
        initializers <- c(initializers,
                          onnx$numpy_helper$from_array(as_fp_mat(means), "mean_vec"),
                          onnx$numpy_helper$from_array(as_fp_mat(sds), "std_vec")
        )
        
        nodes <- c(nodes,
                   helper$make_node("Sub", list(cur_in, "mean_vec"), list("centered"), name="Center"),
                   helper$make_node("Div", list("centered","std_vec"), list("scaled"), name="Scale")
        )
        cur_in <- "scaled"
      }
      
      
      
      mvt <- tryCatch(as.integer(arr$nodes_missing_value_tracks_true), error = function(e) NULL)
      if (is.null(mvt) || length(mvt) != length(arr$nodes_modes)) {
        mvt <- rep(1L, length(arr$nodes_modes))
      }
      
      
      nodes <- c(nodes,
                 helper$make_node(
                   "TreeEnsembleClassifier",
                   inputs = list(cur_in),
                   outputs = list("label_unused", "scores_raw"),
                   domain = "ai.onnx.ml",
                   nodes_treeids = as.integer(arr$nodes_treeids),
                   nodes_nodeids = as.integer(arr$nodes_nodeids),
                   nodes_featureids = as.integer(arr$nodes_featureids),
                   nodes_modes = as.character(arr$nodes_modes),
                   nodes_values = as.numeric(arr$nodes_values),
                   nodes_truenodeids = as.integer(arr$nodes_truenodeids),
                   nodes_falsenodeids  = as.integer(arr$nodes_falsenodeids),
                   nodes_missing_value_tracks_true = as.integer(mvt),  
                   class_treeids = as.integer(arr$leaf_treeids),
                   class_nodeids = as.integer(arr$leaf_nodeids),
                   class_ids = as.integer(arr$leaf_targets),    
                   class_weights = as.numeric(arr$leaf_weights),
                   classlabels_int64s = as.integer(0:(K-1)),
                   post_transform  = "NONE"
                 )
      )
      
      
      nodes <- c(nodes,
                 helper$make_node("Add", list("scores_raw","tie_bias"), list("scores_biased"), name="AddTieBias"),
                 helper$make_node("Identity", list("scores_biased"), list("scores"), name="ScoresOut")
      )
      
      
      nodes <- c(nodes,
                 helper$make_node("ArgMax", list("scores_biased"), list("idx0"), axis = 1L, keepdims = 1L, name = "ArgMaxTop1"),
                 helper$make_node("Cast", list("idx0"), list("idx0_i64"), to = TensorProto$INT64, name = "CastI64"),
                 helper$make_node("Add", list("idx0_i64","one_i"), list("idx1"), name="MakeIdx1") )
      
      graph <- helper$make_graph(
        nodes = nodes,
        name = "rf_tree_ensemble_classifier_idx1",
        inputs = list(inp),
        outputs = outs,
        initializer = initializers
      )
      model_onnx <- helper$make_model(
        graph,
        producer_name = "rf_to_onnx_treeensemble_classifier_idx1",
        opset_imports = list(
          helper$make_operatorsetid("", 13L),
          helper$make_operatorsetid("ai.onnx.ml", 3L)
        )
      )
      
      model_onnx$ir_version <- 10L
      onnx$save_model(model_onnx, out_path)
      cat("ONNX : ", out_path, "\n", sep = "")
    }

    infer_torch_meta <- function(model, input_channels = NULL, time_steps = NULL) {
      `%||%` <- function(a,b) if (!is.null(a) && length(a) > 0) a else b

      has_conv <- tryCatch(
        !is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
          length(model$conv_layers[[1]]) >= 1 &&
          !is.null(model$conv_layers[[1]][[1]]$in_channels),
        error = function(e) FALSE
      )

      if (has_conv) {
        first_conv <- model$conv_layers[[1]][[1]]
        input_channels <- as.integer(first_conv$in_channels %||% input_channels)
        time_steps <- as.integer(model$time_steps %||% time_steps)
        return(list(input_chan = input_channels, time_steps = time_steps, input_layout = "NCT"))
      }

      ic_from_model <- tryCatch(model$input_channels, error = function(e) NULL)
      ts_from_model <- tryCatch(model$time_steps, error = function(e) NULL)
      il_from_model <- tryCatch(model$input_layout, error = function(e) NULL)
      cols_from_mod <- tryCatch(model$input_data_columns, error = function(e) NULL)

      input_channels <- input_channels %||% ic_from_model %||%
        (if (!is.null(cols_from_mod)) length(cols_from_mod) else NULL)

      time_steps <- time_steps %||% ts_from_model %||% 1L
      input_layout <- toupper(il_from_model %||% "NCT")

      if (is.null(input_channels) || input_channels < 1L)
        stop("Could not infer input_channels for generic Torch model.")
      if (is.null(time_steps) || time_steps < 1L)
        stop("Could not infer time_steps for generic Torch model.")

      list(input_chan = as.integer(input_channels),
          time_steps = as.integer(time_steps),
          input_layout = input_layout)
    }


save_torch_model <- function(model, filepath, input_channels = NULL, time_steps = NULL) {

  `%||%` <- function(a,b) if (!is.null(a) && length(a) > 0) a else b

  has_conv <- tryCatch(
    !is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
      length(model$conv_layers[[1]]) >= 1 &&
      !is.null(model$conv_layers[[1]][[1]]$in_channels),
    error = function(e) FALSE
  )

  if (has_conv) {
    first_conv <- model$conv_layers[[1]][[1]]
    input_channels <- as.integer(first_conv$in_channels)
    time_steps <- as.integer(model$time_steps %||% stop("time_steps missing in model"))

    B <- 1L
    dummy <- torch::torch_randn(c(B, input_channels, time_steps))
    model$eval()

    script_model <- torch::jit_trace(model, dummy)
    #torch::jit_save(script_model, filepath)

    script_model$eval()
    torch::jit_save(script_model, filepath)

    return(list(
      pt_path = filepath,
      input_chan = input_channels,
      time_steps = time_steps,
      input_layout = "NCT"
    ))
  }

  ic_from_model <- tryCatch(model$input_channels, error = function(e) NULL)
  ts_from_model <- tryCatch(model$time_steps, error = function(e) NULL)
  il_from_model <- tryCatch(model$input_layout, error = function(e) NULL)
  cols_from_mod <- tryCatch(model$input_data_columns, error = function(e) NULL)

  input_channels <- input_channels %||% ic_from_model %||%
    (if (!is.null(cols_from_mod)) length(cols_from_mod) else NULL)
  time_steps <- time_steps %||% ts_from_model %||% 1L
  input_layout <- toupper(il_from_model %||% "NCT")

  if (is.null(input_channels) || input_channels < 1L)
    stop("input_channels missing for Torch export.")
  if (is.null(time_steps) || time_steps < 1L)
    stop("time_steps missing for Torch export.")

  if (inherits(model, "jit_script_module")) {
    torch::jit_save(model, filepath)
    return(list(
      pt_path = filepath,
      input_chan = as.integer(input_channels),
      time_steps = as.integer(time_steps),
      input_layout = input_layout
    ))
  }

  B <- 1L
  dummy <- if (input_layout == "NC") {
    torch::torch_randn(c(B, as.integer(input_channels)))
  } else {
    torch::torch_randn(c(B, as.integer(input_channels), as.integer(time_steps)))
  }

  model$eval()

  script_model <- torch::jit_trace(model, dummy)

  script_model$eval()
  torch::jit_save(script_model, filepath)

  list(
    pt_path = filepath,
    input_chan = as.integer(input_channels),
    time_steps = as.integer(time_steps),
    input_layout = input_layout
  )
}


    
    
    
convert_torch_to_onnx_from_pt <- function(script_pt, input_chan, time_steps, base_name, output_dir) {
  message("Start ONNX conversion...")
  onnx_path <- ensure_extension(file.path(output_dir, base_name), "onnx")
  py_file <- tempfile(fileext = ".py")

py_code <- sprintf(
"import torch, warnings
warnings.filterwarnings('ignore', category=UserWarning)

pt_path = r'%s'
onnx_path = r'%s'
C = %d
T = %d

sm = torch.jit.load(pt_path)
sm.eval()

try:
    print('method names:', sm._c._method_names())
except Exception as e:
    print('method names: <unavailable>', e)

dummy = torch.randn(1, C, T)

def export_with_evalforward():
    class Wrap(torch.nn.Module):
        def __init__(self, sm):
            super().__init__()
            self.sm = sm
        def forward(self, x):
            return self.sm.evalforward(x)

    m = Wrap(sm).eval()
    m_script = torch.jit.script(m)
    m_script.eval()

    torch.onnx.export(
        m_script, dummy, onnx_path,
        input_names=['input'], output_names=['output'],
        dynamic_axes={'input': {0: 'batch_size', 2: 'time_steps'},
                      'output': {0: 'batch_size'}},
        opset_version=14
    )

def export_with_trainforward():
    class Wrap(torch.nn.Module):
        def __init__(self, sm):
            super().__init__()
            self.sm = sm
        def forward(self, x):
            return self.sm.trainforward(x)

    m = Wrap(sm).eval()
    m_script = torch.jit.script(m)
    m_script.eval()

    torch.onnx.export(
        m_script, dummy, onnx_path,
        input_names=['input'], output_names=['output'],
        dynamic_axes={'input': {0: 'batch_size', 2: 'time_steps'},
                      'output': {0: 'batch_size'}},
        opset_version=14
    )

def export_with_forward():
    class Wrap(torch.nn.Module):
        def __init__(self, sm):
            super().__init__()
            self.sm = sm
        def forward(self, x):
            return self.sm(x)

    m = Wrap(sm).eval()
    m_script = torch.jit.script(m)
    m_script.eval()

    torch.onnx.export(
        m_script, dummy, onnx_path,
        input_names=['input'], output_names=['output'],
        dynamic_axes={'input': {0: 'batch_size', 2: 'time_steps'},
                      'output': {0: 'batch_size'}},
        opset_version=14
    )

# --- Try order: evalforward -> trainforward -> forward ---
try:
    print('Trying evalforward wrapper...')
    export_with_evalforward()
    print('ONNX successfully saved (evalforward):', onnx_path)
except Exception as e1:
    print('evalforward failed:', repr(e1))
    try:
        print('Trying trainforward wrapper...')
        export_with_trainforward()
        print('ONNX successfully saved (trainforward):', onnx_path)
    except Exception as e2:
        print('trainforward failed:', repr(e2))
        print('Trying forward wrapper...')
        export_with_forward()
        print('ONNX successfully saved (forward):', onnx_path)
",
  script_pt, onnx_path, as.integer(input_chan), as.integer(time_steps)
)




  writeLines(py_code, con = py_file)

  python_bin <- find_python_bin()
  res <- system2(python_bin, args = c(py_file), stdout = TRUE, stderr = TRUE)
  cat(res, sep = "\n")

  unlink(py_file)

  if (!file.exists(onnx_path)) stop("ONNX export failed:\n", paste(res, collapse = "\n"))
  message("ONNX stored under: ", onnx_path)
  return(onnx_path)
}

    
  convert_model_to_pkl <- function(model, model_type, filepath) {
      library(reticulate)
      message("Saving model as pickle...")
      xgboost <- import("xgboost")

      if (model_type == "xgb.Booster") {
        message("Saving xgboost Booster model...")
        xgboost::xgb.save(model, paste0(filepath, ".bin"))
      } else {
        stop("Model type is not supported!")
      }
  }
    
  add_metadata_to_onnx <- function(onnx_path, options) {
      message("add metadata to onnx")
      
      if (is.null(options) || length(options) == 0 || length(names(options)) == 0) {
        message("No metadata to add (options empty).")
        return(invisible(NULL))
      }
      message("Adding metadata to ONNX model...")
      message("onnx_path: ", onnx_path)
      message("options: ", paste(names(options), collapse = ", "))

      tryCatch({
        onnx <- reticulate::import("onnx")
        model <- onnx$load_model(onnx_path)
  
        message("onnx imported")
      }, error = function(e) {
        stop("The 'onnx' Python package is required: ", e$message)
      })

  
      for (key in names(options)) {
        val <- options[[key]]
        if(is.list(val)){
          value <- paste(unlist(val), collapse = ", ")
        }else{
          value <- paste(as.character(val), collapse = ", ")
        }
        meta_prop <- onnx$StringStringEntryProto(key = key, value = value)
        model$metadata_props$append(meta_prop)
      }
      onnx$save_model(model, onnx_path)
      message("Metadata has been added to the ONNX model.")
    }
    
    
 save_ml_model_as_onnx <- function(model_type, filepath) {
  if (model_type != "xgb.Booster") stop("Model type is not supported!")

  onnx_file <- paste0(filepath, ".onnx")
  bin_file  <- paste0(filepath, ".bin")
  if (!file.exists(bin_file)) stop("XGBoost model file not found: ", bin_file)

  message("Converting XGBoost model to ONNX...")

  py <- sprintf("
import json
import xgboost as xgb
from onnxmltools import convert_xgboost
from onnxconverter_common.data_types import FloatTensorType
import onnx

bin_path  = r'''%s'''
onnx_path = r'''%s'''

bst = xgb.Booster()
bst.load_model(bin_path)

n_features = int(bst.num_features())
print('num_features =', n_features)

orig_names = bst.feature_names

if orig_names is None:
    try:
        dump = bst.get_dump(dump_format='json')
        seen = []
        for tree_s in dump:
            tree = json.loads(tree_s)
            stack = [tree]
            while stack:
                node = stack.pop()
                if isinstance(node, dict):
                    if 'split' in node:
                        s = node['split']
                        if s not in seen:
                            seen.append(s)
                    for k in ('children',):
                        if k in node and isinstance(node[k], list):
                            stack.extend(node[k])
        if len(seen) == n_features:
            orig_names = seen
        else:
            orig_names = None
    except Exception:
        orig_names = None

print('orig feature_names =', orig_names)

conv_names = [f'f{i}' for i in range(n_features)]
bst.feature_names = conv_names

onnx_model = convert_xgboost(
    bst,
    initial_types=[('float_input', FloatTensorType([None, n_features]))]
)

if orig_names is not None and len(orig_names) == n_features:
    mapping = {conv_names[i]: orig_names[i] for i in range(n_features)}
    meta = onnx_model.metadata_props.add()
    meta.key = 'feature_name_mapping'
    meta.value = json.dumps(mapping)
else:
    meta = onnx_model.metadata_props.add()
    meta.key = 'feature_name_mapping'
    meta.value = ''  # leer = nicht rekonstruierbar

onnx.checker.check_model(onnx_model)
onnx.save_model(onnx_model, onnx_path)
print('OK: saved', onnx_path)
", bin_file, onnx_file)

  reticulate::py_run_string(py)
  return(onnx_file)
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
          hyperparameters <- list(cost = cost, nSV = nSV, kernel = if (model_type == "svmLinear") "linear" else if (model_type == "svmRadial") "radial" else "poly")
        } else if (model_type == "xgbTree") {
          nrounds <- model$finalModel$niter
          model_info$"mlm:total_parameters" <- nrounds
          hyperparameters <- list(
            max_depth = model$finalModel$tuneValue$max_depth,
            nrounds = nrounds,
            eta = model$finalModel$tuneValue$eta,
            gamma = model$finalModel$tuneValue$gamma
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
      
      model_info <- list()
      message("model_info", model_info)
      
      is_tempcnn <- tryCatch(
        !is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
          length(model$conv_layers[[1]]) >= 1 &&
          !is.null(model$conv_layers[[1]][[1]]$in_channels),
        error = function(e) FALSE
      )
      
      if (is_tempcnn) {
        message("tempcnn detected...")
        
        total_params <- tryCatch({
          if (requireNamespace("coro", quietly = TRUE)) {
            s <- 0L
            coro::loop(for (p in model$parameters) {
              s <- s + prod(as.integer(p$size()))
            })
            as.integer(s)
          } else {
            sum(unlist(lapply(model$parameters, function(p) prod(as.integer(p$size())))))
          }
        }, error = function(e) NA_integer_)
        
        model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
        model_info$"mlm:architecture" <- "TempCNN"
        model_info$"mlm:tasks" <- tasks
        model_info$"mlm:framework" <- "R (torch)"
        model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
        model_info$"mlm:total_parameters" <- if (isTRUE(is.finite(total_params))) as.integer(total_params) else NULL
        model_info$"mlm:hyperparameters" <- list(conv_layers = length(model$conv_layers), dense_layers = tryCatch(length(model$dense), error=function(e) 0L))
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- "gpu"
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        
        params <- model$parameters
        input_channels <- params[[1]]$size()[2]
        
        params <- model$parameters
        input_channels <- params[[1]]$size()[2]
        
        time_steps <- tryCatch(model$time_steps, error = function(e) NULL)
        
        bands <- tryCatch(model$input_data_columns, error = function(e) NULL)
        
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
        
      
        output_size <- tryCatch({
          B <- 1L
          library(torch)
          dummy <- torch::torch_zeros(
            c(B, input_channels, time_steps),
            dtype = torch::torch_float()
          )
          model$eval()
          out <- torch::with_no_grad({
            model(dummy)
          })
          out <- out$size()[2]
        
          out
        }, error = function (e){
          message("Error Output_size", conditionMessage(e))
        })


        message("output_size", output_size)


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
            "classification:classes" = if (!is.null(tasks) && length(tasks) >= 1L && identical(tasks[[1]], "classification")) {
              lapply(0:(output_size - 1), function(i) list(value = i, name = paste("class", i), description = paste("Class", i)))
            } else NULL,
            post_processing_function = NULL
          )
        )
        
      } else if ("nn_module" %in% class(model)) {
        
        
        arch_name <- tryCatch({
          cls <- class(model)
          pick <- cls[!cls %in% c("nn_module", "R6", "R6Class")]
          if (length(pick) >= 1) pick[1] else "DLModel"
        }, error = function(e) "DLModel")
        
        total_params <- tryCatch({
          if (requireNamespace("coro", quietly = TRUE)) {
            s <- 0L
            coro::loop(for (p in model$parameters) {
              s <- s + prod(as.integer(p$size()))
            })
            as.integer(s)
          } else {
            sum(unlist(lapply(model$parameters, function(p) prod(as.integer(p$size())))))
          }
        }, error = function(e) NA_integer_)
        
        model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
        model_info$"mlm:architecture" <- arch_name
        model_info$"mlm:tasks" <- tasks
        model_info$"mlm:framework" <- "R (torch)"
        model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
        model_info$"mlm:total_parameters" <- if (isTRUE(is.finite(total_params))) as.integer(total_params) else NULL
        model_info$"mlm:pretrained" <- FALSE
        model_info$"mlm:pretrained_source" <- NULL
        model_info$"mlm:batch_size_suggestion" <- 1
        model_info$"mlm:accelerator" <- "gpu"
        model_info$"mlm:accelerator_constrained" <- FALSE
        model_info$"mlm:accelerator_count" <- 1
        
        input_channels <- tryCatch(model$input_channels, error = function(e) NULL)
        time_steps <- tryCatch(model$time_steps, error = function(e) NULL)
        bands <- tryCatch(model$input_data_columns, error = function(e) NULL)
        layout <- tryCatch(toupper(model$input_layout), error = function(e) "NCT")
        if (is.null(layout) || is.na(layout)) layout <- "NCT"
        
        if (is.null(input_channels) && !is.null(bands)) input_channels <- length(bands)
        if (is.null(input_channels) || input_channels < 1L)
          stop("Could not infer input_channels for generic DL model.")
        if (is.null(time_steps) || time_steps < 1L) time_steps <- 1L  # MLP → 1
        
        if (identical(layout, "NC")) {
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
            name = paste(arch_name, "Input"),
            bands = if (!is.null(bands)) as.list(bands) else list("unknown"),
            input = list(shape = shape, dim_order = dim_order, data_type = "float32"),
            description = desc,
            pre_processing_function = NULL
          )
        )
        
        
        output_size <- tryCatch({
          
          B <- 1L
          library(torch)
          dummy <- torch::torch_zeros(
            c(B, input_channels, time_steps),
            dtype = torch::torch_float()
          )
          
          model$eval()
          out <- torch::with_no_grad({
            model(dummy)
          })
          out <- out$size()[2]
          
          out
        }, error = function (e){
          message("Error Output_size", conditionMessage(e))
        })

        message("output_size", output_size)

        if (!isTRUE(is.finite(output_size) && output_size >= 1L))
          stop("Could not infer output_size for generic DL model.")
        
        model_info$"mlm:output" <- list(
          list(
            name = paste(arch_name, "Output"),
            tasks = tasks,
            result = list(
              shape = list(1, as.integer(output_size)),
              dim_order = list("batch", "features"),
              data_type = "float32"
            ),
            description = paste("Output features from", arch_name),
            "classification:classes" = if (!is.null(tasks) && length(tasks) >= 1L && identical(tasks[[1]], "classification")) {
              lapply(0:(output_size - 1), function(i)
                list(value = i, name = paste("class", i), description = paste("Class", i)))
            } else NULL,
            post_processing_function = NULL
          )
        )
        
        if (length(options) > 0) {
          for (key in names(options)) {
            if (grepl("^mlm:", key)) model_info[[key]] <- options[[key]]
          }
        }
        
      } else {
        stop("Unknown model type: Please check the model!")
      }
      
      mlm_stac_item$properties <- c(mlm_stac_item$properties, model_info)
      
      rds_path <- ensure_extension(sub("\\.json$", "", filepath), "rds")
      con <- rawConnection(raw(0), "wb"); on.exit(close(con), add = TRUE)
      torch::torch_save(model, con)
      raw_model <- rawConnectionValue(con)
      saveRDS(raw_model, file = rds_path)
      
      mlm_stac_item$assets <- list(
        model = list(
          href = rds_path,
          type = "application/octet-stream",
          title = paste0(model_info$`mlm:architecture`, " Model"),
          "mlm:artifact_type" = "R (Raw RDS)",
          roles = list("mlm:model")
        )
      )
      
      jsonlite::write_json(mlm_stac_item, path = filepath, auto_unbox = TRUE, pretty = TRUE)
      message("Model was saved as MLM-STAC-JSON under: ", filepath)
      return(filepath)
    }

    save_model_as_mlm_stac_json_xgboost <- function(model, filepath, tasks = list("classification"), options = list()) {
  
      is_class <- attr(model, "classification")
      labels <- attr(model, "class_levels")
      predictor_names <- attr(model, "predictor_names")
      params <- attr(model, "params")
      nrounds <- attr(model, "nrounds")
      
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
      model_info$"mlm:name" <- basename(sub("\\.json$", "", filepath))
      model_info$"mlm:architecture" <- "XGBoost"
      model_info$"mlm:tasks" <- tasks
      model_info$"mlm:framework" <- "R (xgboost)"
      model_info$"mlm:framework_version" <- paste0("R ", R.version$major, ".", R.version$minor)
      model_info$"mlm:total_parameters" <- nrounds
      
      model_info$"mlm:hyperparameters" <- list(
        max_depth = params$max_depth,
        eta = params$eta,
        nrounds = nrounds,
        objective = params$objective
      )
      
      n_features <- length(predictor_names)
      model_info$"mlm:input" <- list(
        list(
          name = "XGBoost Input",
          bands = as.list(predictor_names),
          input = list(
            shape = list(NULL, as.integer(n_features)),
            dim_order = list("batch", "features"),
            data_type = "float32"
          )
        )
      )
      
      if (isTRUE(is_class)) {
        n_classes <- length(labels)
        model_info$"mlm:output" <- list(
          list(
            name = "XGBoost Output",
            tasks = tasks,
            result = list(
              shape = list(NULL, as.integer(n_classes)),
              dim_order = list("batch", "classes"),
              data_type = "float32"
            ),
            "classification:classes" = lapply(seq_along(labels), function(i) {
              list(value = i - 1, name = labels[i])
            })
          )
        )
      } else {
        model_info$"mlm:output" <- list(
          list(
            name = "XGBoost Output",
            tasks = list("regression"),
            result = list(
              shape = list(NULL, 1L),
              dim_order = list("batch", "value"),
              data_type = "float32"
            )
          )
        )
      }
      
      mlm_stac_item$properties <- c(mlm_stac_item$properties, model_info)
      
      jsonlite::write_json(mlm_stac_item, path = filepath, auto_unbox = TRUE, pretty = TRUE)
      message("MLM STAC JSON saved to: ", filepath)
      return(filepath)
    }
    
    
    
    # ================== OPERATION BODY ==================
    
    if (missing(options) || is.null(options)) options <- list() else options <- as.list(options)
    if (!missing(tasks) && length(tasks) > 0) options[["mlm:tasks"]] <- as.list(tasks)
    
    result <- list()
    tmp <- shared_dir
    
    message("data class: ", paste(class(data), collapse = ", "))
    
    if (is.character(data) && length(data) == 1 && file.exists(data) && grepl("\\.pt$", data, ignore.case = TRUE)) {
      message("Torch model file detected...")
      `%||%` <- function(a,b) if (!is.null(a) && length(a) > 0) a else b
      nz1 <- function(x) if (is.null(x) || length(x) == 0) NULL else x
      is_bad_dim <- function(x) { is.null(x) || length(x) != 1L || !is.finite(x) || x < 1 }
      
      tryCatch({
        model <- torch::torch_load(data)
      }, error = function(e) {
        message("Error loading TorchScript model: ", e$message)
        stop(e)
      })
      message("Model loaded.")


      has_conv <- tryCatch(
        !is.null(model$conv_layers) && length(model$conv_layers) >= 1 &&
          length(model$conv_layers[[1]]) >= 1 &&
          !is.null(model$conv_layers[[1]][[1]]$in_channels),
        error = function(e) FALSE
      )
      if (has_conv) {
        message("TempCNN model detected...")
        first_conv <- model$conv_layers[[1]][[1]]
        input_channels <- first_conv$in_channels
        time_steps <- model$time_steps
        
        pt_meta <- save_torch_model(
          model,
          file.path(shared_dir, paste0(base_name, ".pt")),
          input_channels = input_channels,
          time_steps = time_steps
        )
      }
      else {
        message("Generic Torch model detected...")
        pt_meta <- save_torch_model(
          model,
          file.path(shared_dir, paste0(base_name, ".pt"))
        )
      }
      message("Convertert")
      result$onnx <- convert_torch_to_onnx_from_pt(
        script_pt = pt_meta$pt_path,
        input_chan = pt_meta$input_chan,
        time_steps = pt_meta$time_steps,
        base_name = base_name,
        output_dir = shared_dir
      )
      
      json_file <- file.path(shared_dir, ensure_extension(base_name, "json"))
      tryCatch({
        save_model_as_mlm_stac_json_dl(model, json_file, tasks, options)
      }, error = function(e) {
        message("ERROR in save_model_as_mlm_stac_json_dl: ", conditionMessage(e))
      })
      
      message("STAC JSON erwartet bei: ", json_file, "exists: ", file.exists(json_file))
      result$json <- json_file
         
    } 
    
    else if (inherits(data, "train")) {
      message("Machine model detected...")
      
      if (inherits(data$finalModel, "ksvm")) {
        message("Detected SVM (kernlab::ksvm) -> custom ONNX path.")
        res <- export_caret_ksvm_to_onnx(
          train_obj = data,
          out_base = file.path(tmp, base_name),
          use_rule = "majority",
          primary_output = "idx1",
          dtype = "float32",
          do_checks = TRUE
        )
        result$onnx <- res$onnx
        
      } else if (inherits(data$finalModel, "randomForest")) {
        message("Detected RF...")
        res <- export_rf_onnx(
          data, out_base = file.path(tmp, base_name),
          dtype = "float32", primary_output = "idx1", do_checks = TRUE
        )
        result$onnx <- res$onnx_path
        
        extra_opts <- c(
          options,
          list(
            feature_names = paste(res$features, collapse = ","),
            class_labels = paste(res$classes,  collapse = ","),
            zero_based = "true"
          )
        )
        add_metadata_to_onnx(result$onnx, extra_opts)
        
      } else {
        message("Detected other ML model...")
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
    }
    
    else if(inherits(data, "xgb.Booster")) {
      message("XGBoost Booster model detected...")
      convert_model_to_pkl(data, "xgb.Booster", file.path(tmp, base_name))
      onnx_path <- save_ml_model_as_onnx("xgb.Booster", file.path(tmp, base_name))
      result$onnx <- onnx_path
      json_file <- file.path(tmp, ensure_extension(base_name, "json"))
      save_model_as_mlm_stac_json_xgboost(data, json_file, tasks, options)
      result$json <- json_file
      rds_path <- file.path(tmp, ensure_extension(base_name, "rds"))
      saveRDS(data, rds_path)
      result$rds <- rds_path
      message("RDS saved: ", rds_path)
    }
    else {
      stop("Unknown model type: must be 'nn_module' (Torch), XGBoost or 'train' (Caret).")
    }
  
    rds_path <- file.path(tmp, ensure_extension(base_name, "rds"))
    result$rds <- rds_path
    
    if (length(options) > 0 && length(names(options)) > 0 && !is.null(result$onnx) && file.exists(result$onnx)) {
      add_metadata_to_onnx(result$onnx, options)
    }
    
    download_base <- Sys.getenv("DOWNLOAD_BASE_URL", "http://localhost:8000/download/")
    download_links <- list(
      onnx = sprintf("%s%s", download_base, basename(result$onnx)),
      json = sprintf("%s%s", download_base, basename(result$json)),
      rds = sprintf("%s%s", download_base, basename(result$rds))
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
    "pt" = "application/octet-stream",
    "pkl" = "application/octet-stream",
    "bin" = "application/octet-stream",
    "txt" = "text/plain",
    "application/octet-stream"
  )
  
  res$setHeader("Content-Type", content_type)
  res$setHeader("Content-Disposition", sprintf('attachment; filename="%s"', filename))
  
  file_content <- readBin(file_path, "raw", n = file.info(file_path)$size)
  res$body <- file_content
  
  return(res)
  
}