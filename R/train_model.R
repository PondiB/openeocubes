train_model_opp = function(data, model_type, labeled_polygons, hyperparameters = NULL, save_model = FALSE, model_id = NULL, job)
{
  # show call stack for debugging
  message("train_model called...")

  message("\nCall parameters: ")
  message("\ndata: ")
  print(data)
  message("\nmodel_type: ")
  message(model_type)

  tryCatch({
    message("\nlabeled_polygons: ")

    # read GeoJSON data as sf
    labeled_polygons = sf::st_read(labeled_polygons, quiet = TRUE)

    # change CRS to cube CRS
    labeled_polygons = sf::st_transform(labeled_polygons, crs = gdalcubes::srs(data))

    message("Training Polygons sucessfully loaded!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop("couldn't load training polygons!")
  })

  message("\nhyperparameters: ")
  if (is.null(hyperparameters))
  {
    message("No Hyperparameters passed!")
  }
  else
  {
    for (name in names(hyperparameters))
    {
      message(paste0(name, ": ", hyperparameters[name]))
    }
  }

  message("\nsave_model:")
  message(save_model)

  message("\nmodel_id:")
  print(model_id) # to also show "NULL"


  if (!is.numeric(labeled_polygons$class))
  {
    stop("class labels need to be numeric")
  }


  # obvios boolean check for mor readibility
  if (save_model == TRUE && is.null(model_id))
  {
    message("If the model should be safed, a model_id needs to be given!")
    stop("If the model should be safed, a model_id needs to be given!")
  }

  tryCatch({
    message("\nExtract features...")

    # extract features from cube
    features = gdalcubes::extract_geom(data, labeled_polygons)

    message("all features extracted!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop("Features couldn't be extracted")
  })

  # add FID for merge with 'features'
  labeled_polygons$FID = rownames(labeled_polygons)

  tryCatch({
    message("\nMerge features with training data...")

    # this df contains all information from the datacube and the labeled_polgons
    training_df = merge(labeled_polygons, features, by = "FID")

    message("Merging complete!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop("Merging data.frames failed")
  })

  # make copy to filter out values not needed for training
  training_df_filtered = training_df

  training_df_filtered$time = NULL
  training_df_filtered$geometry = NULL
  # convert numbers to "X<number>" to make valid class names
  training_df_filtered$class = base::make.names(training_df_filtered$class)

  message("\nclasses in training_df ")
  print(unique(training_df_filtered$class))

  message("\nFeatures in 'training_df': ", nrow(training_df))

  #TODO: find reasonable threshold
  if (nrow(training_df) > 10000)
  {
    tryCatch({
      message("\nReduce number of features...")
      # data frame for later storage
      training_df_reduced = data.frame()

      # from all data with the same FID (same polygon) take only 50% of the
      # features for each training polygon as they are assumed to carry similar information
      for (i in as.numeric(unique(training_df_filtered$FID)))
      {
        #TODO: find better "reducing" function
        sub_df = training_df_filtered[training_df_filtered$FID == i,]

        # take 50% of sub_df rows
        sub_df = sub_df[1:(nrow(sub_df)/2),]

        # append new rows
        training_df_reduced = rbind(training_df_reduced, sub_df)
      }

      # overwrite filtered df
      training_df_filtered = training_df_reduced

      message("Reducing completed!")
    },
    error = function(err)
    {
      message("An Error occured!")
      message(toString(err))
      stop("Reducing Features failed")
    })
  }

  # remove FID to not train model on FID
  training_df_filtered$FID = NULL


  tryCatch({
    message("\nSplit training Data...")

    train_row_numbers = caret::createDataPartition(
      training_df_filtered$class, p=0.8, list=FALSE
    )
    training_data = training_df_filtered[train_row_numbers,]
    testing_data = training_df_filtered[-train_row_numbers,]

    message("Data splitting completed!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop("Splitting training data failed")
  })

  # build specific model given by "model_type"
  if (model_type == "RF")
  {
    # set seed for reproducibility while model training
    set.seed(1)

    # use fixed hyperparams given by the user
    # (this may result in a lack of accuracy for the model)
    if (!is.null(hyperparameters))
    {

      message("\nChecking hyperparameters for Random Forest...")

      if (!all(c("mtry", "ntree") %in% names(hyperparameters)))
      {
        message("'hyperparameters' has to contain 'mtry' and 'ntree'!")
        stop("'hyperparameters' has to contain 'mtry' and 'ntree'!")
      }

      message("hyperparameters for Random Forest checked!")

      tryCatch({
        message("\nTrain Model with fixed hyperparameters...")

        # no parameters are tuned
        trainCtrl <- caret::trainControl(method = "none", classProbs = TRUE)

        model <- caret::train(
          class ~ .,
          data = training_data,
          method = "rf",
          trControl = trainCtrl,
          # only one model is passed (fixed hyperparams are given)
          tuneGrid = expand.grid(mtry = hyperparameters$mtry),
          ntree = hyperparameters$ntree)

        message("Model training finished!")
      },
      error = function(err)
      {
        message("An Error occured!")
        message(toString(err))
        stop("model training failed")
      })

    }
    else
    {
      # else tune model hyperparameters
      tryCatch({
        message("\nTrain Model with parameter tuning...")
        # cross-validate training data with random-parameter search
        trainCtrl <- caret::trainControl(
          search = "random",
          # 10-fold CV
          method = "repeatedcv",
          number = 10,
          # repeated 10 times
          repeats = 10)

        model <- caret::train(
          class ~ .,
          data = training_data,
          method = "rf",
          trControl = trainCtrl,
          tuneLength = 10)

        # print model and confusion matrix, to evaluate if the model is well trained
        message("\nModel Details: ")
        print(model)

        predicted_test_classes = stats::predict(model, newdata = testing_data)

        message("\nConfusion Matrix based on Test Dataset: ")
        print(caret::confusionMatrix(predicted_test_classes, as.factor(testing_data$class)))

        message("Model training finished!")
      },
      error = function(err)
      {
        message("An Error occured!")
        message(toString(err))
        stop("model training failed")
      })
    }
  }

  # save model to user workspace
  if (save_model)
  {
    tryCatch({
      message("\nSaving model to user workspace...")

      saveRDS(model, paste0(Session$getConfig()$workspace.path, "/", model_id, ".rds"))

      message("Saving complete!")
    },
    error = function(err)
    {
      message("An Error occured!")
      message(toString(err))
      stop("model saving failed")
    })
  }

  return(model)
}


#' train_model
train_model <- Process$new(
  id = "train_model",
  description = "Train a machine learning algorithm based on the provided training data on satellite imagery gathered from a datacube. This process will convert integer class values into factors by prefixing a 'X'. ",
  categories = as.array("machine-learning", "cubes"),
  summary = "train machine learning model.",
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
      name = "model_type",
      description = "Type of the model to be trained. Must be one of the following types: RF.",
      schema = list(
        type = "string"
      ),
    ),
    Parameter$new(
      name = "labeled_polygons",
      description = "String containing the GeoJSON with Polygons. These contain class labels used to train the model.",
      schema = list(
        type = "string",
        subtype = "GeoJSON"
      )
    ),
    Parameter$new(
      name = "hyperparameters",
      description = "List of Hyperparameters used for the model. If no hyperparameters are passed, the algorithm will tune the hyperparameters by random grid search and 10-times-10-fold-crossvalidation. This may take very long!",
      schema = list(
        type = "list"
      ),
      optional = TRUE
    ),
    Parameter$new(
      name = "save_model",
      description = "Declare wether the computed model should be saved in the user workspace. Defaults to false.",
      schema = list(
        type = "boolean"
      ),
      optional = TRUE
    ),
    Parameter$new(
      name = "model_id",
      description = "Id under which the model should be stored. Defaults to NULL",
      schema = list(
        type = "string"
      ),
      optional = TRUE
    )

  ),
  returns = list(
    description = "The trained model.",
    schema = list(type = "object", subtype = list("train", "train.formula"))
  ),
  operation = train_model_opp
)
