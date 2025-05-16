#' cube processes openEO standards mapped to gdalcubes processes
#'
#' @include Process-class.R
#' @import gdalcubes
#' @import rstac
#' @import useful
#' @import sf
#' @import jsonlite
NULL

#' load_process_metadata
#' @description Loads process metadata from a JSON file in the inst/processes directory
#'
#' @param process_id The ID of the process to load metadata for
#' @return A list containing the process metadata
#' @keywords internal
load_process_metadata <- function(process_id) {
  # Construct path to the JSON file
  json_path <- system.file("processes", paste0(process_id, ".json"), package = "openeocubes")

  if (json_path == "") {
    stop(paste0("Process metadata file not found for process: ", process_id))
  }

  # Read the JSON file
  metadata <- jsonlite::fromJSON(json_path)

  # Extract standard fields
  result <- list(
    id = metadata$id,
    description = metadata$description,
    summary = metadata$summary,
    categories = as.array(metadata$categories)
  )

  # Process parameters
  if (!is.null(metadata$parameters)) {
    params <- list()
    for (i in seq_along(metadata$parameters)) {
      param <- metadata$parameters[[i]]
      # Only process if param is a list and has a name field
      if (is.list(param) && !is.null(param$name)) {
        # Handle default: treat {} (empty list) as NULL
        param_default <- if (!is.null(param$default) && is.list(param$default) && length(param$default) == 0) {
          NULL
        } else {
          param$default
        }
        param_obj <- Parameter$new(
          name = param$name,
          description = param$description,
          schema = param$schema,
          optional = if (!is.null(param$optional)) {
            param$optional
          } else {
            FALSE
          },
          default = param_default
        )
        params[[length(params) + 1]] <- param_obj
      } else {
        warning(sprintf("Skipping parameter at index %d: not a valid parameter object", i))
      }
    }
    result$parameters <- params
  }

  # Process returns
  if (!is.null(metadata$returns)) {
    result$returns <- list(
      description = metadata$returns$description,
      schema = metadata$returns$schema
    )
  }

  return(result)
}


#' load collection
load_collection <- do.call(Process$new, c(
  load_process_metadata("load_collection"),
  list(
    operation = function(id,
                         spatial_extent,
                         temporal_extent,
                         bands = NULL,
                         job) {
      crs <- ifelse("crs" %in% names(spatial_extent),
        as.numeric(spatial_extent$crs),
        4326
      )
      message("crs is : ", crs)

      t0 <- temporal_extent[[1]]
      t1 <- temporal_extent[[2]]
      duration <- c(t0, t1)
      time_range <- paste(duration, collapse = "/")
      message("After Temporal extent: ", time_range)

      xmin <- as.numeric(spatial_extent$west)
      ymin <- as.numeric(spatial_extent$south)
      xmax <- as.numeric(spatial_extent$east)
      ymax <- as.numeric(spatial_extent$north)
      message("After Spatial extent ...")

      xmin_stac <- xmin
      ymin_stac <- ymin
      xmax_stac <- xmax
      ymax_stac <- ymax
      message("After default Spatial extent for stac..")
      if (crs != 4326) {
        message("crs is not 4326...")
        min_pt <- sf::st_sfc(sf::st_point(c(xmin, ymin)), crs = crs)
        min_pt <- sf::st_transform(min_pt, crs = 4326)
        min_bbx <- sf::st_bbox(min_pt)
        xmin_stac <- min_bbx$xmin
        ymin_stac <- min_bbx$ymin
        max_pt <- sf::st_sfc(sf::st_point(c(xmax, ymax)), crs = crs)
        max_pt <- sf::st_transform(max_pt, crs = 4326)
        max_bbx <- sf::st_bbox(max_pt)
        xmax_stac <- max_bbx$xmax
        ymax_stac <- max_bbx$ymax
        message("Transformed to 4326...")
      }

      message("STAC API call....")
      stac_object <- stac("https://earth-search.aws.element84.com/v0")
      items <- stac_object %>%
        stac_search(
          collections = id,
          bbox = c(xmin_stac, ymin_stac, xmax_stac, ymax_stac),
          datetime = time_range,
          limit = 10000
        ) %>%
        post_request() %>%
        items_fetch()

      img.col <- stac_image_collection(
        items$features,
        property_filter = function(x) {
          x[["eo:cloud_cover"]] < 30
        }
      )
      message("Image collection created...")

      crs <- c("EPSG", crs)
      crs <- paste(crs, collapse = ":")
      v.overview <- gdalcubes::cube_view(
        srs = crs,
        dx = 30,
        dy = 30,
        dt = "P1M",
        aggregation = "median",
        resampling = "average",
        extent = list(
          t0 = t0,
          t1 = t1,
          left = xmin,
          right = xmax,
          top = ymax,
          bottom = ymin
        )
      )

      cube <- gdalcubes::raster_cube(img.col, v.overview)

      if (!is.null(bands)) {
        cube <- gdalcubes::select_bands(cube, bands)
      }
      message("data cube is created: ")
      message(as_json(cube))
      return(cube)
    }
  )
))

#' aggregate temporal period
aggregate_temporal_period <- do.call(Process$new, c(
  load_process_metadata("aggregate_temporal_period"),
  list(
    operation = function(data,
                         period,
                         reducer,
                         dimension = NULL,
                         context = NULL,
                         job) {
      dt_period <- switch(period,
        week = "P7D",
        dekad = "P10D",
        month = "P1M",
        year = "P1Y",
        decade = "P10Y",
        stop("The specified period is not supported")
      )

      message("Aggregate temporal period ...")
      message(
        "Aggregate temporal period: ",
        dt_period,
        ", using reducer: ",
        reducer
      )

      cube <- gdalcubes::aggregate_time(
        cube = data,
        dt = dt_period,
        method = reducer
      )
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' filter bands
filter_bands <- do.call(Process$new, c(
  load_process_metadata("filter_bands"),
  list(
    operation = function(data, bands, job) {
      if (!is.null(bands)) {
        cube <- gdalcubes::select_bands(data, bands)
      }
      message("Filtered data cube ....")
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' filter bbox
filter_bbox <- do.call(Process$new, c(
  load_process_metadata("filter_bbox"),
  list(
    operation = function(data, extent, job) {
      crs <- gdalcubes::srs(data)
      nw <- c(extent$west, extent$north)
      sw <- c(extent$west, extent$south)
      se <- c(extent$east, extent$south)
      ne <- c(extent$east, extent$north)

      p <- list(rbind(nw, sw, se, ne, nw))
      pol <- sf::st_polygon(p)

      cube <- gdalcubes::filter_geom(data, pol, srs = crs)
      return(cube)
    }
  )
))

#' filter_spatial
filter_spatial <- do.call(Process$new, c(
  load_process_metadata("filter_spatial"),
  list(
    operation = function(data, geometries, job) {
      geo_data <- sf::read_sf(geometries)
      geo_data <- geo_data$geometry
      geo_data <- sf::st_transform(geo_data, 3857)
      cube <- gdalcubes::filter_geom(data, geo_data)
      return(cube)
    }
  )
))

#' filter temporal
filter_temporal <- do.call(Process$new, c(
  load_process_metadata("filter_temporal"),
  list(
    operation = function(data, extent, dimension = NULL, job) {
      if (is.null(extent)) {
        stop("The extent cannot be null.")
      }
      cube <- gdalcubes::select_time(data, c(extent[1], extent[2]))
      return(cube)
    }
  )
))

#' ndvi
ndvi <- do.call(Process$new, c(
  load_process_metadata("ndvi"),
  list(
    operation = function(data,
                         nir = "nir",
                         red = "red",
                         target_band = NULL,
                         job) {
      format_band_name <- function(band) {
        if (grepl("^B\\d{2}$", band, ignore.case = TRUE)) {
          return(toupper(band))
        } else {
          return(band)
        }
      }
      nir_formatted <- format_band_name(nir)
      red_formatted <- format_band_name(red)
      ndvi_formula <- sprintf(
        "(%s-%s)/(%s+%s)",
        nir_formatted,
        red_formatted,
        nir_formatted,
        red_formatted
      )
      cube <- gdalcubes::apply_pixel(data, ndvi_formula, names = "NDVI", keep_bands = FALSE)
      message("NDVI calculated ....")
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' evi
evi <- do.call(Process$new, c(
  load_process_metadata("evi"),
  list(
    operation = function(data,
                         nir = "nir",
                         shortwl_nir = "shortwl_nir",
                         red = "red",
                         blue = "blue",
                         target_band = NULL,
                         job) {
      format_band_name <- function(band) {
        if (grepl("^B\\d{2}$", band, ignore.case = TRUE)) {
          return(toupper(band))
        } else {
          return(band)
        }
      }
      nir_formatted <- format_band_name(nir)
      red_formatted <- format_band_name(red)
      blue_formatted <- format_band_name(blue)
      shortwl_nir_formatted <- format_band_name(shortwl_nir)
      evi_formula <- sprintf(
        "2.5*((%s-%s)/(%s+6*(%s)-7.5*(%s))+1)",
        nir_formatted,
        red_formatted,
        nir_formatted,
        shortwl_nir_formatted,
        blue_formatted
      )
      cube <- gdalcubes::apply_pixel(data, evi_formula, names = "EVI", keep_bands = FALSE)
      message("EVI calculated ....")
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' rename_dimension
rename_dimension <- do.call(Process$new, c(
  load_process_metadata("rename_dimension"),
  list(
    operation = function(data, ..., job) {
      arguments <- list(data, ...)
      cube <- do.call(rename_bands, arguments)
      message("Renamed Data Cube....")
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' reduce dimension
reduce_dimension <- do.call(Process$new, c(
  load_process_metadata("reduce_dimension"),
  list(
    operation = function(data, reducer, dimension, job) {
      if (dimension == "t" || dimension == "time") {
        bands <- bands(data)$name
        bandStr <- c()
        for (i in seq_along(bands)) {
          bandStr <- append(bandStr, sprintf("%s(%s)", reducer, bands[i]))
        }
        cube <- gdalcubes::reduce_time(data, bandStr)
        return(cube)
      } else if (dimension == "bands") {
        cube <- gdalcubes::apply_pixel(data, reducer, keep_bands = FALSE)
        return(cube)
      } else {
        stop('Please select "t", "time" or "bands" as dimension')
      }
    }
  )
))

#' resample spatial
resample_spatial <- do.call(Process$new, c(
  load_process_metadata("resample_spatial"),
  list(
    operation = function(data,
                         resolution = 0,
                         projection = NULL,
                         method = "mean",
                         align = "upper-left",
                         job) {
      if (resolution == 0 && is.null(projection)) {
        stop("At least resolution or projection must be specified.")
      }
      valid_methods <- c(
        "mean",
        "min",
        "max",
        "median",
        "count",
        "sum",
        "prod",
        "var",
        "sd"
      )
      if (!(method %in% valid_methods)) {
        stop(paste(
          "Invalid method. Please choose one of",
          toString(valid_methods)
        ))
      }
      if (!is.null(projection)) {
        stop("Currently, only resampling spatial resolution is implemented.")
      }
      cube <- if (resolution != 0) {
        gdalcubes::aggregate_space(
          cube = data,
          dx = resolution,
          dy = resolution,
          method = method
        )
      } else {
        stop("Currently, only resampling spatial resolution is implemented.")
      }
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' merge_cubes
merge_cubes <- do.call(Process$new, c(
  load_process_metadata("merge_cubes"),
  list(
    operation = function(data1, data2, context, job) {
      if ("cube" %in% class(data1) && "cube" %in% class(data2)) {
        compare <- compare.list(dimensions(data1), dimensions(data2))
        if (FALSE %in% compare) {
          stop("Dimensions of datacubes are not equal")
        } else {
          cube <- gdalcubes::join_bands(c(data1, data2))
          return(cube)
        }
      } else {
        stop('Provided cubes are not of class "cube"')
      }
    }
  )
))

#' array element
array_element <- do.call(Process$new, c(
  load_process_metadata("array_element"),
  list(
    operation = function(data,
                         index = NULL,
                         label = NULL,
                         return_nodata = FALSE,
                         job) {
      if (class(data) == "list") {
        bands <- bands(data$data)$name
      } else {
        bands <- bands(data)$name
      }
      if (!is.null(index)) {
        band <- bands[index]
      } else if (!is.null(label) && label %in% bands) {
        band <- label
      } else {
        stop("Band not found")
      }
      return(band)
    }
  )
))

#' rename labels
rename_labels <- do.call(Process$new, c(
  load_process_metadata("rename_labels"),
  list(
    operation = function(data, dimension, target, source = NULL, job) {
      if (dimension == "bands") {
        if (!is.null(source)) {
          if (class(source) == "number" || class(source) == "integer") {
            band <- as.character(bands(data)$name[source])
            cube <- gdalcubes::apply_pixel(data, band, names = target)
          } else if (class(source) == "string" ||
            class(source) == "character") {
            cube <- gdalcubes::apply_pixel(data, source, names = target)
          } else {
            stop("Source is not a number or string")
          }
        } else {
          band <- as.character(bands(data)$name[1])
          cube <- gdalcubes::apply_pixel(data, band, names = target)
        }
        return(cube)
      } else {
        stop("Only bands dimension supported")
      }
    }
  )
))

#' run_udf
run_udf <- do.call(Process$new, c(
  load_process_metadata("run_udf"),
  list(
    operation = function(data,
                         udf,
                         runtime = "R",
                         version = NULL,
                         context = NULL,
                         job) {
      if (runtime != "R") {
        stop("Only R runtime is supported.")
      }
      forbidden_patterns <- c(
        "system",
        "file",
        "write",
        "read",
        "parse",
        "eval",
        "source",
        "system2",
        "Sys.",
        "processx"
      )
      if (any(sapply(forbidden_patterns, grepl, udf))) {
        stop("UDF contains forbidden functions or commands.")
      }
      message("run UDF called")
      reducer_keywords <- c(
        "sum",
        "bfast",
        "sd",
        "mean",
        "median",
        "min",
        "reduce",
        "product",
        "max",
        "count",
        "var"
      )
      if (!("cube" %in% class(data))) {
        stop('Provided cube is not of class "cube"')
      }
      if (grepl("function", udf)) {
        if (any(sapply(reducer_keywords, grepl, udf))) {
          func_parse <- parse(text = udf)
          user_function <- eval(func_parse)
          message("reducer function -> time")
          data <- gdalcubes::reduce_time(data, names = context, FUN = user_function)
          return(data)
        } else {
          message("apply per pixel function")
          func_parse <- parse(text = udf)
          user_function <- eval(func_parse)
          data <- gdalcubes::apply_pixel(data, FUN = user_function)
          return(data)
        }
      } else {
        message("simple reducer udf")
        data <- gdalcubes::reduce_time(data, udf)
        return(data)
      }
    }
  )
))

#' load stac
load_stac <- do.call(Process$new, c(
  load_process_metadata("load_stac"),
  list(
    operation = function(url,
                         spatial_extent,
                         temporal_extent,
                         bands = NULL,
                         properties = NULL,
                         job) {
      duration <- paste(temporal_extent[[1]], temporal_extent[[2]], collapse = "/")
      xmin <- as.numeric(spatial_extent$west)
      ymin <- as.numeric(spatial_extent$south)
      xmax <- as.numeric(spatial_extent$east)
      ymax <- as.numeric(spatial_extent$north)
      stac_metadata <- rstac::stac(url) %>% rstac::get_request()
      stac_base_url <- stac_metadata$links[[4]]$href
      id <- stac_metadata$id
      stac_object <- rstac::stac(stac_base_url)
      items <- stac_object %>%
        rstac::stac_search(
          collections = id,
          bbox = c(xmin, ymin, xmax, ymax),
          datetime = duration,
          limit = 10000
        ) %>%
        rstac::post_request() %>%
        rstac::items_fetch()

      img_col <- gdalcubes::stac_image_collection(items$features)
      cube_view <- gdalcubes::cube_view(
        srs = "EPSG:4326",
        dx = 30,
        dy = 30,
        dt = "P1M",
        aggregation = "median",
        resampling = "average",
        extent = list(
          t0 = temporal_extent[[1]],
          t1 = temporal_extent[[2]],
          left = xmin,
          right = xmax,
          top = ymax,
          bottom = ymin
        )
      )
      cube <- gdalcubes::raster_cube(img_col, cube_view)
      if (!is.null(bands)) {
        cube <- gdalcubes::select_bands(cube, bands)
      }
      message(gdalcubes::as_json(cube))
      return(cube)
    }
  )
))

#' array interpolate linear
array_interpolate_linear <- do.call(Process$new, c(
  load_process_metadata("array_interpolate_linear"),
  list(
    operation = function(data, job) {
      method <- "linear"
      tryCatch(
        expr = {
          message("\nFill NA values...")
          data <- gdalcubes::fill_time(data, method)
          message("NA values filled!")
        },
        error = function(err) {
          message("An Error occured!")
          message(toString(err))
          stop(toString(err$message))
        }
      )
      return(data)
    }
  )
))

#' save result
save_result <- do.call(Process$new, c(
  load_process_metadata("save_result"),
  list(
    operation = function(data, format, options = NULL, job) {
      CORES <- parallel::detectCores()
      gdalcubes::gdalcubes_options(parallel = CORES)
      message("Data is being saved in format :")
      message(format)
      message("The above format is being saved")
      job$setOutput(format)
      return(data)
    }
  )
))
