#' @import dplyr
#' @import gdalcubes
#' @include Session-Class.R
#' @include Router.R
#' @include math-processes.R
#' @include ml-processes.R
#' @include processes.R
#' @include api_job.R
#' @include api_process_graphs.R
NULL

# Capabilities handler
.capabilities = function() {

  config = Session$getConfig()
  endpoints = Session$getEndpoints()

  endpoints = endpoints %>% group_by(path) %>% summarise(
    paths=list(tibble(path,method) %>% (function(x,...){
      return(list(path=unique(x$path),methods=as.list(x$method)))
    })))

  list = list()
  list$api_version = config$api_version
  list$backend_version = config$backend_version
  list$stac_version = config$stac_version
  list$id = config$id
  list$title = config$title
  list$description = config$description
  list$endpoints = endpoints$paths
  list$links = list(list(
    rel = "self",
    href = paste(config$base_url, "", sep = "/")))

  return(list)

}

.well_known = function() {

  version = list(versions = list())

  obj = tibble(url = Session$getConfig()$base_url,
               api_version = Session$getConfig()$api_version,
               production = FALSE)
  version$versions = obj

  return(version)


}

.file_formats = function() {

  config = Session$getConfig()

  list = list()
  list$output = config$outputFormats
  list$input = config$inputFormats

  return(list)
}

.conformance = function() {

  list = list()
  list$conformsTo = list(Session$getConfig()$OGC_conformanceLink)

  return(list)
}

.collections = function() {
  tryCatch({
    collections = list(collections = unname(lapply(Session$data, function(x) {
      return(x$collectionInfo())
    })))
    collections$links = list(list(
       rel = "self",
       href = paste(Session$getConfig()$base_url, "collections", sep = "/")
     ))

    return(collections)
  },error = handleError)
}

.collectionId = function(req, res, collection_id) {
  tryCatch({
    return (Session$data[[collection_id]]$collectionInfoExtended())
  }, error = handleError)
}

.processes = function() {
  message("processes at api.R called")
  tryCatch({
    processes = list(processes = unname(lapply(Session$processes, function(process){
      return(process$processInfo())
    })))

    processes$links = list(list(
      rel = "self",
      href = paste(Session$getConfig()$base_url, "processes", sep = "/")
    ))

    return(processes)
  }, error = handleError)
}

.login_basic = function(req, res) {
  # GDAL parameters to make accessing COGs on AWS faster
  gdalcubes::gdalcubes_options(parallel = 8) # depends on the system
  gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE", "TRUE")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_CACHEMAX","20%")
  gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE_SIZE","5000000")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MULTIPLEX","YES")
  message("GDAL commands set up to speed CoG calls")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_INGESTED_BYTES_AT_OPEN","32000")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_DISABLE_READDIR_ON_OPEN","EMPTY_DIR")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_VERSION","2")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
  message("log in from api.R called")
  auth = req$HTTP_AUTHORIZATION
  encoded = substr(auth,7,nchar(auth))
  decoded = rawToChar(base64enc::base64decode(encoded))
  user_name = unlist(strsplit(decoded,":"))[1]
  user_pwd = unlist(strsplit(decoded,":"))[2]

  tryCatch({
    config = Session$getConfig()
    if(user_name != config$user && user_pwd != config$password) {
      throwError("CredentialsInvalid")
    }
    token = "b34ba2bdf9ac9ee1"
    Session$setToken(token)

    return(list(access_token = token))
  },
  error = handleError)
}

.authorized = function(req, res) {
  tryCatch({
      auth = req$HTTP_AUTHORIZATION
      sub = substr(auth,15,nchar(auth))
      token = Session$getToken()

      if (is.null(auth)) {
        throwError("AuthenticationRequired")
      }
      else if (sub != token) {
        res$status <- 403
        list(error="AuthenticationFailed")
      }
       else {
        forward()
      }
  }, error = handleError)
}


.executeSynchronous = function(req, res) {
 tryCatch({
  sent_job = jsonlite::fromJSON(req$rook.input$read_lines(),simplifyDataFrame = FALSE)
  process_graph = sent_job$process
  newJob = Job$new(process = process_graph)

  job = newJob$run()
  format = job$output

  if (class(format) == "list") {
    if (format$title == "Network Common Data Form") {
      file = gdalcubes::write_ncdf(job$results)
    }
    else if (format$title == "GeoTiff") {
      file = gdalcubes::write_tif(job$results)
    }
    else {
      throwError("FormatUnsupported")
    }
  }
  else {
    if (format == "NetCDF") {
      file = gdalcubes::write_ncdf(job$results)
    }
    else if (format == "GTiff") {
      file = gdalcubes::write_tif(job$results)
    }
    else {
      throwError("FormatUnsupported")
    }
  }

  first = file[1]
  res$status = 200
  res$body = readBin(first, "raw", n = file.info(first)$size)
  content_type = plumber:::getContentType(tools::file_ext(first))
  res$setHeader("Content-Type", content_type)

  return(res)
},error=handleError)
}

.cors_filter = function(req,res) {
  res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
  res$setHeader("Access-Control-Expose-Headers", "Location, OpenEO-Identifier, OpenEO-Costs")
  forward()
}

.cors_option = function(req,res, ...) {
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH")
  res$status = 204
}

#' dedicate the handler functions to the corresponding paths
addEndpoint = function() {

  Session$createEndpoint(path = "/",
                         method = "GET",
                         handler = .capabilities)

  Session$createEndpoint(path = "/.well-known/openeo",
                         method = "GET",
                         handler = .well_known)

  Session$createEndpoint(path = "/file_formats",
                         method = "GET",
                         handler = .file_formats)

  Session$createEndpoint(path = "/conformance",
                         method = "GET",
                         handler = .conformance)

  Session$createEndpoint(path = "/collections",
                         method = "GET",
                         handler = .collections)

  Session$createEndpoint(path = "/collections/{collection_id}",
                         method = "GET",
                         handler = .collectionId)

  Session$createEndpoint(path = "/processes",
                         method = "GET",
                         handler = .processes)

  Session$createEndpoint(path = "/process_graphs",
                         method = "POST",
                         handler = .createProcessGraph)

  Session$createEndpoint(path = "/jobs",
                         method = "GET",
                         handler = .listAllJobs,
                         filter = TRUE)

  Session$createEndpoint(path = "/jobs",
                         method = "POST",
                         handler = .createNewJob,
                         filter = TRUE)

  Session$createEndpoint(path = "/jobs/{job_id}",
                         method = "GET",
                         handler = .getJobById,
                         filter = TRUE)

  Session$createEndpoint(path = "/jobs/{job_id}/results",
                         method = "POST",
                         handler = .startJob,
                         filter = TRUE)

 Session$createEndpoint(path = "/jobs/{job_id}/results",
                        method = "GET",
                        handler = .getJobResults,
                        filter = TRUE)

  Session$createEndpoint(path = "/jobs/{job_id}/{file}",
                         method = "GET",
                         handler = .getJobFiles,
                         filter = TRUE)

  Session$createEndpoint(path = "/credentials/basic",
                         method = "GET",
                         handler = .login_basic)

  Session$createEndpoint(path = "/result",
                         method = "POST",
                         handler = .executeSynchronous,
                         filter = TRUE)

# assign data collection
  Session$assignData(sentinel_s2_l2a_cogs)
  Session$assignData(sentinel_s2_l2a)
  Session$assignData(sentinel_s2_l1c)
  Session$assignData(landsat_8_l1_c1)
# assign processes
  Session$assignProcess(load_collection)
  Session$assignProcess(load_stac)
  Session$assignProcess(save_result)
  Session$assignProcess(aggregate_temporal_period)
  Session$assignProcess(array_element)
  Session$assignProcess(array_interpolate_linear)
  Session$assignProcess(filter_bands)
  Session$assignProcess(filter_bbox)
  Session$assignProcess(filter_spatial)
  Session$assignProcess(filter_temporal)
  Session$assignProcess(merge_cubes)
  Session$assignProcess(ndvi)
  Session$assignProcess(rename_dimension)
  Session$assignProcess(reduce_dimension)
  Session$assignProcess(rename_labels)
  Session$assignProcess(resample_spatial)
  Session$assignProcess(run_udf)
  Session$assignProcess(min)
  Session$assignProcess(max)
  Session$assignProcess(median)
  Session$assignProcess(mean)
  Session$assignProcess(add)
  Session$assignProcess(subtract)
  Session$assignProcess(multiply)
  Session$assignProcess(divide)
  Session$assignProcess(evi)
# assign ml processes
  Session$assignProcess(ml_predict)


}
