#' Session class
#'
#' @field graphs User defined process graphs
#' @field processes Package processes
#' @field data Data of current Session
#' @field jobs Stored jobs of current Session
#'
#' @include api.R
#' @importFrom R6 R6Class
#' @importFrom tibble add_row
#' @import plumber
#' @import gdalcubes
#' @import callr
#' @export
SessionInstance <- R6Class(
  "SessionInstance",
  public = list(

    graphs = NULL,
    processes = NULL,
    data = NULL,
    jobs =NULL,
    job_executor = NULL,

    #' @description Create a new session
    #' @param configuration Session configuration
    #' 
    initialize = function(configuration = NULL) {

      self$graphs = list()
      self$processes = list()
      self$data = list()
      self$jobs = list()

      if (is.null(configuration) || class(configuration) != "ServerConfig") {
        configuration = SessionConfig()
      }

      private$config = configuration
      self$initEndpoints()
    },

    #' @description Get endpoints
    #'
    #' @return endpoints
    getEndpoints = function() {
      return(private$endpoints)
    },

    #' @description Get configuration
    #' @return configuration
    getConfig = function() {
      return(private$config)
    },

    #' @description Get token
    #' @return token
    getToken = function() {
      return(private$token)
    },

    #' @description Assign this token to the session
    #' @param new New token
    setToken = function(new) {
      private$token = NULL
      private$token = new
    },


    #' @description Start the session
    startSession = function(){

      port = private$config$api.port
      host = private$config$host

      private$initRouter()
      self$initDirectory(cleanup = TRUE)
      

      addEndpoint()

      private$router$run(port = port, host = host)
    },

    #' @description Set base url
    #' @param port Which port is currently saved
    #' @param host Which host is currently saved
    setBaseUrl = function(port, host) {
      private$base_url = NULL
      private$base_url = paste("http://",host, ":", port,  sep = "")
    },

    #' @description Get base url
    #' @return base url
    getBaseUrl = function() {
      return(private$base_url)
    },

    #' @description initializes workspace and data paths
  
    initDirectory = function(cleanup = TRUE) {
      
      shared_dir <- Sys.getenv("SHARED_TEMP_DIR", unset = "")
      
      if (shared_dir == "") {
        if (.Platform$OS.type == "unix") {
          base_tmp <- if (Sys.info()[["sysname"]] == "Darwin") "/private/var/tmp" else "/tmp"
          shared_dir <- file.path(base_tmp, "openeocubes_shared_temp")
        } else {
          shared_dir <- file.path(tempdir(), "openeocubes_shared_temp")
        }
        Sys.setenv(SHARED_TEMP_DIR = shared_dir)
      }
      
      # Verzeichnis sicher anlegen
      dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
      
      # >>> Cleanup wirklich ausführen <<<
      if (isTRUE(cleanup) && dir.exists(shared_dir)) {
        # löscht nur Inhalt, nicht den Ordner selbst
        old <- list.files(shared_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
        if (length(old) > 0) unlink(old, recursive = TRUE, force = TRUE)
        message("Shared directory cleaned: ", normalizePath(shared_dir))
      }
      
      message("Using internal shared dir: ", normalizePath(shared_dir))
    },
    
    
    
    

    #' @description build a df to add the endpoints later on
    #'
    initEndpoints = function() {
      private$endpoints = tibble(path=character(0), method = character(0))
    },

    #' @description Create an endpoint
    #'
    #' @param path path for the endpoint
    #' @param method type of request
    #' @param handler function to be executed
    #' @param serializer plumber serializer to be used
    #' @param filter deactivate filter for several endpoints
    #'
    #' @return created Endpoint
    #'
    createEndpoint = function(path, method, handler = NULL, filter = FALSE, serializer = serializer_unboxed_json()) {

      private$endpoints = private$endpoints %>% add_row(path=path,method=method)
      replPath = path %>% gsub(pattern="\\{",replacement="<") %>% gsub(pattern="\\}",replacement=">")

      if (filter == TRUE) {
        private$router$handle(path = replPath, method = method, handler = handler, serializer = serializer)
      }
      else {
        private$router$handle(path = replPath,preempt = "authorization", method = method, handler = handler, serializer = serializer)
      }

      private$router$handle(path = replPath, methods = "OPTIONS", handler = .cors_option)
    },

    #' @description Function to assign data of collection to the data path
    #'
    #' @param col Collection of class 'Collection'
    #'
    assignData = function(col) {

      if (col$id %in% names(Session$data)) {
        stop("This collection id is already assigned")
      }

      if(! is.Collection(col)) {
        stop("Delivered data is not a collection")
      }

      newCol = list(col)
      names(newCol) = col$id
      self$data = append(self$data, newCol)

    },

    #' @description Function to assign the process to the Session
    #'
    #' @param pro Process of class 'Process'
    #'
    assignProcess = function(pro) {

      if (pro$id %in% names(Session$processes)) {
        stop("This process is already assigned")
      }

      if(! is.Process(pro)) {
        stop("Delivered process is not a process")
      }

      newPro = list(pro)
      names(newPro) = pro$id
      self$processes = append(self$processes, newPro)

    },

    #' @description Function to assign a job to the Session
    #'
    #' @param job Job of class 'Job'
    #'
    assignJob = function(job) {

      if (job$id %in% names(Session$jobs)) {
        stop("This job is already assigned")
      }

      if(! is.Job(job)) {
        stop("Delivered job is not a job")
      }

      newJob = list(job)
      names(newJob) = job$id
      self$jobs = append(self$jobs, newJob)
    },
    
    #' @description Enqueue a job for asynchronous execution
    enqueueJob = function(job) {
      shared_dir <- Sys.getenv("SHARED_TEMP_DIR", unset = "")
      callr::r_bg(
      func = function(job_obj, config, shared_dir) {
        Sys.setenv(SHARED_TEMP_DIR = shared_dir)
        library(openeocubes)
        gdalcubes::gdalcubes_options(parallel = 8)
        gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE", "TRUE")
        gdalcubes::gdalcubes_set_gdal_config("GDAL_CACHEMAX", "20%")
        gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE_SIZE", "5000000")
        gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MULTIPLEX", "YES")
        gdalcubes::gdalcubes_set_gdal_config("GDAL_INGESTED_BYTES_AT_OPEN", "32000")
        gdalcubes::gdalcubes_set_gdal_config("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
        gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_VERSION", "2")
        gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES", "YES")

        createSessionInstance(config)
        Session$initDirectory(cleanup = TRUE)
        Session$assignJob(job_obj)
        Session$runJob(job_obj)
      },
        args = list(job, Session$getConfig(), shared_dir),
        stdout = "",
        stderr = ""
      )
    },
    
    

    #' @description Execute the job (asynchronous path)
    #'
    #' @param job Job to be executed
    #'
    runJob = function(job) {
      tryCatch({
        # Job-spezifisches Ausgabe-Verzeichnis unterhalb des workspace
        dir <- file.path(Session$getConfig()$workspace.path, job$output.folder)
        if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
        
        job$status <- "running"
        writeJobInfo(job)
        message("run")
        job <- job$run()
        format <- job$output
        
        out_files <- character(0)
        
        if (class(format) == "list") {
          message("Here in the session list")
          if (format$title == "Network Common Data Form") {
            out_files <- gdalcubes::write_ncdf(
              job$results,
              file.path(dir, basename(tempfile(fileext = ".nc")))
            )
          } else if (format$title == "GeoTiff") {
            message("Geotiff run ")
            out_files <- gdalcubes::write_tif(job$results, dir = dir)
          } else {
            throwError("FormatUnsupported")
          }
        } else {
          if (format == "NetCDF") {
            out_files <- gdalcubes::write_ncdf(
              job$results,
              file.path(dir, basename(tempfile(fileext = ".nc")))
            )
          } else if (format == "GTiff") {
            message("GeoTiff_output erkannt in der session ")
            out_files <- gdalcubes::write_tif(job$results, dir = dir)
          } else {
            throwError("FormatUnsupported")
          }
        }
        message("finished eingeleitet")
       
        shared_dir <- Sys.getenv("SHARED_TEMP_DIR", unset = NA)
        
        if (!is.na(shared_dir) && dir.exists(shared_dir)) {
          existing <- out_files[file.exists(out_files)]
          
          if (length(existing) > 0) {
            target <- file.path(shared_dir, basename(existing))
            file.copy(existing, target, overwrite = TRUE)
            message("Copied job results to shared download dir: ", shared_dir)
          } else {
            message("runJob: no existing output files to copy.")
          }
        } else {
          message("runJob: SHARED_TEMP_DIR not set or missing - skipping copy to /download")
        }
        
        job$status <- "finished"
        writeJobInfo(job)
        
      }, error = function(e) {
        job$status <- "error"
        writeJobInfo(job)
        throwError("Internal", message = e$message)
      })
    }
    
  ),
  private = list(
    endpoints = NULL,
    router = NULL,
    config = NULL,
    token = NULL,
    base_url = NULL,
    
    initRouter = function() {
      private$router = Router$new()
      private$router$registerHook("postroute", .cors_filter)
      private$router$filter("authorization", .authorized, serializer = serializer_unboxed_json())
      
  
      private$router$handle(
        path   = "/<path:.*>",
        method = "GET",
        handler = function(req, res, path) {
          res$status <- 404
          res$setHeader("Content-Type", "application/json; charset=utf-8")
          list(
            code    = "NotFound",
            message = paste0("The requested resource '", req$PATH_INFO, "' does not exist.")
          )
        },
        serializer = serializer_unboxed_json()
      )
    }
  )
  
  
)


#' Creates a new instance from the class 'SessionInstance' and assigns the name 'Session'
#' @param configuration Edited configuration for the session
#'
#' @export
createSessionInstance = function(configuration = NULL) {
  assign("Session", SessionInstance$new(configuration),envir=.GlobalEnv)
  invisible(Session)
}


