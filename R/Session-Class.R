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
#' @export
SessionInstance <- R6Class(
  "SessionInstance",
  public = list(

    graphs = NULL,
    processes = NULL,
    data = NULL,
    jobs =NULL,

    #' @description Create a new session
    #' @param configuration Session configuration
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
      self$initDirectory()

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
    #'
    initDirectory = function() {

      if (is.null(private$config$workspace.path)) {
        private$config$workspace.path <- getwd()
      }
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

    #' @description Execute the job
    #'
    #' @param job Job to be executed
    #'
    runJob = function(job) {

     tryCatch({
        dir = paste(Session$getConfig()$workspace.path, job$output.folder, sep = "/")

        job = job$run()
        format = job$output

        if (class(format) == "list") {
          if (format$title == "Network Common Data Form") {
            write_ncdf(job$results, file.path(dir, basename(tempfile(fileext = ".nc"))))
          }
          else if (format$title == "GeoTiff") {
            write_tif(job$results, dir = dir)
          }
          else {
            throwError("FormatUnsupported")
          }
        }
        else {
          if (format == "NetCDF") {
            write_ncdf(job$results, file.path(dir, basename(tempfile(fileext = ".nc"))))
          }
          else if (format == "GTiff") {
            write_tif(job$results, dir = dir)
          }
          else {
            throwError("FormatUnsupported")
          }
        }
      }, error = function(e) {
          throwError("Internal",message=e$message)
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
      private$router$registerHook("postroute",.cors_filter)
      private$router$filter("authorization", .authorized, serializer = serializer_unboxed_json())
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
