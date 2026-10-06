Router = R6Class(
  "Router",
  inherit = plumber,
  
  public = list(
    initialize = function(filters = plumber:::defaultPlumberFilters, envir) {
      if (missing(envir)) {
        private$envir <- new.env(parent = .GlobalEnv)
      } else {
        private$envir <- envir
      }

      # plumber's own initialize() sets this; without it, routes registered
      # without a serializer (OPTIONS, not-found handler) fail with
      # "Serializers must be closures".
      self$setSerializer(serializer_unboxed_json())
      
      private$errorHandler <- function(req, res, err) {
        handleError(err)  
      }
      
      private$notFoundHandler <- function(req, res) {
        e <- structure(
          list(
            code    = "NotFound",
            message = paste0("Endpoint '", req$PATH_INFO, "' not found"),
            status  = 404L
          ),
          class = c("OpenEOError", "error", "condition")
        )
        handleError(e) 
      }
    }
  )
)
