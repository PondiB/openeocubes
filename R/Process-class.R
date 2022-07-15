#' Process
#'
#' @field id Id or name of the process
#' @field summary Shortly what the process does
#' @field description Extended description of the process
#' @field categories To which categories the process belongs
#' @field parameters Used parameters in the process
#' @field returns The output of the process
#' @field operation Function that executes the process
#'
#' @include Parameter-class.R
#' @include processes.R
#' @importFrom R6 R6Class
#'
#' @export
Process <- R6Class(
  "Process",
  public = list(
    id = NULL,
    summary = NULL,
    description = NULL,
    categories = NULL,
    parameters = NULL,
    returns = NULL,
    operation = NULL,


    #' @description  Initialize process
    #'
    #' @param id Id or name of the process
    #' @param summary Shortly what the process does
    #' @param description Extended description of the process
    #' @param categories To which categories the process belongs
    #' @param parameters Used parameters in the process
    #' @param returns The output of the process
    #' @param operation Function that executes the process
    #'
    initialize = function(id = NA,
                          summary = NA,
                          description = NA,
                          categories = NA,
                          parameters = NA,
                          returns = NA,
                          operation = NA) {

      self$id = id
      self$summary = summary
      self$description = description
      self$categories = categories
      self$parameters = parameters
      self$returns = returns
      self$operation = operation
    },

    #' @description List information about the process
    #'
    processInfo = function() {
      parameterList = list()
      for (par in self$parameters) {
        parameterList = append(parameterList, list( par$parameterInfo()))
      }
      info = list(
        id = self$id,
        summary = self$summary,
        description = self$description,
        categories = self$categories,
        parameters = parameterList,
        returns = self$returns
      )
      return(info)
    },

    #' @description Set parameter for the executable process graph
    #'
    #' @param name Name of parameter
    #' @param value Value to be set
    #'
    setParameter = function(name, value) {

      parameter_names = lapply(self$parameters, function(x) {
        return(x$name)
      })

      index = match(name, parameter_names)
      if (is.na(index)) {
        stop(paste("Unable to find: '",name,"' in process '",self$id,"'",sep=""))
      }

      self$parameters[[index]]$value = value

      invisible(self)
    }
  )
)

#' Check if given process is a process
#' @export
is.Process = function(obj) {
  return("Process" %in% class(obj))
}
