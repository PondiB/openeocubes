#' Executable process
#'
#' @include Process-class.R
#' @importFrom rlang invoke
#' @field job Attached job to the process
#'
#' @export
ExecutableProcess <- R6Class(
  "ExecutableProcess",
  inherit = Process,
  public = list(
    job = NULL,

    #' @description Initialize executable process
    #'
    #' @param id Id or name of the proces
    #' @param description Shortly what the process does
    #' @param parameters Used parameters in the process
    #' @param operation Function that executes the process
    #' @param process Processes which will be executed
    #'
    initialize = function(id = NA,
                        description = NA,
                        parameters = NA,
                        operation = NA,
                        process= NULL) {

          if (! is.null(process)) {

            variables = names(process)
            for (key in variables) {
              value = process[[key]]
              if (class(value) == "function" || class(value) == "environment") {
                next()
              }
              else {
                self[[key]] = value
              }
            }
            self$operation = process$operation
          }
          else {
            stop("No process provided")
          }
    },

    #' @description Run the operation including a generated list of parameters
    #'
    execute = function() {

        parameterList = list()
        for (key in 1:length(self$parameters)) {
          name = self$parameters[[key]]$name
          value = self$parameters[[key]]$value

          if (is.ExecutableProcess(value)) {
            parameterList[[name]] = value$execute()
          }
          else if (class(value) == "list" && "from_parameter" %in% names(value)) {


            for (i in 1:99) {
              parent = parent.frame(i)

              if (parent$name == "reducer") {
                break
              }
            }
            par = parent$parameterList
            parameterList[[name]] = par
          }
          else {
            parameterList[[name]] = value
          }

        }
        parameterList$job = self$job
        result = invoke(self$operation, parameterList)

        return(result)
    }
  )
)

#' Check if given process is a process
#' @param obj Process to be checked
is.ExecutableProcess = function(obj) {
  return(all(c("ExecutableProcess", "Process") %in% class(obj)) )
}
