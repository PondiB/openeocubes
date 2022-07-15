#' Parameter
#'
#' @field name Name of the parameter
#' @field description Short description of the parameter
#' @field schema Type and subtype of the parameter
#' @field optional Is this parameter required for the process
#' @field value Value of the parameter
#' @field parameters Included parameters
#'
#' @include Process-class.R
#' @importFrom R6 R6Class
#'
#' @export
Parameter <- R6Class(
  "Parameter",
  public = list(
    name = NA,
    description = NA,
    schema = NA,
    optional = NA,
    value = NULL,
    parameters = NULL,

    #' @description
    #'
    #' @param name Name of the parameter
    #' @param description Short description of the parameter
    #' @param optional Is this parameter required for the process
    #' @param schema Type and subtype of the parameter
    #'
    initialize = function(name = NA,
                          description = NA,
                          schema = NA,
                          optional = FALSE) {

      self$name = name
      self$description = description
      self$schema = schema
      self$optional = optional
   },

    #' @description Get the information of the parameter
    #'
    #' @return list of information
    #'
    parameterInfo = function() {
      info = list()

      if ("parameters" %in% names(self$schema)) {
        schema = list()
        schema$type = self$schema$type
        schema$subtype = self$schema$subtype

        schema$parameters = lapply(self$schema$parameters, function(x) {
          return(x$parameterInfo())
        })
      }
      else {
        schema = self$schema
      }

      info = appendInfo(
        name = self$name,
        description = self$description,
        schema = schema,
        optional = self$optional
      )
      return(info)
    }
  )
)

#' appendInfo
#'
#' @description Create a list with appended parameter
#'
#' @param name name of parameter
#' @param description description of parameter
#' @param type type of parameter
#' @param subtype subtype of parameter
#'
#' @return list with appended parameter
#'
appendInfo = function(name, description, schema, optional = NA) {

  info = list()
  info = append(info, list(name = name))
  info = append(info, list(description = description))
  info = append(info, list(schema = schema))

  if (! is.na(optional)) {
    info = append(info, list(optional = optional))
  }
  return(info)
}
