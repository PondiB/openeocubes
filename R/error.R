#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite validate
#' @docType data
#' @usage data(errors)
NULL

handleError = function(e) {

  i = 1
  env=parent.frame(n = i)
  while (!"res" %in% names(env)) {
    i = i+1
    env=parent.frame(n = i)
  }

  if (validate(e$message) == TRUE) {
    error_obj = fromJSON(e$message)
    env$res$status = error_obj$status

    # id and links are spared for now
    return(list(
      code = error_obj$code,
      message=error_obj$msg,
      links = list())
    )
  }
  else {
    env$res$status = 500
    return(list(message = e$message))
  }
}


throwError = function(id, ...) {
  # errors exists at runtime. it will be called by data("errors")

  # errors$code - openeo specific error code
  # errors$name - human readable label for the error
  # errors$description - human readable description of the error
  # errors$msg - the predefined error message with variables
  # errors$status - the HTTP status code to be returned by this error
  # errors$parameter - list of variable names to be replaced by information in runtime

  result = errors %>% filter(name == id)

  if (nrow(result) == 1) {
    result = as.list(result)
    result$description = NULL
    result$name = NULL
    result$parameter = unlist(result$parameter)
    # resolve ...
    variables = list(...)

    if (length(result$parameter) > 0) {
      # replace each variable
      for (index in seq_along(result$parameter)) {
        variable_name = result$parameter[[index]]

        if (! variable_name %in% names(variables)) {
          # replace with empty
          value = "<missing variable>"
        } else {
          value = variables[[variable_name]]
        }
        result$msg = gsub(x=result$msg, pattern=paste0("\\{",variable_name,"\\}"),replacement = paste0(value))
      }
    }


    result$parameter = NULL
    stop(toJSON(result,auto_unbox = TRUE))
  } else {
    stop(toJSON(list(code=NULL,msg=NULL,status=500),auto_unbox = TRUE))
  }
}
