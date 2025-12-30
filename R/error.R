#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite validate
#' @importFrom tibble tibble
#' @import dplyr

# Internal errors dataset - OpenEO error definitions
# This dataset contains all error codes, messages, and HTTP status codes used by the API
.errors_dataset <- tibble(
  name = c(
    "ProcessGraphMissing",
    "Internal",
    "JobNotFound",
    "JobNotStarted",
    "JobNotFinished",
    "CredentialsInvalid",
    "AuthenticationRequired",
    "FormatUnsupported"
  ),
  code = c(
    "ProcessGraphMissing",
    "Internal",
    "JobNotFound",
    "JobNotStarted",
    "JobNotFinished",
    "CredentialsInvalid",
    "AuthenticationRequired",
    "FormatUnsupported"
  ),
  description = c(
    "The process graph is missing or malformed",
    "An internal server error occurred",
    "The requested job was not found",
    "The job has not been started yet",
    "The job has not finished yet",
    "The provided credentials are invalid",
    "Authentication is required for this endpoint",
    "The requested format is not supported"
  ),
  msg = c(
    "The process graph is missing or malformed in the request",
    "An internal server error occurred: {message}",
    "The requested job with ID '{job_id}' was not found",
    "The job has not been started yet",
    "The job has not finished yet",
    "The provided credentials are invalid",
    "Authentication is required for this endpoint",
    "The requested format '{format}' is not supported"
  ),
  status = c(400L, 500L, 404L, 400L, 400L, 401L, 401L, 400L),
  parameter = list(NULL, "message", "job_id", NULL, NULL, NULL, NULL, "format")
)

#' Handle errors from process execution
#'
#' @param e Error object containing the error message
#' @return List with error code, message, and links
handleError = function(e) {
  i = 1
  env = parent.frame(n = i)
  while (!"res" %in% names(env)) {
    i = i + 1
    env = parent.frame(n = i)
  }

  if (validate(e$message) == TRUE) {
    error_obj = fromJSON(e$message)
    env$res$status = error_obj$status

    # id and links are spared for now
    return(list(
      code = error_obj$code,
      message = error_obj$msg,
      links = list()
    ))
  } else {
    env$res$status = 500
    return(list(message = e$message))
  }
}

#' Throw an OpenEO-compliant error
#'
#' @param id Error identifier (e.g., "JobNotFound", "Internal")
#' @param ... Named variables to substitute in the error message template
#' @return Stops execution with a JSON-formatted error message
#'
#' @details
#' This function looks up the error definition by ID and formats it according to
#' OpenEO error specifications. Variable substitution is performed on the message
#' template using the provided named arguments.
#'
#' Error structure:
#' - code: OpenEO-specific error code
#' - msg: Human-readable error message (with variable substitution)
#' - status: HTTP status code to be returned
throwError = function(id, ...) {
  # Get error definition from internal dataset
  result = .errors_dataset %>% filter(name == id)

  if (nrow(result) == 1) {
    result = as.list(result)
    result$description = NULL
    result$name = NULL
    result$parameter = unlist(result$parameter)
    
    # Resolve variable substitutions
    variables = list(...)

    if (length(result$parameter) > 0 && !is.null(result$parameter)) {
      # Replace each variable in the message template
      for (index in seq_along(result$parameter)) {
        variable_name = result$parameter[[index]]

        if (!variable_name %in% names(variables)) {
          # Replace with placeholder if variable is missing
          value = "<missing variable>"
        } else {
          value = variables[[variable_name]]
        }
        result$msg = gsub(
          x = result$msg,
          pattern = paste0("\\{", variable_name, "\\}"),
          replacement = paste0(value)
        )
      }
    }

    result$parameter = NULL
    stop(toJSON(result, auto_unbox = TRUE))
  } else {
    # Unknown error ID - return generic internal error
    stop(toJSON(list(code = "Internal", msg = paste("Unknown error ID:", id), status = 500), auto_unbox = TRUE))
  }
}
