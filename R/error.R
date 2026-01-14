#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite validate
#' @docType data
#' @usage data(errors)
NULL


throwError <- function(code = "Internal", message = NULL, status = NULL) {
  if (is.null(status)) {
    status <- switch(
      code,
      "AuthenticationRequired" = 401L,
      "CredentialsInvalid" = 401L,
      "JobNotFound" = 404L,
      "JobNotFinished" = 400L,
      "JobFailed" = 500L,
      "FormatUnsupported" = 400L,
      "Internal" = 500L,
      500L
    )
  }
  
  if (inherits(message, "condition")) {
    msg_text <- conditionMessage(message)
  } else if (is.null(message)) {
    msg_text <- code
  } else {
    msg_text <- as.character(message)
  }
  
  cond <- structure(
    list(
      code = code,
      message = msg_text,
      status = status
    ),
    class = c("OpenEOError", "error", "condition")
  )
  stop(cond)
}


handleError <- function(e) {
  msg <- conditionMessage(e)
  message("ERROR in API: ", msg)
  
  if (inherits(e, "OpenEOError")) {
    code <- e$code
    status <- e$status
    text <- e$message
  } else {
    code <- "Internal"
    status <- 500L
    text <- msg
  }
  
  pf <- parent.frame()
  res <- tryCatch(get("res", envir = pf), error = function(...) NULL)
  if (!is.null(res)) {
    res$status <- status
    res$setHeader("Content-Type", "application/json; charset=utf-8")
  }
    list(
    code = code,
    message = text
  )
}


