#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite validate
#' @docType data
#' @usage data(errors)
NULL
#' Fehler-Helfer für openEO Backend
#' Nutzt KEIN 'errors'-Dataset mehr.

#' Wirft einen openEO-Fehler als Condition
throwError <- function(code = "Internal", message = NULL, status = NULL) {
  # HTTP-Status aus dem Code ableiten, falls nicht explizit angegeben
  if (is.null(status)) {
    status <- switch(
      code,
      "AuthenticationRequired" = 401L,
      "CredentialsInvalid"     = 401L,
      "TokenInvalid"           = 403L,
      "CollectionNotFound"     = 404L,
      "JobNotFound"            = 404L,
      "JobNotFinished"         = 400L,
      "JobFailed"              = 500L,
      "FormatUnsupported"      = 400L,
      "Internal"               = 500L,
      500L
    )
  }
  
  # message robust in String umwandeln
  if (inherits(message, "condition")) {
    msg_text <- conditionMessage(message)
  } else if (is.null(message)) {
    msg_text <- code
  } else {
    msg_text <- as.character(message)
  }
  
  cond <- structure(
    list(
      code    = code,
      message = msg_text,
      status  = status
    ),
    class = c("OpenEOError", "error", "condition")
  )
  stop(cond)
}

# Find the plumber response of the handler that is currently running.
# The call stack is searched rather than the lexical scope: as a tryCatch()
# handler, handleError() is called from base R frames, and a lexical lookup of
# `res` reaches the search path and finds terra::res().
.currentPlumberResponse <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    env <- sys.frame(i)
    if (exists("res", envir = env, inherits = FALSE)) {
      candidate <- get("res", envir = env, inherits = FALSE)
      if (inherits(candidate, "PlumberResponse")) {
        return(candidate)
      }
    }
  }
  NULL
}

#' Generischer Error-Handler für tryCatch(error = handleError)
handleError <- function(e) {
  # Logging
  msg <- conditionMessage(e)
  message("ERROR in API: ", msg)
  
  # openEO-Fehlercode + HTTP-Status extrahieren
  if (inherits(e, "OpenEOError")) {
    code   <- e$code
    status <- e$status
    text   <- e$message
  } else {
    code   <- "Internal"
    status <- 500L
    text   <- msg
  }
  
  # HTTP-Status auf der plumber-Response des aufrufenden Handlers setzen.
  # Content-Type setzt der Serializer; ein zweiter setHeader() würde den
  # Header duplizieren.
  res <- .currentPlumberResponse()
  if (!is.null(res)) {
    res$status <- status
  }
  
  # Genau das, worauf dein Test prüft:
  list(
    code    = code,
    message = text
  )
}

