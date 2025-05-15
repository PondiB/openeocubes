#' @include Process-class.R
#' @include processes.R
#' @import gdalcubes
#' @import jsonlite
NULL

#' Common returns for math processes
number_or_null_returns <- list(
  description = "The computed result.",
  schema = list(type = c("number", "null"))
)

#' min
min <- do.call(Process$new, c(
  load_process_metadata("min"),
  list(
    operation = function(data, job) {
      if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
        return("min")
      } else {
        return(min(data))
      }
    }
  )
))

#' max
max <- do.call(Process$new, c(
  load_process_metadata("max"),
  list(
    operation = function(data, job) {
      if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
        return("max")
      } else {
        return(max(data))
      }
    }
  )
))

#' mean
mean <- do.call(Process$new, c(
  load_process_metadata("mean"),
  list(
    operation = function(data, job) {
      if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
        return("mean")
      } else {
        return(mean(data))
      }
    }
  )
))

#' median
median <- do.call(Process$new, c(
  load_process_metadata("median"),
  list(
    operation = function(data, job) {
      if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
        return("median")
      } else {
        return(median(data))
      }
    }
  )
))

#' add
add <- do.call(Process$new, c(
  load_process_metadata("add"),
  list(
    operation = function(x, y, job) {
      classes <- c("number", "null")
      if (class(x) %in% names(classes) &&
        class(y) %in% names(classes)) {
        return(x + y)
      } else {
        return(sprintf("(%s+%s)", x, y))
      }
    }
  )
))

#' subtract
subtract <- do.call(Process$new, c(
  load_process_metadata("subtract"),
  list(
    operation = function(x, y, job) {
      classes <- c("number", "null")
      if (class(x) %in% names(classes) &&
        class(y) %in% names(classes)) {
        return(x - y)
      } else {
        return(sprintf("(%s-%s)", x, y))
      }
    }
  )
))

#' multiply
multiply <- do.call(Process$new, c(
  load_process_metadata("multiply"),
  list(
    operation = function(x, y, job) {
      classes <- c("number", "null")
      if (class(x) %in% names(classes) &&
        class(y) %in% names(classes)) {
        return(x * y)
      } else {
        return(sprintf("(%s*%s)", x, y))
      }
    }
  )
))

#' divide
divide <- do.call(Process$new, c(
  load_process_metadata("divide"),
  list(
    operation = function(x, y, job) {
      classes <- c("number", "null")
      if (class(x) %in% names(classes) &&
        class(y) %in% names(classes)) {
        return(x / y)
      } else {
        return(sprintf("(%s/%s)", x, y))
      }
    }
  )
))
