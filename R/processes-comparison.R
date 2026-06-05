#' @include Process-class.R
NULL

.openeo_is_nodata <- function(x) {
  is.null(x) || (length(x) == 1L && is.na(x))
}

.openeo_scalar_type <- function(x) {
  if (.openeo_is_nodata(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return("boolean")
  }
  if (is.character(x)) {
    return("string")
  }
  if (is.numeric(x)) {
    return("number")
  }
  NA_character_
}

.openeo_eq <- function(x, y, delta = NULL, case_sensitive = TRUE) {
  if (.openeo_is_nodata(x) || .openeo_is_nodata(y)) {
    return(NULL)
  }

  x_type <- .openeo_scalar_type(x)
  y_type <- .openeo_scalar_type(y)
  if (x_type != y_type) {
    return(FALSE)
  }

  if (x_type == "number") {
    if (is.nan(x) || is.nan(y)) {
      return(FALSE)
    }
    if (!is.null(delta)) {
      return(abs(x - y) <= delta)
    }
    return(x == y)
  }

  if (x_type == "boolean") {
    return(x == y)
  }

  if (x_type == "string") {
    if (isTRUE(case_sensitive)) {
      return(x == y)
    }
    return(tolower(x) == tolower(y))
  }

  FALSE
}

.openeo_lte <- function(x, y) {
  if (.openeo_is_nodata(x) || .openeo_is_nodata(y)) {
    return(NULL)
  }
  if (isTRUE(.openeo_eq(x, y))) {
    return(TRUE)
  }

  x_type <- .openeo_scalar_type(x)
  y_type <- .openeo_scalar_type(y)
  if (x_type != "number" || y_type != "number") {
    return(FALSE)
  }

  if (is.nan(x) || is.nan(y)) {
    return(FALSE)
  }

  x <= y
}

.openeo_or <- function(x, y) {
  if (isTRUE(x) || isTRUE(y)) {
    return(TRUE)
  }
  if (isFALSE(x) && isFALSE(y)) {
    return(FALSE)
  }
  NULL
}

#' eq
eq <- Process$new(
  id = "eq",
  summary = "Equal to comparison",
  description = paste(
    "Compares whether `x` is strictly equal to `y`.",
    "Data types are checked strictly; integer values are equal to floating-point numbers.",
    "If any operand is a no-data value, the result is `null`."
  ),
  categories = as.array(c("texts", "comparison")),
  parameters = list(
    Parameter$new(
      name = "x",
      description = "First operand.",
      schema = list(type = c("number", "boolean", "string", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "Second operand.",
      schema = list(type = c("number", "boolean", "string", "null"))
    ),
    Parameter$new(
      name = "delta",
      description = "Only applicable for comparing two numbers. Checks equality against a delta value.",
      schema = list(type = c("number", "null")),
      optional = TRUE
    ),
    Parameter$new(
      name = "case_sensitive",
      description = "Only applicable for comparing two strings.",
      schema = list(type = "boolean"),
      optional = TRUE
    )
  ),
  returns = list(
    description = "`true` if `x` is equal to `y`, otherwise `false` or `null`.",
    schema = list(type = c("boolean", "null"))
  ),
  operation = function(x, y = NULL, delta = NULL, case_sensitive = TRUE, job) {
    .openeo_eq(x, y, delta = delta, case_sensitive = case_sensitive)
  }
)

#' lte
lte <- Process$new(
  id = "lte",
  summary = "Less than or equal to comparison",
  description = paste(
    "Compares whether `x` is less than or equal to `y`.",
    "If any operand is a no-data value, the result is `null`.",
    "If the operands are not equal and any of them is not a number, the process returns `false`."
  ),
  categories = as.array("comparison"),
  parameters = list(
    Parameter$new(
      name = "x",
      description = "First operand.",
      schema = list(type = c("number", "boolean", "string", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "Second operand.",
      schema = list(type = c("number", "boolean", "string", "null"))
    )
  ),
  returns = list(
    description = "`true` if `x` is less than or equal to `y`, otherwise `false` or `null`.",
    schema = list(type = c("boolean", "null"))
  ),
  operation = function(x, y = NULL, job) {
    .openeo_lte(x, y)
  }
)

#' or
or <- Process$new(
  id = "or",
  summary = "Logical OR",
  description = paste(
    "Checks if at least one of the values is true.",
    "If any argument is a no-data value, the result is `null` whenever the outcome is ambiguous."
  ),
  categories = as.array("logic"),
  parameters = list(
    Parameter$new(
      name = "x",
      description = "A boolean value.",
      schema = list(type = c("boolean", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "A boolean value.",
      schema = list(type = c("boolean", "null"))
    )
  ),
  returns = list(
    description = "Boolean result of the logical OR.",
    schema = list(type = c("boolean", "null"))
  ),
  operation = function(x = NULL, y = NULL, job) {
    .openeo_or(x, y)
  }
)
