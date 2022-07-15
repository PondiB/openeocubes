#' @include Process-class.R
#' @import gdalcubes
NULL

#' min
min = Process$new(
  id = "min",
  description = "Computes the smallest value of an array of numbers, which is is equal to the last element of a sorted (i.e., ordered) version the array. ",
  categories = as.array("math", "reducer"),
  summary = "Minimum value",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A array of numbers",
      schema = list(
        type = "array")
  )),
  returns = list(
    description = "The minimum value or min as string for further processes",
    schema = list(type = c("number", "string"))),
  operation = function(data, job) {

    if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
      return("min")
    }
    else {
      return(min(data))
    }
  }
)

#' max
max = Process$new(
  id = "max",
  description = "Computes the largest value of an array of numbers, which is is equal to the last element of a sorted (i.e., ordered) version the array. ",
  categories = as.array("math", "reducer"),
  summary = "maximum value",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A array of numbers",
      schema = list(
        type = "array")
  )),
  returns = list(
    description = "The maximum value or max as string for further processes",
    schema = list(type = c("number", "string"))),
  operation = function(data, job) {

    if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
      return("max")
    }
    else {
      return(max(data))
    }
  }
)

#' mean
mean = Process$new(
  id = "mean",
  description = "The arithmetic mean of an array of numbers is the quantity commonly called the average. It is defined as the sum of all elements divided by the number of elements. ",
  categories = as.array("math", "reducer"),
  summary = "Arithmetic mean (average)",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A array of numbers or mean as string for further processes",
      schema = list(
        type = "array")
  )),
  returns = list(
    description = "The computed arithmetic mean.",
    schema = list(type = c("number", "string"))),
  operation = function(data, job) {

    if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
      return("mean")
    }
    else {
      return(mean(data))
    }
  }
)

#' median
median = Process$new(
  id = "median",
  description = "The statistical median of an array of numbers is the value separating the higher half from the lower half of the data.",
  categories = as.array("math", "reducer"),
  summary = "Statistical median",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A array of numbers",
      schema = list(
        type = "array")
  )),
  returns = list(
    description = "The computed statistical median or median as string for further processes.",
    schema = list(type = c("number", "string"))),
  operation = function(data, job) {

    if ("cube" %in% class(data) || "cube" %in% class(data$data)) {
      return("median")
    }
    else {
      return(median(data))
    }
  }
)

#' add
add = Process$new(
  id = "add",
  description = "Sums up the two numbers x and y (x + y) and returns the computed sum.",
  categories = as.array("math"),
  summary = "Addition of two numbers",
  parameters = list(
    Parameter$new(
      name = "x",
      description = "The first summand",
      schema = list(
        type = c("number", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "The second summand",
      schema = list(
        type = c("number", "null"))
    )
  ),
  returns = list(
    description = "The computed sum of the two numbers.",
    schema = list(type = c("number", "null"))),
  operation = function(x, y, job) {

    classes = c("number", "null")
    if(class(x) %in% names(classes) && class(y) %in% names(classes)) {
      return(x + y)
    }
    else {
      return(sprintf("(%s+%s)", x, y))
    }
  }
)

#' subtract
subtract = Process$new(
  id = "subtract",
  description = "Subtracts argument y from the argument x (x - y) and returns the computed result.",
  categories = as.array("math"),
  summary = "Subtraction of two numbers",
  parameters = list(
    Parameter$new(
      name = "x",
      description = "The minuend",
      schema = list(
        type = c("number", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "The subtrahend",
      schema = list(
        type = c("number", "null"))
    )
  ),
  returns = list(
    description = "The computed result.",
    schema = list(type = c("number", "null"))),
  operation = function(x, y, job) {

    classes = c("number", "null")
    if(class(x) %in% names(classes) && class(y) %in% names(classes)) {
      return(x - y)
    }
    else {
      return(sprintf("(%s-%s)", x, y))
    }
  }
)

#' multiply
multiply = Process$new(
  id = "multiply",
  description = "Multiplies the two numbers x and y (x * y) and returns the computed product.",
  categories = as.array("math"),
  summary = "Multiplication of two numbers",
  parameters = list(
    Parameter$new(
      name = "x",
      description = "The multiplier",
      schema = list(
        type = c("number", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "The multiplicant",
      schema = list(
        type = c("number", "null"))
    )
  ),
  returns = list(
    description = "The computed product of the two numbers.",
    schema = list(type = c("number", "null"))),
  operation = function(x, y, job) {
    classes = c("number", "null")
    if(class(x) %in% names(classes) && class(y) %in% names(classes)) {
      return(x * y)
    }
    else {
      return(sprintf("(%s*%s)", x, y))
    }
  }
)

#' divide
divide = Process$new(
  id = "divide",
  description = "Divides argument x by the argument y (x / y) and returns the computed result.",
  categories = as.array("math"),
  summary = "Division of two numbers",
  parameters = list(
    Parameter$new(
      name = "x",
      description = "The dividend",
      schema = list(
        type = c("number", "null"))
    ),
    Parameter$new(
      name = "y",
      description = "The divisor",
      schema = list(
        type = c("number", "null"))
    )
  ),
  returns = list(
    description = "The computed result.",
    schema = list(type = c("number", "null"))),
  operation = function(x, y, job) {

    classes = c("number", "null")
    if(class(x) %in% names(classes) && class(y) %in% names(classes)) {
      return(x / y)
    }
    else {
      return(sprintf("(%s/%s)", x, y))
    }
  }
)
