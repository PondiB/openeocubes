test_that("ExecutableProcess initialization requires process parameter", {
    expect_error(
        ExecutableProcess$new(),
        "No process provided"
    )
})

test_that("ExecutableProcess initialization with process works", {
    # Create a process first
    param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
    process <- Process$new(
        id = "test_process",
        summary = "Test",
        description = "Test",
        parameters = list(param),
        operation = function(test_param, job) {
            return("result")
        }
    )

    executable <- ExecutableProcess$new(process = process)

    expect_equal(executable$id, "test_process")
    expect_equal(executable$summary, "Test")
    expect_null(executable$job)
})

test_that("ExecutableProcess inherits from Process", {
    param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
    process <- Process$new(
        id = "test_process",
        parameters = list(param),
        operation = function(test_param, job) {
            return("result")
        }
    )

    executable <- ExecutableProcess$new(process = process)

    expect_true("Process" %in% class(executable))
    expect_true("ExecutableProcess" %in% class(executable))
})

test_that("ExecutableProcess execute calls operation with parameters", {
    param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
    param$value <- "test_value"

    process <- Process$new(
        id = "test_process",
        parameters = list(param),
        operation = function(test_param, job) {
            return(paste("Result:", test_param))
        }
    )

    executable <- ExecutableProcess$new(process = process)
    result <- executable$execute()

    expect_equal(result, "Result: test_value")
})

test_that("ExecutableProcess execute handles nested ExecutableProcess", {
    # Create nested processes
    inner_param <- Parameter$new(name = "inner_param", description = "Inner", schema = list(type = "string"))
    inner_param$value <- "inner_value"

    inner_process <- Process$new(
        id = "inner_process",
        parameters = list(inner_param),
        operation = function(inner_param, job) {
            return("inner_result")
        }
    )

    inner_executable <- ExecutableProcess$new(process = inner_process)

    outer_param <- Parameter$new(name = "nested", description = "Nested", schema = list(type = "object"))
    outer_param$value <- inner_executable

    outer_process <- Process$new(
        id = "outer_process",
        parameters = list(outer_param),
        operation = function(nested, job) {
            return(paste("Outer:", nested))
        }
    )

    outer_executable <- ExecutableProcess$new(process = outer_process)
    result <- outer_executable$execute()

    expect_equal(result, "Outer: inner_result")
})

test_that("is.ExecutableProcess correctly identifies ExecutableProcess objects", {
    param <- Parameter$new(name = "test_param", description = "Test", schema = list(type = "string"))
    process <- Process$new(
        id = "test_process",
        parameters = list(param),
        operation = function(test_param, job) {
            return("result")
        }
    )

    executable <- ExecutableProcess$new(process = process)

    expect_true(is.ExecutableProcess(executable))
    expect_false(is.ExecutableProcess(process))
    expect_false(is.ExecutableProcess("not an executable process"))
    expect_false(is.ExecutableProcess(NULL))
})
