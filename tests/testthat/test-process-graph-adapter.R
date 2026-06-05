test_that("adapter wraps bare process graph payload", {
  bare <- list(
    node1 = list(
      process_id = "save_result",
      arguments = list(
        data = 1,
        format = "GTiff",
        options = list()
      ),
      result = TRUE
    )
  )

  adapted <- adaptProcessGraph(bare)
  expect_true("process_graph" %in% names(adapted))
  expect_true("node1" %in% names(adapted$process_graph))
})

test_that("adapter rewrites load_collection cloud_cover properties", {
  graph <- list(
    process_graph = list(
      load1 = list(
        process_id = "load_collection",
        arguments = list(
          id = "sentinel-2-l2a",
          spatial_extent = list(west = 0, south = 0, east = 1, north = 1, crs = 4326),
          temporal_extent = list("2020-01-01", "2020-02-01"),
          properties = list(
            "eo:cloud_cover" = list(
              process_graph = list(
                lte1 = list(
                  process_id = "lte",
                  arguments = list(
                    x = list(from_parameter = "value"),
                    y = 50
                  ),
                  result = TRUE
                )
              )
            )
          )
        )
      )
    )
  )

  adapted <- adaptProcessGraph(graph)
  load_args <- adapted$process_graph$load1$arguments
  expect_equal(load_args$cloud_cover, 50)
  expect_null(load_args$properties)
})

test_that("adapter rewrites reducer process_graph median to reducer string", {
  graph <- list(
    process_graph = list(
      atp1 = list(
        process_id = "aggregate_temporal_period",
        arguments = list(
          data = list(from_node = "x"),
          period = "month",
          reducer = list(
            process_graph = list(
              median1 = list(
                process_id = "median",
                arguments = list(data = list(from_parameter = "data")),
                result = TRUE
              )
            )
          )
        )
      )
    )
  )

  adapted <- adaptProcessGraph(graph)
  expect_equal(adapted$process_graph$atp1$arguments$reducer, "median")
})

test_that("adapter rewrites full_pg mask pattern to mask_scl", {
  graph <- list(
    process_graph = list(
      loadcollection1 = list(
        process_id = "load_collection",
        arguments = list(
          id = "sentinel-2-l2a",
          spatial_extent = list(west = 0, south = 0, east = 1, north = 1, crs = 4326),
          temporal_extent = list("2020-01-01", "2020-02-01"),
          bands = list("red", "nir", "scl")
        )
      ),
      reducedimension1 = list(
        process_id = "reduce_dimension",
        arguments = list(
          data = list(from_node = "loadcollection1"),
          dimension = "bands",
          reducer = list(
            process_graph = list(
              arrayelement1 = list(
                process_id = "array_element",
                arguments = list(
                  data = list(from_parameter = "data"),
                  index = 12
                )
              ),
              eq1 = list(
                process_id = "eq",
                arguments = list(x = list(from_node = "arrayelement1"), y = 3)
              ),
              eq2 = list(
                process_id = "eq",
                arguments = list(x = list(from_node = "arrayelement1"), y = 8)
              ),
              eq3 = list(
                process_id = "eq",
                arguments = list(x = list(from_node = "arrayelement1"), y = 9)
              ),
              or1 = list(
                process_id = "or",
                arguments = list(x = list(from_node = "eq1"), y = list(from_node = "eq2"))
              ),
              or2 = list(
                process_id = "or",
                arguments = list(x = list(from_node = "or1"), y = list(from_node = "eq3")),
                result = TRUE
              )
            )
          )
        )
      ),
      filterbands1 = list(
        process_id = "filter_bands",
        arguments = list(
          data = list(from_node = "loadcollection1"),
          bands = list("red", "nir")
        )
      ),
      mask1 = list(
        process_id = "mask",
        arguments = list(
          data = list(from_node = "filterbands1"),
          mask = list(from_node = "reducedimension1")
        )
      )
    )
  )

  adapted <- adaptProcessGraph(graph)
  mask_node <- adapted$process_graph$mask1

  expect_equal(mask_node$process_id, "mask_scl")
  expect_equal(mask_node$arguments$invalid_values, c(3L, 8L, 9L))
  expect_equal(mask_node$arguments$data$from_node, "loadcollection1")
})

test_that("adapter fails fast for unsupported reducer subgraph patterns", {
  graph <- list(
    process_graph = list(
      atp1 = list(
        process_id = "aggregate_temporal_period",
        arguments = list(
          data = list(from_node = "x"),
          period = "month",
          reducer = list(
            process_graph = list(
              mean1 = list(
                process_id = "mean",
                arguments = list(data = list(from_parameter = "data")),
                result = TRUE
              )
            )
          )
        )
      )
    )
  )

  expect_error(
    adaptProcessGraph(graph),
    "unsupported_subgraph_pattern"
  )
})
