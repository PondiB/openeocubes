#' Normalize process graph envelope and rewrite known patterns.
NULL

.adapterError <- function(node_id, detail) {
  stop(sprintf("unsupported_subgraph_pattern (%s): %s", node_id, detail))
}

.resultNodeId <- function(process_graph) {
  result_nodes <- names(process_graph)[vapply(
    process_graph,
    function(node) is.list(node) && isTRUE(node$result),
    logical(1)
  )]
  if (length(result_nodes) > 0) {
    return(result_nodes[[1]])
  }
  names(process_graph)[[length(process_graph)]]
}

.asProcessGraphEnvelope <- function(process) {
  if (is.null(process) || !is.list(process)) {
    stop("Invalid process graph payload.")
  }
  if ("process_graph" %in% names(process)) {
    if (!is.list(process$process_graph)) {
      stop("Invalid process graph payload.")
    }
    return(process)
  }
  list(process_graph = process)
}

.rewriteCloudCoverProperty <- function(node_id, node) {
  args <- node$arguments
  properties <- args$properties
  if (is.null(properties) || is.null(properties[["eo:cloud_cover"]])) {
    return(list(node = node, rewritten = FALSE))
  }

  cloud_cover <- properties[["eo:cloud_cover"]]
  if (!is.list(cloud_cover) || is.null(cloud_cover$process_graph)) {
    .adapterError(node_id, "eo:cloud_cover must be a process_graph.")
  }
  subgraph <- cloud_cover$process_graph
  root_id <- .resultNodeId(subgraph)
  root <- subgraph[[root_id]]
  if (is.null(root) || root$process_id != "lte") {
    .adapterError(node_id, "eo:cloud_cover requires lte(value, threshold).")
  }
  x <- root$arguments$x
  y <- root$arguments$y
  if (!(is.list(x) && identical(x$from_parameter, "value"))) {
    .adapterError(node_id, "lte x argument must come from_parameter=value.")
  }
  if (!is.numeric(y) || length(y) != 1) {
    .adapterError(node_id, "lte y argument must be a single number.")
  }
  other_props <- setdiff(names(properties), "eo:cloud_cover")
  if (length(other_props) > 0) {
    .adapterError(node_id, paste0("unsupported properties: ", paste(other_props, collapse = ", ")))
  }

  args$cloud_cover <- as.numeric(y)
  args$properties <- NULL
  node$arguments <- args
  list(node = node, rewritten = TRUE)
}

.rewriteMedianReducer <- function(node_id, node) {
  args <- node$arguments
  reducer <- args$reducer
  if (!(is.list(reducer) && "process_graph" %in% names(reducer))) {
    return(list(node = node, rewritten = FALSE))
  }

  subgraph <- reducer$process_graph
  root_id <- .resultNodeId(subgraph)
  root <- subgraph[[root_id]]
  if (is.null(root) || root$process_id != "median") {
    .adapterError(node_id, "only reducer process_graph median(data) is supported.")
  }

  input <- root$arguments$data
  if (!(is.list(input) && identical(input$from_parameter, "data"))) {
    .adapterError(node_id, "median reducer input must come from_parameter=data.")
  }

  args$reducer <- "median"
  node$arguments <- args
  list(node = node, rewritten = TRUE)
}

.parseMaskLogicNode <- function(reducer_graph, node_id, expected_array_node = NULL) {
  node <- reducer_graph[[node_id]]
  if (is.null(node)) {
    .adapterError(node_id, "missing reducer node.")
  }

  pid <- node$process_id
  if (pid == "or") {
    x <- node$arguments$x
    y <- node$arguments$y
    if (!(is.list(x) && !is.null(x$from_node) && is.list(y) && !is.null(y$from_node))) {
      .adapterError(node_id, "or arguments must both come from_node.")
    }
    left <- .parseMaskLogicNode(reducer_graph, x$from_node, expected_array_node)
    right <- .parseMaskLogicNode(reducer_graph, y$from_node, left$array_node)
    values <- sort(unique(c(left$values, right$values)))
    return(list(values = values, array_node = left$array_node))
  }

  if (pid != "eq") {
    .adapterError(node_id, "reducer root must be composed of eq/or nodes.")
  }

  x <- node$arguments$x
  y <- node$arguments$y
  if (!(is.list(x) && !is.null(x$from_node))) {
    .adapterError(node_id, "eq x argument must come from_node.")
  }
  if (!is.numeric(y) || length(y) != 1) {
    .adapterError(node_id, "eq y argument must be a single number.")
  }

  array_node <- x$from_node
  if (!is.null(expected_array_node) && !identical(array_node, expected_array_node)) {
    .adapterError(node_id, "all eq nodes must target the same array_element node.")
  }

  array_element <- reducer_graph[[array_node]]
  if (is.null(array_element) || array_element$process_id != "array_element") {
    .adapterError(node_id, "eq x source must be array_element.")
  }
  data_arg <- array_element$arguments$data
  index_arg <- array_element$arguments$index
  if (!(is.list(data_arg) && identical(data_arg$from_parameter, "data"))) {
    .adapterError(node_id, "array_element data must come from_parameter=data.")
  }
  if (!identical(index_arg, 12) && !identical(as.integer(index_arg), as.integer(12))) {
    .adapterError(node_id, "array_element index must be 12 for SCL masking.")
  }

  list(values = as.integer(y), array_node = array_node)
}

.extractInvalidSclValues <- function(node_id, reducer) {
  if (!(is.list(reducer) && "process_graph" %in% names(reducer))) {
    .adapterError(node_id, "mask reducer must be a process_graph.")
  }
  reducer_graph <- reducer$process_graph
  root_id <- .resultNodeId(reducer_graph)
  parsed <- .parseMaskLogicNode(reducer_graph, root_id)
  values <- sort(unique(as.integer(parsed$values)))
  if (length(values) < 1) {
    .adapterError(node_id, "no invalid SCL values found.")
  }
  values
}

.resolveMaskDataSource <- function(process_graph, data_argument) {
  if (!(is.list(data_argument) && !is.null(data_argument$from_node))) {
    return(data_argument)
  }
  source_id <- data_argument$from_node
  source_node <- process_graph[[source_id]]
  if (is.null(source_node) || source_node$process_id != "filter_bands") {
    return(data_argument)
  }

  upstream <- source_node$arguments$data
  if (is.list(upstream) && !is.null(upstream$from_node)) {
    return(list(from_node = upstream$from_node))
  }
  data_argument
}

.rewriteMaskNodeIfSclPattern <- function(node_id, node, process_graph) {
  args <- node$arguments
  if (is.null(args$mask) || !(is.list(args$mask) && !is.null(args$mask$from_node))) {
    return(list(node = node, rewritten = FALSE))
  }

  mask_source_id <- args$mask$from_node
  mask_source <- process_graph[[mask_source_id]]
  if (is.null(mask_source) || mask_source$process_id != "reduce_dimension") {
    return(list(node = node, rewritten = FALSE))
  }

  reduce_args <- mask_source$arguments
  if (!identical(reduce_args$dimension, "bands")) {
    .adapterError(node_id, "mask reduce_dimension must target dimension=bands.")
  }
  invalid_values <- .extractInvalidSclValues(mask_source_id, reduce_args$reducer)

  rewritten_data <- .resolveMaskDataSource(process_graph, args$data)
  node$process_id <- "mask_scl"
  node$arguments <- list(
    data = rewritten_data,
    invalid_values = invalid_values
  )
  list(node = node, rewritten = TRUE)
}

#' Adapt incoming process graph payload to supported execution subset.
#' @param process process entry from request payload
#' @return adapted process list containing process_graph
adaptProcessGraph <- function(process) {
  wrapped <- .asProcessGraphEnvelope(process)
  process_graph <- wrapped$process_graph
  rewrites <- character(0)

  for (node_id in names(process_graph)) {
    node <- process_graph[[node_id]]
    if (!is.list(node) || is.null(node$process_id)) {
      next
    }

    if (node$process_id == "load_collection") {
      rewrite <- .rewriteCloudCoverProperty(node_id, node)
      node <- rewrite$node
      if (isTRUE(rewrite$rewritten)) {
        rewrites <- c(rewrites, sprintf("%s: properties->cloud_cover", node_id))
      }
    }

    if (node$process_id %in% c("aggregate_temporal_period", "aggregate_spatial")) {
      rewrite <- .rewriteMedianReducer(node_id, node)
      node <- rewrite$node
      if (isTRUE(rewrite$rewritten)) {
        rewrites <- c(rewrites, sprintf("%s: reducer_subgraph->median", node_id))
      }
    }

    if (node$process_id == "mask") {
      rewrite <- .rewriteMaskNodeIfSclPattern(node_id, node, process_graph)
      node <- rewrite$node
      if (isTRUE(rewrite$rewritten)) {
        rewrites <- c(rewrites, sprintf("%s: mask_subgraph->mask_scl", node_id))
      }
    }

    process_graph[[node_id]] <- node
  }

  if (length(rewrites) > 0) {
    message("Process graph adapter rewrites: ", paste(rewrites, collapse = "; "))
  }

  wrapped$process_graph <- process_graph
  wrapped
}
