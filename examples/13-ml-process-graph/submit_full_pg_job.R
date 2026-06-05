#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(openeo)
  library(jsonlite)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(y)
  }
  x
}

script_file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1]])))
} else {
  getwd()
}

graph_path <- Sys.getenv(
  "OPENEO_GRAPH_PATH",
  unset = file.path(script_dir, "full_pg.json")
)
backend_url <- Sys.getenv("OPENEO_BACKEND_URL", unset = "http://localhost:8000")
username <- Sys.getenv("OPENEO_USER", unset = "user")
password <- Sys.getenv("OPENEO_PASSWORD", unset = "password")
poll_seconds <- as.numeric(Sys.getenv("OPENEO_POLL_SECONDS", unset = "10"))
max_polls <- as.integer(Sys.getenv("OPENEO_MAX_POLLS", unset = "240"))
wait_for_finish <- tolower(Sys.getenv("OPENEO_WAIT_FOR_FINISH", unset = "true")) %in% c("1", "true", "yes")
download_dir <- Sys.getenv(
  "OPENEO_DOWNLOAD_DIR",
  unset = file.path(script_dir, "results")
)

cat("Backend URL:", backend_url, "\n")
cat("Graph path :", graph_path, "\n")

if (!file.exists(graph_path)) {
  stop("Process graph file not found: ", graph_path)
}

process_graph <- jsonlite::fromJSON(graph_path, simplifyDataFrame = FALSE)
if (!is.list(process_graph)) {
  stop("Process graph must be a JSON object.")
}

connect(backend_url)
login(user = username, password = password)

get_job_status <- function(job_id) {
  status <- tryCatch({
    info <- describe_job(job_id)
    info$status %||% NA_character_
  }, error = function(e) {
    NA_character_
  })

  if (!is.na(status) && nzchar(status)) {
    return(status)
  }

  jobs <- list_jobs()
  idx <- which(jobs$id == job_id)
  if (length(idx) < 1) {
    return("not_found")
  }
  as.character(jobs$status[idx[[1]]])
}

con <- active_connection()
payload <- list(
  title = "full_pg.json submission",
  description = "Submit full_pg.json graph via openeo R client",
  process = list(process_graph = process_graph)
)

response <- con$request(
  tag = "jobs_define",
  authorized = TRUE,
  data = payload,
  parsed = FALSE
)

is_httr2 <- inherits(response, "httr2_response")

response_headers <- if (is_httr2) {
  httr2::resp_headers(response)
} else {
  httr::headers(response)
}
job_id <- response_headers[["openeo-identifier"]] %||%
  response_headers[["OpenEO-Identifier"]] %||%
  response_headers[["openEO-identifier"]] %||%
  NA_character_

if (is.na(job_id) || !nzchar(job_id)) {
  parsed_body <- tryCatch(
    if (is_httr2) httr2::resp_body_json(response) else httr::content(response, as = "parsed", type = "application/json"),
    error = function(e) NULL
  )
  body_id <- if (!is.null(parsed_body) && !is.null(parsed_body$id)) parsed_body$id else NA_character_
  job_id <- body_id %||% NA_character_
}

if (is.na(job_id) || !nzchar(job_id)) {
  body_text <- tryCatch(
    if (is_httr2) httr2::resp_body_string(response) else httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) ""
  )
  stop("Failed to retrieve job id from /jobs response. Body: ", body_text)
}

cat("Created job:", job_id, "\n")
start_job(job_id)
cat("Job queued.\n")

if (!wait_for_finish) {
  cat("Current job status:", get_job_status(job_id), "\n")
  cat("Set OPENEO_WAIT_FOR_FINISH=true to poll until completion and download assets.\n")
  quit(save = "no", status = 0)
}

final_status <- "unknown"
for (i in seq_len(max_polls)) {
  status <- get_job_status(job_id)
  cat(sprintf("Poll %d/%d - status: %s\n", i, max_polls, status))
  final_status <- status

  if (status %in% c("finished", "error", "canceled", "not_found")) {
    break
  }
  Sys.sleep(poll_seconds)
}

if (final_status == "finished") {
  cat("Job finished successfully.\n")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  assets <- list_results(job_id)
  cat("Assets:\n")
  print(assets)
  download_results(job_id, folder = download_dir)

  backend_base <- sub("/+$", "", backend_url)
  for (asset_name in c("prediction.tif", "prediction_classes.json")) {
    asset_href <- tryCatch({
      info <- list_results(job_id)
      info$assets[[asset_name]]$href
    }, error = function(e) NULL)
    if (is.null(asset_href) || !nzchar(asset_href)) {
      next
    }
    target <- file.path(download_dir, asset_name)
    download_url <- if (grepl("^https?://", asset_href)) {
      asset_href
    } else {
      paste0(backend_base, asset_href)
    }
    tryCatch({
      utils::download.file(download_url, target, mode = "wb", quiet = TRUE)
      cat("Downloaded:", target, "\n")
    }, error = function(e) {
      cat("Could not download", asset_name, ":", conditionMessage(e), "\n")
    })
  }

  cat("Results downloaded to:", normalizePath(download_dir), "\n")
} else if (final_status == "error") {
  cat("Job failed. Backend logs:\n")
  print(logs(job_id))
  stop("Job ended with status 'error'.")
} else if (final_status == "canceled") {
  stop("Job ended with status 'canceled'.")
} else if (final_status == "not_found") {
  stop("Job id was not found in backend job list.")
} else {
  stop("Job did not finish within polling window. Last status: ", final_status)
}
