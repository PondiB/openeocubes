
refreshJobFromFile <- function(job) {
  tryCatch({
    info_dir  <- file.path(Session$getConfig()$workspace.path, job$output.folder)
    info_file <- file.path(info_dir, "jobInfo.txt")
    
    if (!file.exists(info_file)) {
      return(job)
    }
    
    lines <- readLines(info_file, warn = FALSE)
    if (length(lines) == 0) {
      return(job)
    }
    
    kv <- strsplit(lines, ": ", fixed = TRUE)
    
    keys <- vapply(kv, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
    vals <- vapply(kv, function(p) {
      if (length(p) >= 2) {
        paste(p[-1], collapse = ": ")
      } else {
        NA_character_
      }
    }, character(1))
    
    meta <- as.list(vals)
    names(meta) <- keys
    
    get_field <- function(name) {
      if (!is.null(meta[[name]]) && !is.na(meta[[name]])) meta[[name]] else NULL
    }
    
    id <- get_field("Job_ID")
    title <- get_field("Job_Title")
    description <- get_field("Job_Description")
    status <- get_field("Job_Status")
    created <- get_field("Job_Created")

    if (!is.null(id)) job$id <- id
    if (!is.null(title)) job$title <- title
    if (!is.null(description)) job$description <- description
    if (!is.null(status)) job$status <- status
    if (!is.null(created)) job$created <- created

    return(job)
  }, error = function(e) {
    message("refreshJobFromFile failed: ", conditionMessage(e))
    return(job)
  })
}

.listAllJobs = function() {
  tryCatch({
    jobs = list(jobs = unname(lapply(Session$jobs, function(job) {
      job <- refreshJobFromFile(job)
      return(job$jobInfo())
    })))
    
    links = list(
      rel = "self",
      href = paste(Session$getConfig()$base_url, "jobs", sep = "/")
    )
    
    result = as.vector(c(jobs, links = list(list(links))))
    return(result)
  }, error = handleError)
}



.getJobById = function(req, res, job_id) {
  tryCatch({
    index = getJobIdIndex(job_id)
    
    if (!is.na(index)) {
      job = Session$jobs[[index]]
      
      job <- refreshJobFromFile(job)
      
      tryCatch({
        res$body = jsonlite::toJSON(job$jobInfo(), na = "null", null = "null", auto_unbox = TRUE)
        res$setHeader("Content-Type", "application/json")
      },
      error = function(e) {
        throwError("Internal", message = e)
      })
    } else {
      throwError("JobNotFound")
    }
    return(res)
  }, error = handleError)
}



.createNewJob = function(req, res) {
  tryCatch({
    sent_job = jsonlite::fromJSON(req$rook.input$read_lines(), simplifyDataFrame = FALSE)
    process_graph = sent_job$process
    
    job = Job$new(process = process_graph)
    job$status  = "created"
    job$created = as.character(Sys.time())

    if (!is.null(sent_job$title)) job$title = sent_job$title
    if (!is.null(sent_job$description)) job$description = sent_job$description
    
    writeJobInfo(job)
    Session$assignJob(job)
    
    res$setHeader(
      name  = "Location",
      value = paste(Session$getConfig()$base_url, "jobs", job$id, sep = "/")
    )
    res$setHeader(name = "OpenEO-Identifier", value = job$id)
    res$status <- 201
    
    out <- list(
      id = job$id,
      status = job$status,
      created = job$created
    )
    return(out)
  }, error = handleError)
}

.startJob = function(req, res, job_id) {
  tryCatch({
    index = getJobIdIndex(job_id)
    if (is.na(index)) {
      throwError("JobNotFound")
    }
    
    job <- Session$jobs[[index]]
    
    job$status <- "queued"
    writeJobInfo(job)
    
    Session$enqueueJob(job)
    
    res$status <- 202
    
    out <- list(
      id = job_id,
      status = job$status,
      message = "Job accepted for processing"
    )
    return(out)
  }, error = handleError)
}

.getJobResults = function(req, res, job_id) {
  tryCatch({
    index = getJobIdIndex(job_id)

    if (is.na(index)) {
      throwError("JobNotFound")
    } else {
      job <- Session$jobs[[index]]

      job_results <- file.path(Session$getConfig()$workspace.path, "jobs", job_id)
      files <- if (dir.exists(job_results)) list.files(job_results) else character(0)

      files <- setdiff(files, "jobInfo.txt")
      tifs <- files[grepl("\\.tif(f)?$", files, ignore.case = TRUE)]
      others <- setdiff(files, tifs)

      chosen_tif <- NULL
      if (length(tifs) >= 1) chosen_tif <- tifs[1]

      has_results <- (length(others) > 0) || (!is.null(chosen_tif))
      if (!has_results) {
        if (!is.null(job$status) && job$status == "error") {
          throwError("JobFailed")
        } else {
          throwError("JobNotFinished")
        }
      }

      assets <- list()

      if (length(others)) {
        links_others <- paste(Session$getConfig()$base_url, "jobs", job_id, others, sep = "/")
        for (i in seq_along(others)) {
          apList <- list(list(href = links_others[i]))
          names(apList) <- others[i]
          assets <- append(assets, apList)
        }
      }

      if (!is.null(chosen_tif)) {
        link_tif <- paste(Session$getConfig()$base_url, "jobs", job_id, chosen_tif, sep = "/")
        apList <- list(list(href = link_tif))
        names(apList) <- "prediction.tif"
        assets <- append(assets, apList)
      }

      return(list(
        title = job$title,
        description = job$description,
        assets = assets
      ))
    }
  }, error = handleError)
}


.getJobFiles = function(req, res, job_id, file) {
  tryCatch({
    message("getJobFiles called with job_id = ", job_id, ", file = ", file)
    
    resultFile <- file.path(Session$getConfig()$workspace.path, "jobs", job_id, file)
    message("resultFile path = ", resultFile)

    exists <- file.exists(resultFile)
    message("file.exists = ", exists)
    if (!exists) {
      throwError("JobFailed", message = paste("Result file not found:", resultFile))
    }
    
    info <- file.info(resultFile)
    size <- info$size
    message("file size = ", size)
    if (is.na(size) || size <= 0) {
      throwError("JobFailed", message = paste("Result file is empty or invalid:", resultFile))
    }
    
    ext <- tolower(tools::file_ext(resultFile))
    content_type <- switch(
      ext,
      "tif" = "image/tiff",
      "tiff" = "image/tiff",
      "nc" = "application/x-netcdf",
      "txt" = "text/plain; charset=UTF-8",
      "json" = "application/json; charset=UTF-8",
      "application/octet-stream"
    )
    message("content_type = ", content_type)

    res$body <- readBin(resultFile, "raw", n = size)
    res$setHeader("Content-Type", content_type)

    message("getJobFiles finished OK")
    return(res)
  }, error = handleError)
}
