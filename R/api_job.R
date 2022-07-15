# job handlers

.listAllJobs = function() {
  tryCatch({
    jobs = list(jobs = unname(lapply(Session$jobs, function(job){
        return(job$jobInfo())
      })))

    links = list(
      rel = "self",
      href = paste(Session$getConfig()$base_url, "jobs", sep = "/")
      )

    result = as.vector(c(jobs, links =list(list(links))))
    return(result)
  }, error = handleError)
}

.getJobById = function(req, res, job_id) {
  tryCatch({
    index = getJobIdIndex(job_id)

    if (! is.na(index)) {
      job = Session$jobs[[index]]

      tryCatch({
          res$body = toJSON(job$jobInfo(),na="null",null="null",auto_unbox = TRUE)
          res$setHeader("Content-Type","application/json")
        },
        error = function(e) {
          throwError("Internal",message=e)
      })
    }
    else {
      throwError("JobNotFound")
    }
    return(res)
  }, error = handleError)
}

.createNewJob = function(req,res) {
  tryCatch({
    sent_job = fromJSON(req$rook.input$read_lines(),simplifyDataFrame = FALSE)
    process_graph = sent_job$process

    job = Job$new(process = process_graph)
    job$status = "created"
    job$created = as.character(Sys.time())

    if (!is.null(sent_job$title)) { job$title = sent_job$title }
    if (!is.null(sent_job$description)) { job$description = sent_job$description }

    writeJobInfo(job)
    Session$assignJob(job)

    res$setHeader(name = "Location",
                  value= paste(Session$getConfig()$base_url, "jobs", job$id, sep ="/"))
    res$setHeader(name = "OpenEO-Identifier",value = job$id)
    res$status = 201

    return(res)
  }, error = handleError)
}

.startJob = function(req, res, job_id) {

  tryCatch({
    index = getJobIdIndex(job_id)

    if (is.na(index)) {
      throwError("JobNotFound")
    }
    job = Session$jobs[[index]]
    Session$runJob(job = job)
    res$status = 202

    return(res)
    },error=handleError)
}

.getJobResults = function(req, res, job_id) {

  tryCatch({
    index = getJobIdIndex(job_id)

    if (is.na(index)) {
      throwError("JobNotFound")
    }
    else {
      job = Session$jobs[[index]]
      if (job$status == "created") {
        throwError("JobNotStarted")
      }
      if (job$status == "running") {
        throwError("JobNotFinished")
      }
      job_results = paste(Session$getConfig()$workspace.path, "jobs", job_id, sep="/")
      base = paste0(Session$getConfig()$base_url, "/","result/", job_id)

      links = paste(Session$getConfig()$base_url, "jobs",job_id, list.files(job_results), sep="/")
      files = list.files(job_results)

      assets = list()
      for (i in 1:length(files)) {

        apList = list(list(href = links[i]))
        names(apList) = files[i]
        assets = append(assets, apList)
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
  resultFile = paste(Session$getConfig()$workspace.path, "jobs", job_id, file,sep="/")
  content_type = plumber:::getContentType(tools::file_ext(resultFile))

  res$body = readBin(resultFile, "raw", n = file.info(resultFile)$size)
  res$setHeader("Content-Type", content_type)

  return(res)
}, error = handleError)
}
