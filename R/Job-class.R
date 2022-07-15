#' Job
#'
#' @field id Id of the job
#' @field status Current status of the job
#' @field process Process of the job
#' @field created When the job was created
#' @field title Title of the job
#' @field description Shortly description of the planned result
#' @field results Result of the executed process
#' @field output Format of the output
#' @field openEoGraph Store the openEo graph for the job info
#'
#' @importFrom ids random_id
#'
#' @export
Job <- R6Class(
  "Job",
  public = list(
    id = NULL,
    status=NA,
    process = NULL,
    created = NULL,
    title = NULL,
    description = NULL,
    results = NULL,
    output = NULL,
    openEoGraph = NULL,

    #' @description Initialize job
    #'
    #' @param id Id of the job
    #' @param status Current status of the job
    #' @param process Process of the job
    #' @param created When the job was created
    #' @param title Title of the job
    #' @param description Shortly description of the planned result
    #' @param results Result of the executed process
    #'
    initialize = function(id = NA, process = NULL) {

      if (is.na(id)) {
        self$id = random_id(bytes = 6)
      }
      else {
        self$id = id
      }
      self$created = Sys.time()
      self$status = "created"
      self$openEoGraph = process$process_graph

      if (!is.null(process)) {
        if (!is.ProcessGraph(process)) {
            proGraph = ProcessGraph$new(process_graph = process)
        }
        else {
          proGraph$process_graph = process
        }
        self$process$process_graph = proGraph$buildExecutableProcessGraph(job=self)
      }
      return(self)
    },

    #' @description Set the output format
    #' @param output Format of the output
    setOutput = function(output) {
      self$output = output
    },

    #' @description Store the job in the Session
    #'
    store = function() {

      if (is.null(getJobIdIndex(self$id))) {

        Session$jobs = append(Session$jobs, list(list(id = self$id,
                                                      status = self$status,
                                                      created = self$created,
                                                      process = self$process,
                                                      title = self$title,
                                                      description = self$description )))
      }
      invisible(self)
    },

    #' @description Load the job properties from the stored jobs
    #'
    load = function() {
      index = getJobIdIndex(self$id)

      if (! is.na(index)) {
        storedJob = Session$jobs[[index]]

        self$status = storedJob$status
        self$created = storedJob$created
        self$title = storedJob$title
        self$description = storedJob$description

        proGraph = ProcessGraph$new(process_graph = storedJob$process)
        self$process = proGraph$buildExecutableProcessGraph(job = self)

        invisible(self)
      }
    },

    #' @description Execute the executable process graph and store it in the results of the job
    #'
    #' @return The executed job
    run = function() {

      tryCatch({
        self$status = "running"
        writeJobInfo(self)
        self$results = self$process$process_graph$execute()

        self$status = "finished"
        writeJobInfo(self)

     },
        error=function (e) {
        self$status = "error"
        self$results = NULL
        writeJobInfo(self)
      },
      finally = {
        return(self)
      })
    },

    #' @description Get information about the job
    #'
    #' @return Info list
    #'
    jobInfo = function() {

      info = list(
        id = self$id,
        title = self$title,
        description = self$description,
        process = list(process_graph = self$openEoGraph),
        status = self$status,
        created = self$created)

      return(info)
    }
  ),

  #' @field output.folder Set a new output folder
  active = list(
    output.folder = function() {
      return(paste(Session$config$workspace.path, "jobs", self$id,sep="/"))
    }
  )
)

#' getJobIdIndex
#' @param jid Job id
#' @return Index of the given id in the stored jobs
#'
#' @export
getJobIdIndex = function(jid) {
  ids = lapply(Session$jobs, function(x) {
    return(x$id)
  })
  index = match(jid, ids)
  return(index)
}


#' Check if given job is a job
#' @param obj Job to be checked
#' @return Is obj a job or not
#' @export
is.Job = function(obj) {
  return("Job" %in% class(obj))
}


#' Write the job properties to a txt file
#' @param job Job from which the properties will be stored
#' @export
writeJobInfo = function(job) {

  if ("Job" %in% class(job)) {
    dir = paste(Session$getConfig()$workspace.path, job$output.folder,sep = "/")
    if (!dir.exists(dir)) {
      dir.create(dir,recursive = TRUE)
    }

    txtDir = paste(dir, "jobInfo.txt",sep = "/")
    txt <- list(Job_ID=job$id, Job_Title=job$title, Job_Description=job$description, Job_Status=job$status, Job_Created=job$created)
    write.table(as.matrix(txt), txtDir, quote = FALSE, col.names = "Job info",sep = ": ")
  }
}
