# process graph handler

.createProcessGraph = function(req,res) {

    if (!is.null(req$rook.input$read_lines())) {
      sent_graph = jsonlite::fromJSON(req$rook.input$read_lines(),simplifyDataFrame = FALSE)

      graph = ProcessGraph$new(process_graph = process_graph[["process_graph"]],
                               title = process_graph[["title"]],
                               description = process_graph[["description"]])
      graph$store()

    #  res$setHeader(name = "Location",value=paste0(openeo.server$configuration$baseserver.url,"/","process_graphs/",graph$graph_id))
      res$setHeader(name = "OpenEO-Identifier",value=graph$graph_id)

      res$status = 201

      return(res)
    } else {
      throwError("ProcessGraphMissing")
      #No data or malformed json was send"
    }

}
