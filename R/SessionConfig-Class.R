#' SessionConfig class
#' @description Session configuration
#' @param api.port On which port to run the plumer API
#' @param host Host of the plumber API
#'
#' @export
SessionConfig = function(api.port = NULL, host = NULL) {

  if (is.null(api.port)) {
    api.port = 8000
  }
  if (is.null(host)) {
    host = "127.0.0.1"
  }

  if (host == "0.0.0.0") {
    base = paste("http://", "localhost:", api.port,  sep = "")
  }
  else {
    base = paste("http://",host, ":", api.port,  sep = "")
  }

  default = list(
    api_version = "1.0.0",
    backend_version = "0.1.0",
    stac_version = "0.9.0",
    stac_extensions = "datacube",

    id = "openeo-gdalcubes-R-driver",
    title = "openeo-gdalcubes-R-driver",
    description = "This is an openEo-compliant R driver interfacing with the package gdalcubes",

    links = NULL,
    demo.path = NULL,
    workspace.path = NULL,

    user = "user",
    password = "password",

    api.port = api.port,
    host = host,
    base_url = base,

    outputFormats = list(
      GTiff = list(
        title = "GeoTiff",
        description = "Export to GeoTiff.",
        gis_data_types = list("raster"),
        parameters = list(
          format = list(
            type = "string",
            description = "GeoTiff"
          )
        )
      ),
      NetCDF = list(
        title = "Network Common Data Form",
        description = "Export to NetCDF",
        gis_data_types = list("raster"),
        parameters = list(
          format = list(
            type = "string",
            description = "NetCDF"
          )
        )

      )
    ),
    inputFormats = list(
      ImageCollection = list(
        title = "ImageCollection",
        description = "Import from image collection",
        gis_data_types = list("raster"),
        parameters = list(
          format = list(
            type = "string",
            description = "gdalcubes collection formats"
          )
        )

      )
    ),

    OGC_conformanceLink = "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core"
  )

  class(default) = "ServerConfig"
  return(default)
}
