#' SessionConfig class
#' @description Session configuration
#' @param api.port On which port to run the plumber API
#' @param host Host of the plumber API
#'
#' @export
SessionConfig = function(api.port = NULL, host = NULL, aws.ipv4 = NULL) {

  if (is.null(api.port)) {
    api.port = 8000
  }
  if (is.null(host)) {
    host = "127.0.0.1"
  }

  if (host == "0.0.0.0" && is.null(aws.ipv4)) {
    base = paste("http://", "localhost:", api.port,  sep = "")
  }
  else if(host == "0.0.0.0" && !is.null(aws.ipv4)){
    base = paste("http://", aws.ipv4, ":", api.port,  sep = "")
  }
  else {
    base = paste("http://",host, ":", api.port,  sep = "")
  }

  default = list(
    api_version = "1.2.0",
    backend_version = "0.1.0",
    stac_version = "1.0.0",
    stac_extensions = "datacube",

    id = "openeo-gdalcubes-R-backend",
    title = "openeo-gdalcubes-R-backend",
    description = "This is an OpenEO compliant R backend interfacing with gdalcubes package and STAC API",

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
      GTiff = list(
        title = "GeoTiff",
        description = "Geotiff is one of the most widely supported formats. This backend allows reading from Geotiff to create raster data cubes.",
        gis_data_types = list("raster"),
        parameters = list(
          format = list(
            type = "string",
            description = "GeoTiff"
          )
        )
      )
      ),

    OGC_conformanceLink = "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core"
  )

  class(default) = "ServerConfig"
  return(default)
}
