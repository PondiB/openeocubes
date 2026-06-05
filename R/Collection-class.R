#' Collection
#'
#' This class represents the collections, which contain a set of Granules.
#' @field id Id of the collection
#' @field title Collection title
#' @field description Short description of the collection
#' @field spatialExtent Spatial extent of the collection
#' @field temporalExtent Temporal extent of the collection
#' @field bands Bands of the collection
#' @field constellation Constellation of the collection
#'
#' @include Session-Class.R
#' @importFrom R6 R6Class
#' @export
Collection <- R6Class(
  "Collection",
  public = list(
    id = NULL,
    title = NULL,
    description = NULL,
    spatialExtent = NULL,
    temporalExtent = NULL,
    bands = NULL,
    constellation = NULL,

    #' @description Initialize collection
    #'
    #' @param id Id of the collection
    #' @param title Collection title
    #' @param description Short description of the collection
    #' @param spatialExtent Spatial extent of the collection
    #' @param temporalExtent Temporal extent of the collection
    #' @param bands Bands of the collection
    #' @param constellation Constellation of the collection
    #'
    initialize = function(id = NA, title = NA, description = NA, spatialExtent = NA, temporalExtent = NA, bands = NA, constellation = NA) {
      self$id = id
      self$title = title
      self$description = description
      self$spatialExtent = spatialExtent
      self$temporalExtent = temporalExtent
      self$bands = bands
      self$constellation = constellation
    },

    #' @description Get collection information
    #'
    #' @return A list containing collection information
    collectionInfo = function() {
      list(
        stac_version = Session$getConfig()$stac_version,
        id = self$id,
        title = self$title,
        description = self$description,
        license = "proprietary",
        extent = list(
          spatial = list(
            bbox = list(self$spatialExtent)
          ),
          temporal = list(
            interval = list(self$temporalExtent)
          )
        ),
        links = list(
          list(
            rel = "self",
            href = paste(Session$getConfig()$base_url, "collections", self$id, sep = "/")
          ),
          list(
            rel = "parent",
            href = paste(Session$getConfig()$base_url, "collections", sep = "/")
          )
        )
      )
    },

    #' @description Get extended collection information
    #'
    #' @return A list containing extended collection information
    collectionInfoExtended = function() {
      list(
        stac_version = Session$getConfig()$stac_version,
        stac_extensions = list(Session$getConfig()$stac_extensions),
        id = self$id,
        title = self$title,
        description = self$description,
        license = "proprietary",
        extent = list(
          spatial = list(
            bbox = list(self$spatialExtent)
          ),
          temporal = list(
            interval = list(self$temporalExtent)
          )
        ),
        links = list(
          list(
            rel = "root",
            href = paste(Session$getConfig()$base_url, "collections", sep = "/")
          )
        ),
        "cube:dimensions" = list(
          x = list(
            type = "spatial",
            axis = "x",
            extent = list(self$spatialExtent[1], self$spatialExtent[3])
          ),
          y = list(
            type = "spatial",
            axis = "y",
            extent = list(self$spatialExtent[2], self$spatialExtent[4])
          ),
          t = list(
            type = "temporal",
            extent = self$temporalExtent
          ),
          bands = list(
            type = "bands",
            values = self$bands
          )
        ),
        summaries = list(
          constellation = list(self$constellation),
          'eo:bands' = self$bands
        )
      )
    }
  )
)

#' @export
is.Collection = function(obj) {
  return("Collection" %in% class(obj))
}


#'sentinel-2-l2a
sentinel_2_l2a <- Collection$new(
  id = "sentinel-2-l2a",
  title = "Sentinel 2 L2A",
  description = "Global Sentinel-2 data from the Multispectral Instrument (MSI) onboard Sentinel-2",
  spatialExtent = list(-180, -90, 180, 90),
  temporalExtent = list("2015-06-27T10:25:31.456000Z", "null"),
  bands = c("coastal", "blue", "green", "red", "rededge1", "rededge2", "rededge3", "nir", "nir08", "nir09", "cirrus", "swir16", "swir22", "scl"),
  constellation = list("sentinel-2")
)
# sentinel-2-l1c


landsat_c2_l2 <- Collection$new(
  id = "landsat-c2-l2",
  title = "Landsat Collection 2 Level-2",
  description = "Landsat Collection 2 Level-2 data from Landsat 4, 5, 7, 8 and 9.",
  spatialExtent = list(-180, -90, 180, 90),
  temporalExtent = list("1982-08-22T00:00:00Z", "null"),
  bands = c("blue", "green", "red", "nir08", "swir16", "swir22", "lwir", "qa_pixel"),
  constellation = list("landsat-4", "landsat-5", "landsat-7", "landsat-8", "landsat-9")
)