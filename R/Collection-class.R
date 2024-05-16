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

# Example collections
# Instantiate collections

#'sentinel-s2-l2a-cogs
sentinel_s2_l2a_cogs <- Collection$new(
  id = "sentinel-s2-l2a-cogs",
  title = "Sentinel 2 L2A COGs",
  description = "Sentinel-2a and Sentinel-2b imagery, processed to Level 2A (Surface Reflectance) and converted to Cloud-Optimized GeoTIFFs.",
  spatialExtent = list(-180, -90, 180, 90),
  temporalExtent = list("2015-06-27T10:25:31.456000Z", "null"),
  bands = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B8", "B8A", "B9", "B10", "B11", "B12", "AOT", "WVP","SCL"),
  constellation = list("sentinel-2")
)

# sentinel-s2-l2a
sentinel_s2_l2a <- Collection$new(
  id = "sentinel-s2-l2a",
  title = "Sentinel 2 L2A",
  description = "Sentinel-2a and Sentinel-2b imagery, processed to Level 2A (Surface Reflectance).",
  spatialExtent = list(-180, -90, 180, 90),
  temporalExtent = list("2015-06-27T10:25:31.456000Z", "null"),
  bands = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B8", "B8A", "B9", "B10", "B11", "B12", "AOT", "WVP","SCL"),
  constellation = list("sentinel-2")
)


# sentinel-s2-l1c
sentinel_s2_l1c <- Collection$new(
  id = "sentinel-s2-l1c",
  title = "Sentinel 2 L1C",
  description = "Sentinel-2a and Sentinel-2b imagery, processed to Level 1C (Top-Of-Atmosphere Geometrically Corrected).",
  spatialExtent = list(-180, -90, 180, 90),
  temporalExtent = list("2015-06-27T10:25:31.456000Z","null"),
  bands = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B8", "B8A", "B9", "B10", "B11", "B12"),
  constellation = list("sentinel-2")
)

# landsat-8-l1-c1
landsat_8_l1_c1 <- Collection$new(
  id = "landsat-8-l1-c1",
  title = "Landsat-8 L1 Collection-1",
  description = "Landat-8 L1 Collection-1 imagery radiometrically calibrated and orthorectified using ground points and Digital Elevation Model (DEM) data to correct relief displacement.",
  spatialExtent = list(-180, -90, 180, 90),
  temporalExtent = list("2013-06-01T00:00:00Z", "null"),
  bands = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B9", "B10", "B11"),
  constellation = list("Landsat 8")
)
