#' Collection
#'
#' This class represents the collections, which contain a set of Granules.
#' @field id Id of the collection
#' @field title Collection title
#' @field description Short description of the collection
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

    #' @description Initialize collection
    #'
    #' @param id Id of the collection
    #' @param title Collection title
    #' @param description Short description of the collection
    #'
    initialize = function(id = NA, title = NA, description = NA) {

      self$id = id
      self$title = title
      self$description = description
    },

    #' @description Add image collection to the collection class object
    #'
    #' @param ImageCollection Collection of class 'image collection'
    #'
    setCollection = function(ImageCollection) {
      if (! gdalcubes:::is.image_collection(ImageCollection)) {
        stop("Assigned data is not an image collection")
      }

      private$imageCollection = ImageCollection
      self$setMetadata()
    },

    #' @description Get assigned image collection
    #'
    #' @return image collection
    #'
    getCollection = function() {
      return(private$imageCollection)
    },

    #' @description add extent and bands to the metadata of the collection object
    #'
    setMetadata = function() {

      private$metadata = list(
        extent = extent(private$imageCollection),
        bands = gdalcubes:::libgdalcubes_image_collection_info(private$imageCollection)$bands$name
      )
    },

    #' @description Get metadata of the collection
    #'
    #' @return metadata of the collection
    #'
    getMetadata = function() {
      return(private$metadata)
    },

    #' @description Convert bandlist to be openeo compliant
    #'
    #' @return converted bandlist
    #'
    getEoBands = function() {

      list = list()
      bands = as.list(self$getMetadata()$bands)
      list = lapply(bands, function(x) {
               append(list,  list(name = x))
             })
      return(list)
    },

    #' @description List metadata for the collection handler
    #'
    collectionInfo = function() {
      list(
        stac_version = Session$getConfig()$stac_version,
        id = self$id,
        title = self$title,
        description = self$description,
        license = "proprietary",
        extent = list(
          spatial = list(
            bbox = list(list(self$getMetadata()$extent$left, self$getMetadata()$extent$bottom,
            self$getMetadata()$extent$right, self$getMetadata()$extent$top))
          ),
          temporal = list(
            interval = list(list(self$getMetadata()$extent$t0, self$getMetadata()$extent$t1))
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
         ))
      )
    },

    #' @description List extended metadata for the collection handler
    #'
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
            bbox = list(list(self$getMetadata()$extent$left, self$getMetadata()$extent$bottom,
                             self$getMetadata()$extent$right, self$getMetadata()$extent$top))
          ),
          temporal = list(
            interval = list(list(self$getMetadata()$extent$t0, self$getMetadata()$extent$t1))
          )
        ),
        links = list(list(
          rel = "root",
          href = paste(Session$getConfig()$base_url, "collections", sep = "/"))
        ),
        "cube:dimensions" = list(
          x = list(
            type = "spatial",
            axis = "x",
            extent = list(self$getMetadata()$extent$left, self$getMetadata()$extent$right
            )
          ),
          y = list(
            type = "spatial",
            axis = "y",
            extent = list(self$getMetadata()$extent$bottom, self$getMetadata()$extent$top
            )
          ),
          t = list(
            type = "temporal",
            extent = list(self$getMetadata()$extent$t0, self$getMetadata()$extent$t1)
          ),
          bands = list(
            type = "bands",
            values = list(self$getMetadata()$bands)
          )
        ),
        summaries = list(constellation = list(""), 'eo:bands' = self$getEoBands())
      )
    }
  ),
  private = list(
    imageCollection = NULL,
    metadata = NULL
  )
)

#' @export
is.Collection = function(obj) {
  return("Collection" %in% class(obj))
}


#'sentinel-s2-l2a-cogs
sentinel_s2_l2a_cogs <- Collection$new(
  id = "sentinel-s2-l2a-cogs",
  title = "Sentinel 2 L2A COGs",
  description = "Sentinel-2a and Sentinel-2b imagery, processed to Level 2A (Surface Reflectance) and converted to Cloud-Optimized GeoTIFFs."
)

#'sentinel-s2-l2a
sentinel_s2_l2a <- Collection$new(
  id = "sentinel-s2-l2a",
  title = "Sentinel 2 L2A",
  description = "Sentinel-2a and Sentinel-2b imagery, processed to Level 2A (Surface Reflectance)."
)

#'sentinel-s2-l1c
sentinel_s2_l1c <- Collection$new(
  id = "sentinel-s2-l1c",
  title = "Sentinel 2 L1C",
  description = "Sentinel-2a and Sentinel-2b imagery, processed to Level 1C (Top-Of-Atmosphere Geometrically Corrected)."
)

#'landsat-8-l1-c1
landsat_8_l1_c1 <- Collection$new(
  id = "landsat-8-l1-c1",
  title = "Landsat-8 L1 Collection-1",
  description = "Landat-8 L1 Collection-1 imagery radiometrically calibrated and orthorectified using gound points and Digital Elevation Model (DEM) data to correct relief displacement."
)
