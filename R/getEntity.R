## Get an entity from Synapse
##
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################


setMethod(
  f = "getEntity",
  signature = signature("character", "missing"),
  definition = function(entity){
    ## download entity and annotations to disk
    synapseClient:::getAnnotationsFromSynapse(entity)
    synapseClient:::getEntityFromSynapse(entity)

    ## instantiate the entity
    ee <- getEntityFromFileCache(entity)

    ## load annotations from disk
    ee@annotations <- getAnnotationsFromFileCache(entity)

    ee
  }
)

setMethod(
  f = "getEntity",
  signature = signature("character", "numeric"),
  definition = function(entity, versionId){
    getEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("character", "character"),
  definition = function(entity, versionId){
    ## download entity and annotations to disk
    synapseClient:::getAnnotationsFromSynapse(entity, versionId)
    synapseClient:::getEntityFromSynapse(entity, versionId)

    ## instantiate the entity
    ee <- getEntityFromFileCache(entity, versionId)

    ## load annotations from disk
    ee@annotations <- getAnnotationsFromFileCache(entity, versionId)

    ee
  }
)



setMethod(
  f = "getEntity",
  signature = signature("numeric", "missing"),
  definition = function(entity){
    getEntity(as.character(entity))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("integer", "missing"),
  definition = function(entity){
    getEntity(as.character(entity))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("SynapseEntity"),
  definition = function(entity){
    id <- propertyValue(entity, "id")
    if(is.null(id))
      stop("entity id cannot be null")

    getEntity(as.character(id))
  }
)


setMethod(
  f = "getEntity",
  signature = signature("list"),
  definition = function(entity){
    if(!("id" %in% names(entity)))
      stop("entity must have a field named id")
    if(is.null(entity$id))
      stop("id cannot be null")
    getEntity(as.character(entity$id))
  }
)
