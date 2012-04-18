## Get an entity from Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "getEntity",
  signature = signature("character", "missing"),
  definition = function(entity){
    getEntity(entity, FALSE)
  }

)

setMethod(
  f = "getEntity",
  signature = signature("character", "logical"),
  definition = function(entity, fromCache){
    
    if(fromCache){
      ## if the entity isn't cached, throw an exception
      if(!file.exists(path.expand(file.path(.entityFileCachePath(id), .getCache("ENTITY_FILE_NAME"))))
        || !file.exists(path.expand(file.path(.entityFileCachePath(id), .getCache("ANNOTATIONS_FILE_NAME"))))){
        stop("entity is not cached on local disk.")
      }
    }else{
      ## download entity and annotations to disk
      getAnnotationsFromSynapse(entity)
      getEntityFromSynapse(entity)
    }
    
    ## instantiate the entity
    ee <- getEntityFromFileCache(entity)
    
    ## load annotations from disk
    ee@annotations <- getAnnotationsFromFileCache(entity)
    
    ee
  }
)

setMethod(
  f = "getEntity",
  signature = signature("numeric","missing"),
  definition = function(entity){
    getEntity(as.character(entity))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("numeric","logical"),
  definition = function(entity, fromCache){
    getEntity(as.character(entity, fromCache))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("integer","logical"),
  definition = function(entity, fromCache){
    getEntity(as.character(entity, fromCache))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("integer","missing"),
  definition = function(entity){
    getEntity(as.character(entity))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("SynapseEntity", "logical"),
  definition = function(entity, fromCache){
    id <- propertyValue(entity, "id")
    if(is.null(id))
      stop("entity id cannot be null")
    
    getEntity(as.character(id), fromCache)
  }
)

setMethod(
  f = "getEntity",
  signature = signature("SynapseEntity", "missing"),
  definition = function(entity){
    id <- propertyValue(entity, "id")
    if(is.null(id))
      stop("entity id cannot be null")
    
    getEntity(as.character(id))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("list", "missing"),
  definition = function(entity){
    if(!("id" %in% names(entity)))
      stop("entity must have a field named id")
    if(is.null(entity$id))
      stop("id cannot be null")
    getEntity(as.character(entity$id))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("list", "logical"),
  definition = function(entity, fromCache){
    if(!("id" %in% names(entity)))
      stop("entity must have a field named id")
    if(is.null(entity$id))
      stop("id cannot be null")
    getEntity(as.character(entity$id), fromCache)
  }
)
