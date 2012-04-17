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
      if(!file.exists(path.expand(synapseClient:::.getAbsoluteFileCachePath(synapseClient:::.entityFileCachePath(entity)))))
        stop("entity is not cached on local disk.")
    }
    
    if(!fromCache){
      ## download entity and annotations to disk
      synapseEntityToFileCache(entity)
    }
    ## load from disk cache
    json <- getEntityFromFileCache(entity)
    if(!is.list(json))
      json<-as.list(json)
    
    ## instantiate the entity
    ee <- getEntityInstance(json)
    
    ## load annotations from disk
    json <- getEntityAnnotationsFromFileCache(entity)
    if(!is.list(json))
      json <- as.list(json)
    
    ## instantiate the annotations store and put it
    ## into the annotations slot
    ee@annotations <- SynapseAnnotations(json)
    
    ## return the entity
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
