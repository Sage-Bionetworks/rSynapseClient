## Get an entity from Synapse
##
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "getEntity",
  signature = signature("character", "missing"),
  definition = function(entity){
    getEntity(entity, "")
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
    ## instantiate the entity
    if(versionId == ""){
      ee <- synapseGet(.generateEntityUri(entity))
    }else{
      ee <- synapseGet(.generateEntityUri(entity, versionId))
    }
    ee <- getEntityInstance(ee)

    ## get annotations
    ee@annotations <- getAnnotations(ee)

    ## get the 'generatedBy' activity, if any
    ## !!!! Note, it's crucial to retrieve the 'generatedBy' info when !!!!
    ## !!!! retrieving the entity, otherwise on the subsequent call to  !!!!
    ## !!!! storeEntity the link will be lost !!!!
    ee@generatedBy <- getGeneratedBy(ee)

    ## cache the entity to disk
    cacheEntity(ee)
	
    ee
  }
)

# returns the Activity linked to the entity by the 'generatedBy' relationship,
# or NULL if there is no such Activity
setMethod(
  f = "getGeneratedBy",
  signature = signature("Entity"),
  definition = function(entity) {
    if(is.null(propertyValue(entity, "versionNumber"))) {
    	uri<-paste("/entity/", 
    				propertyValue(entity, "id"), 
    				"/generatedBy", 
    				sep="")
    } else {
    	uri<-paste("/entity/", 
    				propertyValue(entity, "id"), 
    				"/version/", 
    				propertyValue(entity, "versionNumber"), 
    				"/generatedBy", 
    				sep="")
    }
    curlHandle<-getCurlHandle()
    activityList <- synapseGet(uri, curlHandle=curlHandle, checkHttpStatus=F)
    info <- .getCurlInfo(curlHandle)
    if(info$response.code == 404) {
      return(NULL) # not found
    }
    .checkCurlResponse(curlHandle)
      
    Activity(activityList)
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
  signature = signature("list"),
  definition = function(entity){
    if(!("id" %in% names(entity)))
      stop("entity must have a field named id")
    if(is.null(entity$id))
      stop("id cannot be null")

    version <- entity$versionNumber
    if(is.null(version)){
      ee <- getEntity(as.character(entity$id))
    }else{
      ee <- getEntity(as.character(entity$id), version)
    }
    ee
  }
)
