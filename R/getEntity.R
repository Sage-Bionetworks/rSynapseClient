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
	ee@generatedBy <- getGeneratedBy(ee)

    ## cache the entity to disk
    cacheEntity(ee)
	
    ee
  }
)

# returns the Activity ID linked to the entity by the 'generatedBy' relationship,
# or "" (empty character string) if there is no such Activity
getGeneratedBy<-function(entity) {
	if (is.null(propertyValue(entity, "versionNumber"))) {
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
	activity <- try(synapseGet(uri), silent=T)
	if (class(activity)=='try-error') {
		if (length(grep("404", activity, fixed=T))>0) {
			# it's a 404 Not Found status
			return("")
		} else {
			stop(activity)
		}
	}
	activity$id
}

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
