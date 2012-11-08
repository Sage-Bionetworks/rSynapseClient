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

    ## cache the entity to disk
    cacheEntity(ee)
	
	generatedBy <- try(synapseGet(
			paste("/entity/", 
					propertyValue(entity, "id"), 
					"/version/", 
					propertyValue(entity, "versionNumber"), 
					"/generatedBy", 
					sep="")))
	if (class(generatedBy)=='try-error') {
		if (length(grep("404", generatedBy, fixed=T))>0) {
			# it's a 404 Not Found status
			ee@generatedBy<-NULL
		} else {
			stop(foo)
		}
	} else {
		ee@generatedBy<-generatedBy
	}
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
