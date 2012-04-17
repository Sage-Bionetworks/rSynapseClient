# TODO: Add comment
# 
# Author: furia
###############################################################################

## Constructor
SynapseAnnotations <- 
  function(entity)
{
  if(!is.list(entity))
    stop("entity must be a list.")
  if(any(names(entity) == "") && length(entity) > 0)
	stop("all elements of the entity must be named")

  annotations <- new(Class = "SynapseAnnotations")
  .populateSlotsFromEntity(annotations, entity)
}

## show method
setMethod(
  f = "show",
  signature = "SynapseAnnotations",
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    lapply(annotationNames(object),FUN=function(n){cat(n,"=",paste(annotValue(object,n),collapse=","),"\n",sep="")})
  } 
)

## getter for annotation names
setMethod(
  f = "annotationNames",
  signature = "SynapseAnnotations",
  definition = function(object){
    propertyNames(object@annotations)
  }
)

setMethod(
  f = "annotValue",
  signature = signature("SynapseAnnotations", "character"),
  definition = function(object, which){
    propertyValue(objet@annotations, which) 
  }
)

setMethod(
  f = "annotValue<-",
  signature = signature("SynapseAnnotations", "character", "ANY"),
  definition = function(object, which, value){
    object@annotations <- propertyValue(objet@annotations, which, value)
    invisible(object)
  }
)

setMethod(
  f = "annotationValues",
  signature = "SynapseAnnotations",
  definition = function(object){
    propertyValues(object@annotations)
  }
)

setMethod(
  f = "annotationValues<-",
  signature = signature("SynapseAnnotations", "list"),
  def = function(object, value){
    ## this method is broken. need to implement  "propertyValues<-" for PropertyStore class
    object@annotations <- propertyValues(object) <- value
    invisible(object)
  }
)

setMethod(
  f = "propertyNames",
  signature = "SynapseAnnotations",
  definition = function(object){
    propertyNames(object@properties)
  }
)

setMethod(
  f = "propertyValues",
  signature = "SynapseAnnotations",
  definition = function(object){
    propertyValues(object@properties)
  }
)

## S3 method for converting to list
as.list.SynapseAnnotation <- 
  function(x, ...){
  as.list(x@annotations, ...)
}


as.list.SynapseAnnotations<-function(x) {
	c(as.list(x@properties), as.list(x@annotations))
}

# move content from 'entity' (a list) to 'object' ( a SynapseAnnotations)
setMethod(
  f = ".populateSlotsFromEntity",
  signature = signature("SynapseAnnotations", "list", "missing"),
  definition = function(object, entity){
    for (label in names(entity)) {
		if (label %in% c("stringAnnotations", "dateAnnotations", "doubleAnnotations", "longAnnotations")) {
			slot(object,label)<-entity[[label]]
		} else if (label =="blobAnnotations") {
				slot(object,label)<-entity[[label]]
		} else {
			propertyValue(object, label)<-entity[[label]]
		}
	}
    object
  }
)

setMethod(
  f = ".populateSlotsFromEntity",
  signature = signature("SynapseAnnotations", "missing", "character"),
  definition = function(object, json){
    data <- fromJSON(json)
    .populateSlotsFromEntity(object, list=data)
  }
)




