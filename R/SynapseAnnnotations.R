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
  
  annotations <- new(Class = "SynapseAnnotations")
  if(any(names(entity) == "") && length(entity) > 0)
    stop("all elements of the entity must be named")
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
    getProperty(object@annotations, which) 
  }
)

setMethod(
  f = "annotValue<-",
  signature = signature("SynapseAnnotations", "character", "ANY"),
  definition = function(object, which, value){
    object@annotations <- setProperty(object@annotations, which, value)
    object
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
    f = "propertyValues<-",
    signature = signature("SynapseAnnotations", "list"),
    definition = function(object, value){
      if(any(names(value)))
      lapply(names(value))
      object
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
as.list.SynapseAnnotations <- 
  function(x, ...){
  as.list(x@annotations, ...)
}


## ******* instead of doing this, write the S3 method as.list.SynapseAnnotations as.string(c(as.list(t), as.list(p)))
## ******* where t is the TypedPropertyStore and p SimplePropertyOwner
## Bruce: the methods below are for you
## need to 
setMethod(
  f = ".extractEntityFromSlots",
  signature = "SynapseAnnotations",
  definition = function(object){
    entity <- list() #TODO needs to be emptyNamedList(), not list()
	# TODO:  pull values from SynapseAnnotations and put in list
			# need to get the properties as well as the annots
			# see PropertyStore.R, as.list.TypedPropertyStore
    stop("need to implement this")
    entity
  }
)

setMethod(
  f = ".populateSlotsFromEntity",
  signature = signature("SynapseAnnotations", "list", "missing"),
  definition = function(object, entity){
    
    stop("Need to implement this")
    ##*** bruce to do this ***
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

##
## End Bruce
##



