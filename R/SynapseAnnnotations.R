# TODO: Add comment
# 
# Author: furia
###############################################################################

## Constructor
SynapseAnnotations <- 
  function(entity)
{
  if (missing(entity)) stop("entity is required")
  if(!is.list(entity))
    stop("entity must be a list.")
  if(any(names(entity) == "") && length(entity) > 0)
	stop("all elements of the entity must be named")

    stop("all elements of the entity must be named")
  
  aa <- new(Class = "SynapseAnnotations")
  ps <- new("TypedPropertyStore")
  aa@annotations <- .populateSlotsFromEntity(ps, entity)
  
  # anything that's not an annotation becomes a property
  nms <- c("stringAnnotations",
		  "doubleAnnotations",
		  "longAnnotations",
		  "dateAnnotations",
		  "blobAnnotations")
  
  for (name in names(entity)) {
	  if (!(name %in% nms)) propertyValue(aa, name)<-entity[[name]]
  }
  aa
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
    propertyValues(object@annotations) <- value
    object
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


# for some reason the method above doesn't do what you expect, but this DOES
as_list_SynapseAnnotations <- 
		function(x, ...){
	as.list(x@annotations, ...)
}


as.list.SynapseAnnotations<-function(x) {
  annotations <- as.list(x@annotations)
    
    ## make sure all scalars are converted to lists since the service expects all 
    ## annotation values to be arrays instead of scalars
    for(key in names(annotations)){
      ## This is one of our annotation buckets
      if(is.list(annotations[[key]])) {
        for(annotKey in names(annotations[[key]])) {
          if(!is.list(annotations[[key]][[annotKey]]) && !is.vector(annotations[[key]][[annotKey]])) {
            annotations[[key]][[annotKey]] <- list(annotations[[key]][[annotKey]])
          }
        }
      }
    }
  
  c(as.list(x@properties), annotations)
}

# move content from 'entity' (a list) to 'object' ( a SynapseAnnotations)
setMethod(
  f = ".populateSlotsFromEntity",
  signature = signature("TypedPropertyStore", "list", "missing"),
  definition = function(object, entity){
    
    nms <- c("stringAnnotations",
      "doubleAnnotations",
      "longAnnotations",
      "dateAnnotations",
      "blobAnnotations")
    
    # for some reason this doesn't work
    #lapply(nms, function(n) slot(object, n) <- entity[[n]])
    # but this does
    for (n in nms) {
      if(!is.list(entity[[n]])){
        slot(object, n)<- as.list(entity[[n]])
      }else{
        slot(object, n)<- entity[[n]]
      }
    }
    
    object
  }
)

setMethod(
  f = ".populateSlotsFromEntity",
  signature = signature("TypedPropertyStore", "missing", "character"),
  definition = function(object, json){
    data <- fromJSON(json)
    .populateSlotsFromEntity(object, list=data)
  }
)


setMethod(
  f = "show",
  signature = "SynapseAnnotations",
  definition = function(object){
    cat(class(object),"\n")
    show(object@annotations)
  }
)


