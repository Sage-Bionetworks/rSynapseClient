# TODO: Add comment
# 
# Author: furia
###############################################################################

#####
## SynapseEntity "show" method
#####
setMethod(
  f = "show",
  signature = signature("SynapseEntity"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    
    cat("Synapse Entity Name : ", properties(object)$name, "\n", sep="")
    cat("Synapse Entity Id   : ", properties(object)$id, "\n", sep="")
    
    if (!is.null(properties(object)$parentId))
      cat("Parent Id           : ", properties(object)$parentId, "\n", sep="")
    if (!is.null(properties(object)$type))
      cat("Type                : ", properties(object)$type, "\n", sep="")
    if (!is.null(properties(object)$versionNumber)) {
      cat("Version Number      : ", properties(object)$versionNumber, "\n", sep="")
      cat("Version Label       : ", properties(object)$versionLabel, "\n", sep="")
    }
    
    cat("\nFor complete list of annotations, please use the annotations() function.\n")
    
  }
)

setMethod(
  f = "createEntity",
  signature = "SynapseEntity",
  definition = function(entity){
    createSynapseEntity(entity)
  }
)

setMethod(
    f = "deleteEntity",
    signature = "SynapseEntity",
    definition = function(entity){
      envir <- parent.frame(2)
      inherits <- FALSE
      name <- deparse(substitute(entity, env=parent.frame()))
      deleteSynapseEntity(propertyValue(entity,"id"))
      if(any(grepl(name,ls(envir=envir))))
        remove(list = name, envir=envir, inherits=inherits)
      entity <- deleteProperty(entity, "id")
      entity <- deleteProperty(entity, "accessControlList")
      entity <- deleteProperty(entity, "uri")
      entity <- deleteProperty(entity, "annotations")
      entity <- deleteProperty(entity, "etag")
      invisible(entity)
    }
)

setMethod(
  f = "updateEntity",
  signature = "SynapseEntity",
  definition = function(entity)
  {
    updateSynapseEntity(entity)
  }
)

setMethod(
  f = "downloadEntity",
  signature = "SynapseEntity",
  definition = function(entity){
    getEntity(entity)
  }
)


#####
## as.list function. Coerce SynapseEntity to list by returning annotations
#####
as.list.SynapseEntity <- 
  function(x, ...){
  as.list(annotations(x))         
}

#####
## Get annotation names
#####
setMethod(
  f = "annotationNames",
  signature = "SynapseEntity",
  definition = function(object){
    annotationNames(annotations(object))
  }
)

#####
## Get annotation values
#####
setMethod(
  f = "annotationValues",
  signature = "SynapseEntity",
  definition = function(object){
    annotationValues(annotations(object))
  }
)

#####
## Set the values for multiple annotations
#####
setMethod(
  f = "annotationValues<-",
  signature = signature("SynapseEntity","list"),
  definition = function(object, value){
    annotationValues(annotations(object)) <- value
    object
  }
)

setMethod(
  f = "annotValue<-",
  signature = signature("SynapseEntity", "character", "ANY"),
  definition = function(object, which, value){
    annotValue(object@annotations, which = which) <- value
    object
  }
)

#####
## return the annotations object 
#####
setMethod(
  f = "annotations",
  signature = "SynapseEntity",
  definition = function(object){
    object@annotations
  }
)


setMethod(
    f = "annotations<-",
    signature = signature("SynapseEntity", "list"),
    definition = function(object, value){
      if(any(names(value) == ""))
        stop("all elements of the list must be named")
      aa <- SynapseAnnotations(properties(object))
      for(n in names(value)){
            annotValue(aa, n) <- value[[n]]
          }
      annotations(object) <- aa
      object
    }
)

#####
## set the annotations object
#####
setMethod(
  f = "annotations<-",
  signature = signature("SynapseEntity","SynapseAnnotations"),
  definition = function(object, value){
    object@annotations <- value
    object
  }
)

#####
## replace annotations with the values
#####
setMethod(
  f = "annotations<-",
  signature = signature("SynapseEntity", "list"),
  definition = function(object, value){

	a <- new("SynapseAnnotations")
	annotations(a)<-value
    annotations(object) <- a
    object
  }
)

#####
## get an annotation value by name
#####
setMethod(
  f = "annotValue",
  signature = signature("SynapseEntity", "character"),
  definition = function(object, which){
    annotValue(annotations(object), which)  
  }
)



#####
## Delete an annotation
#####
setMethod(
  f = "deleteAnnotation",
  signature = signature("SynapseEntity", "character"),
  definition = function(object, which){
    annotations(object) <- deleteAnnotation(annotations(object), which)
    object
  }
)

#####
## constructor that takes a list entity
#####
setMethod(
		f = "SynapseEntity",
		signature = signature("list"),
		definition = function(entity){
			ee <- new("SynapseEntity")
			ee@properties <- entity
      ee
		}
)

#####
## constructor that takes a list entity
#####
setMethod(
  f = "SynapseEntity",
  signature = signature("missing"),
  definition = function(entity){
    SynapseEntity(emptyNamedList)
  }
)

#####
## constructor that takes a serialized JSON object
#####
setMethod(
		f = "SynapseEntity",
		signature = signature("character"),
		definition = function(entity){
      ee<-fromJSON(entity)
      ee@properties <- entity
      ee
		}
)

#####
## convert the S4 entity to a list entity
#####
setMethod(
  f = ".extractEntityFromSlots",
  signature = "SynapseEntity",
  definition = function(object){
	properties(object)
  }
)

#####
## convert the list entity to an S4 entity
#####
setMethod(
  f = ".populateSlotsFromEntity",
  signature = signature("SynapseEntity", "list"),
  definition = function(object, entity){
    if(any(names(entity) == "") && length(entity) > 0)
      stop("All elements of the entity must be named")
    
    ## all entity fields should be stored as properties
    for(name in names(entity))
      propertyValue(object, name) <- entity[[name]]
    object
  }
)

#####
## Get the Synapse entity kind
#####
setMethod(
  f = "synapseEntityKind",
  signature = "SynapseEntity",
  definition = function(entity){
    entity@synapseEntityKind
  }
)

#####
## Set the entity kind
#####
setMethod(
  f = "synapseEntityKind<-",
  signature = "SynapseEntity",
  definition = function(entity, value){
    entity@synapseEntityKind <- value
    entity
  }
)

#####
## Refresh the entities annotations
#####
setMethod(
  f = "refreshAnnotations",
  signature = "SynapseEntity",
  definition = function(entity){
	  #  MF will refactor this code
    annotations(entity) <- do.call(class(annotations(entity)), list(entity = getAnnotations(.extractEntityFromSlots(entity))))
    entity
  }     
)


setMethod(
  f = "getAnnotations",
  signature = "SynapseEntity",
  definition = function(entity){
    as.list(entity@annotations)
  }
)

names.SynapseEntity <-
  function(x)
{
  c("properties", "annotations")
}

setMethod(
  f = "[",
  signature = "SynapseEntity",
  definition = function(x, i, j, ...){
    if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
      stop("incorrect number of subscripts")
    if(is.numeric(i)){
      if(any(i > length(names(x))))
        stop("subscript out of bounds")
      i <- names(x)[i]
    }else if(is.character(i)){
      if(!all(i %in% names(x)))
        stop("undefined objects selected")
    }else{
      stop(sprintf("invalid subscript type '%s'", class(i)))
    }
    retVal <- lapply(i, function(i){
        if(i %in% names(x)){
          retVal <- slot(x, i)
        }else{
          retVal <- NULL
        }
      }
    )
    names(retVal) <- i
    retVal
  }
)

setMethod(
  f = "[[",
  signature = "SynapseEntity",
  definition = function(x, i, j, ...){
    if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
      stop("incorrect number of subscripts")
    if(length(i) > 1)
      stop("subscript out of bounds")
    x[i][[1]]
  }
)

setMethod(
  f = "$",
  signature = "SynapseEntity",
  definition = function(x, name){
    x[[name]]
  }
)

setReplaceMethod("$", 
  signature = "SynapseEntity",
  definition = function(x, name, value) {
    if(!(name %in% names(x)))
      stop("invalid element")
    slot(x, name) <- value
    x
  }
)

