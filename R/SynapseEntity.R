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
    annotValue(object = annotations(object), which = which) <- as.character(value)
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
    object@annotations <- new("SynapseAnnotations")
    annotationValues(object) <- value
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
			s4Entity <- new("SynapseEntity")
			.populateSlotsFromEntity(s4Entity, entity)
		}
)

#####
## constructor that takes a serialized JSON object
#####
setMethod(
		f = "SynapseEntity",
		signature = signature("character"),
		definition = function(entity){
			listEntity<-fromJSON(entity)
			.populateSlotsFromEntity(s4Entity, listEntity)
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
