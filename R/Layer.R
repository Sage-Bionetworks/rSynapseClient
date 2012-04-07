## Layer entity constructors
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

PhenotypeLayer <- function(entity){
  entity <- Layer(entity)
  class(entity) <- "PhenotypeLayer"
  return(entity)	
}
IlluminaExpressionLayer <- function(entity){
  entity <- Layer(entity)
  class(entity) <- "IlluminaExpressionLayer"
  return(entity)	
}
GenotypeLayer <- function(entity){
  entity <- Layer(entity)
  class(entity) <- "GenotypeLayer"
  return(entity)	
}
AgilentExpressionLayer <- function(entity){
  entity <- Layer(entity)
  class(entity) <- "AgilentExpressionLayer"
  return(entity)	
}
AffyExpressionLayer <- function(entity){
  entity <- Layer(entity)
  class(entity) <- "AffyExpressionLayer"
  return(entity)	
}
ExpressionLayer <- function(entity){
  entity <- Layer(entity)
  class(entity) <- "ExpressionLayer"
  return(entity)	
}
#####
## Layer constructors
#####
setMethod(
  f = "Layer",
  signature = "list",
  definition = function(entity){
    
    ## call super class constructor
    layer <- new(Class = "Layer")
    layer <- .populateSlotsFromEntity(layer, entity)
    
    ## add annotations
    annotations <- new(Class="SynapseAnnotation")
    if(!is.null(entity$id)){
      annotations <- tryCatch(
        SynapseAnnotation(getAnnotations(entity=entity)),
        error = function(e){
          warning("Unable to retrieve annotations for entity.")
        }
      )
    }
    layer@annotations<- annotations
    if(!is.null(propertyValue(layer, "id")))
      layer@synapseWebUrl <- .buildSynapseUrl(propertyValue(layer, "id"))
    setSubclass(layer)
  }
)

setMethod(
  f = "Layer",
  signature = "missing",
  definition = function(entity){
    Layer(list())
  }
)

####
## Method for setting the Layer subtype. Uses the annotation type to set the subclass
####
setMethod(
  f = "setSubclass",
  signature = "Layer",
  definition = function(object){
    
    ## determine the Layer type and subtype class
    ## TODO: this is a bit of  a kluge. need to redesign
    ## the way subclases are determined
    if(!is.null(layerType <- propertyValue(object, "type"))){
      if(layerType == "M"){
        format <- annotValue(object, "format")
        layerType <- "Media"
        if(!is.null(format) && tolower(format) == "code")
          layerType <- "Code"
      }
      if(!is.null(subClassType <- .getCache("layerCodeTypeMap")[[layerType]]))
        layerType <- subClassType
    }
    
    
    
    ## coerce to the correct subclass
    if(!is.null(layerType))
      class(object) <- layerType
    synapseEntityKind(object) <- synapseEntityKind(new(Class="Layer"))
    return(object)
  }
)





