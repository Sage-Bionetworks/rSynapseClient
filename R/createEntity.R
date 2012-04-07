## Create an entity is Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.createEntity <- 
  function(kind, entity)
{
  if(missing(entity)) {
    stop("missing entity parameter")
  }
  
  if(!is.list(entity)){
    stop("the entity must be an R list")
  }
  
  uri <- paste("/", kind, sep = "")
  
  synapsePost(uri=uri, entity=entity, anonymous = FALSE)
}

setMethod(
  f = createEntity,
  signature = signature("list", "character"),
  definition = function(entity, className){
    if(any(names(entity) == "") && length(entity) > 0)
      stop("all entity elements must be named")
    kind <- synapseEntityKind(new(Class=className))
    entity <- .createEntity(kind=kind, entity)
    do.call(className, args = list(entity=entity))
  }
)

setMethod(
  f = "createEntity",
  signature = signature("SynapseEntity", "missing"),
  definition = function(entity){
    ## create the entity
    oldAnnotations <- annotations(entity)
    oldClass <- class(entity)
    uri <- paste("/", synapseEntityKind(entity), sep = "")
    tmp <- synapsePost(uri=uri, entity=.extractEntityFromSlots(entity))
    entity <- SynapseEntity(tmp)
    class(entity) <- oldClass
    
    ## update entity annotations
    newAnnotations <- annotations(entity)
    if(length(as.list(oldAnnotations)) > 0L){
      annotationValues(newAnnotations) <- as.list(oldAnnotations)
      tryCatch(
        annotations(entity) <- updateEntity(newAnnotations),
        error = function(e){
          ## unable to update annotations. delete parent entity.
          deleteEntity(entity)
          stop("Could not set annotations: ", e)
        }
      )
      entity <- getEntity(entity)
    }	
    entity
  }
)

######
## Creating Layer entities must be handled differently since get and post are asymmetrical.
## Specifically, after updating the annotations on the newly created enity, the etag is changed
## so we must refresh the entity before returning it. For locations, this refevts the url and md5sum
## to the values for the new Location, which were returned by the createEntity call.
######

setMethod(
  f = "createEntity",
  signature = signature("LocationOwner", "missing"),
  definition = function(entity){
    oldClass <- class(entity)
    class(entity) <- "SynapseEntity"
    createdEntity <- createEntity(entity)
    class(createdEntity) <- oldClass
    class(entity) <- oldClass
    createdEntity@location <- entity@location
    createdEntity@synapseWebUrl <- .buildSynapseUrl(propertyValue(createdEntity, "id"))
    createdEntity
  }
)
