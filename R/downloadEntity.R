## get an entity from Synapse and download it's files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "downloadEntity",
  signature = signature("character", "missing"),
  definition = function(entity){
    downloadEntity(getEntity(entity))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("character", "character"),
  definition = function(entity, versionId){
    downloadEntity(getEntity(entity, versionId))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("character", "numeric"),
  definition = function(entity, versionId){
    downloadEntity(getEntity(entity, as.character(versionId)))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("numeric", "missing"),
  definition = function(entity){
    downloadEntity(as.character(entity))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("numeric", "character"),
  definition = function(entity, versionId){
    downloadEntity(as.character(entity), versionId)
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("numeric", "numeric"),
  definition = function(entity, versionId){
    downloadEntity(as.character(entity), as.character(versionId))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("list", "missing"),
  definition = function(entity){
    versionId = entity$versionNumber
    if(is.null(versionId)){
      entity <- downloadEntity(getEntity(entity))
    } else {
      entity <- downloadEntity(getEntity(entity), as.character(versionId))
    }
    entity
  }
)




