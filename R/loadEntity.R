## Load Entity Objects
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "loadEntity",
  signature = signature("character", "missing"),
  definition = function(entity){
    entity <- getEntity(entity)
    loadEntity(entity)
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("character", "character"),
  definition = function(entity, versionId){
    entity <- getEntity(entity, versionId)
    loadEntity(entity, versionId)
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("character", "numeric"),
  definition = function(entity, versionId){
    loadEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("numeric", "missing"),
  definition = function(entity){
    loadEntity(as.character(entity))
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("numeric", "character"),
  definition = function(entity, versionId){
    loadEntity(as.character(entity), versionId)
  }
)
setMethod(
  f = "loadEntity",
  signature = signature("numeric", "numeric"),
  definition = function(entity, versionId){
    loadEntity(as.character(entity), as.character(versionId))
  }
)


