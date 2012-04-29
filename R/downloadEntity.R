## get an entity from Synapse and download it's files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "downloadEntity",
  signature = "character",
  definition = function(entity){
    downloadEntity(getEntity(entity))
  }
)
setMethod(
  f = "downloadEntity",
  signature = "numeric",
  definition = function(entity){
    downloadEntity(as.character(entity))
  }
)

setMethod(
  f = "downloadEntity",
  signature = "list",
  definition = function(entity){
    downloadEntity(getEntity(entity))
  }
)

