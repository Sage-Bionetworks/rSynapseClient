# TODO: Add comment
# 
# Author: mfuria
###############################################################################

setMethod(
    f = "deleteEntity",
    signature = "numeric",
    definition = function(entity){
      deleteEntity(as.character(entity))
    }
)

setMethod(
    f = "deleteEntity",
    signature = "character",
    definition = function(entity){
      synapseDelete(.generateEntityUri(entity))
    }
)

