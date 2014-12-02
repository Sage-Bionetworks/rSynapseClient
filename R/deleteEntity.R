#
# 
# Author: mfuria
###############################################################################

setMethod(
    f = "deleteEntity",
    signature = "numeric",
    definition = function(entity) {
      deleteEntity(as.character(entity))
    }
)

setMethod(
    f = "deleteEntity",
    signature = "character",
    definition = function(entity) {
      if (isSynapseId(entity)) {
        synapseDelete(.generateEntityUri(entity))
      } else {
        stop(sprintf("%s is not a Synapse Id.", entity[1]))
      }
    }
)

