## S4 Class definition, constructors and associated methods for Synapse steps
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
###############################################################################

setMethod(
  f = "Step",
  signature = "numeric",
  definition = function(entity, ...){
    Step(as.character(entity))
  }
)

setMethod(
  f = "Step",
  signature = "character",
  definition = function(entity, ...){
    entity <- getStep(entity = entity)
    Step(entity)
  }
)

setMethod(
  f = "Step",
  signature = "list",
  definition = function(entity, ...){
    Constructor("Step", entity, ...)
  }
)

setMethod(
  f = "Step",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("Step", ...)
  }
)

