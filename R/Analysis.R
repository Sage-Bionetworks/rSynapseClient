## S4 Class definition, constructors and associated methods for Synapse analyses
## 
## Author: Bruce Hoff <bruce.hoff@sagebase.org>
###############################################################################

setMethod(
  f = "Analysis",
  signature = "numeric",
  definition = function(entity){
    Analysis(as.character(entity))
  }
)

setMethod(
  f = "Analysis",
  signature = "character",
  definition = function(entity){
    entity <- getAnalysis(entity = entity)
    Analysis(entity)
  }
)

setMethod(
  f = "Analysis",
  signature = "list",
  definition = function(entity){
    ee <- new("Analysis")
    ee@properties <- entity
    ee
  }
)

setMethod(
  f = "Analysis",
  signature = "missing",
  definition = function(entity){
    Analysis(list())
  }
)
