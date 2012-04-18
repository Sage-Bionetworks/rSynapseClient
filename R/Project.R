## S4 Class definition, constructors and associated methods for Synapse projects
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################

setMethod(
  f = "Project",
  signature = "list",
  definition = function(entity){
    ee <- new("Project")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
  f = "Project",
  signature = "missing",
  definition = function(entity){
    Project(list())
  }
)
