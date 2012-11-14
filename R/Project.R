## S4 Class definition, constructors and associated methods for Synapse projects
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################

setMethod(
  f = "Project",
  signature = "list",
  definition = function(entity, ...){
    Constructor("Project", entity, ...)
  }
)

setMethod(
  f = "Project",
  signature = "missing",
  definition = function(...){
    Constructor("Project", ...)
  }
)
