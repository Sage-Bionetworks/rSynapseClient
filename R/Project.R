## S4 Class definition, constructors and associated methods for Synapse projects
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################

setMethod(
  f = "Project",
  signature = "list",
  definition = function(entity){
    ## call the superClass constructor
    s4Entity <- SynapseEntity(entity)
    class(s4Entity) <- "Project"
    synapseEntityKind(s4Entity) <- synapseEntityKind(new(Class="Project"))
    return(s4Entity)
  }
)

setMethod(
  f = "Project",
  signature = "missing",
  definition = function(entity){
    Project(list())
  }
)
