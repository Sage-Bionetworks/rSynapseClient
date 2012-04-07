## Definition, Methods and Constructors for the Dataset S4 object, which represents
## a Synapse Dataset Entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "Dataset",
  signature = signature("list"),
  definition = function(entity){
    ## call the superclass constructor
    dataset <- SynapseEntity(entity=entity)
    
    ## coerce to Dataset
    class(dataset) <- "Dataset"
    synapseEntityKind(dataset) <- synapseEntityKind(new(Class="Dataset"))
    dataset
  }
)

setMethod(
  f = "Dataset",
  signature = "missing",
  definition = function(entity){
    Dataset(list())
  }
)
