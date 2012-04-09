# TODO: Add comment
# 
# Author: furia
###############################################################################


## Generic method for addObject
setGeneric(
  name = "addObject",
  def = function(owner, object, name, unlist){
    standardGeneric("addObject")
  }
)

## generic method for deleteObject
setGeneric(
  name = "deleteObject",
  def = function(owner, which){
    standardGeneric("deleteObject")
  }
)

## Generic method for renameObject
setGeneric(
  name = "renameObject",
  def = function(owner, which, name){
    standardGeneric("renameObject")
  }
)

## Generic method for getObject
setGeneric(
  name = "getObject",
  def = function(owner, which){
    standardGeneric("getObject")
  }
)