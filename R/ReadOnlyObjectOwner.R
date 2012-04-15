# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "getObject",
  signature = signature("ReadOnlyObjectOwner", "character"),
  definition = function(owner, which){
    owner$objects[[which]]
  }
)

#setMethod(
#  f = "addObject",
#  signature = signature("ReadOnlyObjectOwner", "ANY", "ANY", "ANY"),
#  definition = function(owner, object, name, unlist){
#    stop("Unable to add object. this entity's objects are read-only.")
#  }
#)


