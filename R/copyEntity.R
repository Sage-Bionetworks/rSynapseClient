## Make a copy of an entity that is not attached to the origial.
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "copyEntity",
  signature = "LocationOwner",
  definition = function(entity){
    copy <- entity
    copy@location <- copyEntity(entity@location)
    copy
  }
)

setMethod(
  f = "copyEntity",
  signature = "Location",
  definition = function(entity){
    copy <- entity
    copy@objects <- new.env()
    for(key in objects(entity@objects))
      assign(key, get(key,envir=entity@objects), envir=copy@objects)
    copy
  }
)
