###############################################################################
# methods for Activity class
# Note, this class ought to be named 'Activity' but it must come
# after 'AllGenerics' alphabetically
# 
# Author: bhoff
###############################################################################

#####
## constructor that takes a list argument
#####
setMethod(
  f = "Activity",
  signature = signature("list"),
  definition = function(activity){
    ee <- new("Activity")
    usedEntitiesReferencesOrURLs <- activity$used
    if (!is.null(usedEntitiesReferencesOrURLs)) {
      usedReferences<-lapply(usedEntitiesReferencesOrURLs, usedListEntry)
      activity$used<-usedReferences
    }
    for(prop in names(activity))
      propertyValue(ee, prop) <- activity[[prop]]
    ee
  }
)

setMethod(
  f = "Activity",
  signature = "missing",
  definition = function(...){
    ## GRAB NAMED ARGUMENTS AND ADD TO ENTITY LIST
    entity <- list(...)
    do.call("Activity", list(entity))
  }
)

#####
## constructor that takes a serialized JSON object
#####
setMethod(
  f = "Activity",
  signature = signature("character"),
  definition = function(activity){
    Activity(fromJSON(activity))
  }
)

#####
## Activity "show" method
#####
setMethod(
  f = "showEntity",
  signature = signature("Activity"),
  definition = function(activity){
    cat('An object of class "', class(activity), '"\n', sep="")
    
    if (!is.null(properties(object)$name))
      cat("Activity Name : ", properties(object)$name, "\n", sep="")
    if (!is.null(properties(object)$id))
      cat("Activity Id : ", properties(object)$id, "\n", sep="")
    if (!is.null(properties(object)$description))
      cat("Activity Description  : ", properties(object)$id, "\n", sep="")
  }
)

setMethod(
  f = "deleteEntity",
  signature = "Activity",
  definition = function(entity){
    activity<-entity
    envir <- parent.frame(2)
    inherits <- FALSE
    name <- deparse(substitute(activity, env=parent.frame()))
    
    ## delete the activity in synapse
    if(!is.null(activity$properties$id))
      synapseDelete(.generateActivityUri(activity$properties$id))
    
    ## remove the activity from the parent environment
    if(any(grepl(name,ls(envir=envir))))
      remove(list = name, envir=envir, inherits=inherits)
    
    ## strip out the system controlled properties and invisibly
    ## return the activity
    activity <- deleteProperty(activity, "id")
    activity <- deleteProperty(activity, "etag")
    invisible(activity)
  }
)

setMethod(
  f = "getEntity",
  signature = signature("Activity"),
  definition = function(entity){
    activity<-entity
    ee<-Activity(synapseGet(.generateActivityUri(activity$properties$id)))
    ee
  }
)

setMethod(
  f = "getActivity",
  signature = signature("character"),
  definition = function(activity){
    ee<-Activity(synapseGet(.generateActivityUri(activity)))
    ee
  }
)


setMethod(
  f = "createEntity",
  signature = "Activity",
  definition = function(entity)
  {
    activity<-entity
    activity <- as.list.SimplePropertyOwner(activity)
    activity <- Activity(synapsePost("/activity", activity))
    activity
  }
)

setMethod(
  f = "updateEntity",
  signature = "Activity",
  definition = function(entity)
  {
    activity<-entity
    if(is.null(activity$properties$id)) {
      stop("Cannot update an Activity which has no ID.")
    } else {
      uri <- .generateActivityUri(activity$properties$id)
      activity <- as.list.SimplePropertyOwner(activity)
      activity <- Activity(synapsePut(uri, activity))
    }
    activity
  }
)

setMethod(
  f = "storeEntity",
  signature = "Activity",
  definition = function(entity)
  {
    activity<-entity
    if(is.null(activity$properties$id)) {
      activity <- createEntity(activity)
    } else {
      activity <- updateEntity(activity)
    }
    activity
  }
)



#####
## convert the S4 activity to a list activity
#####
setMethod(
  f = ".extractEntityFromSlots",
  signature = "Activity",
  definition = function(object){
    properties(object)
  }
)

#####
## convert the list activity to an S4 activity
#####
setMethod(
  f = ".populateSlotsFromActivity",
  signature = signature("Activity", "list"),
  definition = function(object, activity){
    if(any(names(activity) == "") && length(activity) > 0)
      stop("All elements of the activity must be named")
    
    ## all activity fields should be stored as properties
    for(name in names(activity))
      propertyValue(object, name) <- activity[[name]]
    object
  }
)


names.Activity <-
  function(x)
{
  c("properties")
}

setMethod(
  f = "[",
  signature = "Activity",
  definition = function(x, i, j, ...){
    if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
      stop("incorrect number of subscripts")
    if(is.numeric(i)){
      if(any(i > length(names(x))))
        stop("subscript out of bounds")
      i <- names(x)[i]
    }else if(is.character(i)){
      if(!all(i %in% names(x)))
        stop("undefined objects selected")
    }else{
      stop(sprintf("invalid subscript type '%s'", class(i)))
    }
    retVal <- lapply(i, function(i){
        if(i == "attachDir"){
          retVal <- attachDir(x)
        }else if(i == "attachments"){
          attachments(x)
        } else if(i == "available.versions"){
          if(is.null(x$properties$id)){
            retVal <- NULL
          }else{
            retVal <- available.versions(x$properties$id)
          }
        } else if(i %in% names(x)){
          retVal <- slot(x, i)
        }else{
          retVal <- NULL
        }
      }
    )
    names(retVal) <- i
    retVal
  }
)

setMethod(
  f = "[[",
  signature = "Activity",
  definition = function(x, i, j, ...){
    if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
      stop("incorrect number of subscripts")
    if(length(i) > 1)
      stop("subscript out of bounds")
    x[i][[1]]
  }
)

setMethod(
  f = "$",
  signature = "Activity",
  definition = function(x, name){
    x[[name]]
  }
)

setReplaceMethod("$", 
  signature = "Activity",
  definition = function(x, name, value) {
    if(!(name %in% names(x)))
      stop("invalid element")
    slot(x, name) <- value
    x
  }
)



