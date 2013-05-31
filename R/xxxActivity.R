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
    # we 'intercept the 'used' and 'executed' fields and process them specially
    entity$used<-combineUsedAndExecutedLists(entity$used, entity$executed)
    entity$executed<-NULL
    activity<-do.call("Activity", list(entity))
    activity
  }
)


combineUsedAndExecutedLists<-function(used, executed) {
  usedAndExecuted<-list()
  if (!missing(used) && !is.null(used)) {
    if (!is(used, "list")) used<-list(used)
    usedAndExecuted<-c(usedAndExecuted, lapply(X=used, FUN=usedListEntry, wasExecuted=F))
  }
  if (!missing(executed) && !is.null(executed)) {
    if (!is(executed, "list")) executed<-list(executed)
    usedAndExecuted<-c(usedAndExecuted, lapply(X=executed, FUN=usedListEntry, wasExecuted=T))
  }
  usedAndExecuted
}

setUsedMethod<-function(activity, referenceList, wasExecuted) {
  if (is.null(referenceList)) {
    usedList<-NULL
  } else {
    usedList <- lapply(X=referenceList, FUN=usedListEntry, wasExecuted=wasExecuted)
  }
  # combine the new 'used' list with the old 'executed' list
  usedList <- c(usedList, selectUsed(activity, wasExecuted=(!wasExecuted)))
  propertyValue(activity, "used") <- usedList
  activity
}

setMethod(
  f = "used<-",
  signature = signature("Activity", "list"),
  definition = function(entity, value) {
    setUsedMethod(entity, value, FALSE)
  }

)

setMethod(
  f = "used<-",
  signature = signature("Activity", "NULL"),
  definition = function(entity, value) {
    setUsedMethod(entity, NULL, FALSE)
  }
)

setMethod(
  f = "used",
  signature = signature("Activity"),
  definition = function(entity) {
    selectUsed(entity, FALSE)
  }
)

setMethod(
  f = "executed<-",
  signature = signature("Activity", "list"),
  definition = function(entity, value) {
    setUsedMethod(entity, value, TRUE)
  }

)

setMethod(
  f = "executed<-",
  signature = signature("Activity", "NULL"),
  definition = function(entity, value) {
    setUsedMethod(entity, NULL, TRUE)
  }
)

setMethod(
  f = "executed",
  signature = signature("Activity"),
  definition = function(entity) {
    selectUsed(entity, TRUE)
  }
)


# given the 'used' list from an Activity, return just the ones which match 
# the logical 'wasExecuted'
selectUsed<-function(activity, wasExecuted) {
  ans <- list()
  for (entry in propertyValue(activity, "used")) {
    entryWasExecuted <- entry$wasExecuted
    if (is.null(entryWasExecuted)) stop(sprintf("'wasExecuted' is not specified for a 'used' item in Activity %s", propertyValue(activity, "name")))
    if (entryWasExecuted==wasExecuted) ans[[length(ans)+1]]<-entry
  }
  ans
}



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
  f = "show",
  signature = signature("Activity"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    
    cat("Activity Name : ", properties(object)$name, "\n", sep="")
    cat("Activity Id   : ", properties(object)$id, "\n", sep="")
    cat("Description   : ", properties(object)$description, "\n", sep="")
    cat("--------------------\n")
    
    ## DISPLAY USED LIST
    if( !is.null(properties(object)$used) ){
      cat("USED:\n")
      for( i in 1:length(properties(object)$used) ){
        thisUsed <- object$properties$used[[i]]
        if( thisUsed$concreteType == "org.sagebionetworks.repo.model.provenance.UsedEntity" ){
          cat("id          : ", thisUsed$reference$targetId, "\n", sep="")
          cat("version     : ", thisUsed$reference$targetVersionNumber, "\n", sep="")
          cat("wasExecuted : ", thisUsed$wasExecuted, "\n\n", sep="")
        } else if( thisUsed$concreteType == "org.sagebionetworks.repo.model.provenance.UsedURL" ){
          cat("url         : ", thisUsed$url, "\n", sep="")
          cat("wasExecuted : ", thisUsed$wasExecuted, "\n\n", sep="")
        }
      }
    }
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
  f = "synGetActivity",
  signature = signature("character", "missing"),
  definition = function(entity){
    getGeneratedBy(synGet(entity, downloadFile=FALSE))
  }
)

setMethod(
  f = "synGetActivity",
  signature = signature("Entity", "missing"),
  definition = function(entity){
    getGeneratedBy(entity)
  }
)

setMethod(
  f = "synGetActivity",
  signature = signature("character", "character"),
  definition = function(entity, version){
    getGeneratedBy(synGet(entity, version=version, downloadFile=FALSE))
  }
)

setMethod(
  f = "synSetActivity<-",
  signature=signature("Entity", "Activity"),
  definition = function(entity, activity) {
    generatedBy(entity)<-activity
  })

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



