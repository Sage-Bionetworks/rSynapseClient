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
    if (is(used, "vector")) {
      used <- as.list(used)
    }
    if (!is(used, "list")) {
      used <- list(used)
    }
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
  signature = signature("Activity", "character"),
  definition = function(entity, value) {
    setUsedMethod(entity, value, FALSE)
  }

)

setMethod(
  f = "used<-",
  signature = signature("Activity", "Entity"),
  definition = function(entity, value) {
    setUsedMethod(entity, list(value), FALSE)
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
  signature = signature("Activity", "character"),
  definition = function(entity, value) {
    setUsedMethod(entity, value, TRUE)
  }

)

setMethod(
  f = "executed<-",
  signature = signature("Activity", "Entity"),
  definition = function(entity, value) {
    setUsedMethod(entity, list(value), TRUE)
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
    Activity(synFromJson(activity))
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
    if( length(properties(object)$used) > 0 ){
      cat("USED:\n")
      for( i in 1:length(properties(object)$used) ){
        thisUsed <- properties(object)$used[[i]]
        if( thisUsed$concreteType == "org.sagebionetworks.repo.model.provenance.UsedEntity" ){
          cat("id          : ", thisUsed$reference$targetId, "\n", sep="")
          cat("version     : ", thisUsed$reference$targetVersionNumber, "\n", sep="")
          cat("wasExecuted : ", thisUsed$wasExecuted, "\n\n", sep="")
        } else if( thisUsed$concreteType == "org.sagebionetworks.repo.model.provenance.UsedURL" ){
					cat("url         : ", thisUsed$url, "\n", sep="")
					cat("name        : ", thisUsed$name, "\n", sep="")
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
    if(!is.null(activity$id))
      synapseDelete(.generateActivityUri(activity$id))
    
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
    ee<-Activity(synapseGet(.generateActivityUri(activity$id)))
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
  f = "synStore",
  signature = "Activity",
  definition = function(entity) {
    storeEntity(entity)
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
    if(is.null(activity$id)) {
      stop("Cannot update an Activity which has no ID.")
    } else {
      uri <- .generateActivityUri(activity$id)
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
    if(is.null(activity$id)) {
      activity <- createEntity(activity)
    } else {
      activity <- updateEntity(activity)
    }
    activity
  }
)

setMethod(
  f = "synDelete",
  signature = "Activity",
  definition = function(entity) {
    deleteEntity(entity)
  }
)

setMethod(
  f = "$",
  signature = "Activity",
  definition = function(x, name){
    if (name=="used") {
      used(x)
    } else if (name=="executed") {
      executed(x) 
    } else if (any(name==propertyNames(x))) {
      propertyValue(x, name)
    } else {
      stop(sprintf("invalid name %s", name))
    }
  }
)

setReplaceMethod("$", 
  signature = "Activity",
  definition = function(x, name, value) {
    if (name=="used") {
      used(x)<-value
      x
    } else if (name=="executed") {
      executed(x) <-value
      x
    } else if (any(name==propertyNames(x))) {
      propertyValue(x, name)<-value
      x
    } else {
      stop(sprintf("invalid name %s", name))
    }
  }
)



