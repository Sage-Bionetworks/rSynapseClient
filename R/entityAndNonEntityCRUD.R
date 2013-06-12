# Entry point for synStore and synGet, dispatching to entities and non-entities based on type
# 
# Author: brucehoff
###############################################################################


synStore <- function(entity, activity=NULL, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T) {  
  if (is(entity, "Entity")) {
    if (class(entity)=="File" || class(entity)=="Record") {
      entity<-synStoreFile(file=entity, createOrUpdate, forceVersion)
      # TODO: Handle Record
    }
    # Now save the metadata
    if (!is.null(activity)) {
      generatedBy(entity)<-activity
    } else if (!is.null(used) || !is.null(executed)) {
      activity<-Activity(list(name=activityName, description=activityDescription))
      usedAndExecuted<-list()
      if (!is.null(used)) {
        if (!is(used, "list")) used<-list(used)
        usedAndExecuted<-c(usedAndExecuted, lapply(X=used, FUN=usedListEntry, wasExecuted=F))
      }
      if (!is.null(executed)) {
        if (!is(executed, "list")) executed<-list(executed)
        usedAndExecuted<-c(usedAndExecuted, lapply(X=executed, FUN=usedListEntry, wasExecuted=T))
      }
      if (length(usedAndExecuted)>0) propertyValue(activity, "used") <- usedAndExecuted
      generatedBy(entity)<-activity
    }
    if (is.null(propertyValue(entity, "id"))) {
      storedEntity<-createEntityMethod(entity, createOrUpdate, forceVersion)
    } else {
      storedEntity<-updateEntityMethod(entity, forceVersion)
    }
    if (class(entity)=="File" || class(entity)=="Record") {
      # now copy the class-specific fields into the newly created object
      if (fileHasFilePath(entity)) storedEntity@filePath <- entity@filePath
      storedEntity@synapseStore <- entity@synapseStore
      storedEntity@fileHandle <- entity@fileHandle
      storedEntity@objects <- entity@objects
    }
    storedEntity
  } else {
    synStoreNonEntityObject(entity)
  }
}

synStoreNonEntityObject<-function(object) {
  if (is(object, "Evaluation")) {
    if (is.null(propertyValue(object, "id"))) {
      synCreateEvaluation(object)
    } else {
      synUpdate(object)
    }
  } else if (is(object, "UserProfile")) {
    # note, user can't create a UserProfile, only update one
    synUpdateUserProfile(object)
  } else if (is(object, "SubmissionStatus")) {
    # note, user can't create a SubmissionStatus, only update one
    synUpdate(object)   
  } else if (is(object, "WikiPage")) {
    if (is.null(propertyValue(object, "id"))) {
      synCreateWiki(object)
    } else {
      synUpdateWiki(object)
    }
  } else if (is(object, "Activity")) {
    storeEntity(object)
  } else {
    stop("%s is not supported.", class(object))
  }
}

synUpdate<-function(object) {
  objectConstructor <- getMethod(class(object), signature = "list", where="synapseClient")
  listResult<-synRestPUT(object@updateUri, object)
  objectResult<-objectConstructor(listResult)
  objectResult@updateUri<-object@updateUri
  objectResult
}

synDelete<-function(object) {
  if (is(object, "Entity") || is(object, "Activity")) {
    deleteEntity(object)
  } else if (is(object, "Evaluation") || is(object, "WikiPage") || is(object, "Submission")){
    synRestDELETE(object@updateUri)
  } else {
    stop(sprintf("%s is not supported.", class(object)))
  }
}


synGet<-function(id, version=NULL, downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=F) {
  if (isSynapseId(id)) {
    if (is.null(version)) {
      file<-getEntity(id)
    } else {
      file<-getEntity(id, version=version)
    }   
    if ((class(file)=="File" || class(file)=="Record")) {
      file<-synGetFile(file, downloadFile, downloadLocation, ifcollision, load)
      # TODO: Handle Record
    } else {
      file
    }
  } else {
    stop(sprintf("%s is not a Synapse ID.", id))
  }
}


