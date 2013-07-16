# Entry point for synStore and synGet, dispatching to entities and non-entities based on type
# 
# Author: brucehoff
###############################################################################


synStore <- function(entity, activity=NULL, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T, isRestricted=F) {  
  if (is(entity, "Entity")) {
    if (is(entity, "Locationable")) stop("For 'Locationable' entities you must use createEntity, storeEntity, or updateEntity.")
    if (class(entity)=="File" || class(entity)=="Record") {
      entity<-synStoreFile(file=entity, createOrUpdate, forceVersion, isRestricted)
      # TODO: Handle Record
    }
    # Now save the metadata
    if (!is.null(activity)) {
      generatedBy(entity)<-activity
    } else if (!is.null(used) || !is.null(executed)) {
      activity<-Activity(name=activityName, description=activityDescription, used=used, executed=executed)
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
    stop(sprintf("%s is not supported.", class(object)))
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
  if (isSynapseId(object) || is(object, "Entity") || is(object, "Activity")) {
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
      if (is (file, "Locationable") && downloadFile) {
        if (!is.null(downloadLocation)) {
          warning("Cannot specify download location for 'Locationable' entities")
        }
        if (load) {
          loadEntity(file)
        } else {
          downloadEntity(file)
        }
      } else {
        file
      }
    }
  } else {
    stop(sprintf("%s is not a Synapse ID.", id))
  }
}


