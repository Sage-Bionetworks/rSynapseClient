# Entry point for synStore and synGet, dispatching to entities and non-entities based on type
# 
# Author: brucehoff
###############################################################################


synStore <- function(entity, activity=NULL, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T, isRestricted=F, contentType=NULL) {  
  if (is(entity, "Entity")) {
    if (is(entity, "Locationable")) {
      stop("For 'Locationable' entities you must use createEntity, storeEntity, or updateEntity.")
    }
    
    if (is.null(propertyValue(entity, "id")) && createOrUpdate) {
      entityAsList<-try(findExistingEntity(propertyValue(entity, "name"), propertyValue(entity, "parentId")), silent=TRUE)
      if (class(entityAsList)!='try-error') {
        # Found it!
        # This copies retrieved properties not overwritten by the given entity
        mergedProperties<-copyProperties(as.list.SimplePropertyOwner(entity), entityAsList)
        
        # This also includes ID, which turns a "create" into an "update"
        propertyValues(entity)<-mergedProperties
        if (class(entity)=="File") {
            entity@fileHandle<-getFileHandle(entity)
        }
        
        # But the create-or-update logic lies in the "create" operation
        # So the ID must be nullified before proceeding
        propertyValue(entity, "id") <- NULL
      }
    }
    
    if (class(entity)=="File") {
      entity<-synStoreFile(file=entity, createOrUpdate, forceVersion, contentType)
    }
    # Now save the metadata
    generatingActivity<-NULL
    if (!is.null(activity)) {
      generatingActivity<-activity
    } else if (!is.null(used) || !is.null(executed)) {
      generatingActivity<-Activity(name=activityName, description=activityDescription, used=used, executed=executed)
    } else if (entity@generatedByChanged) {
      # this takes care of the case in which generatedBy(entity)<- 
      # is called rather than specifying the activity in the synStore() parameters
      generatingActivity<-generatedBy(entity)
    }
    if (is.null(propertyValue(entity, "id"))) {
      storedEntity<-createEntityMethod(entity, generatingActivity, createOrUpdate, forceVersion)
    } else {
      storedEntity<-updateEntityMethod(entity, generatingActivity, forceVersion)
    }
    if (class(entity)=="File") {
      # now copy the class-specific fields into the newly created object
      if (fileHasFilePath(entity)) storedEntity@filePath <- entity@filePath
      storedEntity@synapseStore <- entity@synapseStore
      storedEntity@fileHandle <- entity@fileHandle
      storedEntity@objects <- entity@objects
      if (class(storedEntity)=="File" && isRestricted) {
        # check to see if access restriction(s) is/are in place already
        id<-propertyValue(storedEntity, "id")
        if (!.hasAccessRequirement(id)) {
          # nothing in place, so we create the restriction
          .createLockAccessRequirement(id)
        }
      }
    }
    storedEntity
  } else {
    synStoreNonEntityObject(entity)
  }
}

# we define these functions to allow mocking during testing
.hasAccessRequirement<-function(entityId) {
  currentAccessRequirements<-synRestGET(sprintf("/entity/%s/accessRequirement", entityId))
  currentAccessRequirements$totalNumberOfResults>0
}

.createLockAccessRequirement<-function(entityId) {
  synRestPOST(sprintf("/entity/%s/lockAccessRequirement", entityId), list())
}

synStoreNonEntityObject<-function(object) {
  if (is(object, "Evaluation")) {
    if (is.null(propertyValue(object, "id"))) {
      synCreateEvaluation(object)
    } else {
      synUpdate(object)
    }
  #} else if (is(object, "UserProfile")) { # SYNR-671 will restore this
    # note, user can't create a UserProfile, only update one
   # synUpdateUserProfile(object) # SYNR-671 will restore this
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
    if ((class(file)=="File")) {
      file<-synGetFile(file, downloadFile, downloadLocation, ifcollision, load)
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


