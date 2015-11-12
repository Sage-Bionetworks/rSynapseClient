# definition of Submission class
# 
# Author: brucehoff
###############################################################################

setClass(
  Class = "Submission",
  representation = representation(
    # fields:
    submissionContent = "SubmissionContent",
    # filePath: full path to local file.
    filePath = "character",
    # fileHandle (generated from JSON schema, empty before entity is created)
    fileHandle = "list",
    # objects to be serialized/deserialized
    objects = "environmentOrNull"
  ),
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    objects = NULL
  )
)

# this is the required constructor for a Submission, 
# taking a list of properties as its argument
# the logic is based on definedEntityConstructors in AAAschema
setMethod(
  f = "createSubmissionFromProperties",
  signature = signature("list"),
  definition = function(propertiesList) {
    submission <- new("Submission")
    submission@submissionContent<-createS4ObjectFromList(propertiesList, "SubmissionContent")
    fileHandle<-as.list(getFileHandleFromEntityBundleJSON(propertiesList$entityBundleJSON))
    submission@fileHandle<-fileHandle
    
    submission
  }
)

  setMethod(
    f = "$",
    signature = "Submission",
    definition = function(x, name){
      slot(x@submissionContent, name)
    }
  )
  
  setReplaceMethod(
    f = "$",
    signature = "Submission",
    definition = function(x, name, value) {
      slot(x@submissionContent, name)<-value
      x
    }
  )

  # for backwards compatibility
  setMethod(
    f = "propertyValue",
    signature = signature("Submission", "character"),
    definition = function(object, which){
      slot(object@submissionContent, which)
    }
  )
  
  # for backwards compatibility
  setReplaceMethod(
    f = "propertyValue",
    signature = signature("Submission", "character"),
    definition = function(object, which, value) {
      slot(object@submissionContent, which) <- value
      object
    }
  )  


  
getFileHandleFromEntityBundleJSON<-function(entityBundleJSON) {
  if (missing(entityBundleJSON) || is.null(entityBundleJSON)) return(list())
  entityBundle<-synFromJson(entityBundleJSON)
  fileHandles<-entityBundle$fileHandles
  if (is.null(fileHandles)) return(list()) # this is the case if the submitted entity is not a File
  if (length(fileHandles)<1) return(list())
  if (length(fileHandles)==1) {
    return(fileHandles[[1]])
  } else {
    for (fileHandle in fileHandles) {
      if (fileHandle$concreteType!="org.sagebionetworks.repo.model.file.PreviewFileHandle") {
        return(fileHandle)
      }
    }
  }
  stop("Unable to find file handle")
}

synGetSubmission<-function(id, downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=F) {
  submission<-createSubmissionFromProperties(synRestGET(sprintf("/evaluation/submission/%s", id)))
  
  if (!is.null(submission@fileHandle$id)) { # otherwise it's not a File
    downloadUri<-sprintf("/evaluation/submission/%s/file/%s?redirect=FALSE", submission$id, submission@fileHandle$id)

    filePath<-retrieveAttachedFileHandle(
      downloadUri,
      "REPO",
      submission@fileHandle,
      downloadFile,
      downloadLocation,
      ifcollision,
      load
    )
  
    # now construct Submission from 'result', which has filePath
    if (!is.null(filePath)) submission@filePath<-filePath
    if (load) {
      if (is.null(submission@objects)) submission@objects<-new.env(parent=emptyenv())
        # Note: the following only works if 'path' is a file system path, not a URL
      load(file=submission@filePath, envir = as.environment(submission@objects))
    } 
  }

  submission
}

setMethod(
  f = "synStore",
  signature = "SubmissionStatus",
  definition = function(entity) {
    # note, user can't create a SubmissionStatus, only update one
    updateS4Object(entity, sprintf("/evaluation/submission/%s/status",entity$id))   
  }
)

setMethod(
  f = "synDelete",
  signature = "Submission",
  definition = function(entity) {
    synRestDELETE(sprintf("/evaluation/submission/%s",entity$id))
  }
)

synCreateSubmission<-function(submission, entityEtag, submissionEligibilityHash) {
	if (missing(submissionEligibilityHash)) {
		uri<-sprintf("/evaluation/submission?etag=%s", entityEtag)
	} else {
		uri<-sprintf("/evaluation/submission?etag=%s&submissionEligibilityHash=%s", 
				entityEtag, submissionEligibilityHash)
	}
	requestBody<-createListFromS4Object(submission@submissionContent)
	response<-synRestPOST(uri, requestBody)
	createSubmissionFromProperties(response)
}

newSubmissionStatus<-function(content) {
  createS4ObjectFromList(content, "SubmissionStatus")
}

synGetSubmissionStatus<-function(submission) {
  if (class(submission)=="Submission") {
    id <-submission$id
  } else {
    id <- submission
  }
  newSubmissionStatus(synRestGET(sprintf("/evaluation/submission/%s/status", id)))
}

newSubmissionPaginatedResults<-function(content) {
  paginatedResults<-new("PaginatedResults")
  paginatedResults@totalNumberOfResults<-as.integer(content$totalNumberOfResults)
  for (s in content$results) {
    n<-length(paginatedResults@results)
    paginatedResults@results[[n+1]]<-createSubmissionFromProperties(as.list(s))
  }
  paginatedResults
}

synGetSubmissions<-function(evaluationId, status, limit, offset, myOwn=FALSE) {
  uri<-sprintf("/evaluation/%s/submission%s", evaluationId, if (myOwn) "" else "/all")
  
  params<-list()
  if (!missing(status)) params$status<-status  
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))
  
  newSubmissionPaginatedResults(synRestGET(uri))
}

setMethod(
  f = "getObject",
  signature = signature("Submission", "character"),
  definition = function(owner, which) {
    get(x=which, envir=owner@objects)
  }
)

setMethod(
  f = "getObject",
  signature = signature("Submission", "missing"),
  definition = function(owner) {
    getObjectMethod(owner)
  }
)

