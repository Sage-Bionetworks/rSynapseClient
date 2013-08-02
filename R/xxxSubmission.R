# definition of Submission class
# 
# Author: brucehoff
###############################################################################

setClass(
  Class = "Submission",
  contains = "SimplePropertyOwner",
  representation = representation(
    # fields:
    # uri for update and delete operations
    updateUri = "character",
    # filePath: full path to local file.
    filePath = "character",
    # fileHandle (generated from JSON schema, empty before entity is created)
    fileHandle = "list",
    # objects to be serialized/deserialized
    objects = "environmentOrNull"
  ),
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    properties = initializeProperties("org.sagebionetworks.evaluation.model.Submission"),
    objects = NULL
  )
)

# this is the required constructor for a Submission, 
# taking a list of properties as its argument
# the logic is based on definedEntityConstructors in AAAschema
setMethod(
  f = "SubmissionListConstructor",
  signature = signature("list"),
  definition = function(propertiesList) {
    submission <- new("Submission")
    for (prop in names(propertiesList))
      propertyValue(submission, prop)<-propertiesList[[prop]]
    if (!is.null(submission$id)) submission@updateUri<-sprintf("/evaluation/submission/%s", submission$id)
    fileHandle<-getFileHandleFromEntityBundleJSON(submission$entityBundleJSON)
    submission@fileHandle<-fileHandle
    
    submission
  }
)

getFileHandleFromEntityBundleJSON<-function(entityBundleJSON) {
  if (missing(entityBundleJSON) || is.null(entityBundleJSON)) return(list())
  entityBundle<-fromJSON(entityBundleJSON)
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
  submission<-SubmissionListConstructor(synRestGET(sprintf("/evaluation/submission/%s", id)))
  
  if (!is.null(submission@fileHandle$id)) { # otherwise it's not a File
    downloadUri<-sprintf("/evaluation/submission/%s/file/%s", submission$id, submission@fileHandle$id)

    result<-synGetFileAttachment(
      downloadUri,
      submission@fileHandle,
      downloadFile,
      downloadLocation,
      ifcollision,
      load
    )
  
    # now construct Submission from 'result', which has filePath
    if (!is.null(result$filePath)) submission@filePath<-result$filePath
    if (load) {
      if (is.null(submission@objects)) submission@objects<-new.env(parent=emptyenv())
        # Note: the following only works if 'path' is a file system path, not a URL
      load(file=submission@filePath, envir = as.environment(submission@objects))
    } 
  }

  submission
}

synCreateSubmission<-function(submission, entityEtag) {
  SubmissionListConstructor(synRestPOST(sprintf("/evaluation/submission?etag=%s", entityEtag), submission))
}

newSubmissionStatus<-function(content) {
  submissionStatus<-SubmissionStatus(content)
  submissionStatus@updateUri<-sprintf("/evaluation/submission/%s/status", content$id)
  submissionStatus
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
    paginatedResults@results[[n+1]]<-SubmissionListConstructor(as.list(s))
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

