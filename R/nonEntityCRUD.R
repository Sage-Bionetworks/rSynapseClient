# CRUD functions for non-entities
# 
# Author: brucehoff
###############################################################################
synStoreNonEntityObject<-function(object) {
  if (is(object, "Evaluation")) {
    if (is.null(propertyValue(object, "id"))) {
      synCreateEvaluation(object)
    } else {
      synUpdate(object)
    }
  } else if (is(object, "UserProfile")) {
    # note, user can't create a UserProfile, only update one
    synUpdate(object)
  } else if (is(object, "SubmissionStatus")) {
    # note, user can't create a SubmissionStatus, only update one
    synUpdate(object)   
  } else if (is(object, "WikiPage")) {
    if (is.null(propertyValue(object, "id"))) {
      synCreateWiki(object)
    } else {
      synUpdate(object)
    }
  } else {
    stop("%s is not supported.", class(object))
  }
}

synUpdate<-function(object) {
  objectConstructor <- getMethod(class("object"), signature = "list", where="synapseClient")
  listResult<-synRestPUT(object@updateUri, object)
  objectResult<-objectConstructor(listResult)
  result@updateUri<-object@updateUri
  result
}

synDelete<-function(object) {synRestDELETE(object@updateUri)}


synCreateEvaluation<-function(evaluation) {
  result<-Evaluation(synRestPOST("/evaluation", evaluation))
  result@updateUri<-sprintf("/evaluation/%s", propertyValue(result, "id"))
  result
}

synGetEvaluation<-function(id) {
  result<-Evaluation(synRestGET(sprintf("/evaluation/%s", id)))
}


## given a file on disk and a wiki, upload the file and update the wiki
#addFileToWiki<-function(filePath, wiki) {
#  # upload the file, get the file handle
#  fileHandle<-uploadAndAddToCacheMap(filePath)
#  fileIdList<-propertyValue(wiki, "attachmentFileHandleIds")
#  if (is.null(fileIdList)) {
#    fileIdList<-list(fileHandle$id)
#  } else {
#    fileIdList[[length(fileIdList)+1]]<-fileHandle$id
#  }
#  propertyValue(wiki, "attachmentFileHandleIds")<-fileIdList
#  synUpdate(wiki)
#}

synGetUserProfile<-function(id) {
  if (missing(id)) {
    getOrUpdateUri<-"/userProfile"
  } else {
    getOrUpdateUri<-sprintf("/userProfile/%s", id)
  }
  result<-UserProfile(synRestGET(getOrUpdateUri))
  result@updateUri<-getOrUpdateUri
  result
}

newSubmission<-function(content) {
  submission<-Submission(content)
  submission@updateUri<-spintf("/evaluation/submission/%s", content$id)
  submission
}

synGetSubmission<-function(id) {
  newSubmission(synRestGET(sprintf("/evaluation/submission/%s", id)))
}

synCreateSubmission<-function(submission, entityEtag) {
  newSubmission(synRestPOST(sprintf("/evaluation/submission?etag=%s", entityEtag), submission))
}

newSubmissionStatus<-function(content) {
  submissionStatus<-SubmissionStatus(content)
  submissionStatus@updateUri<-spintf("/evaluation/submission/%s/status", content$id)
  submissionStatus
}

synGetSubmissionStatus<-function(id) {
  newSubmissionStatus(synRestGET(sprintf("/evaluation/submission/%s/status", id)))
}

newSubmissionPaginatedResults<-function(content) {
  paginatedResults<-PaginatedResults()
  paginatedResults@totalNumberOfResults<-content$totalNumberOfResults
  for (s in content$results) {
    n<-length(paginatedResults@results)
    paginatedResults@results[[n+1]]<-newSubmission(s)
  }
  paginatedResults
}


newParticipantPaginatedResults<-function(content) {
  paginatedResults<-PaginatedResults()
  paginatedResults@totalNumberOfResults<-content$totalNumberOfResults
  for (s in content$results) {
    n<-length(paginatedResults@results)
    paginatedResults@results[[n+1]]<-Participant(s)
  }
  paginatedResults
}


# convert a list into a string of URL parameters
listToURLParams<-function(x) {
  result<-NULL
  for (n in names(x)) result[length(result)+1]<-sprintf("%s=%s", n, x[[n]])
  paste(result, collapse="&")
}

synGetOwnSubmissions<-function(evaluationId, limit, offset) {
  uri<-sprintf("/evaluation/%s/submission", evaluationId)
  
  params<-list()
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))

  newPaginatedResults(synRestGET(uri))
}

synGetSubmissions<-function(evaluationId, status, limit, offset) {
  uri<-sprintf("/evaluation/%s/submission/all", evaluationId)

  params<-list()
  if (!missing(status)) params$status<-status  
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))
  
  newPaginatedResults(synRestGET(uri))
}

synGetParticipants<-function(evaluationId,limit,offset) {
  uri<-sprintf("/evaluation/%s/participant", evaluationId)
  
  params<-list()
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))
  
  newPaginatedResults(synRestGET(uri))
}
