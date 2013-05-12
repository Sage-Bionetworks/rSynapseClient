# CRUD functions for Evaluations
# 
# Author: brucehoff
###############################################################################

synCreateEvaluation<-function(evaluation) {
  result<-Evaluation(synRestPOST("/evaluation", evaluation))
  result@updateUri<-sprintf("/evaluation/%s", propertyValue(result, "id"))
  result
}

synGetEvaluation<-function(id) {
  uri<-sprintf("/evaluation/%s", id)
  result<-Evaluation(synRestGET(uri))
  result@updateUri<-uri
  result
}


newSubmission<-function(content) {
  submission<-Submission(content)
  submission@updateUri<-sprintf("/evaluation/submission/%s", content$id)
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
  submissionStatus@updateUri<-sprintf("/evaluation/submission/%s/status", content$id)
  submissionStatus
}

synGetSubmissionStatus<-function(id) {
  newSubmissionStatus(synRestGET(sprintf("/evaluation/submission/%s/status", id)))
}

newSubmissionPaginatedResults<-function(content) {
  paginatedResults<-new("PaginatedResults")
  paginatedResults@totalNumberOfResults<-as.integer(content$totalNumberOfResults)
  for (s in content$results) {
    n<-length(paginatedResults@results)
    paginatedResults@results[[n+1]]<-newSubmission(s)
  }
  paginatedResults
}


newParticipantPaginatedResults<-function(content) {
  paginatedResults<-new("PaginatedResults")
  paginatedResults@totalNumberOfResults<-as.integer(content$totalNumberOfResults)
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

  newSubmissionPaginatedResults(synRestGET(uri))
}

synGetSubmissions<-function(evaluationId, status, limit, offset) {
  uri<-sprintf("/evaluation/%s/submission/all", evaluationId)

  params<-list()
  if (!missing(status)) params$status<-status  
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))
  
  newSubmissionPaginatedResults(synRestGET(uri))
}

synGetParticipants<-function(evaluationId,limit,offset) {
  uri<-sprintf("/evaluation/%s/participant", evaluationId)
  
  params<-list()
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))
  
  newParticipantPaginatedResults(synRestGET(uri))
}

