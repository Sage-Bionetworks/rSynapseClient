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

newParticipantPaginatedResults<-function(content) {
  paginatedResults<-new("PaginatedResults")
  paginatedResults@totalNumberOfResults<-as.integer(content$totalNumberOfResults)
  for (s in content$results) {
    n<-length(paginatedResults@results)
    paginatedResults@results[[n+1]]<-Participant(as.list(s))
  }
  paginatedResults
}


# convert a list into a string of URL parameters
listToURLParams<-function(x) {
  result<-NULL
  for (n in names(x)) result[length(result)+1]<-sprintf("%s=%s", n, x[[n]])
  paste(result, collapse="&")
}

synGetParticipants<-function(evaluationId,limit,offset) {
  uri<-sprintf("/evaluation/%s/participant", evaluationId)
  
  params<-list()
  if (!missing(limit)) params$limit<-limit  
  if (!missing(offset)) params$offset<-offset  
  if (length(params)>0) uri<-sprintf("%s?%s", uri, listToURLParams(params))
  
  newParticipantPaginatedResults(synRestGET(uri))
}

