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

synGetEvaluationByProject <- function(id) {
    uri <- sprintf("/evaluation/project/%s", id)
    content <- synRestGET(uri)
    
    paginatedResults <- new("PaginatedResults")
    paginatedResults@totalNumberOfResults <- as.integer(content$totalNumberOfResults)
    for (s in content$results) {
        result <- Evaluation(as.list(s))
        result@updateUri <- sprintf("/evaluation/%s", propertyValue(result, "id"))
        
        n <- length(paginatedResults@results)
        paginatedResults@results[[n+1]] <- result
    }
    return(paginatedResults)
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

# Experimental method! liable to change in future without notice
.allowParticipation <- function(evaluationId, user, 
        rights=c("READ", "PARTICIPATE", "SUBMIT", "UPDATE_SUBMISSION")) {
    # Treat integers as user IDs and strings as user groups
    userId <- suppressWarnings(as.integer(user))
    
    # Fetch the user ID for a user group
    if (is.na(userId)) {
        groups <- synRestGET(sprintf('/userGroupHeaders?prefix=%s', user))
        for (child in groups$children) {
            if (all(!child$isIndividual && child$displayName == user)) {
                userId <- as.integer(child$ownerId)
            }
        }
    }
    
    # Grab the ACL 
    acl <<- synRestGET(sprintf('/evaluation/%s/acl', evaluationId))
    acl$resourceAccess[[length(acl$resourceAccess) + 1]] <- list(accessType=rights, principalId=userId)
    
    # The JSON dumper may convert IDs into floating point (i.e. 1234.5e+06)
    for (access in 1:length(acl$resourceAccess)) {
        acl$resourceAccess[[access]]$principalId <- as.character(acl$resourceAccess[[access]]$principalId)
    }
    synRestPUT('/evaluation/acl', acl)
}

