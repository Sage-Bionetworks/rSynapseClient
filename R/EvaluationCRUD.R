# CRUD functions for Evaluations
# 
# Author: brucehoff
###############################################################################

synCreateEvaluation<-function(evaluation) {
  evaluationAsList<-createListFromS4Object(evaluation)
  result<-createS4ObjectFromList(synRestPOST("/evaluation", evaluationAsList), "Evaluation")
  result
}

synGetEvaluation<-function(id) {
  uri<-sprintf("/evaluation/%s", id)
  result<-createS4ObjectFromList(synRestGET(uri), "Evaluation")
  result
}

synGetEvaluationByContentSource <- function(id) {
    uri <- sprintf("/entity/%s/evaluation", id)
    content <- synRestGET(uri)
    
    paginatedResults <- new("PaginatedResults")
    paginatedResults@totalNumberOfResults <- as.integer(content$totalNumberOfResults)
    for (s in content$results) {
        result <- createS4ObjectFromList(as.list(s), "Evaluation")
        
        n <- length(paginatedResults@results)
        paginatedResults@results[[n+1]] <- result
    }
    return(paginatedResults)
}

setMethod(
  f = "synStore",
  signature = "Evaluation",
  definition = function(entity) {
    if (is.null(entity$id) || length(entity$id)==0) {
      synCreateEvaluation(entity)
    } else {
      updateS4Object(entity, sprintf("/evaluation/%s",entity$id))
    }
  }
)

setMethod(
  f = "synDelete",
  signature = "Evaluation",
  definition = function(entity) {
    synRestDELETE(sprintf("/evaluation/%s",entity$id))
  }
)


# convert a list into a string of URL parameters
listToURLParams<-function(x) {
  result<-NULL
  for (n in names(x)) result[length(result)+1]<-sprintf("%s=%s", n, x[[n]])
  paste(result, collapse="&")
}

# Experimental method! liable to change in future without notice
.allowParticipation <- function(evaluationId, userPrincipalId, 
        rights=c("READ", "PARTICIPATE", "SUBMIT", "UPDATE_SUBMISSION")) {
    # Treat integers as user IDs and strings as user groups
    userId <- suppressWarnings(as.integer(userPrincipalId))
    
    if (is.na(userId)) {
      stop(sprintf("Expected user's principal Id but found %s", userPrincipalId))
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

