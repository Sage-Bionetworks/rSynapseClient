# Function for submitting an entity to an Evaluation
# 
# Author: brucehoff
###############################################################################

submit<-function(evaluation, entity, submissionName, teamName, silent=F) {
  if (missing(entity)) stop("entity is required.")
  if (is(entity, "Entity")) {
    entityId<-propertyValue(entity, "id")
    if (is.null(entityId)) stop("The entity provided does not have an ID.")
    entityVersion<-propertyValue(entity, "versionNumber")
    if (is.null(entityVersion)) entityVersion<-"1" # takes care of non-versioned entities
    etag<-propertyValue(entity, "etag")
    if (is.null(etag)) stop("Entity is missing etag.")
    # if it's an old version we have to retrieve the latest to get the etag
    if (etag=="00000000-0000-0000-0000-000000000000") {
      latestEntity<-synGet(entityId, downloadFile=F)
      etag<-propertyValue(latestEntity, "etag")
    }
  } else {
    stop("You must provide an entity.")
  }
  if (missing(evaluation)) stop ("evaluation is required.")
  if (is(evaluation, "Evaluation")) {
    evaluationId<-propertyValue(evaluation, "id")
    if (is.null(evaluationId)) stop("The Evaluation provided does not have an ID.")
  } else if (is(evaluation, "character")) {
    evaluationId<-evaluation
    evaluation <- synGetEvaluation(evaluationId)
  } else {
    stop("You must provide an evaluation or and evaluation ID.")
  }
  if (missing(submissionName)) submissionName<-propertyValue(entity, "name")
  
  # Check for unmet access requirements
  kService <- sprintf('/evaluation/%s/accessRequirementUnfulfilled', evaluationId)
  response <- synapseGet(uri=kService, anonymous=FALSE)
  if (response[['totalNumberOfResults']] > 0) {
    accessTerms <- lapply(response[['results']], function(x) sprintf("%s - %s", x[['accessType']], x[['termsOfUse']]))
    accessTerms <- paste(accessTerms, collapse="\n")
    stop(sprintf('You have unmet access requirements: \n%s', accessTerms))
  }

  if (missing(teamName)) {
    submission<-SubmissionListConstructor(list(evaluationId=evaluationId, 
        entityId=entityId, 
        versionNumber=entityVersion, 
        name=submissionName))
  } else {
    submission<-SubmissionListConstructor(list(evaluationId=evaluationId, 
        entityId=entityId, 
        versionNumber=entityVersion, 
        name=submissionName,
        submitterAlias=teamName))
  } 
  
  createdSubmission<-synCreateSubmission(submission, entityEtag=etag)
  if (!silent) message(evaluation$submissionReceiptMessage)
  list(submission=createdSubmission, submissionReceiptMessage=evaluation$submissionReceiptMessage)
}

