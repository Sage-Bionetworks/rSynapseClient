# Function for submitting an entity to an Evaluation
# 
# Author: brucehoff
###############################################################################

submit<-function(evaluation, entity, submissionName) {
  if (missing(entity)) stop("entity is required.")
  if (is(entity, "Entity")) {
    entityId<-propertyValue(entity, "id")
    if (is.null(entityId)) stop("The entity provided does not have an ID.")
    entityVersion<-propertyValue(entity, "versionNumber")
    if (is.null(entityVersion)) stop("Entity version is required.")
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
  } else {
    stop("You must provide an evaluation or and evaluation ID.")
  }
  if (missing(submissionName)) submissionName<-propertyValue(entity, "name")
  submission<-list(evaluationId=evaluationId, entityId=entityId, versionNumber=entityVersion, name=submissionName)
  listResult<-synRestPOST(sprintf("/evaluation/submission?etag=%s", etag), submission)
  Submission(listResult)
}

