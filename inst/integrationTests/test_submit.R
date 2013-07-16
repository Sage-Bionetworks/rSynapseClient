# Test submit (to evaluation)
# 
# Author: brucehoff
###############################################################################

.setUp <- function() {
  project <- createEntity(Project())
  projectId<-propertyValue(project, "id")
  synapseClient:::.setCache("testProject", project)
  
  evaluation<-Evaluation(name=sprintf("test_submit_%d", sample(10000,1)), status="OPEN", contentSource=projectId)
  evaluation<-synStore(evaluation)
  synapseClient:::.setCache("testEvaluation", evaluation)
}

.tearDown <- function() {
  evaluation<-synapseClient:::.getCache("testEvaluation")
  synDelete(evaluation)
  deleteEntity(synapseClient:::.getCache("testProject"))
}

integrationTest_submit <- function() {
  # create an entity
  project<-synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  file<-File(parentId=pid, name="foo")
  file<-addObject(file,c(1,2,3))
  file<-synStore(file)
  
  # join the evaluation
  myOwnId<-propertyValue(synGetUserProfile(), "ownerId")
  evaluation<-synapseClient:::.getCache("testEvaluation")
  eid<-propertyValue(evaluation, "id")
  synRestPOST(sprintf("/evaluation/%s/participant/%s", eid, myOwnId), list())
  
  # submit the entity
  submissionName<-"test-sub-name"
  teamName<-"test-team-name"
  missingTeamName<-try(submit(evaluation=evaluation, entity=file, submissionName=submissionName), silent=T)
  checkEquals("try-error", class(missingTeamName))
  submission<-submit(evaluation=evaluation, entity=file, submissionName=submissionName, teamName=teamName)
  checkEquals(propertyValue(file, "id"), propertyValue(submission, "entityId"))
  checkEquals(propertyValue(file, "versionNumber"), propertyValue(submission, "versionNumber"))
  checkEquals(eid, propertyValue(submission, "evaluationId"))
  checkEquals(submissionName, submission$name)
  checkEquals(teamName, submission$submitterAlias)
  
  # retrieve the submission
  submission2<-synGetSubmission(propertyValue(submission, "id"))
  checkEquals(submission, submission2)
  
  # delete the submission
  synDelete(submission)
  
  # rev the entity
  file<-addObject(file, c(4,5,6))
  file<-synStore(file)
  # changing the file automatically increments the version
  checkEquals(2, propertyValue(file, "versionNumber"))
  
  # now submit the old version
  oldFile<-synGet(propertyValue(file, "id"), version=1, downloadFile=F)
  checkEquals(1, propertyValue(oldFile, "versionNumber"))
  submission2<-submit(evaluation, oldFile, teamName=teamName)
  
  checkEquals(propertyValue(oldFile, "id"), propertyValue(submission2, "entityId"))
  checkEquals(propertyValue(oldFile, "versionNumber"), propertyValue(submission2, "versionNumber"))
  checkEquals(eid, propertyValue(submission2, "evaluationId"))
  checkEquals(propertyValue(oldFile, "name"), propertyValue(submission2, "name"))
  
  # retrieve the submission
  submission3<-synGetSubmission(propertyValue(submission2, "id"))
  checkEquals(submission2, submission3)
  
  # delete the submission
  synDelete(submission3)
  
  checkException(synGetSubmission(propertyValue(submission3, "id")))
}
  

