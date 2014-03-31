# Test submit (to evaluation)
# 
# Author: brucehoff
###############################################################################

.setUp <- function() {
  project <- createEntity(Project())
  projectId<-propertyValue(project, "id")
  synapseClient:::.setCache("testProject", project)
  submissionReceiptMessage<-"Your submission has been received. Please check the leader board for your score."
  evaluation<-Evaluation(name=sprintf("test_submit_%d", sample(10000,1)), status="OPEN", contentSource=projectId, submissionReceiptMessage=submissionReceiptMessage)
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
  evaluation<-synapseClient:::.getCache("testEvaluation")
  eid<-propertyValue(evaluation, "id")
  PUBLIC_GROUP_PRINCIPAL_ID<-273949 # This is defined in org.sagebionetworks.repo.model.AuthorizationConstants
  synapseClient:::.allowParticipation(eid, PUBLIC_GROUP_PRINCIPAL_ID)
  synRestPOST(sprintf("/evaluation/%s/participant", eid), list())
  
  # submit the entity
  submissionName<-"test-sub-name"
  teamName<-"test-team-name"
  submissionResult<-submit(evaluation=evaluation, entity=file, submissionName=submissionName, teamName=teamName, silent=T)
  submission<-submissionResult$submission
  submissionReceiptMessage<-"Your submission has been received. Please check the leader board for your score." # duplicates def'n above
  checkEquals(submissionReceiptMessage, submissionResult$submissionReceiptMessage)
  checkEquals(propertyValue(file, "id"), propertyValue(submission, "entityId"))
  checkEquals(propertyValue(file, "versionNumber"), propertyValue(submission, "versionNumber"))
  checkEquals(eid, propertyValue(submission, "evaluationId"))
  checkEquals(submissionName, submission$name)
  checkEquals(teamName, submission$submitterAlias)
  
  # retrieve the submission
  # first, get rid of the local copy
  origChecksum<- as.character(tools::md5sum(getFileLocation(file)))
  unlink(getFileLocation(file))
  unlink(synapseClient:::cacheMapFilePath(file@fileHandle$id))
  
  submission2<-synGetSubmission(propertyValue(submission, "id"))
  # make sure they're the same (except the download file path)
  submission2MinusFilePath<-submission2
  submission2MinusFilePath@filePath<-character(0)
  checkEquals(submission, submission2MinusFilePath)
  
  # check that the file was downloaded
  checkTrue(!is.null(getFileLocation(submission2)))
  checkTrue(!is.null(submission2@fileHandle))
  # check that the file content is correct!
  submissionCheckSum<-as.character(tools::md5sum(getFileLocation(submission2)))
  checkEquals(origChecksum, submissionCheckSum)
  
  checkEquals(0, length(listObjects(submission2)))
  
  # now download with load=T
  submission2<-synGetSubmission(submission$id, load=T)
  checkTrue(!is.null(getFileLocation(submission2)))
  checkTrue(!is.null(submission2@fileHandle))
  checkEquals(1, length(listObjects(submission2)))
  checkEquals(c(1,2,3), getObject(submission2))
  
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
  submissionResult2<-submit(evaluation, oldFile, teamName=teamName, silent=T)
  submission2<-submissionResult2$submission
  
  checkEquals(propertyValue(oldFile, "id"), propertyValue(submission2, "entityId"))
  checkEquals(propertyValue(oldFile, "versionNumber"), propertyValue(submission2, "versionNumber"))
  checkEquals(eid, propertyValue(submission2, "evaluationId"))
  checkEquals(propertyValue(oldFile, "name"), propertyValue(submission2, "name"))
  
  # retrieve the submission
  submission3<-synGetSubmission(propertyValue(submission2, "id"))
  submission3MinusFilePath<-submission3
  submission3MinusFilePath@filePath<-character(0)
  checkEquals(submission2, submission3MinusFilePath)
  
  # delete the submission
  synDelete(submission3)
  
  checkException(synGetSubmission(propertyValue(submission3, "id")))
}
  
integrationTest_submit_noTeamName <- function() {
  # create an entity
  project<-synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  file<-File(parentId=pid, name="foo")
  file<-addObject(file,c(1,2,3))
  file<-synStore(file)
  
  # join the evaluation
  evaluation<-synapseClient:::.getCache("testEvaluation")
  eid<-propertyValue(evaluation, "id")
  PUBLIC_GROUP_PRINCIPAL_ID<-273949 # This is defined in org.sagebionetworks.repo.model.AuthorizationConstants
  synapseClient:::.allowParticipation(eid, PUBLIC_GROUP_PRINCIPAL_ID)
  synRestPOST(sprintf("/evaluation/%s/participant", eid), list())
  
  # submit the entity
  submissionName<-"test-sub-name"
  submissionResult<-submit(evaluation=evaluation, entity=file, submissionName=submissionName, silent=T)
  submission<-submissionResult$submission
  submissionReceiptMessage<-"Your submission has been received. Please check the leader board for your score." # duplicates def'n above
  checkEquals(submissionReceiptMessage, submissionResult$submissionReceiptMessage)
  checkEquals(propertyValue(file, "id"), propertyValue(submission, "entityId"))
  checkEquals(propertyValue(file, "versionNumber"), propertyValue(submission, "versionNumber"))
  checkEquals(eid, propertyValue(submission, "evaluationId"))
  checkEquals(submissionName, submission$name)
  
}

# this is the test for SYNR-613
integrationTest_externalURL <- function() {
  # create an entity
  project<-synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  file<-File(path="http://www.google.com", parentId=pid, name="foo", synapseStore=FALSE)
  file<-synStore(file)
  
  # join the evaluation
  evaluation<-synapseClient:::.getCache("testEvaluation")
  eid<-propertyValue(evaluation, "id")
  PUBLIC_GROUP_PRINCIPAL_ID<-273949 # This is defined in org.sagebionetworks.repo.model.AuthorizationConstants
  synapseClient:::.allowParticipation(eid, PUBLIC_GROUP_PRINCIPAL_ID)
  synRestPOST(sprintf("/evaluation/%s/participant", eid), list())
  
  # submit the entity
  submissionName<-"test-sub-name"
  submissionResult<-submit(evaluation=evaluation, entity=file, submissionName=submissionName, silent=T)
  
  submission<-submissionResult$submission
  submissionReceiptMessage<-"Your submission has been received. Please check the leader board for your score." # duplicates def'n above
  checkEquals(submissionReceiptMessage, submissionResult$submissionReceiptMessage)
  
  # In SYNR-613 the following breaks due to submitting an external URL
  retrievedSubmission<-synGetSubmission(submission$id)
  
  checkEquals(propertyValue(file, "id"), propertyValue(retrievedSubmission, "entityId"))
  checkEquals(propertyValue(file, "versionNumber"), propertyValue(retrievedSubmission, "versionNumber"))
  checkEquals(eid, propertyValue(retrievedSubmission, "evaluationId"))
  checkEquals(submissionName, retrievedSubmission$name)
  
}
