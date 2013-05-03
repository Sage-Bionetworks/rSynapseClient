# Test submit (to evaluation)
# 
# Author: brucehoff
###############################################################################

.setUp <- function() {
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  
  evaluation<-Evaluation(name=sprintf("test_submit_%d", sample(100,1)), status="OPEN", contentSource="")
  evaluation<-Evaluation(synRestPOST("/evaluation", evaluation))
  synapseClient:::.setCache("testEvaluation", evaluation)
}

.tearDown <- function() {
  deleteEntity(synapseClient:::.getCache("testProject"))
  
  evaluation<-synapseClient:::.getCache("testEvaluation")
  synRestDELETE(sprintf("/evaluation/%s", propertyValue(evaluation, "id")))
}

integrationTest_submit <- function() {
  # create an entity
  project<-synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  file<-File(parentId=pid, name="foo")
  file<-addObject(file,c(1,2,3))
  file<-synStore(file)
  
  # join the evaluation
  myOwnId<-propertyValue(UserProfile(synRestGET("/userProfile")), "ownerId")
  evaluation<-synapseClient:::.getCache("testEvaluation")
  eid<-propertyValue(evaluation, "id")
  synRestPOST(sprintf("/evaluation/%s/participant/%s", eid, myOwnId), list())
  
  # submit the entity
  submission<-submit(evaluation, file)
  checkEquals(propertyValue(file, "id"), propertyValue(submission, "entityId"))
  checkEquals(propertyValue(file, "versionNumber"), propertyValue(submission, "versionNumber"))
  checkEquals(eid, propertyValue(submission, "evaluationId"))
  checkEquals(propertyValue(file, "name"), propertyValue(submission, "name"))
  
  # retrieve the submission
  submissionURI<-sprintf("/evaluation/submission/%s", propertyValue(submission, "id"))
  submission2<-Submission(synRestGET(submissionURI))
  checkEquals(submission, submission2)
  
  # delete the submission
  synRestDELETE(submissionURI)
}
  

