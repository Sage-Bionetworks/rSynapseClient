### Test CRUD operations for S4 Activity objects
### 
### Author: Bruce Hoff
################################################################################ 

.setUp <-
		function()
{
	### create a project
	project <- createEntity(Project())
	synapseClient:::.setCache("testProject", project)

	## Create Data
	data <- Data(list(name="test data", parentId=propertyValue(project, "id")))
	createdData <- createEntity(data)
	synapseClient:::.setCache("testData", createdData)
}

.tearDown <- 
  function()
{
	if(!is.null(synapseClient:::.getCache("testActivity"))) {
		try(deleteEntity(synapseClient:::.getCache("testActivity")))
		synapseClient:::.deleteCache("testActivity")
	}
	if(!is.null(synapseClient:::.getCache("testData"))) {
		try(deleteEntity(synapseClient:::.getCache("testData")))
		synapseClient:::.deleteCache("testData")
	}
	if(!is.null(synapseClient:::.getCache("testProject"))) {
	  try(deleteEntity(synapseClient:::.getCache("testProject")))
	  synapseClient:::.deleteCache("testProject")
  }
}

integrationTestCRUDS4Activity <- 
  function()
{
  ## Create Activity
  name<-"testName"
  activity<-Activity(list(name=name))
  description<-"a description of the activity"
  propertyValue(activity, "description")<-description
  testData <-synapseClient:::.getCache("testData")
  propertyValue(activity, "used")<-list(list(reference=list(targetId=propertyValue(testData, "id")), wasExecuted=F))
  activity<-createEntity(activity)
  activityId<-propertyValue(activity, "id")
  checkTrue(!is.null(activityId))
  synapseClient:::.setCache("testActivity", activity)
  
  # get
  act2<-getActivity(activityId)
  checkEquals(name, propertyValue(act2, "name"))
  checkEquals(description, propertyValue(act2, "description"))
  used2<-propertyValue(act2, "used")
  checkTrue(!is.null(used2))
  checkEquals(1, length(used2))
  checkEquals(2, length(used2[[1]]))
  targetId<-used2[[1]]$reference["targetId"]
  names(targetId)<-NULL # needed to make the following check work
  checkEquals(propertyValue(testData, "id"), targetId)
  checkEquals(F, used2[[1]]$wasExecuted)
  
  # update
  descr2<-"another description"
  propertyValue(act2, "description")<-descr2
  propertyValue(act2, "used")<-list(list(reference=list(targetId=propertyValue(testData, "id")), wasExecuted=T))
  act2<-updateEntity(act2)
  checkEquals(descr2, propertyValue(act2, "description"))
  used2<-propertyValue(act2, "used")
  checkTrue(!is.null(used2))
  checkEquals(1, length(used2))
  checkEquals(2, length(used2[[1]]))
  targetId<-used2[[1]]$reference["targetId"]
  names(targetId)<-NULL # needed to make the following check work
  checkEquals(propertyValue(testData, "id"), targetId)
  checkEquals(T, used2[[1]]$wasExecuted)
  
  # delete
  deleteEntity(activity)	
  synapseClient:::.deleteCache("testActivity")
  shouldBeError<-try(getActivity(activityId), silent=T)
  checkTrue(class(shouldBeError)=="try-error")
}

integrationTestReferenceConstructor <- 
  function()
{
  ## Create Activity
  name<-"testName"
  description<-"a description of the activity"
  testData <-synapseClient:::.getCache("testData")
  activity<-Activity(list(name=name, description=description, used=list(list(reference=list(targetId=propertyValue(testData, "id")), wasExecuted=F))))
  activity<-createEntity(activity)
  activityId<-propertyValue(activity, "id")
  checkTrue(!is.null(activityId))
  synapseClient:::.setCache("testActivity", activity)
  
  # delete
  deleteEntity(activity)	
  synapseClient:::.deleteCache("testActivity")
  shouldBeError<-try(getActivity(activityId), silent=T)
  checkTrue(class(shouldBeError)=="try-error")
}

integrationTestEntityConstructor <- 
  function()
{
  ## Create Activity
  name<-"testName"
  description<-"a description of the activity"
  testData <-synapseClient:::.getCache("testData")
  activity<-Activity(list(name=name, description=description, used=list(testData)))
  activity<-createEntity(activity)
  activityId<-propertyValue(activity, "id")
  checkTrue(!is.null(activityId))
  synapseClient:::.setCache("testActivity", activity)
  
  # delete
  deleteEntity(activity)	
  synapseClient:::.deleteCache("testActivity")
  shouldBeError<-try(getActivity(activityId), silent=T)
  checkTrue(class(shouldBeError)=="try-error")
}

