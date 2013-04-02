### This is a test of the retrofit of the Entity-based commands to the File class
### 
### Author: bruce.hoff@sagebase.org
################################################################################ 

.setUp <-
		function()
{
	activity<-Activity(list(name="test activity"))
	activity<-createEntity(activity)
	synapseClient:::.setCache("testActivity", activity)
}

.tearDown <- 
  function()
{
	if(!is.null(synapseClient:::.getCache("testActivity"))) {
		try(deleteEntity(synapseClient:::.getCache("testActivity")))
		synapseClient:::.deleteCache("testActivity")
	}
	if(!is.null(synapseClient:::.getCache("testProject"))) {
		try(deleteEntity(synapseClient:::.getCache("testProject")))
		synapseClient:::.deleteCache("testProject")
	}
	if(!is.null(synapseClient:::.getCache("testProject2"))) {
		try(deleteEntity(synapseClient:::.getCache("testProject2")))
		synapseClient:::.deleteCache("testProject2")
	}
}

createFileInMemory<-function(project) {
  filePath<- system.file("NAMESPACE", package = "synapseClient")
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  file
}

integrationTestCreateS4Files <- 
  function()
{
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  ## Create File
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testStudyName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  createdFile <- createEntity(file)
  checkEquals(propertyValue(createdFile,"name"), propertyValue(file, "name"))
  checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdProject, "id"))
  file <- createdFile
}

integrationTestCreateFileWithAnnotations <- 
		function()
{
	## Create Project
	project <- Project()
	annotValue(project, "annotationKey") <- "projectAnnotationValue"
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
	checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
	
  ## Create File
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testStudyName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  createdFile <- createEntity(file)
  annotValue(file, "annotKey") <- "annotValue"
  createdFile <- createEntity(file)
	checkEquals(propertyValue(createdFile,"name"), propertyValue(file, "name"))
	checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdProject, "id"))
	checkEquals(annotValue(createdFile,"annotKey"), annotValue(file, "annotKey"))
	checkEquals(NULL, generatedBy(createdFile))
	
}

integrationTestCreateFileWithGeneratedBy <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testDataName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  testActivity <-synapseClient:::.getCache("testActivity")
  checkTrue(!is.null(testActivity))
  generatedBy(data)<-testActivity
  file <- createEntity(file)
  createdFile<-getEntity(propertyValue(file, "id"))
  checkEquals(propertyValue(createdFile,"name"), propertyValue(file, "name"))
  checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdProject, "id"))
  checkTrue(!is.null(generatedBy(createdFile)))
  checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(createdFile), "id"))
}

integrationTestUpdateFileWithGeneratedBy <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testDataName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  file <- createEntity(file)
  testActivity <-synapseClient:::.getCache("testActivity")
  checkTrue(!is.null(testActivity))
  generatedBy(data)<-testActivity
  file <- updateEntity(file)
  updatedFile<-getEntity(propertyValue(file, "id"))
  checkEquals(propertyValue(updatedFile,"name"), propertyValue(file, "name"))
  checkEquals(propertyValue(updatedFile,"parentId"), propertyValue(createdProject, "id"))
  checkTrue(!is.null(generatedBy(updatedFile)))
  checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(updatedFile), "id"))
}

integrationTestStoreFileWithGeneratedBy <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testDataName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  file <- createEntity(file)
  testActivity <-synapseClient:::.getCache("testActivity")
  checkTrue(!is.null(testActivity))
  generatedBy(file)<-testActivity
  file <- storeEntity(file)
  updatedFile<-getEntity(propertyValue(file, "id"))
  checkEquals(propertyValue(updatedFile,"name"), propertyValue(file, "name"))
  checkEquals(propertyValue(updatedFile,"parentId"), propertyValue(createdProject, "id"))
  checkTrue(!is.null(generatedBy(updatedFile)))
  checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(updatedFile), "id"))
}

integrationTestCreateFileWithNAAnnotations <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testStudyName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  
  annots <- list()
  annots$rawdataavailable <- TRUE 
  annots$number_of_samples <- 33 
  annots$contact <- NA 
  annots$platform <- "HG-U133_Plus_2"
  annotationValues(file) <- annots
  
  createdFile <- createEntity(file)
  
  checkEquals(propertyValue(createdFile,"name"), propertyValue(file, "name"))
  checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdProject, "id"))
  checkEquals(annotValue(createdFile,"platform"), "HG-U133_Plus_2")
  checkEquals(annotValue(createdFile,"number_of_samples"), 33)
  checkEquals(annotValue(createdFile,"rawdataavailable"), "TRUE")
  checkTrue(is.null(annotValue(createdFile,"contact")[[1]]))
}

integrationTestUpdateS4File <-
  function()
{
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  ## set an annotation value and update. 
  annotValue(createdProject, "newKey") <- "newValue"
  updatedProject <- updateEntity(createdProject)
  checkEquals(propertyValue(updatedProject,"id"), propertyValue(createdProject,"id"))
  checkTrue(propertyValue(updatedProject, "etag") != propertyValue(createdProject, "etag"))
  
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testStudyName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  createdFile <- createEntity(file)
  
  ## update the study annotations
  annotValue(createdFile, "newKey") <- "newValue"
  updatedFile <- updateEntity(createdFile)
  checkEquals(annotValue(createdFile, "newKey"), annotValue(createdFile, "newKey"))
  checkTrue(propertyValue(createdFile, "etag") != propertyValue(createdFile, "etag"))
  checkEquals(propertyValue(createdFile, "id"), propertyValue(createdFile, "id"))
}

integrationTestDeleteFileById <-
  function()
{
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  study <- Study()
  propertyValue(study, "name") <- "testStudyName"
  propertyValue(study,"parentId") <- propertyValue(createdProject, "id")
  createdStudy <- createEntity(study)
  createdData <- Data(list(name="aData", type="C", parentId=propertyValue(createdStudy, "id")))
  data <- addObject(createdData, "foo", "bar")
  createdData <- storeEntity(createdData)
  
  cacheDir <- createdData$cacheDir
  checkTrue(file.exists(cacheDir))
  deleteEntity(createdData)
  checkTrue(!file.exists(cacheDir))
  
  
  deleteEntity(createdProject$properties$id)
  checkException(getEntity(createdStudy))
  checkException(getEntity(createdProject))
  synapseClient:::.deleteCache("testProject")
}

integrationTestUpdateS4FileWithGeneratedBy <-
		function()
{
	## Create Project
	project <- Project()
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
	
  file<-createFileInMemory(createdProject)
  file<-storeEntity(file)
  
	## set generatedBy and update. 
	testActivity <-synapseClient:::.getCache("testActivity")
	checkTrue(!is.null(testActivity))
	generatedBy(file) <- testActivity
	updatedFile <- updateEntity(file)
	testActivity <- generatedBy(updatedFile)
	# since storing the entity also stores the activity, we need to update the cached value
	synapseClient:::.setCache("testActivity", testActivity)
	checkEquals(propertyValue(testActivity, "id"), propertyValue(generatedBy(updatedFile), "id"))
	checkTrue(propertyValue(updatedFile, "etag") != propertyValue(file, "etag"))

  #  get the entity by ID and verify that the generatedBy is not null
  gotFile <- getEntity(propertyValue(file, "id"))
  checkTrue(!is.null(gotFile))
  checkTrue(!is.null(generatedBy(gotFile)))
  
	## remove generatedBy and update
	file<-updatedFile
	generatedBy(file) <- NULL
  updatedFile <- updateEntity(file)
	checkTrue(is.null(generatedBy(updatedFile)))
	
	## now *create* an Entity having a generatedBy initially
	deleteEntity(file)	

  file<-createFileInMemory(createdProject)

	generatedBy(file) <- testActivity
	createdFile <- createEntity(file)
	checkTrue(!is.null(generatedBy(createdFile)))
	synapseClient:::.setCache("testProject", createdFile)
	testActivity <- generatedBy(createdFile)
	# since storing the entity also stores the activity, we need to update the cached value
	synapseClient:::.setCache("testActivity", testActivity)
	
  #  get the entity by ID and verify that the generatedBy is not null
  gotFile <- getEntity(propertyValue(createdFile, "id"))
  checkTrue(!is.null(gotFile))
  checkTrue(!is.null(generatedBy(gotFile)))
  
  ## remove generatedBy and update
	generatedBy(createdFile)<-NULL
	updatedFile <- updateEntity(createdFile)
	checkTrue(is.null(generatedBy(updatedFile)))
	
}

# a variation of the previous test, using the 'used' convenience function
integrationTestUpdateS4FileWithUsed <-
		function()
{
	## Create File
	project <- Project()
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
  file<-createFileInMemory(createdProject)
  createdFile<-storeEntity(file)
  
	project2 <- Project()
	createdProject2 <- createEntity(project2)
	synapseClient:::.setCache("testProject2", createdProject2)
  file2<-createFileInMemory(createdProject2)
  createdFile2<-storeEntity(file2)
  
	checkTrue(is.null(used(createdFile)))
	## 2nd file was 'used' to generate 1st file
	used(createdFile)<-list(createdFile2)
	updatedFile <- updateEntity(createdFile)
	checkTrue(propertyValue(updatedFile, "etag") != propertyValue(createdFile, "etag"))
	usedList<-used(updatedFile)
	checkTrue(!is.null(usedList))
	checkEquals(1, length(usedList))
	targetId<-usedList[[1]]$reference$targetId
	names(targetId)<-NULL # needed to make the following check work
	checkEquals(propertyValue(createdFile2, "id"), targetId)
	
	## remove "used" list and update
	createdFile<-updatedFile
	used(createdFile) <- NULL
  updatedFile <- updateEntity(createdFile)
	checkTrue(is.null(used(updatedFile)))
  
  deleteEntity(updatedFile)
	
	## now *create* an Entity having a "used" list initially
  file<-createFileInMemory(createdProject)
  
	used(file)<-list(list(entity=createdFile2, wasExecuted=F, concreteType="org.sagebionetworks.repo.model.provenance.UsedEntity"))
	
	createdFile <- createEntity(file)
	usedList2 <- used(createdFile)
	checkTrue(!is.null(usedList2))
	checkEquals(1, length(usedList2))
	targetId<-usedList2[[1]]$reference$targetId
	names(targetId)<-NULL # needed to make the following check work
	checkEquals(propertyValue(createdFile2, "id"), targetId)
	checkEquals(F, usedList2[[1]]$wasExecuted)
	
	## remove "used" list and update
	used(createdFile)<-NULL
	updatedFile <- updateEntity(createdFile)
	checkTrue(is.null(used(updatedFile)))
}

integrationTestDeleteFile <- 
  function()
{
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  file<-createFileInMemory(createdProject)
  createdFile <- createEntity(file)

  deleteEntity(createdFile)
  
  # should get a 404 error 
  result<-try(getEntity(propertyValue(createdFile, "id")))
  checkEquals("try-error", class(result))
}

integrationTestGetFile <-
  function()
{
  ## Create Project and File
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  file<-createFileInMemory(createdProject)
  createdFile<-storeEntity(file)
  
  fetchedFile <- getEntity(propertyValue(createdFile, "id"))
  checkEquals(propertyValue(fetchedFile, "id"), propertyValue(createdFile, "id"))
  
  fetchedFile <- getEntity(as.character(propertyValue(createdFile, "id")))
  checkEquals(propertyValue(fetchedFile, "id"), propertyValue(createdFile, "id"))
  
  fetchedFile <- getEntity(synapseClient:::.extractEntityFromSlots(createdFile))
  checkEquals(propertyValue(fetchedFile, "id"), propertyValue(createdFile, "id"))
  
  fetchedFile <- getEntity(createdFile)
  checkEquals(propertyValue(fetchedFile, "id"), propertyValue(createdFile, "id"))
}

integrationTestReplaceFileAnnotations <- 
  function()
{
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  file<-createFileInMemory(createdProject)
  
  annotations(file) <- list(annotation1="value1", annotation2="value2")
  createdFile <- createEntity(file)
  
  annotations(createdFile) <- list(annotation3="value3", annotation4="value4", annotation5="value5")
  createdFile <- updateEntity(createdFile)
  
  checkEquals(length(annotationNames(createdFile)), 3L)
  checkTrue(all(c("annotation3", "annotation4", "annotation5") %in% annotationNames(createdFile)))
}

