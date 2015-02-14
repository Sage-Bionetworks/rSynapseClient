### Test CRUD operations for S4 entities
### 
### Author: Matthew D. Furia <matt.furia@sagebase.org>
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

createFileInMemory<-function(pId) {
  filePath<- tempfile()
  connection<-file(filePath)
  writeChar("this is a test", connection, eos=NULL)
  close(connection)  
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=pId)
  file
}

integrationTestCreateS4Entities <- 
  function()
{
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  ## Create Folder
  folder <- Folder()
  propertyValue(folder, "name") <- "testFolderName"
  propertyValue(folder,"parentId") <- propertyValue(createdProject, "id")
  createdFolder <- createEntity(folder)
  checkEquals(propertyValue(createdFolder,"name"), propertyValue(folder, "name"))
  checkEquals(propertyValue(createdFolder,"parentId"), propertyValue(createdProject, "id"))
  folder <- createdFolder
  
  ## Create File
  f <- createFileInMemory(propertyValue(createdFolder,"id"))
  propertyValue(f, "name") <- "testFileName"
  createdFile <- createEntity(f)
  checkEquals(propertyValue(createdFile,"name"), propertyValue(f, "name"))
  checkEquals(propertyValue(createdFile,"parentId"), propertyValue(folder, "id")) 
  
}

integrationTestCreateEntityWithAnnotations <- 
		function()
{
	## Create Project
	project <- Project()
	annotValue(project, "annotationKey") <- "projectAnnotationValue"
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
	checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
	
	## Create Folder
	folder <- Folder()
	propertyValue(folder, "name") <- "testFolderName"
	propertyValue(folder,"parentId") <- propertyValue(createdProject, "id")
	annotValue(folder, "annotKey") <- "annotValue"
	createdFolder <- createEntity(folder)
	checkEquals(propertyValue(createdFolder,"name"), propertyValue(folder, "name"))
	checkEquals(propertyValue(createdFolder,"parentId"), propertyValue(createdProject, "id"))
	checkEquals(annotValue(createdFolder,"annotKey"), annotValue(folder, "annotKey"))
	checkEquals(NULL, generatedBy(createdFolder))
	
}

integrationTestCreateEntityWithGeneratedBy <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  ## Create File
  f <- createFileInMemory(propertyValue(createdProject,"id"))
  propertyValue(f, "name") <- "testFileName"
  testActivity <-synapseClient:::.getCache("testActivity")
  checkTrue(!is.null(testActivity))
  generatedBy(f)<-testActivity
  file <- createEntity(f)
  createdFile<-getEntity(propertyValue(file, "id"))
  checkEquals(propertyValue(createdFile,"name"), propertyValue(f, "name"))
  checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdProject, "id"))
  checkTrue(!is.null(generatedBy(createdFile)))
  checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(createdFile), "id"))
}

integrationTestUpdateEntityWithGeneratedBy <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  ## Create File
  f <- createFileInMemory(propertyValue(createdProject,"id"))
  propertyValue(f, "name") <- "testFileName"
  createdFile <- createEntity(f)
  testActivity <-synapseClient:::.getCache("testActivity")
  checkTrue(!is.null(testActivity))
  generatedBy(createdFile)<-testActivity
  updatedFile <- updateEntity(createdFile)
  updatedFile<-getEntity(propertyValue(createdFile, "id"))
  checkEquals(propertyValue(updatedFile,"name"), propertyValue(createdFile, "name"))
  checkEquals(propertyValue(updatedFile,"parentId"), propertyValue(createdProject, "id"))
  checkTrue(!is.null(generatedBy(updatedFile)))
  checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(updatedFile), "id"))
}

integrationTestStoreEntityWithGeneratedBy <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  ## Create File
  f <- createFileInMemory(propertyValue(createdProject, "id"))
  propertyValue(f, "name") <- "testFileName"
  file <- createEntity(f)
  testActivity <-synapseClient:::.getCache("testActivity")
  checkTrue(!is.null(testActivity))
  generatedBy(file)<-testActivity
  createdFile <- storeEntity(file)
  updatedFile<-getEntity(propertyValue(createdFile, "id"))
  checkEquals(propertyValue(updatedFile,"name"), propertyValue(createdFile, "name"))
  checkEquals(propertyValue(updatedFile,"parentId"), propertyValue(createdProject, "id"))
  checkTrue(!is.null(generatedBy(updatedFile)))
  checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(updatedFile), "id"))
}

integrationTestCreateEntityWithNAAnnotations <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  ## Create Study
  folder <- Folder()
  propertyValue(folder, "name") <- "testFolderName"
  propertyValue(folder,"parentId") <- propertyValue(createdProject, "id")
  
  annots <- list()
  annots$rawdataavailable <- TRUE 
  annots$number_of_samples <- 33 
  annots$contact <- NA 
  annots$platform <- "HG-U133_Plus_2"
  annotationValues(folder) <- annots
  
  createdFolder <- createEntity(folder)
  
  checkEquals(propertyValue(createdFolder,"name"), propertyValue(folder, "name"))
  checkEquals(propertyValue(createdFolder,"parentId"), propertyValue(createdProject, "id"))
  checkEquals(annotValue(createdFolder,"platform"), "HG-U133_Plus_2")
  checkEquals(annotValue(createdFolder,"number_of_samples"), 33)
  checkEquals(annotValue(createdFolder,"rawdataavailable"), "TRUE")
  checkTrue(is.null(annotValue(createdFolder,"contact")[[1]]))
}

integrationTestUpdateS4Entity <-
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
  
  ## create a folder
  folder <- Folder()
  propertyValue(folder, "name") <- "testFolderName"
  propertyValue(folder,"parentId") <- propertyValue(createdProject, "id")
  createdFolder <- createEntity(folder)
  
  ## update the folder annotations
  annotValue(createdFolder, "newKey") <- "newValue"
  updatedFolder <- updateEntity(createdFolder)
  checkEquals(annotValue(createdFolder, "newKey"), annotValue(updatedFolder, "newKey"))
  checkTrue(propertyValue(createdFolder, "etag") != propertyValue(updatedFolder, "etag"))
  checkEquals(propertyValue(createdFolder, "id"), propertyValue(updatedFolder, "id"))
  
  ## create a File
  f <- createFileInMemory(propertyValue(createdFolder,"id"))
  propertyValue(f, "name") <- "testFileName"
  createdFile <- createEntity(f)
  checkEquals(propertyValue(createdFile,"name"), propertyValue(f, "name"))
  checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdFolder, "id")) 
  
  ## update the description property of the File
  propertyValue(createdFile, "description") <- "This is a description"
  updatedFile <- updateEntity(createdFile)
  checkEquals(propertyValue(createdFile, "description"), propertyValue(updatedFile, "description"))
  
  ## update the description property on a project
  createdProject <- getEntity(createdProject)
  propertyValue(createdProject, "description") <- "This is a new description"
  updatedProject <- updateEntity(createdProject)
  checkEquals(propertyValue(createdProject, "description"), propertyValue(updatedProject, "description"))
  
}

integrationTestDeleteEntityById <-
  function()
{
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  folder <- Folder()
  propertyValue(folder, "name") <- "testFolderName"
  propertyValue(folder,"parentId") <- propertyValue(createdProject, "id")
  createdFolder <- createEntity(folder)
  f <- createFileInMemory(propertyValue(createdFolder, "id"))
  createdFile <- storeEntity(f)
  # $cacheDir not defined
  createdFile <- synGet(propertyValue(createdFile, "id"))
  
  synDelete(createdProject$properties$id)
  checkException(getEntity(createdFolder))
  checkException(getEntity(createdProject))
  checkException(getEntity(createdFile))
  
  synapseClient:::.deleteCache("testProject")
}

createFile<-function(content, filePath) {
  if (missing(content)) content<-"this is a test"
  if (missing(filePath)) filePath<- tempfile()
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
  filePath
}

integrationTestSYNR580<-function() {
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  testActivity <-synapseClient:::.getCache("testActivity")
  genF <- File(path=createFile(), parentId=propertyValue(createdProject, "id"))
  generatedBy(genF) <- testActivity
  genF <- synStore(genF)
  checkTrue(!is.null(generatedBy(genF)))
}

integrationTestUpdateS4EntityWithGeneratedBy <-
		function()
{
	## Create Project
	project <- Project()
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
	
	## set generatedBy and update. 
	testActivity <-synapseClient:::.getCache("testActivity")
	checkTrue(!is.null(testActivity))
	generatedBy(createdProject) <- testActivity
	updatedProject <- updateEntity(createdProject)
	testActivity <- generatedBy(updatedProject)
	# since storing the entity also stores the activity, we need to update the cached value
	synapseClient:::.setCache("testActivity", testActivity)
	checkEquals(propertyValue(testActivity, "id"), propertyValue(generatedBy(updatedProject), "id"))
	checkTrue(propertyValue(updatedProject, "etag") != propertyValue(createdProject, "etag"))

  #  get the entity by ID and verify that the generatedBy is not null
  gotProject <- getEntity(propertyValue(createdProject, "id"))
  checkTrue(!is.null(gotProject))
  checkTrue(!is.null(generatedBy(gotProject)))
  
	## remove generatedBy and update
	createdProject<-updatedProject
	generatedBy(createdProject) <- NULL
	updatedProject <- updateEntity(createdProject)
	checkTrue(is.null(generatedBy(updatedProject)))
	
	## now *create* an Entity having a generatedBy initially
	deleteEntity(synapseClient:::.getCache("testProject"))	
	synapseClient:::.deleteCache("testProject")
	project <- Project()
	generatedBy(project) <- testActivity
	createdProject <- createEntity(project)
	checkTrue(!is.null(generatedBy(createdProject)))
	synapseClient:::.setCache("testProject", createdProject)
	testActivity <- generatedBy(createdProject)
	# since storing the entity also stores the activity, we need to update the cached value
	synapseClient:::.setCache("testActivity", testActivity)
	
  #  get the entity by ID and verify that the generatedBy is not null
  gotProject <- getEntity(propertyValue(createdProject, "id"))
  checkTrue(!is.null(gotProject))
  checkTrue(!is.null(generatedBy(gotProject)))
  
  ## remove generatedBy and update
	generatedBy(createdProject)<-NULL
	updatedProject <- updateEntity(createdProject)
	checkTrue(is.null(generatedBy(updatedProject)))
	
}
# a variation of the previous test, using the 'used' convenience function
integrationTestUpdateS4EntityWithUsed <-
		function()
{
	## Create Project
	project <- Project()
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
	
	project2 <- Project()
	createdProject2 <- createEntity(project2)
	synapseClient:::.setCache("testProject2", createdProject2)
	
	checkTrue(is.null(used(createdProject)))
	## 2nd project was 'used' to generate 1st project
	used(createdProject)<-list(createdProject2)
	updatedProject <- updateEntity(createdProject)
	checkTrue(propertyValue(updatedProject, "etag") != propertyValue(createdProject, "etag"))
	usedList<-used(updatedProject)
	checkTrue(!is.null(usedList))
	checkEquals(1, length(usedList))
	targetId<-usedList[[1]]$reference$targetId
	names(targetId)<-NULL # needed to make the following check work
	checkEquals(propertyValue(createdProject2, "id"), targetId)
	
	## remove "used" list and update
	createdProject<-updatedProject
	used(createdProject) <- NULL
	updatedProject <- updateEntity(createdProject)
	checkTrue(is.null(used(updatedProject)))
	
	## now *create* an Entity having a "used" list initially
	deleteEntity(synapseClient:::.getCache("testProject"))	
	synapseClient:::.deleteCache("testProject")
	project <- Project()
	used(project)<-list(list(entity=createdProject2, wasExecuted=F))
	
	createdProject <- createEntity(project)
	synapseClient:::.setCache("testProject", createdProject)
	usedList2 <- used(createdProject)
	checkTrue(!is.null(usedList2))
	checkEquals(1, length(usedList2))
	targetId<-usedList2[[1]]$reference$targetId
	names(targetId)<-NULL # needed to make the following check work
	checkEquals(propertyValue(createdProject2, "id"), targetId)
	checkEquals(F, usedList2[[1]]$wasExecuted)
	
	## remove "used" list and update
	used(createdProject)<-NULL
	updatedProject <- updateEntity(createdProject)
	checkTrue(is.null(used(updatedProject)))
}

# this is the test to show that SYNR-327 is fixed
# use the same activity in two different entities
integrationTestTwoEntitiesOneActivity<-function() {
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  pid<-propertyValue(createdProject, "id")
  foo<-Folder(name="foo", parentId=pid)
  foo<-storeEntity(foo)
  bar<-Folder(name="bar", parentId=pid)
  bar<-storeEntity(bar)
  
  activity<-Activity(list(name="foobarActivity"))
  activity<-storeEntity(activity)

  generatedBy(foo)<-activity
  generatedBy(bar)<-activity
  foo<-storeEntity(foo)
  bar<-storeEntity(bar) 
}

integrationTestDeleteEntity <- 
  function()
{
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  folder <- Folder()
  propertyValue(folder, "name") <- "testFolderName"
  propertyValue(folder,"parentId") <- propertyValue(createdProject, "id")
  createdFolder <- createEntity(folder)
  file <- createFileInMemory(propertyValue(createdFolder, "id"))
  createdFile <- storeEntity(file)
  
  deletedProject <- deleteEntity(createdProject)
  checkEquals(propertyValue(deletedProject, "id"), NULL)
  
  ## need to fix this
  ##checkTrue(!any(grepl('createdProject', ls())))
  createdProject <- synapseClient:::.getCache("testProject")
  checkEquals(propertyValue(deletedProject,"id"), NULL)
  
  checkException(getEntity(createdFile))
  checkException(getEntity(createdFolder))
  checkException(getEntity(createdProject))
  synapseClient:::.deleteCache("testProject")
}

integrationTestGetEntity <-
  function()
{
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  fetchedProject <- getEntity(propertyValue(createdProject, "id"))
  checkEquals(propertyValue(fetchedProject, "id"), propertyValue(createdProject, "id"))
  
  fetchedProject <- getEntity(as.character(propertyValue(createdProject, "id")))
  checkEquals(propertyValue(fetchedProject, "id"), propertyValue(createdProject, "id"))
  
  fetchedProject <- getEntity(synapseClient:::.extractEntityFromSlots(createdProject))
  checkEquals(propertyValue(fetchedProject, "id"), propertyValue(createdProject, "id"))
  
  fetchedProject <- getEntity(createdProject)
  checkEquals(propertyValue(fetchedProject, "id"), propertyValue(createdProject, "id"))
}

integrationTestReplaceAnnotations <- 
  function()
{
  # fix this test
  ## Create Project
  project <- Project()
  annotations(project) <- list(annotation1="value1", annotation2="value2")
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  annotations(createdProject) <- list(annotation3="value3", annotation4="value4", annotation5="value5")
  createdProject <- updateEntity(createdProject)
  
  checkEquals(length(annotationNames(createdProject)), 3L)
  checkTrue(all(c("annotation3", "annotation4", "annotation5") %in% annotationNames(createdProject)))
}

integrationTestFindExistingEntity <- function(){
  ## Create Project, append a random integer to make it unique
  projectName<-sprintf("integrationTestFindExistingEntity_%d", sample(1000,1))
  project <- Project(name=projectName)
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  pid<-propertyValue(createdProject, "id")
  
  folder<-Folder(name="testName", parentId=pid)
  folder<-synStore(folder)
  
  result<-synapseClient:::findExistingEntity(projectName)
  checkEquals(pid, result$id)
  
  result<-synapseClient:::findExistingEntity(name="testName", parentId=pid)
  checkEquals(propertyValue(folder, "id"), result$id)
  
  
}

