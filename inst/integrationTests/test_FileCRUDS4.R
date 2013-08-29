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
  filePath<- tempfile()
  connection<-file(filePath)
  writeChar("this is a test", connection, eos=NULL)
  close(connection)  
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
  propertyValue(file, "name") <- "testFileName"
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
  propertyValue(file, "name") <- "testFileName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  annotValue(file, "annotKey") <- "annotValue"
  createdFile <- createEntity(file)
	checkEquals(propertyValue(createdFile,"name"), propertyValue(file, "name"))
	checkEquals(propertyValue(createdFile,"parentId"), propertyValue(createdProject, "id"))
	checkEquals(annotValue(createdFile,"annotKey"), annotValue(file, "annotKey"))
	checkEquals(NULL, generatedBy(createdFile))
	
}

integrationTestUpdateFile <- 
  function()
{
  ## Create Project
  project <- Project()
  annotValue(project, "annotationKey") <- "projectAnnotationValue"
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  checkEquals(annotValue(createdProject, "annotationKey"), annotValue(project, "annotationKey"))
  
  file<-createFileInMemory(createdProject)
  propertyValue(file, "name") <- "testFileName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  file <- createEntity(file)
  file <- updateEntity(file)
  updatedFile<-getEntity(propertyValue(file, "id"))
  checkEquals(propertyValue(updatedFile,"name"), propertyValue(file, "name"))
  checkEquals(propertyValue(updatedFile,"parentId"), propertyValue(createdProject, "id"))
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
  propertyValue(file, "name") <- "testFileName"
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
  propertyValue(file, "name") <- "testFileName"
  propertyValue(file,"parentId") <- propertyValue(createdProject, "id")
  createdFile <- createEntity(file)
  
  ## update the annotations
  annotValue(createdFile, "newKey") <- "newValue"
  updatedFile <- updateEntity(createdFile)
  checkEquals(annotValue(updatedFile, "newKey"), annotValue(createdFile, "newKey"))
  checkTrue(propertyValue(updatedFile, "etag") != propertyValue(createdFile, "etag"))
  checkEquals(propertyValue(updatedFile, "id"), propertyValue(createdFile, "id"))
}

integrationTestDeleteFileById <-
  function()
{
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  filePath<-file.path(tempdir(), sample(1000,1)) # create a random file path in the temp' dir
  file<-File(filePath, synapseStore=TRUE, parentId=propertyValue(createdProject, "id"))
  file <- addObject(file, "foo", "bar")
  
  createdFile <- createEntity(file)
  
  cacheDir <- createdFile@filePath
  checkTrue(file.exists(cacheDir))
  deleteEntity(file)
  
  deleteEntity(createdProject$properties$id)
  checkException(getEntity(createdFile), silent=TRUE)
  synapseClient:::.deleteCache("testProject")
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
  result<-try(getEntity(propertyValue(createdFile, "id")), silent=TRUE)
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

