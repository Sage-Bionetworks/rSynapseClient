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
		deleteEntity(synapseClient:::.getCache("testActivity"))	
		synapseClient:::.deleteCache("testActivity")
	}
	if(!is.null(synapseClient:::.getCache("testProject"))) {
    deleteEntity(synapseClient:::.getCache("testProject"))	
    synapseClient:::.deleteCache("testProject")
  }
}

integrationTestCreateS4Entities <- 
  function()
{
  ## Create Project
  project <- Project()
  createdProject <- createEntity(project)
  synapseClient:::.setCache("testProject", createdProject)
  
  ## Create Study
  study <- Study()
  propertyValue(study, "name") <- "testStudyName"
  propertyValue(study,"parentId") <- propertyValue(createdProject, "id")
  createdStudy <- createEntity(study)
  checkEquals(propertyValue(createdStudy,"name"), propertyValue(study, "name"))
  checkEquals(propertyValue(createdStudy,"parentId"), propertyValue(createdProject, "id"))
  study <- createdStudy
  
  ## Create Data
  data <- Data()
  propertyValue(data, "name") <- "testPhenoDataName"
  propertyValue(data, "parentId") <- propertyValue(study,"id")
  createdData <- createEntity(data)
  checkEquals(propertyValue(createdData,"name"), propertyValue(data, "name"))
  checkEquals(propertyValue(createdData,"parentId"), propertyValue(study, "id")) 
  
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
	
	## Create Study
	study <- Study()
	propertyValue(study, "name") <- "testStudyName"
	propertyValue(study,"parentId") <- propertyValue(createdProject, "id")
	annotValue(study, "annotKey") <- "annotValue"
	createdStudy <- createEntity(study)
	checkEquals(propertyValue(createdStudy,"name"), propertyValue(study, "name"))
	checkEquals(propertyValue(createdStudy,"parentId"), propertyValue(createdProject, "id"))
	checkEquals(annotValue(createdStudy,"annotKey"), annotValue(study, "annotKey"))
	checkEquals(NULL, generatedBy(createdStudy))
	
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
	
	## Create Study
	study <- Study()
	propertyValue(study, "name") <- "testStudyName"
	propertyValue(study,"parentId") <- propertyValue(createdProject, "id")
	testActivity <-synapseClient:::.getCache("testActivity")
	checkTrue(!is.null(testActivity))
	generatedBy(study)<-testActivity
	createdStudy <- createEntity(study)
	checkEquals(propertyValue(createdStudy,"name"), propertyValue(study, "name"))
	checkEquals(propertyValue(createdStudy,"parentId"), propertyValue(createdProject, "id"))
	checkTrue(!is.null(generatedBy(createdStudy)))
	checkEquals(propertyValue(testActivity,"id"), propertyValue(generatedBy(createdStudy), "id"))
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
  study <- Study()
  propertyValue(study, "name") <- "testStudyName"
  propertyValue(study,"parentId") <- propertyValue(createdProject, "id")
  
  annots <- list()
  annots$rawdataavailable <- TRUE 
  annots$number_of_samples <- 33 
  annots$contact <- NA 
  annots$platform <- "HG-U133_Plus_2"
  annotationValues(study) <- annots
  
  createdStudy <- createEntity(study)
  
  checkEquals(propertyValue(createdStudy,"name"), propertyValue(study, "name"))
  checkEquals(propertyValue(createdStudy,"parentId"), propertyValue(createdProject, "id"))
  checkEquals(annotValue(createdStudy,"platform"), "HG-U133_Plus_2")
# TODO this should be a number, not a string
  checkEquals(annotValue(createdStudy,"number_of_samples"), 33)
# TODO this should be a boolean, not a string
  checkEquals(annotValue(createdStudy,"rawdataavailable"), "TRUE")
# TODO this should probably be NA instead of NULL
  checkTrue(is.null(annotValue(createdStudy,"contact")[[1]]))
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
  
  ## create a study
  study <- Study()
  propertyValue(study, "name") <- "testStudyName"
  propertyValue(study,"parentId") <- propertyValue(createdProject, "id")
  createdStudy <- createEntity(study)
  
  ## update the study annotations
  annotValue(createdStudy, "newKey") <- "newValue"
  updatedStudy <- updateEntity(createdStudy)
  checkEquals(annotValue(createdStudy, "newKey"), annotValue(updatedStudy, "newKey"))
  checkTrue(propertyValue(createdStudy, "etag") != propertyValue(updatedStudy, "etag"))
  checkEquals(propertyValue(createdStudy, "id"), propertyValue(updatedStudy, "id"))
  
  ## create a data
  data <- Data()
  propertyValue(data, "name") <- "testPhenoDataName"
  propertyValue(data, "parentId") <- propertyValue(createdStudy,"id")
  createdData <- createEntity(data)
  checkEquals(propertyValue(createdData,"name"), propertyValue(data,"name"))
  
  
  ## update the description property
  propertyValue(createdData, "description") <- "This is a description"
  updatedData <- updateEntity(createdData)
  checkEquals(propertyValue(createdData, "description"), propertyValue(updatedData, "description"))
  
  ## update the description property on a project
  createdProject <- getEntity(createdProject)
  propertyValue(createdProject, "description") <- "This is a new description"
  updatedProject <- updateEntity(createdProject)
  checkEquals(propertyValue(createdProject, "description"), propertyValue(updatedProject, "description"))
  
}

integrationTestDeleteEntity <- 
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
  
  
  deletedProject <- deleteEntity(createdProject)
  checkEquals(propertyValue(deletedProject, "id"), NULL)
  
  ## need to fix this
  ##checkTrue(!any(grepl('createdProject', ls())))
  createdProject <- synapseClient:::.getCache("testProject")
  checkEquals(propertyValue(deletedProject,"id"), NULL)
  
  checkException(getEntity(createdStudy))
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

