## Integration tests for provenance
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org
################################################################################
.setUp <- 
  function()
{
  synapseClient:::.setCache("testProjectName", paste('Provenance Integration Test Project', gsub(':', '_', date())))
}

.tearDown <- 
  function()
{
  if(!is.null(synapseClient:::.getCache("testProject"))) {
    deleteEntity(synapseClient:::.getCache("testProject"))	
    synapseClient:::.deleteCache("testProject")
  }
}

integrationTestProvenance <- 
  function() 
{
  ## Create Project
  project <- createEntity(
    Project(
      list(
        name=synapseClient:::.getCache("testProjectName")
      )))
  synapseClient:::.setCache("testProject", project)
  checkEquals(propertyValue(project,"name"), synapseClient:::.getCache("testProjectName"))
  
  ## Create Study
  study <- createEntity(
    Study(
      list(
        name="testStudyName",
        parentId=propertyValue(project, "id")
      )))
  checkEquals(propertyValue(data,"name"), "testStudyName")
  checkEquals(propertyValue(data,"parentId"), propertyValue(project, "id"))
  
  ## Create Data
  data <- new(Class = "PhenotypeData")
  propertyValue(data, "name") <- "testPhenoDataName"
  propertyValue(data, "parentId") <- propertyValue(data,"id")
  checkEquals(propertyValue(data,"type"), "C")
  data <- createEntity(data)
  checkEquals(propertyValue(data,"name"), "testPhenoDataName")
  checkEquals(propertyValue(data,"parentId"), propertyValue(data, "id"))
  inputData <- data
  
  ## Start a new step
  step <- startStep()
  
  ## The command line used to invoke this should be stored in the commandLine field
  checkEquals(paste(commandArgs(), collapse=" "), propertyValue(step, 'commandLine'))
  
  ## Get a data, it will be added as input
  data <- getEntity(inputData)
  step <- getStep()
  checkEquals(propertyValue(inputData, "id"), propertyValue(step, "input")[[1]]$targetId)
  
  ## Create a data, it will be added as output
  data <- new(Class = "ExpressionData")
  propertyValue(data, "name") <- "testExprDataName"
  propertyValue(data, "parentId") <- propertyValue(data,"id")
  checkEquals(propertyValue(data,"type"), "E")
  data <- createEntity(data)
  checkEquals(propertyValue(data,"name"), "testExprDataName")
  checkEquals(propertyValue(data,"parentId"), propertyValue(data, "id"))
  outputData <- data
  step <- getStep()
  checkEquals(propertyValue(outputData, "id"), propertyValue(step, "output")[[1]]$targetId)
  
  ## Create an Analysis, it will become the parent of the step
  analysis <- createEntity(Analysis(list(parentId=propertyValue(project, "id"),
        name='test analysis name',
        description='test analysis description')))
  step <- getStep()
  checkEquals(propertyValue(analysis, "id"), propertyValue(step, "parentId"))
  
  step <- stopStep()
  checkTrue(0 < propertyValue(step, 'endDate'))
  checkTrue(10 < length(propertyValue(step, 'environmentDescriptors')))
  checkTrue(0 < length(annotValue(step, 'rHistory')))
}
