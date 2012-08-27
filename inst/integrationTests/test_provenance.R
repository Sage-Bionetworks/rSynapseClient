## Integration tests for provenance
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org
################################################################################
.setUp <- 
  function()
{
  synapseClient:::.setCache("testProject", createEntity(Project()))
  synapseClient:::.setCache("oldProvPref", synapseClient:::.getCache('enableProvenance'))
  if(is.null(getStep()))
    ss <- startStep()
}

.tearDown <- 
  function()
{
  if(!is.null(synapseClient:::.getCache("testProject"))) {
    deleteEntity(synapseClient:::.getCache("testProject"))	
    synapseClient:::.deleteCache("testProject")
    pp <- synapseClient:::.getCache("oldProvPref")
    if(is.null(pp)){
      synapseClient:::.deleteCache('enableProvenance')
    }else{
      synapseClient:::.setCache('enableProvenance', pp)
    }
    synapseClient:::.deleteCache('enableProvenance')
  }
  
  if(!is.null(getStep()))
    stopStep()
}

integrationTestProvenance <- 
  function() 
{
  ## Create Project
  project <- synapseClient:::.getCache("testProject")
   
  ## Create Study
  study <- createEntity(
    Study(
      list(
        name="testStudyName",
        parentId=propertyValue(project, "id")
      )))
  checkEquals(propertyValue(study,"name"), "testStudyName")
  checkEquals(propertyValue(study,"parentId"), propertyValue(project, "id"))
  
  ## Create Data
  data <- PhenotypeData()
  propertyValue(data, "name") <- "testPhenoDataName"
  propertyValue(data, "parentId") <- propertyValue(study,"id")
  data <- createEntity(data)
  checkEquals(propertyValue(data,"name"), "testPhenoDataName")
  checkEquals(propertyValue(data,"parentId"), propertyValue(study, "id"))
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
  data <- Data()
  propertyValue(data, "name") <- "testExprDataName"
  propertyValue(data, "parentId") <- propertyValue(study,"id")
  data <- createEntity(data)
  checkEquals(propertyValue(data,"name"), "testExprDataName")
  checkEquals(propertyValue(data,"parentId"), propertyValue(study, "id"))
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
