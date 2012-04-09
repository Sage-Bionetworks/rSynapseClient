## Integration tests for entity CRUD
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
#################################################################################

integrationTestCRUD <- 
  function() 
{
  ## Create a project
  project <- RJSONIO::emptyNamedList
  project$name = paste('R Synapse CRUD Integration Test Project', gsub(':', '_', date()))
  project <- Project(project)
  createdProject <- createEntity(entity=project)
  checkEquals(propertyValue(project, "name"), propertyValue(createdProject, "name"))
  
  ## Create a dataset
  dataset <- RJSONIO::emptyNamedList
  dataset$name = 'R Integration Test Dataset'
  dataset$parentId <- propertyValue(createdProject, "id")
  dataset$species <- "Homo sapiens"
  createdDataset <- synapseClient:::synapsePost(uri='/dataset', entity=dataset)
  checkEquals(dataset$name, createdDataset$name)
  
  ## Get a dataset
  storedDataset <- synapseClient:::synapseGet(uri=createdDataset$uri)
  checkEquals(dataset$name, storedDataset$name)
  
  ## Modify a dataset
  storedDataset$species <- "Mus musculus"
  modifiedDataset <- synapseClient:::synapsePut(uri=storedDataset$uri, entity=storedDataset)
  checkEquals(dataset$name, modifiedDataset$name)
  checkEquals('Mus musculus', modifiedDataset$species)
  
  ## Get dataset annotations
  annotations <- synapseClient:::getAnnotations(entity=modifiedDataset)
  annotations$stringAnnotations$myNewAnnotationKey <- 'my new annotation value'
  storedAnnotations <- synapseClient:::updateAnnotations(annotations=annotations)
  checkEquals('my new annotation value', storedAnnotations$stringAnnotations$myNewAnnotationKey)
  
  ## Delete a dataset
  synapseClient:::synapseDelete(uri=modifiedDataset$uri)
  
  ## Confirm that its gone
  checkException(synapseClient:::synapseGet(uri=createdDataset$uri))
  
  ## Delete a Project
  deleteEntity(entity=createdProject)
}
