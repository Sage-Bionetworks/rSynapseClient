### Integration tests for entity CRUD
### 
### Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
##################################################################################
#
#integrationTestCRUD <- 
#  function() 
#{
#  ## Create a project
#  project <- RJSONIO::emptyNamedList
#  project$name = paste('R Synapse CRUD Integration Test Project', gsub(':', '_', date()))
#  project <- Project(project)
#  createdProject <- createEntity(entity=project)
#  checkEquals(propertyValue(project, "name"), propertyValue(createdProject, "name"))
#  
#  ## Create a study
#  study <- RJSONIO::emptyNamedList
#  study$name = 'R Integration Test Study'
#  study$parentId <- propertyValue(createdProject, "id")
#  study$species <- "Homo sapiens"
#  createdStudy <- synapseClient:::synapsePost(uri='/study', entity=study)
#  checkEquals(study$name, createdStudy$name)
#  
#  ## Get a study
#  storedStudy <- synapseClient:::synapseGet(uri=createdStudy$uri)
#  checkEquals(study$name, storedStudy$name)
#  
#  ## Modify a study
#  storedStudy$species <- "Mus musculus"
#  modifiedStudy <- synapseClient:::synapsePut(uri=storedStudy$uri, entity=storedStudy)
#  checkEquals(study$name, modifiedStudy$name)
#  checkEquals('Mus musculus', modifiedStudy$species)
#  
#  ## Get study annotations
#  annotations <- synapseClient:::getAnnotations(entity=modifiedStudy)
#  annotations$stringAnnotations$myNewAnnotationKey <- 'my new annotation value'
#  storedAnnotations <- synapseClient:::updateAnnotations(annotations=annotations)
#  checkEquals('my new annotation value', storedAnnotations$stringAnnotations$myNewAnnotationKey)
#  
#  ## Delete a study
#  synapseClient:::synapseDelete(uri=modifiedStudy$uri)
#  
#  ## Confirm that its gone
#  checkException(synapseClient:::synapseGet(uri=createdStudy$uri))
#  
#  ## Delete a Project
#  deleteEntity(entity=createdProject)
#}

integrationTestWarnMe <-
  function(){
  warning("need to fix tests for test_synapseCRUD.R")
}