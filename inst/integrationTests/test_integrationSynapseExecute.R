.setUp <-
  function()
{
  ### create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
}

.tearDown <- 
  function()
{
  if(!is.null(synapseClient:::.getCache("testData"))) {
    try(deleteEntity(synapseClient:::.getCache("testData")))
    synapseClient:::.deleteCache("testData")
  }
  if(!is.null(synapseClient:::.getCache("testProject"))) {
    try(deleteEntity(synapseClient:::.getCache("testProject")))
    synapseClient:::.deleteCache("testProject")
  }
}

integrationTestGetOrCreateEntity <- 
  function()
{
  project<-synapseClient:::.getCache("testProject")
  createdData<-synapseClient:::getOrCreateEntity(name="test data", parentId=propertyValue(project, "id"), entityType="Data")
  synapseClient:::.setCache("testData", createdData)
  firstId <- propertyValue(createdData, "id")
  checkTrue(!is.null(firstId))
  checkEquals("test data", propertyValue(createdData, "name"))
  checkTrue("Data"==class(createdData))
  # now verify that if we try to get/create the same data, we get the same entity, not a new one
  createdData2<-synapseClient:::getOrCreateEntity(name="test data", parentId=propertyValue(project, "id"), entityType="Data")
  secondId <- propertyValue(createdData2, "id")
  checkEquals(firstId, secondId)
}

integrationTestGetOrCreateParentlessContainerEntity <- 
  function()
{
  name<-sprintf("integrationTestGetOrCreateParentlessContainerEntity_%s", format(Sys.time(),"%Y-%m-%dT%H:%M:%OS2", tz="GMT"))
  name<-synapseClient:::scrubEntityName(name)
  createdFolder<-synapseClient:::getOrCreateParentlessContainerEntity(name=name, entityType="Folder")
  synapseClient:::.setCache("testData", createdFolder)
  checkTrue("Folder"==class(createdFolder))
  firstId <- propertyValue(createdFolder, "id")
  checkTrue(!is.null(firstId))
  checkEquals(name, propertyValue(createdFolder, "name"))
  # now verify that if we try to get/create another with the same name, we get the same entity, not a new one
  createdFolder2<-synapseClient:::getOrCreateParentlessContainerEntity(name=name, entityType="Folder")
  secondId <- propertyValue(createdFolder2, "id")
  checkEquals(firstId, secondId)
}

# TODO try omitting the Code folder

integrationTestCreateFileCodeEntity<-function() {
  workingDir<-tempdir()
  filePath<-paste(workingDir, "testFunction.R", sep="/")
  checkTrue(file.copy(system.file("resources/test/testFunction.R", package = "synapseClient"), filePath, overwrite=T))
  project<-synapseClient:::.getCache("testProject")
  createdEntity <- synapseClient:::createFileCodeEntity(filePath, propertyValue(project, "id"))
  id <- propertyValue(createdEntity, "id")
  checkTrue(!is.null(id))
  # check that a new entity exists in the given parent folder, with the correct name and type
  reload<-loadEntity(id)
  checkEquals(propertyValue(project, "id"), propertyValue(reload, "parentId"))
  checkEquals(synapseClient:::scrubEntityName(filePath), propertyValue(reload, "name"))
  checkTrue("Code"==class(reload))
  # check 'description' markdown
  fileContent<-readChar(filePath, file.info(filePath)$size)
  checkEquals(synapseClient:::indent(fileContent), propertyValue(reload, "description"))
  # check that there is a function called 'testFunction'
  reloadedTestFunction<-reload$objects[["testFunction"]]
  checkTrue(!is.null(reloadedTestFunction))
  # run testFunction
  checkEquals(2, reloadedTestFunction(1))
  
  # now try changing the code file
  checkTrue(file.copy(system.file("resources/test/testFunction2.R", package = "synapseClient"), filePath, overwrite=T))
  createdEntity2 <- synapseClient:::createFileCodeEntity(filePath, propertyValue(project, "id"))
  id2 <- propertyValue(createdEntity2, "id")
  checkTrue(!is.null(id2))
  # it should be the same entity
  checkEquals(id, id2)
  # now load it up, and check as before
  reload<-loadEntity(id)
  # it should be a new version
  checkEquals(2, propertyValue(reload, "versionNumber"))
  checkEquals(propertyValue(project, "id"), propertyValue(reload, "parentId"))
  checkEquals(synapseClient:::scrubEntityName(filePath), propertyValue(reload, "name"))
  checkTrue("Code"==class(reload))
  # check 'description' markdown -- here we are checking that the description is updated
  fileContent<-readChar(filePath, file.info(filePath)$size)
  checkEquals(synapseClient:::indent(fileContent), propertyValue(reload, "description"))
  # check that there is a function called 'testFunction'
  reloadedTestFunction<-reload$objects[["testFunction"]]
  checkTrue(!is.null(reloadedTestFunction))
  # run testFunction -- the new code gives a different answer
  checkEquals(3, reloadedTestFunction(1))
}

