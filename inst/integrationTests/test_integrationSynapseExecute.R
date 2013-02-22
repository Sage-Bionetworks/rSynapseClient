library("RCurl")
library("rGithubClient")

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


# test createGithubCodeEntity
integrationTestCreateGithubCodeEntity<-function() {
  project<-synapseClient:::.getCache("testProject")

  repoName<-"/brian-bot/rGithubClient"
  sourceFile<-"R/AllClasses.R"
  createdEntity <- synapseClient:::createGithubCodeEntity(repoName, sourceFile, propertyValue(project, "id"))
  
  # check createdEnity name, url, description
  checkTrue(!is.null(propertyValue(createdEntity, "id")))
  checkEquals("R.AllClasses.R", propertyValue(createdEntity, "name"))

  # TODO this will change with the switch to the new file services
  codeUrl<-propertyValue(createdEntity, "locations")[[1]]["path"]
  names(codeUrl)<-NULL  # necessary for 'checkEquals' to work, below
  checkTrue(!is.null(codeUrl))
  checkEquals("character", class(codeUrl))
  codeUrlPrefix<-sprintf("https://raw.github.com%s", repoName)
  codeUrlSuffix<-sourceFile
  checkEquals(codeUrlPrefix, substr(codeUrl, 1, nchar(codeUrlPrefix)))
  checkEquals(codeUrlSuffix, substr(codeUrl, nchar(codeUrl)-nchar(codeUrlSuffix)+1, nchar(codeUrl)))
  
  # TODO this will change when we introduce the wiki object
  checkEquals(synapseClient:::indent(getURLContent(codeUrl)), propertyValue(createdEntity, "description"))
  
  # retrieve parent (commit)
  commitEntity<-getEntity(propertyValue(createdEntity, "parentId"))
  # check parent name.  The SHA1 hash is a 40 char hexadecimal value
  checkEquals(40, nchar(propertyValue(commitEntity, "name")[[1]]))
  # retrieve grandparent (repo)
  repoEntity<-getEntity(propertyValue(commitEntity, "parentId"))
  # check grandparent name
  checkEquals(synapseClient:::scrubEntityName(repoName), propertyValue(repoEntity, "name"))
  # check that grandparent's parent is 'project'
  checkEquals(propertyValue(project, "id"), propertyValue(repoEntity, "parentId"))
}

integrationTestSynapseExecuteFile<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  # test synapse execute using local file
  workingDir<-tempdir()
  filePath<-paste(workingDir, "testFunction.R", sep="/")
  checkTrue(file.copy(system.file("resources/test/testFunction.R", package = "synapseClient"), filePath, overwrite=T))
  executable<-filePath
  args<-list(x=1)
  resultParentId <-propertyValue(project, "id")
  codeFolder <-project
  codeFolderId <- propertyValue(codeFolder, "id")
  resultEntityProperties <-list(
    foo="bar", 
    bas=1, 
    foo2 = list("foo", "bar"), 
    foo3 = c(1, 2, 3), 
    foo4=list(a="foo", b="bar", c=list("foo", "bar"))
  )
  resultEntityName <- "output"
  resultEntity<-synapseExecute(executable, 
    args, 
    resultParentId, 
    codeFolderId, 
    resultEntityProperties = resultEntityProperties,  
    resultEntityName=resultEntityName)
  
  # check that code has been created
  expectedCodeName <- synapseClient:::scrubEntityName(filePath)
  queryResult <- synapseQuery(sprintf("select id from entity where entity.parentId=='%s' AND entity.name=='%s'", 
      propertyValue(project, "id"), expectedCodeName))
  checkEquals(1, nrow(queryResult))
  codeEntityId <- queryResult$entity.id
  codeEntity<-getEntity(codeEntityId)
  # we don't have to check that the codeEntity is constructed correctly since that is tested elsewhere
  
  # check that result is correct
  resultReload<-loadEntity(propertyValue(resultEntity, "id"))
  checkEquals(resultEntityName, propertyValue(resultReload, "name"))
  checkEquals(resultParentId, propertyValue(resultReload, "parentId"))
 
  # check the result:  the args specified x=1 and the function computes x+1, so the output is 2
  # there should be just one object
  checkEquals(1, length(resultReload$objects))
  # the value of the object should equal 2
  checkEquals(2, resultReload$objects[[1]])
  
  # check properties
  checkEquals("bar", annotValue(resultReload, "foo"))
  checkEquals(1, annotValue(resultReload, "bas"))
  
  # check args
  checkEquals(1, annotValue(resultReload, "x"))

  # check provenance
  activity<-generatedBy(resultReload)
  # check content
  used<-propertyValue(activity, "used")
  checkEquals(1, length(used))
  checkTrue(used[[1]]$wasExecuted)
  checkEquals(propertyValue(codeEntity, "id"), used[[1]]$reference$targetId)
  checkEquals(1, used[[1]]$reference$targetVersionNumber)
  
  # test a revision, i.e. rerun the analysis
  checkTrue(file.copy(system.file("resources/test/testFunction2.R", package = "synapseClient"), filePath, overwrite=T))
  resultEntity<-synapseExecute(executable, 
    args, 
    resultParentId, 
    codeFolderId, 
    resultEntityProperties = resultEntityProperties,  
    resultEntityName=resultEntityName)
  
  
  # there should still be just one code entity, but it's a new version
  queryResult <- synapseQuery(sprintf("select id from entity where entity.parentId=='%s' AND entity.name=='%s'", 
      propertyValue(project, "id"), expectedCodeName))
  checkEquals(1, nrow(queryResult))
  codeEntityId <- queryResult$entity.id
  codeEntity<-getEntity(codeEntityId)
  checkEquals(2, propertyValue(codeEntity, "versionNumber"))
  
  # check that result is correct.  Once again we reload
  resultReload<-loadEntity(propertyValue(resultEntity, "id"))
  checkEquals(resultEntityName, propertyValue(resultReload, "name"))
  checkEquals(resultParentId, propertyValue(resultReload, "parentId"))
  # this time the version should be incremented
  checkEquals(2, propertyValue(resultReload, "versionNumber"))

  # check the result:  the args specified x=1 and the NEW function computes x+2, so the output is 3
  # there should be just one object
  checkEquals(1, length(resultReload$objects))
  # the value of the object should equal 2
  checkEquals(3, resultReload$objects[[1]])
  
  # check provenance
  activity<-generatedBy(resultReload)
  # check content
  used<-propertyValue(activity, "used")
  checkEquals(1, length(used))
  checkTrue(used[[1]]$wasExecuted)
  checkEquals(propertyValue(codeEntity, "id"), used[[1]]$reference$targetId)
  # references the new version of the Code object
  checkEquals(2, used[[1]]$reference$targetVersionNumber)
  
}

# call synapseExecute with a function rather than a file
integrationTestSynapseExecuteFunction<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  executable<-function(x){x+1}
  args<-list(x=1)
  resultParentId <-propertyValue(project, "id")
  codeFolder <-project
  codeFolderId <- propertyValue(codeFolder, "id")
  resultEntityProperties <-list(
    foo="bar", 
    bas=1
  )
  resultEntityName <- "output"
  resultEntity<-synapseExecute(executable, 
    args, 
    resultParentId, 
    codeFolderId, 
    resultEntityProperties = resultEntityProperties,  
    resultEntityName=resultEntityName)
    
  # check that result is correct
  resultReload<-loadEntity(propertyValue(resultEntity, "id"))
  checkEquals(resultEntityName, propertyValue(resultReload, "name"))
  checkEquals(resultParentId, propertyValue(resultReload, "parentId"))
  
  # check the result:  the args specified x=1 and the function computes x+1, so the output is 2
  # there should be just one object
  checkEquals(1, length(resultReload$objects))
  # the value of the object should equal 2
  checkEquals(2, resultReload$objects[[1]])
  
  # check properties
  checkEquals("bar", annotValue(resultReload, "foo"))
  checkEquals(1, annotValue(resultReload, "bas"))
  
  # check args
  checkEquals(1, annotValue(resultReload, "x"))
  
  
  # test a revision, i.e. rerun the analysis
  executable<-function(x){x+2}
  resultEntity<-synapseExecute(executable, 
    args, 
    resultParentId, 
    codeFolderId, 
    resultEntityProperties = resultEntityProperties,  
    resultEntityName=resultEntityName)
  
  # check that result is correct.  Once again we reload
  resultReload<-loadEntity(propertyValue(resultEntity, "id"))
  checkEquals(resultEntityName, propertyValue(resultReload, "name"))
  checkEquals(resultParentId, propertyValue(resultReload, "parentId"))
  # this time the version should be incremented
  checkEquals(2, propertyValue(resultReload, "versionNumber"))
  
  # check the result:  the args specified x=1 and the NEW function computes x+2, so the output is 3
  # there should be just one object
  checkEquals(1, length(resultReload$objects))
  # the value of the object should equal 3
  checkEquals(3, resultReload$objects[[1]])
}
