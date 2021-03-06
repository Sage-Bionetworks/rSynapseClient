.setUp <- function() {
  ## create a project to fill with entities
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
}

.tearDown <- function() {
  ## delete the test project
	project<-synapseClient:::.getCache("testProject")
	synRestDELETE(sprintf("/entity/%s?skipTrashCan=true", propertyValue(project, "id")))
}

integrationTestStringEndpoint <- function() {
  project <- synapseClient:::.getCache("testProject")
  # create something
  pid<-propertyValue(project, "id")
  
  synRestGET(sprintf("/entity/%s", pid), endpoint=synapseClient:::synapseServiceEndpoint("REPO")$endpoint)
}

integrationTestCRUD <- function() {
  project <- synapseClient:::.getCache("testProject")
  # create something
  pid<-propertyValue(project, "id")
  result<-synRestPOST("/entity", list(concreteType="org.sagebionetworks.repo.model.Folder", parentId=pid, name="foo"))
  id<-result$id
  checkTrue(!is.null(id))
  # get it
  result2<-synRestGET(sprintf("/entity/%s", id))
  checkEquals(result2, result)
  # update it
  result2$name<-"bar"
  result<-synRestPUT(sprintf("/entity/%s", id), result2)
  # get it, check that it's right
  result2<-synRestGET(sprintf("/entity/%s", id))
  checkEquals(result2, result)
  # delete it
  synRestDELETE(sprintf("/entity/%s", id))
  # check that it's deleted
  error<-try(synRestGET(sprintf("/entity/%s", id)), silent=T)
  checkEquals("try-error", class(error))
}

integrationTestObjectCRUD <- function() {
  project <- synapseClient:::.getCache("testProject")
  # create something
  pid<-propertyValue(project, "id")
  folder<-Folder(list(concreteType="org.sagebionetworks.repo.model.Folder", parentId=pid, name="foo"))
  result<-Folder(synRestPOST("/entity", folder))
  id<-propertyValue(result, "id")
  checkTrue(!is.null(id))
  # get it
  result2<-Folder(synRestGET(sprintf("/entity/%s", id)))
  checkEquals(result2, result)
  # update it
  propertyValue(result2, "name")<-"bar"
  result<-Folder(synRestPUT(sprintf("/entity/%s", id), result2))
  # get it, check that it's right
  result2<-Folder(synRestGET(sprintf("/entity/%s", id)))
  checkEquals(result2, result)
  # delete it
  synRestDELETE(sprintf("/entity/%s", id))
  # check that it's deleted
  error<-try(synRestGET(sprintf("/entity/%s", id)), silent=T)
  checkEquals("try-error", class(error))
}

