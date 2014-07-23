#
# Tests for schema parsing / class generating functions
#
#########################################################

unitTestGetImplements<-function() {
  checkTrue(is.null(synapseClient:::getImplements(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.table.Row"))))
  folderSchemaDef<-synapseClient:::readEntityDef("org.sagebionetworks.repo.model.Folder")
  checkTrue(!is.null(synapseClient:::getImplements(folderSchemaDef)))
  checkEquals("org.sagebionetworks.repo.model.Entity", synapseClient:::getImplements(folderSchemaDef)[[1]][[1]])
}

unitTestisVirtual<-function() {
  checkTrue(!synapseClient:::isVirtual(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.table.Row")))
  checkTrue(!synapseClient:::isVirtual(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.Folder")))
  checkTrue(synapseClient:::isVirtual(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.Entity")))
}

unitTestGetPropertyTypes<-function() {
  checkEquals(list(dataFileHandleId="string"), synapseClient:::getPropertyTypes(which="org.sagebionetworks.repo.model.FileEntity"))
  upProperties<-synapseClient:::getPropertyTypes(which="org.sagebionetworks.repo.model.UserProfile")
  checkEquals("string", upProperties$lastName)
  checkEquals("org.sagebionetworks.repo.model.message.Settings", upProperties$notificationSettings)
}

unitTestInstantiateGetAndSet<-function() {
  e<-Evaluation(name="name", description="description")
  checkEquals("name", e@name)
  checkEquals("name", e$name)
  checkEquals("name", propertyValue(e, "name"))
  e@name<-"foo"
  checkEquals("foo", e@name)
  checkEquals("foo", e$name)
  checkEquals("foo", propertyValue(e, "name"))
  checkEquals("description", e@description)
  e$name<-"bar"
  checkEquals("bar", e@name)
  checkEquals("bar", e$name)
  checkEquals("bar", propertyValue(e, "name"))
  checkEquals("description", e@description)
  propertyValue(e, "name")<-"bas"
  checkEquals("bas", e@name)
  checkEquals("bas", e$name)
  checkEquals("bas", propertyValue(e, "name"))
  checkEquals("description", e@description)
}

unitTestListConstructor<-function() {
  e<-Evaluation(list(name="name", description="description"))
  checkEquals("name", e@name)
  checkEquals("description", e$description)
}

unitTestNonPrimitiveField<-function() {
  up<-synapseClient:::UserProfile(ownerId="101")
  settings<-synapseClient:::Settings(sendEmailNotifications=TRUE)
  up$notificationSettings<-settings
  checkEquals("101", up$ownerId)
  checkEquals(TRUE, up$notificationSettings$sendEmailNotifications)
}

unitTestEnumField<-function() {
  submissionStatus<-synapseClient:::SubmissionStatus(id="101", entityId="syn987", status="RECEIVED")
  checkEquals("101", submissionStatus$id)
  checkEquals("syn987", submissionStatus$entityId)
  checkEquals("RECEIVED", submissionStatus$status)
}