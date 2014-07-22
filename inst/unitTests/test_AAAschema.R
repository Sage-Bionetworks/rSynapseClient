#
# Tests for schema parsing / class generating functions
#
#########################################################

unitTestGetImplements<-function() {
  checkTrue(is.null(synapseClient:::getImplements(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.table.Row"))))
  folderSchemaDef<-synapseClient:::readEntityDef("org.sagebionetworks.repo.model.Folder")
  checkTrue(!is.null(synapseClient:::getImplements(folderSchemaDef)))
  checkEquals("org.sagebionetworks.repo.model.Entity", synapseClient:::getImplements(folderSchemaDef))
}

unitTestisVirtual<-function() {
  checkTrue(!synapseClient:::isVirtual(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.table.Row")))
  checkTrue(!synapseClient:::isVirtual(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.Folder")))
  checkTrue(synapseClient:::isVirtual(synapseClient:::readEntityDef("org.sagebionetworks.repo.model.Entity")))
}

unitTestGetPropertyTypes<-function() {
  checkEquals(list(dataFileHandleId, "character"), synapseClient:::getPropertyTypes(which="org.sagebionetworks.repo.model.FileEntity"))
  upProperties<-synapseClient:::getPropertyTypes(which="org.sagebionetworks.repo.model.UserProfile")
  checkEquals("character", upProperties$lastName)
  checkEquals("org.sagebionetworks.repo.model.message.Settings", upProperties$notificationSettings)
}