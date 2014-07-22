#
# Tests for schema parsing / class generating functions
#
#########################################################

unitTestGetImplements<-function() {
  checkTrue(is.null(getImplements(readEntityDef("org.sagebionetworks.repo.model.table.Row"))))
  folderSchemaDef<-readEntityDef("org.sagebionetworks.repo.model.Folder")
  checkTrue(!is.null(getImplements(folderSchemaDef)))
  checkEquals("org.sagebionetworks.repo.model.Entity", getImplements(folderSchemaDef))
}

unitTestisVirtual<-function() {
  checkTrue(!isVirtual(readEntityDef("org.sagebionetworks.repo.model.table.Row")))
  checkTrue(!isVirtual(readEntityDef("org.sagebionetworks.repo.model.Folder")))
  checkTrue(isVirtual(readEntityDef("org.sagebionetworks.repo.model.Entity")))
}

unitTestGetPropertyTypes<-function() {
  checkEquals(list(dataFileHandleId, "character"), getPropertyTypes(which="org.sagebionetworks.repo.model.FileEntity"))
  upProperties<-getPropertyTypes(which="org.sagebionetworks.repo.model.UserProfile")
  checkEquals("character", upProperties$lastName)
  checkEquals("org.sagebionetworks.repo.model.message.Settings", upProperties$notificationSettings)
}