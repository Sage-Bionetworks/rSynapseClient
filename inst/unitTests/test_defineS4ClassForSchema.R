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

unitTestListConstructorNonPrimitiveField<-function() {
  # simple case:  just primitive fields
  up<-synapseClient:::UserProfile(list(ownerId="101", etag="10101010"))
  checkEquals("101", up$ownerId)
  checkEquals("10101010", up$etag)
  # now try more complicated fields
  up<-synapseClient:::UserProfile(list(ownerId="101", etag="10101010", notificationSettings=list(sendEmailNotifications=TRUE)))
  checkEquals("101", up$ownerId)
  checkEquals("10101010", up$etag)
  checkEquals(TRUE, up$notificationSettings$sendEmailNotifications)
}

unitTestEnumField<-function() {
  submissionStatus<-synapseClient:::SubmissionStatus(id="101", entityId="syn987", status="RECEIVED")
  checkEquals("101", submissionStatus$id)
  checkEquals("syn987", submissionStatus$entityId)
  checkEquals("RECEIVED", submissionStatus$status)
}

unitTestMapTypesForListSlots<-function() {
  schema<-synapseClient:::readEntityDef("org.sagebionetworks.repo.model.UserProfile")
  result<-mapTypesForListSlots(schema)
  checkEquals(list(emails="string", openIds="string"))
  
  schema<-synapseClient:::readEntityDef("org.sagebionetworks.repo.model.annotations.Annotations")
  result<-mapTypesForListSlots(schema)
  checkEquals(list(stringAnnos="StringAnnotation", longAnnos="LongAnnotation", doubleAnnos="DoubleAnnotation"))
}

unitTestCreateS4ObjectFromList<-function() {
  # simple case: list argument has just primitives
  e<-synapseClient:::createS4ObjectFromList("Evaluation", NULL, list(name="name", description="description"))
  checkEquals("name", e@name)
  checkEquals("description", e@description)
  
  # list argument has list
  up<-synapseClient:::createS4ObjectFromList("UserProfile", NULL, 
    list(ownerId="101", 
      emails=list("foo@bar.com", "bar@bas.com")
    ))
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  
  # list argument has vector
  up<-synapseClient:::createS4ObjectFromList("UserProfile", NULL, 
    list(ownerId="101", 
      emails=c("foo@bar.com", "bar@bas.com")
    ))
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  
  # list argument has content of embedded S4 object
  up<-synapseClient:::createS4ObjectFromList("UserProfile", NULL, 
    list(ownerId="101", 
      emails=list("foo@bar.com", "bar@bas.com"),
      notificationSettings=list(sendEmailNotifications=T, markEmailedMessagesAsRead=F)
    ))
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  checkEquals(synapseClient:::Settings(sendEmailNotifications=T, markEmailedMessagesAsRead=F), up@notificationSettings)
  
  # list has array of embedded S4 objects
  up<-synapseClient:::createS4ObjectFromList("UserProfile", NULL, 
    list(ownerId="101", 
      emails=list("foo@bar.com", "bar@bas.com"),
      notificationSettings=list(sendEmailNotifications=T, markEmailedMessagesAsRead=F),
      preferences=list(
        list(concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean", name="foo", value=T),
        list(concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean", name="bar", value=F)
        )
    ))
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  checkEquals(Settings(sendEmailNotifications=T, markEmailedMessagesAsRead=F), up@notificationSettings)
  prefs<-up@preferences
  checkTrue(!is.null(prefs))
  checkEquals(2, length(prefs))
  checkEquals(UserPreferencesBoolean(name="foo", value=T, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean"), prefs[[1]])
  checkEquals(UserPreferencesBoolean(name="bar", value=F, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean"), prefs[[2]])
  }
