#
# Tests for schema parsing / class generating functions
#
#########################################################

unitTestGetImplements<-function() {
  checkTrue(is.null(synapseClient:::getImplements(synapseClient:::getSchemaFromCache("UserProfile"))))
  folderSchemaDef<-synapseClient:::getSchemaFromCache("Folder")
  checkTrue(!is.null(synapseClient:::getImplements(folderSchemaDef)))
  checkEquals("org.sagebionetworks.repo.model.Entity", synapseClient:::getImplements(folderSchemaDef)[[1]][[1]])
}

unitTestisVirtual<-function() {
  checkTrue(!synapseClient:::isVirtual(synapseClient:::getSchemaFromCache("Row")))
  checkTrue(!synapseClient:::isVirtual(synapseClient:::getSchemaFromCache("Folder")))
  checkTrue(synapseClient:::isVirtual(synapseClient:::getSchemaFromCache("Entity")))
}

unitTestGetPropertyTypes<-function() {
  checkEquals(list(name="string", concreteType="string"), synapseClient:::getPropertyTypes(which="UserPreference"))
  upProperties<-synapseClient:::getPropertyTypes(which="UserProfile")
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

unitTestMapTypesForListSlots<-function() {
  result<-synapseClient:::mapTypesForListSlots("UserProfile")
  checkEquals(list(emails="character", openIds="character", preferences="UserPreference"), result)
  
  result<-synapseClient:::mapTypesForListSlots("Annotations")
  checkEquals(list(stringAnnos="StringAnnotation", longAnnos="LongAnnotation", doubleAnnos="DoubleAnnotation"), result)
}

unitTestCreateS4ObjectFromList<-function() {
  # simple case: list argument has just primitives
  listRep<-list(name="name", description="description")
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
  checkEquals(synapseClient:::Settings(sendEmailNotifications=T, markEmailedMessagesAsRead=F), up@notificationSettings)
  prefs<-up@preferences
  checkTrue(!is.null(prefs))
  checkEquals(2, length(prefs))
  checkEquals(synapseClient:::UserPreferenceBoolean(name="foo", value=T, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean"), prefs[[1]])
  checkEquals(synapseClient:::UserPreferenceBoolean(name="bar", value=F, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean"), prefs[[2]])
}

unitTestS4RoundTrip<-function() {
  e<-Evaluation(name="name", description="description")
  listRep<-synapseClient:::createListFromS4Object(e)
  checkEquals(e, synapseClient:::createS4ObjectFromList("Evaluation", NULL, listRep))
  
  up<-synapseClient:::UserProfile(
    ownerId="101", 
    emails=list("foo@bar.com", "bar@bas.com"),
    notificationSettings=synapseClient:::Settings(sendEmailNotifications=T, markEmailedMessagesAsRead=F),
    preferences=list(
      synapseClient:::UserPreferenceBoolean(name="foo", value=T, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean"),
      synapseClient:::UserPreferenceBoolean(name="bar", value=F, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean")
    )
  )
  
  listRep<-synapseClient:::createListFromS4Object(up)
  checkEquals(up, synapseClient:::createS4ObjectFromList("UserProfile", NULL, listRep))
  
}

