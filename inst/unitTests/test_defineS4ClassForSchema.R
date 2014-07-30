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

unitTestSchemaTypeFromProperty<-function() {
  upSchema<-synapseClient:::getSchemaFromCache("UserProfile")
  propertySchema<-synapseClient:::getElemSchemaFromS4ClassSchema(upSchema, "lastName")
  checkEquals("string", synapseClient:::schemaTypeFromProperty(propertySchema))
  
  propertySchema<-synapseClient:::getElemSchemaFromS4ClassSchema(upSchema, "notificationSettings")
  checkEquals("org.sagebionetworks.repo.model.message.Settings", 
    synapseClient:::schemaTypeFromProperty(propertySchema))
}

unitTestArraySubSchema<-function() {
  upSchema<-synapseClient:::getSchemaFromCache("UserProfile")
  
  propertySchema<-synapseClient:::getElemSchemaFromS4ClassSchema(upSchema, "emails")
  checkEquals("string", synapseClient:::schemaTypeFromProperty(
      synapseClient:::getArraySubSchema(propertySchema)))
  
  propertySchema<-synapseClient:::getElemSchemaFromS4ClassSchema(upSchema, "preferences")
  checkEquals("org.sagebionetworks.repo.model.UserPreference", 
    synapseClient:::schemaTypeFromProperty(
      synapseClient:::getArraySubSchema(propertySchema)))
  
}

unitTestGetEffectiveSchemaProperties<-function() {
  schema <- synapseClient:::getSchemaFromCache("UserPreferenceBoolean")
  effectiveSchemaProperties <- synapseClient:::getEffectiveSchemaProperties(schema)
  checkTrue(!is.null(effectiveSchemaProperties$name))
  checkTrue(!is.null(effectiveSchemaProperties$value))
}

unitTestCreateS4ObjectFromList<-function() {
  # simple case: list argument has just primitives
  listRep<-list(name="name", description="description")
  e<-synapseClient:::createS4ObjectFromList(list(name="name", description="description"), "Evaluation")
  checkEquals("name", e@name)
  checkEquals("description", e@description)
  
  # list argument has list
  up<-synapseClient:::createS4ObjectFromList( 
    list(ownerId="101", 
      emails=list("foo@bar.com", "bar@bas.com")
    ),"UserProfile")
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  
  # list argument has vector
  up<-synapseClient:::createS4ObjectFromList( 
    list(ownerId="101", 
      emails=c("foo@bar.com", "bar@bas.com")
    ), "UserProfile")
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  
  # list argument has content of embedded S4 object
  up<-synapseClient:::createS4ObjectFromList( 
    list(ownerId="101", 
      emails=list("foo@bar.com", "bar@bas.com"),
      notificationSettings=list(sendEmailNotifications=T, markEmailedMessagesAsRead=F)
    ), "UserProfile")
  checkEquals("101", up@ownerId)
  checkEquals(list("foo@bar.com", "bar@bas.com"), up@emails)
  checkEquals(synapseClient:::Settings(sendEmailNotifications=T, markEmailedMessagesAsRead=F), up@notificationSettings)
  
  # list has array of embedded S4 objects
  up<-synapseClient:::createS4ObjectFromList( 
    list(ownerId="101", 
      emails=list("foo@bar.com", "bar@bas.com"),
      notificationSettings=list(sendEmailNotifications=T, markEmailedMessagesAsRead=F),
      preferences=list(
        list(concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean", name="foo", value=T),
        list(concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean", name="bar", value=F)
        )
    ), "UserProfile")
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
  checkEquals(e, synapseClient:::createS4ObjectFromList(listRep, "Evaluation"))
  
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
  checkEquals(up, synapseClient:::createS4ObjectFromList(listRep, "UserProfile"))
  
}

unitTestMissingS4Field<-function() {
  # note:  there's no 'notificationSettings' field
  up<-synapseClient:::UserProfile(
    ownerId="101", 
    emails=list("foo@bar.com", "bar@bas.com"),
    preferences=list(
      synapseClient:::UserPreferenceBoolean(name="foo", value=T, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean"),
      synapseClient:::UserPreferenceBoolean(name="bar", value=F, concreteType="org.sagebionetworks.repo.model.UserPreferenceBoolean")
    )
  )
  
  listRep<-synapseClient:::createListFromS4Object(up)
  
  # There should not be a list entry for 'notificationSettings'
  checkTrue(is.null(listRep$notificationSettings))
  
  # Also double check that it generates the original UserProfile
  checkEquals(up, synapseClient:::createS4ObjectFromList(listRep, "UserProfile"))
}

unitTestRoundTripWithEnumField<-function() {
  # Note:  'status' is defined as an enum field
  s<-synapseClient:::SubmissionStatus(id="12345", status="SCORED", entityId="syn101")
  li<-synapseClient:::createListFromS4Object(s)
  s2<-synapseClient:::createS4ObjectFromList(li, "SubmissionStatus")
  checkEquals(s,s2)
}

