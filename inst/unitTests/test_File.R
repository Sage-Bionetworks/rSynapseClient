# These are the test_Locationable tests adapted for Files
#
###############################################################################

.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2L)
}

.tearDown <-
    function()
{
  options(warn = synapseClient:::.getCache("oldWarn"))
  if(!is.null(name <- synapseClient:::.getCache("detachMe"))){
    detach(name)
    synapseClient:::.deleteCache('detachMe')
  }
}

unitTestSimpleConstructor<-function() {
  # test that it works to give ONLY a file path
  file<-File("/path/to/file")
  
  # now check that S4 is not confused by annotation parameters
  file<-File("/path/to/file", annotName=FALSE)
  checkTrue(file@synapseStore) # this is the default and S4 should mistake 'annotName' for 'synapseStore'
  checkEquals("FALSE", annotValue(file, "annotName")) # the extra param should become an annotation on the file
}

unitTestObjectConstructor<-function() {
  anObject<-list(foo="bar")
  file<-File(anObject)
  checkTrue(synapseClient:::hasObjects(file))
  checkEquals(anObject, getObject(file, "anObject"))
  
# TODO this does NOT work, as including the ellipsis in the method signature
# causes parent.frame() not to work in File("ANY")
#   file<-File(anObject, parentId="syn124", annotName="annot value")
#   checkTrue(synapseClient:::hasObjects(file))
#   checkEquals(anObject, getObject(file, "anObject"))
#   checkEquals("syn124", propertyValue(file, "parentId"))
#   checkEquals("annot value", annotValue(file, "annotName"))
 }

unitTestConstructor<-function() {
  description<-"this describes my File"
  versionComment<-"this is the first version"
  annotName<-"anAnnotation"
  annotValue<-"assigned annotation value"
  file<-File(
    "/path/to/file", 
    TRUE, 
    description=description, 
    versionComment=versionComment,
    anAnnotation=annotValue)
  
  checkEquals("/path/to/file", file@filePath)
  checkEquals(TRUE, file@synapseStore)
  checkEquals(description, propertyValue(file, "description"))
  checkEquals(description, synapseClient:::synAnnotGetMethod(file, "description"))
  checkEquals(versionComment, propertyValue(file, "versionComment"))
  checkEquals("file", propertyValue(file, "name"))
  checkEquals(annotValue, annotValue(file, annotName))
  checkEquals(annotValue, synapseClient:::synAnnotGetMethod(file, annotName))
  checkTrue(!synapseClient:::hasObjects(file))
  checkEquals("org.sagebionetworks.repo.model.FileEntity", propertyValue(file, "entityType"))
}

unitTestListConstructor<-function() {
  description<-"this describes my File"
  annotValue<-"assigned annotation value"
  file<-FileListConstructor(list(description=description, anAnnotation=annotValue))
  checkEquals(description, propertyValue(file, "description"))
  checkEquals(annotValue, annotValue(file, "anAnnotation"))
}

unitTestSynAnnotSetMethod<-function() {
  file<-new("File")
  description<-"this describes my File"
  versionComment<-"this is the first version"
  annotName<-"anAnnotation"
  annotValue<-"assigned annotation value"
  file<-synapseClient:::synAnnotSetMethod(file, "description", description)
  checkEquals(description, synapseClient:::synAnnotGetMethod(file, "description"))
  file<-synapseClient:::synAnnotSetMethod(file, annotName, annotValue)
  checkEquals(annotValue, synapseClient:::synAnnotGetMethod(file, annotName))
}

unitTestFileUtilities<-function() {
  file<-new("File")
  file@fileHandle<-list(concreteType="S3FileHandle")
  checkTrue(!synapseClient:::isExternalFileHandle(file@fileHandle))
  checkTrue(!synapseClient:::fileHasFileHandleId(file))
  checkTrue(!synapseClient:::fileHasFilePath(file))
  file@fileHandle<-list(concreteType="ExternalFileHandle")
  file@fileHandle$id<-"1234"
  file@filePath<-"/path/to/file"
  checkTrue(synapseClient:::isExternalFileHandle(file@fileHandle))
  checkTrue(synapseClient:::fileHasFileHandleId(file))
  checkTrue(synapseClient:::fileHasFilePath(file))
}

unitTestValidateFile<-function() {
  file<-new("File")
  
  synapseClient:::validdateFile(file)
  file@synapseStore<-FALSE
  result<-try(synapseClient:::validdateFile(file), silent=T)
  checkEquals("try-error", class(result))
  
  file@fileHandle<-list(concreteType="ExternalFileHandle")
  file@synapseStore<-TRUE
  result<-try(synapseClient:::validdateFile(file), silent=T)
  checkEquals("try-error", class(result))
}

unitTestAddObject <-
    function()
{
  own <- new("File")
  
  checkTrue(!synapseClient:::hasObjects(own))

  foo<-diag(10)
  copy <- addObject(own, foo)
  checkEquals(foo, getObject(copy, "foo"))
  checkTrue(synapseClient:::hasObjects(copy))
  
  copy<-renameObject(copy, "foo", "boo")
  checkEquals(foo, getObject(copy, "boo"))
  result<-try(getObject(copy, "foo"),silent=T)
  checkTrue(class(result)=="try-error")
  
  deleted<-deleteObject(copy, "boo")
  result<-try(getObject(deleted, "boo"),silent=T)
  checkTrue(class(result)=="try-error")
  checkTrue(!synapseClient:::hasObjects(own))
}


unitTestGetObject <-
    function()
{
  own <- new("File")
  foo <- "boo"
  own<-addObject(own, foo)
  checkEquals(getObject(own, "foo"), "boo")
}


