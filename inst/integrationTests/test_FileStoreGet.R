#
# Integration tests for synGet, synStore and related functions
#

.setUp <- function() {
  ## create a project to fill with entities
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  # initialize this list
  synapseClient:::.setCache("foldersToDelete", list())
}

.tearDown <- function() {
  ## delete the test project
  deleteEntity(synapseClient:::.getCache("testProject"))
  
  foldersToDelete<-synapseClient:::.getCache("foldersToDelete")
  for (folder in foldersToDelete) {
    if (file.exists(folder)) {
      unlink(folder, recursive=TRUE)
    }
  }
  synapseClient:::.setCache("foldersToDelete", list())
  
  ## in the case that we have 'mocked' hasUnfulfilledAccessRequirements, this restores the original function
  if (!is.null(synapseClient:::.getCache("hasUnfulfilledAccessRequirementsIsOverRidden"))) {
    assignInNamespace("hasUnfulfilledAccessRequirements", attr(synapseClient:::hasUnfulfilledAccessRequirements, "origDef"), "synapseClient")
    synapseClient:::.setCache("hasUnfulfilledAccessRequirementsIsOverRidden", NULL)
  }
  ## in the case that we have 'mocked' createLockAccessRequirement, this restores the original function
  if (!is.null(synapseClient:::.getCache("createLockAccessRequirementIsOverRidden"))) {
    assignInNamespace(".createLockAccessRequirement", attr(synapseClient:::.createLockAccessRequirement, "origDef"), "synapseClient")
    assignInNamespace(".hasAccessRequirement", attr(synapseClient:::.hasAccessRequirement, "origDef"), "synapseClient")
    synapseClient:::.setCache("createLockAccessRequirementIsOverRidden", NULL)
  }
  
}

createFile<-function(content, filePath) {
  if (missing(content)) content<-"this is a test"
  if (missing(filePath)) filePath<- tempfile()
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
  filePath
}

integrationTestMakeRestricted<-function() {
  # mock services to create restriction and check it
  myHasAccessRequirement<-function(entityId) {F}
  attr(myHasAccessRequirement, "origDef") <- synapseClient:::.hasAccessRequirement
  assignInNamespace(".hasAccessRequirement", myHasAccessRequirement, "synapseClient")

  myCreateLockAccessRequirement<-function(entityId) {synapseClient:::.setCache("createLockAccessRequirementWasInvoked", TRUE)}
  attr(myCreateLockAccessRequirement, "origDef") <- synapseClient:::.createLockAccessRequirement
  assignInNamespace(".createLockAccessRequirement", myCreateLockAccessRequirement, "synapseClient")
  synapseClient:::.setCache("createLockAccessRequirementWasInvoked", NULL)
  synapseClient:::.setCache("createLockAccessRequirementIsOverRidden", TRUE)
  
  # now create a file and make sure it is restricted
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create a File
  filePath<- createFile()
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  # store, indicating it is restricted
  storedFile<-synStore(file, isRestricted=TRUE)
  id<-propertyValue(storedFile, "id")
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  
  checkTrue(synapseClient:::.getCache("createLockAccessRequirementWasInvoked"))
}

integrationTestUpdateProvenance <- function() {
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  updateProvenanceIntern(project)
}

# this embodies the use case crafted by D. Burdick: https://github.com/Sage-Bionetworks/Synapse-Repository-Services/blob/develop/tools/provenance/Provenance_wiki_example.R
updateProvenanceIntern<-function(project) {
  projectId <- propertyValue(project, "id")
  
  # create some resources to use
  myAnnotationFilePath<-createFile() # replaces "/tmp/myAnnotationFile.txt"
  myAnnotationFile <- synStore(File(myAnnotationFilePath, name="myAnnotationFile.txt", parentId=projectId))
  myRawDataFilePath<-createFile() # replaces "/tmp/myRawDataFile.txt"
  myRawDataFile <- synStore(File(myRawDataFilePath, name="myRawDataFile.txt", parentId=projectId))
  myScriptFilePath <- createFile() # replaces "/tmp/myScript.txt"
  myScript <- synStore(File(myScriptFilePath, name="myScript.txt", parentId=projectId))
  
  ## Example 1
  # Here we explicitly define an "Activity" object, then we create the myOutputFile
  # entity and use the activity to describe how it was made.
  
  activity<-createEntity(Activity(list(name="Manual Curation")))
  myOutputFilePath<-createFile() # replaces "/tmp/myOutputFile.txt"
  myOutputFile <- synStore(File(myOutputFilePath, name="myOutputFile.txt", parentId=projectId), activity)
    
  ## Example 2
  # Create an activity implicitly by describing it in the synStore call for myOutputFile.
  # Here the combination of myAnnotationFile and myRawDataFile are used to generate myOutputFile.
  
  myOutputFile <- synStore(File(myOutputFilePath, name="myOutputFile.txt", parentId=projectId),
  used=list(myAnnotationFile, myRawDataFile),
    activityName="Manual Annotation of Raw Data",
    activityDescription="...")
  
  # the File should be a new version
  checkEquals(2, propertyValue(myOutputFile, "versionNumber"))
  
  # verify that the provenance can be retrieved
  retrievedFile<-synGet(propertyValue(myOutputFile, "id"), downloadFile=FALSE)
  gb<-generatedBy(retrievedFile)
  checkEquals("Manual Annotation of Raw Data", propertyValue(gb, "name"))
  checkEquals("...", propertyValue(gb, "description"))
  used<-propertyValue(gb, "used")
  checkEquals("org.sagebionetworks.repo.model.provenance.UsedEntity", used[[1]]$concreteType)
  checkEquals(FALSE, used[[1]]$wasExecuted)
  targetIds<-c(used[[1]]$reference$targetId, used[[2]]$reference$targetId)
  checkTrue(any(propertyValue(myAnnotationFile, "id")==targetIds))
  checkEquals("org.sagebionetworks.repo.model.provenance.UsedEntity", used[[2]]$concreteType)
  checkEquals(FALSE, used[[2]]$wasExecuted)
  checkTrue(any(propertyValue(myRawDataFile, "id")==targetIds))
  
  ## Example 3
  # Create a provenance record for myOutputFile that uses resources external to Synapse.
  # Here we are describing the execution of Script.py (stored in GitHub)
  # where myAnnotationFile and a raw data file from GEO are used as inputs
  # to generate myOutputFile with the "used" list
  
  myOutputFile <- synStore(File(myOutputFilePath, name="myOutputFile.txt", parentId=projectId), 
    used=list(list(name="Script.py", url="https://raw.github.com/.../Script.py", wasExecuted=T),
      list(entity=myAnnotationFile, wasExecuted=F),
      list(name="GSM349870.CEL", url="http://www.ncbi.nlm.nih.gov/geo/download/...", wasExecuted=F)),
    activityName="Scripted Annotation of Raw Data",
    activityDescription="To execute run: python Script.py [Annotation] [CEL]")
  
  # the File should be a new version
  checkEquals(3, propertyValue(myOutputFile, "versionNumber"))

  # verify that the provenance can be retrieved
  retrievedFile<-synGet(propertyValue(myOutputFile, "id"), downloadFile=FALSE)
  gb<-generatedBy(retrievedFile)
  checkEquals("Scripted Annotation of Raw Data", propertyValue(gb, "name"))
  checkEquals("To execute run: python Script.py [Annotation] [CEL]", propertyValue(gb, "description"))
  used<-propertyValue(gb, "used")
  checkEquals(3, length(used))
  
  
  ## Example 4
  # Create an activity describing the execution of myScript with myRawDataFile as the input
  
  activity<-createEntity(Activity(list(name="Process data and plot",
        used=list(list(entity=myScript, wasExecuted=T),
          list(entity=myRawDataFile, wasExecuted=F)))))
  
  # Record that the script's execution generated two output files, and upload those files
  
  myOutputFile <- synStore(File(myOutputFilePath, name="myOutputFile.txt", parentId=projectId), activity)
  
  # the File should be a new version
  checkEquals(4, propertyValue(myOutputFile, "versionNumber"))
  # ... and should have the activity as its 'generatedBy'
  checkEquals(propertyValue(activity, "id"), propertyValue(generatedBy(myOutputFile), "id"))
  
  # create another output created by the same activity
  myPlotFilePath <-createFile() # replaces "/tmp/myPlot.png"
  myPlot <- synStore(File(myPlotFilePath, name="myPlot.png", parentId=projectId), activity)
  
  # verify that the provenance can be retrieved
  retrievedFile<-synGet(propertyValue(myOutputFile, "id"), downloadFile=FALSE)
  gb<-generatedBy(retrievedFile)
  checkEquals("Process data and plot", propertyValue(gb, "name"))
  used<-propertyValue(gb, "used")
  checkEquals(2, length(used))
}

# Per SYNR-501, if you update an entity the new version
# should not carry forward the provenance record of the old one
integrationTestReviseWithoutProvenance <- function() {
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  projectId <- propertyValue(project, "id")
  
  myOutputFilePath<-createFile()
  
  myOutputFile <- synStore(File(myOutputFilePath, name="myOutputFile.txt", parentId=projectId), 
    used=list(list(name="Script.py", url="https://raw.github.com/.../Script.py", wasExecuted=T),
      list(name="GSM349870.CEL", url="http://www.ncbi.nlm.nih.gov/geo/download/...", wasExecuted=F)),
    activityName="Scripted Annotation of Raw Data",
    activityDescription="To execute run: python Script.py [Annotation] [CEL]")
  
  # its the first new version
  checkEquals(1, propertyValue(myOutputFile, "versionNumber"))
  
  # verify that the provenance can be retrieved
  retrievedFile<-synGet(propertyValue(myOutputFile, "id"))
  gb<-generatedBy(retrievedFile)
  checkEquals("Scripted Annotation of Raw Data", propertyValue(gb, "name"))
  checkEquals("To execute run: python Script.py [Annotation] [CEL]", propertyValue(gb, "description"))
  used<-propertyValue(gb, "used")
  checkEquals(2, length(used))
  
  # now modify the file
  createFile(content="some other content", getFileLocation(retrievedFile))
  
  retrievedFile<-synStore(retrievedFile)
  
  # it's the second new version
  checkEquals(2, propertyValue(retrievedFile, "versionNumber"))
  
  # since we specified no provenance in synStore, there should be none
  checkTrue(is.null(generatedBy(retrievedFile)))
  
  # now modify again
  createFile(content="yet some other, other content", getFileLocation(retrievedFile))
  retrievedFile<-synStore(retrievedFile, used="syn101")
  
  # its the third new version
  checkEquals(3, propertyValue(retrievedFile, "versionNumber"))
  
  # the specified provenance is there!
  gb<-generatedBy(retrievedFile)
  used<-propertyValue(gb, "used")
  checkEquals(1, length(used))
  
}

integrationTestCacheMapRoundTrip <- function() {
  fileHandleId<-"101"
  filePath<- createFile()
  filePath2<- createFile()
  
 
  synapseClient:::addToCacheMap(fileHandleId, filePath)
  synapseClient:::addToCacheMap(fileHandleId, filePath2)
  content<-synapseClient:::getCacheMapFileContent(fileHandleId)
  checkEquals(2, length(content))
  checkTrue(any(normalizePath(filePath, winslash="/")==names(content)))
  checkTrue(any(normalizePath(filePath2, winslash="/")==names(content)))
  checkEquals(synapseClient:::.formatAsISO8601(file.info(filePath)$mtime), synapseClient:::getFromCacheMap(fileHandleId, filePath))
  checkEquals(synapseClient:::.formatAsISO8601(file.info(filePath2)$mtime), synapseClient:::getFromCacheMap(fileHandleId, filePath2))
  checkTrue(synapseClient:::localFileUnchanged(fileHandleId, filePath))
  checkTrue(synapseClient:::localFileUnchanged(fileHandleId, filePath2))
  
  # now clean up
  scheduleCacheFolderForDeletion(fileHandleId)
}

scheduleFolderForDeletion<-function(folder) {
  folderList<-synapseClient:::.getCache("foldersToDelete")
  folderList[[length(folderList)+1]]<-folder
  synapseClient:::.setCache("foldersToDelete", folderList)
}

scheduleCacheFolderForDeletion<-function(fileHandleId) {
  if (is.null(fileHandleId)) stop("In scheduleCacheFolderForDeletion fileHandleId must not be null")
  scheduleFolderForDeletion(synapseClient:::defaultDownloadLocation(fileHandleId))
}

integrationTestMetadataRoundTrip <- function() {
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create a file to be uploaded
  filePath<- createFile()
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  checkTrue(!is.null(propertyValue(file, "name")))
  checkEquals(propertyValue(project, "id"), propertyValue(file, "parentId"))
  
  # now store it
  storedFile<-synStore(file)
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  
  metadataOnly<-synGet(propertyValue(storedFile, "id"),downloadFile=F)
  metadataOnly<-synapseClient:::synAnnotSetMethod(metadataOnly, "annot", "value")
  storedMetadata<-synStore(metadataOnly, forceVersion=F)
  
  checkEquals("value", synapseClient:::synAnnotGetMethod(storedMetadata, "annot"))
  
  checkEquals(1, propertyValue(metadataOnly, "versionNumber"))
  
  # now store again, but force a version update
  storedMetadata<-synStore(storedMetadata) # default is forceVersion=T
  
  retrievedMetadata<-synGet(propertyValue(storedFile, "id"),downloadFile=F)
  checkEquals(2, propertyValue(retrievedMetadata, "versionNumber"))
  # no file location since we haven't downloaded anything
  checkEquals(character(0), getFileLocation(retrievedMetadata))
  
  # of course we should still be able to get the original version
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=F)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  # ...whether or not we download the file
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=T)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  # file location is NOT missing
  checkTrue(length(getFileLocation(originalVersion))>0)
}

integrationTestGovernanceRestriction <- function() {
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create a File
  filePath<- createFile()
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  storedFile<-synStore(file)
  id<-propertyValue(storedFile, "id")
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  
  # mock Governance restriction
  myHasUnfulfilledAccessRequirements<-function(id) {TRUE} # return TRUE, i.e. yes, there are unfulfilled access requiremens
  attr(myHasUnfulfilledAccessRequirements, "origDef") <- synapseClient:::hasUnfulfilledAccessRequirements
  assignInNamespace("hasUnfulfilledAccessRequirements", myHasUnfulfilledAccessRequirements, "synapseClient")
  synapseClient:::.setCache("hasUnfulfilledAccessRequirementsIsOverRidden", TRUE)
  
  # try synGet with downloadFile=F, load=F, should be OK
  synGet(id, downloadFile=F, load=F)
  
  # try synGet with downloadFile=T, should NOT be OK
  result<-try(synGet(id, downloadFile=T, load=F), silent=TRUE)
  checkEquals("try-error", class(result))
  
  # try synGet with load=T, should NOT be OK
  result<-try(synGet(id, downloadFile=F, load=T), silent=TRUE)
  checkEquals("try-error", class(result))

}

# utility to change the timestamp on a file
touchFile<-function(location) {
  orginalTimestamp<-synapseClient:::lastModifiedTimestamp(location)
  Sys.sleep(1.0) # make sure new timestamp will be different from original
  connection<-file(location)
  # result<-paste(readLines(connection), collapse="\n")
  originalSize<-file.info(location)$size
  originalMD5<-tools::md5sum(location)
  result<-readChar(connection, originalSize)
  close(connection)
  connection<-file(location)
  # writeLines(result, connection)
  writeChar(result, connection, eos=NULL)
  close(connection)  
  # check that we indeed modified the time stamp on the file
  newTimestamp<-synapseClient:::lastModifiedTimestamp(location)
  checkTrue(newTimestamp!=orginalTimestamp)
  # check that the file has not been changed
  checkEquals(originalMD5, tools::md5sum(location))
}

checkFilesEqual<-function(file1, file2) {
  checkEquals(normalizePath(file1, winslash="/"), normalizePath(file2, winslash="/"))
}

integrationTestCreateOrUpdate<-function() {
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  createOrUpdateIntern(project)
  
}

createOrUpdateIntern<-function(project) {
  filePath<- createFile()
  name<-"createOrUpdateTest"
  pid<-propertyValue(project, "id")
  file<-File(filePath, name=name, parentId=pid)
  file<-synStore(file)
  
  filePath2<- createFile()
  file2<-File(filePath2, name=name, parentId=pid)
  # since createOrUpdate=T is the default, this should update 'file' rather than create a new one
  file2<-synStore(file2)
  checkEquals(propertyValue(file, "id"), propertyValue(file2, "id"))
  checkEquals(2, propertyValue(file2, "versionNumber")) # this is the test for SYNR-429
  
  filePath3 <- createFile()
  file3<-File(filePath3, name=name, parentId=pid)
  result<-try(synStore(file3, createOrUpdate=F), silent=T)
  checkEquals("try-error", class(result))
  
  # check an entity with no parent
  project2<-Project(name=propertyValue(project, "name"))
  project2<-synStore(project2)
  checkEquals(propertyValue(project2, "id"), pid)
  
  project3<-Project(name=propertyValue(project, "name"))
  result<-try(synStore(project3, createOrUpdate=F), silent=T)
  checkEquals("try-error", class(result))
}

#
# This code exercises the file services underlying upload/download to/from an entity
#
integrationTestRoundtrip <- function()
{
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  roundTripIntern(project)
}

roundTripIntern<-function(project) {  
  # create a file to be uploaded
  filePath<- createFile(content="Some content")
  md5_version_1<- as.character(tools::md5sum(filePath))
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  checkTrue(!is.null(propertyValue(file, "name")))
  checkEquals(propertyValue(project, "id"), propertyValue(file, "parentId"))
  
  # now store it
  storedFile<-synStore(file)
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  
  # check that it worked
  checkTrue(!is.null(storedFile))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  checkEquals(propertyValue(project, "id"), propertyValue(storedFile, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(storedFile, "name"))
  checkEquals(filePath, getFileLocation(storedFile))
  checkEquals(synapseStore, storedFile@synapseStore)
  
  # check that cachemap entry exists
  fileHandleId<-storedFile@fileHandle$id
  cachePath<-sprintf("%s/.cacheMap", synapseClient:::defaultDownloadLocation(fileHandleId))
  checkTrue(file.exists(cachePath))
  modifiedTimeStamp<-synapseClient:::getFromCacheMap(fileHandleId, filePath)
  checkTrue(!is.null(modifiedTimeStamp))
  
  # now download it.  This will pull a new copy into the cache
  downloadedFile<-synGet(id)
  downloadedFilePathInCache<-getFileLocation(downloadedFile)
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(propertyValue(project, "id"), propertyValue(downloadedFile, "parentId"))
  checkEquals(synapseStore, downloadedFile@synapseStore)
  checkTrue(length(getFileLocation(downloadedFile))>0)
  
  # compare MD-5 checksum of filePath and downloadedFile@filePath
  md5_version_1_retrieved <- as.character(tools::md5sum(getFileLocation(downloadedFile)))
  checkEquals(md5_version_1, md5_version_1_retrieved)
  
  checkEquals(storedFile@fileHandle, downloadedFile@fileHandle)
  
  # test synStore of retrieved entity, no change to file
  modifiedTimeStamp<-synapseClient:::getFromCacheMap(fileHandleId, downloadedFilePathInCache)
  checkTrue(!is.null(modifiedTimeStamp))
  Sys.sleep(1.0)
  updatedFile <-synStore(downloadedFile, forceVersion=F)
  # the file handle should be the same
  checkEquals(fileHandleId, propertyValue(updatedFile, "dataFileHandleId"))
  # there should be no change in the time stamp.
  checkEquals(modifiedTimeStamp, synapseClient:::getFromCacheMap(fileHandleId, downloadedFilePathInCache))
  # we are still on version 1
  checkEquals(1, propertyValue(updatedFile, "versionNumber"))

  #  test synStore of retrieved entity, after changing file
  # modify the file, byt making a new one then copying it over
  createFile(content="Some other content", filePath=downloadedFilePathInCache)
  md5_version_2<- as.character(tools::md5sum(downloadedFilePathInCache))
  checkTrue(md5_version_1!=md5_version_2)
  
  updatedFile2 <-synStore(updatedFile, forceVersion=F)
  scheduleCacheFolderForDeletion(updatedFile2@fileHandle$id)
  # fileHandleId is changed
  checkTrue(fileHandleId!=propertyValue(updatedFile2, "dataFileHandleId"))
  # we are now on version 2
  checkEquals(2, propertyValue(updatedFile2, "versionNumber"))
  
  # of course we should still be able to get the original version
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=F)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  # ...whether or not we download the file
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=T)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  md5_version_1_retrieved_again <- as.character(tools::md5sum(getFileLocation(originalVersion)))
  # make sure the right version was retrieved (SYNR-447)
  checkEquals(md5_version_1, md5_version_1_retrieved_again)
  
  # get the current version of the file, but download it to a specified location
  # (make the location unique)
  specifiedLocation<-file.path(tempdir(), "subdir")
  if (file.exists(specifiedLocation)) unlink(specifiedLocation, recursive=T) # in case it already exists
  checkTrue(dir.create(specifiedLocation))
  scheduleFolderForDeletion(specifiedLocation)
  downloadedToSpecified<-synGet(id, downloadLocation=specifiedLocation)
  checkFilesEqual(specifiedLocation, dirname(getFileLocation(downloadedToSpecified)))
  fp<-getFileLocation(downloadedToSpecified)
  checkEquals(fp, file.path(specifiedLocation, basename(filePath)))
  checkTrue(file.exists(fp))
  checkEquals(md5_version_2, as.character(tools::md5sum(fp)))
  touchFile(fp)

  timestamp<-synapseClient:::lastModifiedTimestamp(fp)
  
  # download again with the 'keep.local' choice
  downloadedToSpecified<-synGet(id, downloadLocation=specifiedLocation, ifcollision="keep.local")
  # file path is the same, timestamp should not change
  Sys.sleep(1.0)
  checkEquals(getFileLocation(downloadedToSpecified), fp)
  checkEquals(timestamp, synapseClient:::lastModifiedTimestamp(fp))
  
  # download again with the 'overwrite' choice
  downloadedToSpecified<-synGet(id, downloadLocation=specifiedLocation, ifcollision="overwrite.local")
  checkEquals(getFileLocation(downloadedToSpecified), fp)
  # timestamp SHOULD change
  checkTrue(timestamp!=synapseClient:::lastModifiedTimestamp(fp)) 

  touchFile(fp)
  Sys.sleep(1.0)
  # download with the 'keep both' choice (the default)
  downloadedToSpecified<-synGet(id, downloadLocation=specifiedLocation)
  # there should be a second file
  checkTrue(getFileLocation(downloadedToSpecified)!=fp)
  # it IS in the specified directory
  checkFilesEqual(specifiedLocation, dirname(getFileLocation(downloadedToSpecified)))
  
  # delete the cached file
  deleteEntity(downloadedFile)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", storedFile@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
  handleUri<-sprintf("/fileHandle/%s", updatedFile2@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
}


# test that legacy *Entity based methods work on File objects
integrationTestAddToNewFILEEntity <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  pid <- propertyValue(project, "id")
  checkTrue(!is.null(pid))
  filePath<- createFile()
  file<-synapseClient:::FileListConstructor(list(parentId=pid))
  file<-addFile(file, filePath)
  storedFile<-storeEntity(file)
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  
  checkTrue(!is.null(storedFile))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  checkEquals(pid, propertyValue(storedFile, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(storedFile, "name"))
  checkEquals(filePath, getFileLocation(storedFile))
  checkEquals(TRUE, storedFile@synapseStore) # this is the default
  checkTrue(!is.null(propertyValue(storedFile, "dataFileHandleId")))
  
  gotEntity<-getEntity(storedFile) # get metadata, don't download file
  
  checkTrue(!is.null(gotEntity))
  id<-propertyValue(gotEntity, "id")
  checkTrue(!is.null(id))
  checkEquals(pid, propertyValue(gotEntity, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(gotEntity, "name"))
  checkTrue(!is.null(propertyValue(gotEntity, "dataFileHandleId")))
  checkTrue(length(getFileLocation(gotEntity))==0) # empty since it hasn't been downloaded
  
  # test update of metadata
  annotValue(gotEntity, "foo")<-"bar"
  updatedEntity<-updateEntity(gotEntity)
  gotEntity<-getEntity(updatedEntity)
  checkEquals("bar", annotValue(gotEntity, "foo"))
  
  downloadedFile<-downloadEntity(id)
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(pid, propertyValue(downloadedFile, "parentId"))
  checkEquals(TRUE, downloadedFile@synapseStore) # this is the default
  checkTrue(!is.null(getFileLocation(downloadedFile)))
  
  # compare MD-5 checksum of filePath and downloadedFile@filePath
  origChecksum<- as.character(tools::md5sum(filePath))
  downloadedChecksum <- as.character(tools::md5sum(getFileLocation(downloadedFile)))
  checkEquals(origChecksum, downloadedChecksum)
  
  checkEquals(storedFile@fileHandle, downloadedFile@fileHandle)
  
  # check that downloading a second time doesn't retrieve again
  timeStamp<-synapseClient:::lastModifiedTimestamp(getFileLocation(downloadedFile))
  Sys.sleep(1.0)
  downloadedFile<-downloadEntity(id)
  checkEquals(timeStamp, synapseClient:::lastModifiedTimestamp(getFileLocation(downloadedFile)))
 
  # delete the file
  deleteEntity(downloadedFile)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", storedFile@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
}

# test that legacy *Entity based methods work on File objects, cont.
integrationTestReplaceFile<-function() {
    project <- synapseClient:::.getCache("testProject")
    filePath<- createFile()
    file<-synapseClient:::FileListConstructor(list(parentId=propertyValue(project, "id")))
    file<-addFile(file, filePath)
    # replace storeEntity with createEntity
    storedFile<-createEntity(file)
    scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
    
    # now getEntity, add a different file, store, retrieve
    gotEntity<-getEntity(storedFile) # get metadata, don't download file
    newFile<-system.file("DESCRIPTION", package = "synapseClient")
    gotEntity<-addFile(gotEntity, newFile)
    newStoredFile<-storeEntity(gotEntity)
    scheduleCacheFolderForDeletion(newStoredFile@fileHandle$id)
    
    downloadedFile<-downloadEntity(newStoredFile)
 
    # compare MD-5 checksum of filePath and downloadedFile@filePath
    origChecksum<- as.character(tools::md5sum(newFile))
    downloadedChecksum <- as.character(tools::md5sum(getFileLocation(downloadedFile)))
    checkEquals(origChecksum, downloadedChecksum)
    
    checkEquals(newStoredFile@fileHandle, downloadedFile@fileHandle)
    
    # delete the file
    deleteEntity(downloadedFile)
    # clean up downloaded file
    handleUri<-sprintf("/fileHandle/%s", newStoredFile@fileHandle$id)
    synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
  }



integrationTestLoadEntity<-function() {
  project <- synapseClient:::.getCache("testProject")
  filePath<- createFile()
  file<-synapseClient:::FileListConstructor(list(parentId=propertyValue(project, "id")))
  dataObject<-list(a="A", b="B", c="C")
  file<-addObject(file, dataObject, "dataObjectName")
  storedFile<-createEntity(file)
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  
  loadedEntity<-loadEntity(propertyValue(storedFile, "id"))
  
  checkEquals(dataObject, getObject(loadedEntity, "dataObjectName"))
  
  # can load from an entity as well as from an ID
  loadedEntity2<-loadEntity(storedFile)
  checkEquals(dataObject, getObject(loadedEntity2, "dataObjectName"))
  
  checkEquals("dataObjectName", listObjects(loadedEntity2))
  
  # check getObject(owner) function
  checkEquals(getObject(loadedEntity2), getObject(loadedEntity2, "dataObjectName"))
  
  
  # delete the file
  deleteEntity(loadedEntity)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", loadedEntity2@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
}

integrationTestSerialization<-function() {
  project <- synapseClient:::.getCache("testProject")
  myData<-list(foo="bar", foo2="bas")
  file<-File(parentId=propertyValue(project, "id"))
  result<-try(synStore(file), silent=TRUE) # try storing before adding anything. Should be an error
  checkEquals("try-error", class(result))
  file<-addObject(file, myData)
  storedFile<-synStore(file)
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  checkTrue(!is.null(getFileLocation(storedFile)))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  retrievedFile<-synGet(id, load=T)
  checkTrue(synapseClient:::hasObjects(retrievedFile))
  retrievedObject<-getObject(retrievedFile, "myData")
  checkEquals(myData, retrievedObject)
  
  # check that I can add more data and save again
  newData<-diag(10)
  retrievedFile<-addObject(retrievedFile, newData)
  retrievedFile<-synStore(file)
}

integrationTestSerializeToEmptyFile<-function() {
  # Random, non-existent file
  filePath<-sprintf("%s/integrationTestSerializeToEmptyFile_%d", tempdir(), sample(1000,1))
  project <- synapseClient:::.getCache("testProject")
  myData<-list(foo="bar", foo2="bas")
  file<-File(path=filePath, parentId=propertyValue(project, "id"))
  file<-addObject(file, myData)
  storedFile<-synStore(file)
  scheduleCacheFolderForDeletion(storedFile@fileHandle$id)
  # check that data was written into file
  checkTrue(file.exists(filePath))
  checkTrue(file.info(filePath)$size>0)
}

integrationTestOverwriteProtection<-function() {
  # Random, non-existent file
  filePath<-sprintf("%s/integrationTestOverwriteProtection_%d", tempdir(), sample(1000,1))
  
  connection<-file(filePath)
  writeLines("some text data, not serialized R object", connection)
  close(connection)
  
  project <- synapseClient:::.getCache("testProject")
  myData<-list(foo="bar", foo2="bas")
  file<-File(path=filePath, parentId=propertyValue(project, "id"))
  file<-addObject(file, myData)
  # if you try to store in-memory data to Synapse, and the intermediate file containing non-binary data, then stop, to prevent overwriting
  checkEquals("try-error", class(try(synStore(file), silent=TRUE)))
}

integrationTestNonFile<-function() {
  project <- synapseClient:::.getCache("testProject")
  folder<-Folder(name="test folder", parentId=propertyValue(project, "id"))
  folder<-synapseClient:::synAnnotSetMethod(folder, "annot", "value")
  storedFolder<-synStore(folder)
  id<-propertyValue(storedFolder, "id")
  checkTrue(!is.null(id))
  
  retrievedFolder<-synGet(id)
  checkEquals(propertyValue(project, "id"), propertyValue(retrievedFolder, "parentId"))
  checkEquals("value", synapseClient:::synAnnotGetMethod(retrievedFolder, "annot"))
 
  # now test updating
  retrievedFolder<-synapseClient:::synAnnotSetMethod(retrievedFolder, "annot", "value2")
  retrievedFolder<-synStore(retrievedFolder)
  
  retrievedFolder<-synGet(id)
  checkEquals(propertyValue(project, "id"), propertyValue(retrievedFolder, "parentId"))
  # verify that updated value was persisted
  checkEquals("value2", synapseClient:::synAnnotGetMethod(retrievedFolder, "annot"))
  
  
  # test createORUpdate=T
  folder<-Folder(name="test folder", parentId=propertyValue(project, "id"))
  folder<-synapseClient:::synAnnotSetMethod(folder, "annot", "value3")
  storedFolder<-synStore(folder)
  # check that the id is the same as before (i.e. the previous folder was updated)
  checkEquals(id, propertyValue(storedFolder, "id"))
  
  retrievedFolder<-synGet(id)
  checkEquals(propertyValue(project, "id"), propertyValue(retrievedFolder, "parentId"))
  checkEquals("value3", synapseClient:::synAnnotGetMethod(retrievedFolder, "annot"))
  
  # test createORUpdate=FALSE
  folder<-Folder(name="test folder", parentId=propertyValue(project, "id"))
  checkException(synStore(folder, createOrUpdate=FALSE), silent=TRUE)
  
}

# this tests synStore in which the activity name, description, used, and executed param's are passed in
integrationTestProvenance<-function() {
  project <- synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  executed<-Folder(name="executed", parentId=pid)
  executed<-synStore(executed)
  
  folder<-Folder(name="test folder", parentId=pid)
  # this tests (1) linking a URL, (2) passing a list, (3) passing a single entity, (4) passing an entity ID
  storedFolder<-synStore(folder, used=list("http://foo.bar.com", project), executed=propertyValue(executed, "id"), 
    activityName="activity name", activityDescription="activity description")
  id<-propertyValue(storedFolder, "id")
  checkTrue(!is.null(id))
  
  retrievedFolder<-synGet(id)
  checkEquals(propertyValue(project, "id"), propertyValue(retrievedFolder, "parentId"))
  activity<-generatedBy(retrievedFolder)
  checkEquals("activity name", propertyValue(activity, "name"))
  checkEquals("activity description", propertyValue(activity, "description"))
  
  # now check the 'used' list
  used<-propertyValue(activity, "used")
  checkEquals(3, length(used))
  foundURL<-F
  foundProject<-F
  foundExecuted<-F
  for (u in used) {
    if (u$concreteType=="org.sagebionetworks.repo.model.provenance.UsedURL") {
      checkEquals(FALSE, u$wasExecuted)
      checkEquals("http://foo.bar.com", u$url)
      foundURL<-T
    } else {
      checkEquals(u$concreteType, "org.sagebionetworks.repo.model.provenance.UsedEntity")
      if (u$wasExecuted) {
        checkEquals(u$reference$targetId, propertyValue(executed, "id"))
        checkEquals(u$reference$targetVersionNumber, 1)
        foundExecuted<- T
      } else {
        checkEquals(u$reference$targetId, propertyValue(project, "id"))
        checkEquals(u$reference$targetVersionNumber, 1)
        foundProject<- T
      }
    }
  }
  checkTrue(foundURL)
  checkTrue(foundProject)
  checkTrue(foundExecuted)
  
  # while we're here, test synGetActivity
  activity2<-synGetActivity(retrievedFolder)
  checkEquals(activity$id, activity2$id)
  activity3<-synGetActivity(propertyValue(retrievedFolder, "id"))
  checkEquals(activity$id, activity3$id)
  }

# this tests synStore in which used and executed param's are passed, not as lists
integrationTestProvenanceNonList<-function() {
  project <- synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  executed<-Folder(name="executed", parentId=pid)
  executed<-synStore(executed)
  
  folder<-Folder(name="test folder", parentId=pid)
  # this tests (1) linking a URL, (2) passing a list, (3) passing a single entity, (4) passing an entity ID
  storedFolder<-synStore(folder, used=project, executed=executed, 
    activityName="activity name", activityDescription="activity description")
  id<-propertyValue(storedFolder, "id")
  checkTrue(!is.null(id))
  
  retrievedFolder<-synGet(id)
  checkEquals(propertyValue(project, "id"), propertyValue(retrievedFolder, "parentId"))
  activity<-generatedBy(retrievedFolder)
  checkEquals("activity name", propertyValue(activity, "name"))
  checkEquals("activity description", propertyValue(activity, "description"))
  
  # now check the 'used' list
  used<-propertyValue(activity, "used")
  checkEquals(2, length(used))
  foundProject<-F
  foundExecuted<-F
  for (u in used) {
    if (u$concreteType=="org.sagebionetworks.repo.model.provenance.UsedEntity") {
      if (u$wasExecuted) {
        checkEquals(u$reference$targetId, propertyValue(executed, "id"))
        checkEquals(u$reference$targetVersionNumber, 1)
        foundExecuted<- T
      } else {
        checkEquals(u$reference$targetId, propertyValue(project, "id"))
        checkEquals(u$reference$targetVersionNumber, 1)
        foundProject<- T
      }
    }
  }
  checkTrue(foundProject)
  checkTrue(foundExecuted)
}

# this tests synStore where an Activity is constructed separately, then passed in
integrationTestProvenance2<-function() {
  project <- synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  executed<-Folder(name="executed", parentId=pid)
  executed<-synStore(executed)
  
  folder<-Folder(name="test folder", parentId=pid)
  # this tests (1) linking a URL, (2) passing a list, (3) passing a single entity, (4) passing an entity ID
  activity<-Activity(
    list(name="activity name", description="activity description",
            used=list(
              list(url="http://foo.bar.com", wasExecuted=F),
              list(entity=pid, wasExecuted=F),
              list(entity=propertyValue(executed, "id"), wasExecuted=T)
          )
      )
  )
  activity<-storeEntity(activity)
  
  storedFolder<-synStore(folder, activity=activity)
  id<-propertyValue(storedFolder, "id")
  checkTrue(!is.null(id))
  
  # make sure that using an Activity elsewhere doesn't cause a problem
  anotherFolder<-Folder(name="another folder", parentId=pid)
  anotherFolder<-synStore(anotherFolder, activity=activity)
  
  checkEquals(propertyValue(generatedBy(storedFolder), "id"), propertyValue(generatedBy(anotherFolder), "id"))
  
  # now retrieve the first folder and check the provenance
  retrievedFolder<-synGet(id)
  checkEquals(propertyValue(project, "id"), propertyValue(retrievedFolder, "parentId"))
  activity<-generatedBy(retrievedFolder)
  checkEquals("activity name", propertyValue(activity, "name"))
  checkEquals("activity description", propertyValue(activity, "description"))
  
  # now check the 'used' list
  used<-propertyValue(activity, "used")
  checkEquals(3, length(used))
  foundURL<-F
  foundProject<-F
  foundExecuted<-F
  for (u in used) {
    if (u$concreteType=="org.sagebionetworks.repo.model.provenance.UsedURL") {
      checkEquals(FALSE, u$wasExecuted)
      checkEquals("http://foo.bar.com", u$url)
      foundURL<-T
    } else {
      checkEquals(u$concreteType, "org.sagebionetworks.repo.model.provenance.UsedEntity")
      if (u$wasExecuted) {
        checkEquals(u$reference$targetId, propertyValue(executed, "id"))
        checkEquals(u$reference$targetVersionNumber, 1)
        foundExecuted<- T
      } else {
        checkEquals(u$reference$targetId, propertyValue(project, "id"))
        checkEquals(u$reference$targetVersionNumber, 1)
        foundProject<- T
      }
    }
  }
  checkTrue(foundURL)
  checkTrue(foundProject)
  checkTrue(foundExecuted)
}

# cannot store a missing external link
integrationTestExternalLinkNoFile<-function() {
  project <- synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  
  # create a file to be uploaded
  file<-File(synapseStore=FALSE, parentId=propertyValue(project, "id"))
  
  checkEquals("try-error", class(try(synStore(file), silent=TRUE)))
}

integrationTestExternalLink<-function() {
  project <- synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  
  # create a file to be uploaded
  synapseStore<-FALSE
  filePath<-"http://dilbert.com/index.html"
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  
  # now store it
  storedFile<-synStore(file)
  
  # check that it worked
  checkTrue(!is.null(storedFile))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  checkEquals(propertyValue(project, "id"), propertyValue(storedFile, "parentId"))
  checkEquals(filePath, getFileLocation(storedFile))
  checkEquals(synapseStore, storedFile@synapseStore)
  
  # check that cachemap entry does NOT exist
  fileHandleId<-storedFile@fileHandle$id
  cachePath<-sprintf("%s/.cacheMap", synapseClient:::defaultDownloadLocation(fileHandleId))
  checkTrue(!file.exists(cachePath))
  
  # retrieve the metadata (no download)
  metadataOnly<-synGet(id, downloadFile=FALSE)
  # we get external URL when retrieving only metadata
  checkEquals(filePath, getFileLocation(metadataOnly))
  
  # now download it.  This will pull a copy into the cache
  downloadedFile<-synGet(id)
  scheduleCacheFolderForDeletion(downloadedFile@fileHandle$id)
  
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(propertyValue(project, "id"), propertyValue(downloadedFile, "parentId"))
  checkEquals(synapseStore, downloadedFile@synapseStore)
  # we get external URL when retrieving only metadata
  checkEquals(filePath, getFileLocation(metadataOnly))
  checkEquals(filePath, downloadedFile@fileHandle$externalURL)
}

