#
#
# Author: furia
###############################################################################

setGeneric(
  name="Activity",
  def = function(activity, ...) {
    standardGeneric("Activity")
  }
)

setGeneric(
  name = "purgeCache",
  def = function(entity){
    standardGeneric("purgeCache")
  }
)

setGeneric(
  name = "available.versions",
  def = function(object){
    standardGeneric('available.versions')
  }
)

setGeneric(
  name = "downloadAttachment",
  def = function(object, which){
    standardGeneric("downloadAttachment")
  }
)

setGeneric(
  name = "storeAttachment",
  def = function(object, which){
    standardGeneric("storeAttachment")
  }
)

setGeneric(
  name = "attachments",
  def = function(object){
    standardGeneric("attachments")
  }
)

setGeneric(
  name = "attachDir",
  def = function(object){
    standardGeneric("attachDir")
  }
)

setGeneric(
  name = "addAttachment",
  def = function(object, file){
    standardGeneric("addAttachment")
  }
)

setGeneric(
  name = "deleteAttachment",
  def = function(object, file){
    standardGeneric("deleteAttachment")
  }
)

setGeneric(
  name = "getFileCacheName",
  def = function(object){
    standardGeneric("getFileCacheName")
  }
)

setGeneric(
  name = "getFetchMethod",
  def = function(object, factory){
    standardGeneric("getFetchMethod")
  }
)

setGeneric(
  name = "setFetchMethod",
  def = function(object, method, factory){
    standardGeneric("setFetchMethod")
  }
)

setGeneric(
  name = "moveFileCache",
  def = function(from, to, factory){
    standardGeneric("moveFileCache")
  }
)

setGeneric(
  name = "ArchiveOwner",
  def = function(path, ...){
    standardGeneric("ArchiveOwner")
  }
)

setGeneric(
  name = "AttachmentOwner",
  def = function(path, ...){
    standardGeneric("AttachmentOwner")
  }
)


setGeneric(
  name = "removeFileCache",
  def = function(object, method, factory){
    standardGeneric("removeFileCache")
  }
)

setGeneric(
  name = "initializeEntity",
  def = function(entity){
    standardGeneric("initializeEntity")
  }
)

setGeneric(
  name = "storeEntityObjects",
  def = function(entity){
    standardGeneric("storeEntityObjects")
  }
)

setGeneric(
  name = "storeEntityFiles",
  def = function(entity){
    standardGeneric("storeEntityFiles")
  }
)


setGeneric(
  name = "createEntity",
  def = function(entity){
    standardGeneric("createEntity")
  }
)


setGeneric(
  name = "storeEntity",
  def = function(entity){
    standardGeneric("storeEntity")
  }
)

setGeneric(
  name = "storeFile",
  def = function(entity, filePath){
    standardGeneric("storeFile")
  }
)

setGeneric(
  name = "getArchiveFilePath",
  def = function(owner){
    standardGeneric("getArchiveFilePath")
  }
)

setGeneric(
  name = "setCacheRoot",
  def = function(object, path, clean, copy){
    standardGeneric("setCacheRoot")
  }
)

setGeneric(
  name = "getChildEntities",
  def = function(entity){
    standardGeneric("getChildEntities")
  }
)

setGeneric(
  name = "updateEntity",
  def = function(entity){
    standardGeneric("updateEntity")
  }
)

setGeneric(
  name = "deleteEntity",
  def = function(entity){
    standardGeneric("deleteEntity")
  }
)

setGeneric(
  name = "getParentEntity",
  def = function(entity){
    standardGeneric("getParentEntity")
  }
)


setGeneric(
  name = "Media",
  def = function(entity){
    standardGeneric("Media")
  }
)

setGeneric(
  name = "onWeb",
  def = function(entity){
    standardGeneric("onWeb")
  }
)

setGeneric(
  name = "downloadEntity",
  def = function(entity, versionId){
    standardGeneric("downloadEntity")
  }
)

setGeneric(
  name = "setFileCache",
  def = function(owner, fileCache, factory){
    standardGeneric("setFileCache")
  }
)

setGeneric(
  name = "createArchive",
  def = function(owner){
    standardGeneric("createArchive")
  }
)

setGeneric(
  name = "unpackArchive",
  def = function(owner){
    standardGeneric("unpackArchive")
  }
)

setGeneric(
  name = "getEntity",
  def = function(entity, versionId){
    standardGeneric("getEntity")
  }
)

setGeneric(
  name = "getEntityInstance",
  def = function(entity){
    standardGeneric("getEntityInstance")
  }
)

setGeneric(
  name = "addGithubTag",
  def = function(entity, url){
    standardGeneric("addGithubTag")
  }
)


setGeneric(
  name = "loadEntity",
  def=function(entity, versionId, ...){
    standardGeneric("loadEntity")
  }
)

setGeneric(
  name = "Locationable",
  def = function(entity){
    standardGeneric("Locationable")
  }
)

setGeneric(
  name = "Code",
  def = function(entity, ...){
    standardGeneric("Code")
  }
)

setGeneric(
  name = ".doGetObjects",
  def = function(x){
    standardGeneric(".doGetObjects")
  }
)

setGeneric(
  name = "summarizeCacheFiles",
  def = function(entity){
    standardGeneric("summarizeCacheFiles")
  }
)

setGeneric(
  name = "summarizeObjects",
  def = function(entity){
    standardGeneric("summarizeObjects")
  }
)

setGeneric(
  name = "getAnnotations",
  def = function(entity){
    standardGeneric("getAnnotations")
  }
)

setGeneric(
  name = "loadObjectsFromFiles",
  def = function(owner, clearEnvironment){
    standardGeneric("loadObjectsFromFiles")
  }
)

setGeneric(
  name = "cacheDir",
  def = function(object){
    standardGeneric("cacheDir")
  }
)
setGeneric(
  name = "files",
  def = function(object){
    standardGeneric("files")
  }
)

setGeneric(
  name = ".generateTmpCacheFileRelativePath",
  function(owner, objectName, suffix){
    standardGeneric(".generateTmpCacheFileRelativePath")
  }
)

setGeneric(
  name = ".generateCacheFileRelativePath",
  def = function(owner, objectName, suffix){
    standardGeneric(".generateCacheFileRelativePath")
  }
)

setGeneric(
  name = ".generateCacheFileName",
  def = function(owner, objectName, suffix){
    standardGeneric(".generateCacheFileName")
  }
)

setGeneric(
  name = ".generateTmpCacheFileName",
  def = function(owner, objectName){
    standardGeneric(".generateTmpCacheFileName")
  }
)

setGeneric(
  name = ".cacheObject",
  def = function(owner, objectName){
    standardGeneric(".cacheObject")
  }
)

setGeneric(
  name = ".tmpCacheObject",
  def = function(destFile){
    standardGeneric(".tmpCacheObject")
  }
)

setGeneric(
  name = ".tmpCacheObject",
  def = function(object, objectName){
    standardGeneric(".tmpCacheObject")
  }
)

setGeneric(
  name = ".renameCacheObjectFromTmp",
  def = function(object, srcName, destName){
    standardGeneric(".renameCacheObjectFromTmp")
  }
)

setGeneric(
  name = ".deleteTmpCacheFile",
  def = function(owner, objectName){
    standardGeneric(".deleteTmpCacheFile")
  }
)

setGeneric(
  name = ".deleteCacheFile",
  def = function(owner, objectName){
    standardGeneric(".deleteCacheFile")
  }
)

setGeneric(
  name = ".loadCachedObjects",
  def = function(owner, clearEnvironment){
    standardGeneric(".loadCachedObjects")
  }
)


setGeneric(
  name = "getEnv",
  def = function(object){
    standardGeneric("getEnv")
  }
)


setGeneric(
  name = "deleteProperty",
  def = function(object, which){
    standardGeneric("deleteProperty")
  }
)

setGeneric(
  name = "properties",
  def = function(object){
    standardGeneric("properties")
  }
)

setGeneric(
  name = "properties<-",
  def = function(object, value){
    standardGeneric("properties<-")
  }
)

setGeneric(
  name = "propertyValues",
  def = function(object){
    standardGeneric("propertyValues")
  }
)

setGeneric(
  name = "propertyValues<-",
  def = function(object, value){
    standardGeneric("propertyValues<-")
  }
)

setGeneric(
  name = "annotations",
  def = function(object){
    standardGeneric("annotations")
  }
)

setGeneric(
  name = "propertyValue",
  def = function(object, which){
    standardGeneric("propertyValue")
  }
)

setGeneric(
  name = "propertyValue<-",
  def = function(object, which, value){
    standardGeneric("propertyValue<-")
  }
)

setGeneric(
  name = "annotations<-",
  def = function(object, value){
    standardGeneric("annotations<-")
  }
)

setGeneric(
  name = "annotationNames",
  def = function(object){
    standardGeneric("annotationNames")
  }
)

setGeneric(
  name = "annotValue",
  def = function(object, which){
    standardGeneric("annotValue")
  }
)

setGeneric(
  name = "deleteAnnotation",
  def = function(object, which){
    standardGeneric("deleteAnnotation")
  }
)

setGeneric(
  name = "annotValue<-",
  def = function(object, which, value){
    standardGeneric("annotValue<-")
  }
)

setGeneric(
  name = "annotationValues",
  def = function(object){
    standardGeneric("annotationValues")
  }
)

setGeneric(
  name = "annotationValues<-",
  def = function(object, value){
    standardGeneric("annotationValues<-")
  }
)

setGeneric(
  name = "TypedPropertyStore",
  def = function(file, data, json){
    standardGeneric("TypedPropertyStore")
  }
)

setGeneric(
  name = "setUpdatePropValue",
  def = function(object, which, value, type){
    standardGeneric("setUpdatePropValue")
  }
)
setGeneric(
  name = "propertyNames",
  def = function(object){
    standardGeneric("propertyNames")
  }
)

setGeneric(
  name = "propertyValues",
  def = function(object){
    standardGeneric("propertyValues")
  }
)

setGeneric(
  name = "propertyType",
  def = function(object, which){
    standardGeneric("propertyType")
  }
)

setGeneric(
  name = "getProperty",
  def = function(object, which){
    standardGeneric("getProperty")
  }
)

setGeneric(
  name = "setProperty",
  def = function(object, which, value){
    standardGeneric("setProperty")
  }
)

setGeneric(
  name = "getFileCache",
  def = function(archivePath, method, factory){
    standardGeneric("getFileCache")
  }
)

setGeneric(
  name = "availFileCaches",
  def = function(method, factory){
    standardGeneric("availFileCaches")
  }
)

setGeneric(
  name = "resetFactory",
  def = function(factory){
    standardGeneric("resetFactory")
  }
)

## Generic method for addObject
setGeneric(
  name = "addObject",
  def = function(owner, object, name, unlist){
    standardGeneric("addObject")
  }
)

## generic method for deleteObject
setGeneric(
  name = "deleteObject",
  def = function(owner, which){
    standardGeneric("deleteObject")
  }
)

## Generic method for renameObject
setGeneric(
  name = "renameObject",
  def = function(owner, which, name){
    standardGeneric("renameObject")
  }
)

## Generic method for getObject
setGeneric(
  name = "getObject",
  def = function(owner, which){
    standardGeneric("getObject")
  }
)

setGeneric(
  name = "addFile",
  def = function(entity, file, path){
    standardGeneric("addFile")
  }
)

setGeneric(
  name = "FileCache",
  def = function(cacheRoot, object, archiveFile){
    standardGeneric("FileCache")
  }
)

setGeneric(
  name = "addFileMetaData",
  def = function(object, srcPath, destPath){
    standardGeneric("addFileMetaData")
  }
)

setGeneric(
  name = "deleteFile",
  def = function(entity, file){
    standardGeneric("deleteFile")
  }
)

setGeneric(
  name = "moveFile",
  def = function(entity, src, dest){
    standardGeneric("moveFile")
  }
)

setGeneric(
  name = "addValue",
  def = function(object, name, value){
    standardGeneric("addValue")
  }
)

setGeneric(
  name = "deleteValue",
  def = function(object, name, value){
    standardGeneric("deleteValue")
  }
)

setGeneric(
  name = "getValue",
  def = function(object, name, value){
    standardGeneric("getValue")
  }
)

setGeneric(
  name = "synapseEntityKind",
  def = function(entity){
    standardGeneric("synapseEntityKind")
  }
)

setGeneric(
  name = "synapseEntityKind<-",
  def = function(entity, value){
    standardGeneric("synapseEntityKind<-")
  }
)

setGeneric(
  name = "refreshAnnotations",
  def = function(entity){
    standardGeneric("refreshAnnotations")
  }
)

setGeneric(
  name = "storeActivity",
  def = function(activity) {
    standardGeneric("storeActivity")
  }
)

setGeneric(
  name = "showEntity",
  def = function(activity) {
    standardGeneric("showEntity")
  }
)

setGeneric(
  name = "getActivity",
  def = function(activity) {
    standardGeneric("getActivity")
  }
)

setGeneric(
  name = "synGetActivity",
  def = function(entity, version) {
    standardGeneric("synGetActivity")
  }
)

setGeneric(
  name = "synSetActivity<-",
  def = function(entity, activity){
    standardGeneric("synSetActivity<-")
  }
)

setGeneric(
  name = "deleteActivity",
  def = function(activity) {
    standardGeneric("deleteActivity")
  }
)

setGeneric(
  name = "generatedBy",
  def = function(entity){
    standardGeneric("generatedBy")
  }
)

setGeneric(
  name = "generatedBy<-",
  def = function(entity, value){
    standardGeneric("generatedBy<-")
  }
)

setGeneric(
  name = "getGeneratedBy",
  def = function(entity){
    standardGeneric("getGeneratedBy")
  }
)

setGeneric(
  name = ".extractEntityFromSlots",
  def = function(object){
    standardGeneric(".extractEntityFromSlots")
  }
)

setGeneric(
  name = ".populateSlotsFromEntity",
  def = function(object, entity, json){
    standardGeneric(".populateSlotsFromEntity")
  }
)

setGeneric(
  name = ".populateSlotsFromActivity",
  def = function(object, activity){
    standardGeneric(".populateSlotsFromActivity")
  }
)

setGeneric(
  name = "used",
  def = function(entity) {
    standardGeneric("used")	
  }
)

setGeneric(
  name = "used<-",
  def = function(entity, value) {
    standardGeneric("used<-")
  }
)

# returns a reference list of the form (targetId="syn1234") or
# (targetId="syn1234", targetVersionNumber="1") referring
# to the given entity.  The argument can be a SynapseEntity or a 
# synapse ID
setGeneric(
  name = "getReference",
  def = function(entity) {
    standardGeneric("getReference")
  }
)

setGeneric(
  name="usedListEntry",
  def = function(listEntry, ...) {
    standardGeneric("usedListEntry")
  }
)

setGeneric(
   name="FileListConstructor",
    def = function(propertiesList) {
     standardGeneric("FileListConstructor")
    }
)
