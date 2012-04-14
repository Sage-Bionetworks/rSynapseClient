# TODO: Add comment
# 
# Author: furia
###############################################################################

setGeneric(
    name = "getEnv",
    def = function(object){
      standardGeneric("getEnv")
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
  def = function(archivePath){
    standardGeneric("getFileCache")
  }
)

setGeneric(
  name = "availFileCaches",
  def = function(factory){
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
  name = "SynapseEntity",
  def = function(entity){
    standardGeneric("SynapseEntity")
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
