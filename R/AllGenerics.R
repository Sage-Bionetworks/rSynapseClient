# TODO: Add comment
# 
# Author: furia
###############################################################################

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
