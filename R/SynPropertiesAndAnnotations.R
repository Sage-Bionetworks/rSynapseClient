# C-4 commands for getting/setting properties and annotations
# 
# Author: brucehoff
###############################################################################


setMethod(
  f = "synGetAnnotations",
  signature = signature("Entity"),
  definition = function(object) {
    sapply(annotationNames(object), function(x){annotValue(object,x)}, USE.NAMES=TRUE)
  }
)

setMethod(
  f = "synSetAnnotations<-",
  signature = signature("Entity", "list"),
  definition = function(object, value) {
    for (x in annotationNames(object)) annotValue(object, x)<-NULL
    for (x in names(value)) annotValue(object, x)<-value[[x]]
    object
  }
)

setMethod(
  f = "synGetProperties",
  signature = signature("Entity"),
  definition = function(object) {
    sapply(annotationNames(object), function(x){propertyValue(object,x)}, USE.NAMES=TRUE)
  }
)

setMethod(
  f = "synSetProperties<-",
  signature = signature("Entity", "list"),
  definition = function(object, value) {
    for (x in propertyNames(object)) propertyValue(object, x)<-NULL
    for (x in names(value)) propertyValue(object, x)<-value[[x]]
    object
  }
)

setMethod(
  f = "synGetAnnotation",
  signature = signature("Entity", "character"),
  definition = function(object, which) {
    annotValue(object, which)
  }
)

setMethod(
  f = "synSetAnnotation<-",
  signature = signature("Entity", "character", "ANY"),
  definition = function(object, which, value) {
    annotValue(object, which)<-value
    object
  }
)

setMethod(
  f = "synGetProperty",
  signature = signature("Entity", "character"),
  definition = function(object, which) {
    if (!(which %in% propertyNames(object))) stop(sprintf("%s is not a property in %s", which, class(object)))
    propertyValue(object, which)
  }
)

setMethod(
  f = "synSetProperty<-",
  signature = signature("Entity", "character", "ANY"),
  definition = function(object, which, value) {
    propertyValue(object, which)<-value
    object
  }
)




