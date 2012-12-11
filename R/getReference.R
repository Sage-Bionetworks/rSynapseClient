#
# Methods to return a Reference (an id and verion) given an Entity or Entity ID
# In the latter case there is no version information in the Reference.
#


setMethod(
  f="getReference",
  signature = signature("SynapseEntity"),
  definition = function(entity) {
    versionNumber <- propertyValue(entity, "versionNumber")
    if (is.null(versionNumber)) {
      list(targetId=propertyValue(entity, "id"))
    } else {
      list(targetId=propertyValue(entity, "id"), targetVersionNumber=versionNumber)
    }
  }
)

setMethod(
  f="getReference",
  signature = signature("character"),
  definition = function(entity) {
    list(targetId=entity)
  }
)

