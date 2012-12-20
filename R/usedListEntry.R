#
# Methods for creating a UsedEntity from an Entity or from a list having an Entity with a boolean 'wasExecuted'
# The Entity may either be a SynapseEntity object, an entity ID, 
# or a "reference" (of the form list(targetId="syn123", targetVersionNumber=1) )
#
setMethod(
  f="usedListEntry",
  signature = signature("Entity"),
  definition = function(listEntry) {
    list(reference=getReference(listEntry), wasExecuted=F)
  }
)

setMethod(
  f="usedListEntry",
  signature = signature("character"),
  definition = function(listEntry) {
    list(reference=getReference(listEntry), wasExecuted=F)
  }
)

setMethod(
  f="usedListEntry",
  signature = signature("list"),
  definition = function(listEntry) {
    if (!is.null(listEntry$reference) && !is.null(listEntry$wasExecuted)) {
      # the list is itself a UsedEntity and no additional processing is needed
      listEntry
    } else if (!is.null(listEntry$targetId)) {
      # then the arg is itself a reference
      list(reference=listEntry, wasExecuted=F)
    } else {
      # get the reference and the 'executed' 
      usedEntity<-listEntry$entity
      if (is.null(usedEntity)) stop("Entity required.")
      executed<-listEntry$wasExecuted
      if (is.null(executed)) stop ("Executed required.")
      list(reference=getReference(usedEntity), wasExecuted=executed)
    }
  }
)

