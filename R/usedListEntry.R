#
# Methods for creating a UsedEntity from an Entity, a URL or a list having an Entity with a boolean 'wasExecuted'
# The Entity may either be a SynapseEntity object, an entity ID, 
# or a "reference" (of the form list(targetId="syn123", targetVersionNumber=1) )
#
setMethod(
  f="usedListEntry",
  signature = signature("Entity"),
  definition = function(listEntry, ...) {
    otherParams<-list(...)
    if (is.null(otherParams$wasExecuted)) {
      wasExecuted=F
    } else {
      wasExecuted<-otherParams$wasExecuted
    }
    list(reference=getReference(listEntry), wasExecuted=wasExecuted, concreteType="org.sagebionetworks.repo.model.provenance.UsedEntity")
  }
)

setMethod(
  f="usedListEntry",
  signature = signature("character"),
  definition = function(listEntry, ...) {
    otherParams<-list(...)
    if (is.null(otherParams$wasExecuted)) {
      wasExecuted=F
    } else {
      wasExecuted<-otherParams$wasExecuted
    }
    if (isSynapseId(listEntry)) {
      list(reference=getReference(listEntry), wasExecuted=wasExecuted, concreteType="org.sagebionetworks.repo.model.provenance.UsedEntity")
    } else {
      # must be a URL
      list(url=listEntry, wasExecuted=wasExecuted, concreteType="org.sagebionetworks.repo.model.provenance.UsedURL")
    }
  }
)

setMethod(
  f="usedListEntry",
  signature = signature("list"),
  definition = function(listEntry, ...) {
    if (!is.null(listEntry$reference)) {
      # the list is itself a UsedEntity
      if (is.null(listEntry$wasExecuted)) stop("'wasExecuted' required.")
      if (is.null(listEntry$concreteType)) listEntry$concreteType<-"org.sagebionetworks.repo.model.provenance.UsedEntity"
      listEntry
    } else if (!is.null(listEntry$url)) {
      # the list is itself a UsedURL
      if (is.null(listEntry$wasExecuted)) stop("'wasExecuted' required.")
      if (is.null(listEntry$concreteType)) listEntry$concreteType<-"org.sagebionetworks.repo.model.provenance.UsedURL"
      listEntry
    } else if (!is.null(listEntry$targetId)) {
      # then the arg is itself a reference
      otherParams<-list(...)
      if (is.null(otherParams$wasExecuted)) {
        wasExecuted=F
      } else {
        wasExecuted<-otherParams$wasExecuted
      }
      list(reference=listEntry, wasExecuted=wasExecuted, concreteType="org.sagebionetworks.repo.model.provenance.UsedEntity")
    } else if (!is.null(listEntry$entity)) {
      # get the reference and the 'executed' 
      usedEntity<-listEntry$entity
      executed<-listEntry$wasExecuted
      if (is.null(executed)) stop ("Executed required.")
      list(reference=getReference(usedEntity), wasExecuted=executed, concreteType="org.sagebionetworks.repo.model.provenance.UsedEntity")
    } else if (!is.null(listEntry$url)) {
      # get the URL and the 'executed' 
      usedURL<-listEntry$entity
      executed<-listEntry$wasExecuted
      if (is.null(executed)) stop ("Executed required.")
      list(url=usedURL, wasExecuted=executed, concreteType="org.sagebionetworks.repo.model.provenance.UsedURL")
    } else {
      stop ("Entity, ID or URL required.")
    }
  }
)

