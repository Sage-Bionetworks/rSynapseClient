#
#
# Author: furia
###############################################################################

getFactoryForConcreteType<-function(concreteType) {
  factoryName<-NULL
  if (concreteType=="org.sagebionetworks.repo.model.FileEntity") factoryName<-"createFileFromProperties"
  if (concreteType=="org.sagebionetworks.repo.model.table.TableEntity") factoryName<-"createTableSchemaFromProperties"
  # by default the factory is a class constructor
  if (is.null(factoryName)) factoryName<-getClassNameFromSchemaName(concreteType)
  getMethod(factoryName, signature = "list", where="synapseClient")
}

setMethod(
  f = "getEntityInstance",
  signature = signature("list"),
  definition = function(entity)
  {
    if (is.null(entity$concreteType)) {
      stop("Entity metadata is missing 'concreteType'.");
    } else {
      factory <- getFactoryForConcreteType(entity$concreteType)
    }
    
    ## call the appropriate constructor and pass the list
    ## representation of the entity
    ee <- factory(entity)
    ee@synapseWebUrl <- .buildSynapseUrl(propertyValue(ee, "id"))
    ee
  }
)


setMethod(
  f = "initializeEntity",
  signature = "Entity",
  definition = function(entity){
    entity
  }
)


