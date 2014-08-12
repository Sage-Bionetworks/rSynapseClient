# Functions for TableSchema
# 
# Author: brucehoff
###############################################################################

# creates a TableSchema from the JSON response body received from Synapse
# this is referenced by 'getEntityInstance' which dispatches based on
# concreteType
setMethod(
  f = "createTableSchemaFromProperties",
  signature = signature("list"),
  definition = function(propertiesList) {
    tableSchema <- new("TableSchema")
    for (prop in names(propertiesList))
      tableSchema<-synAnnotSetMethod(tableSchema, prop, propertiesList[[prop]])
    
    propertyValue(tableSchema, "concreteType") <- "org.sagebionetworks.repo.model.TableEntity"
    
    tableSchema
  }
)

TableSchema<-function(name, parent, columns, ...) {
  result<-new("TableSchema")
  result@properties <- initializeProperties("org.sagebionetworks.repo.model.table.TableEntity", TRUE)
  propertyValue(result, "name")<-name
  if (is(parent, "Entity")) {
    parentId<-propertyValue(parent, "id")
  } else if (isSynapseId(parent)) {
    parentId<-parent
  } else {
    stop("Illegal 'parent' parameter.")
  }
  propertyValue(result, "parentId")<-parentId
  # TODO columns
  # TODO additional annotations
  result
}


  