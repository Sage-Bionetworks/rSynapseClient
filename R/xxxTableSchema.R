# Functions for TableSchema
# 
# Author: brucehoff
###############################################################################



setClass(
  Class = "TableSchema",
  contains = "Entity",
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    synapseEntityKind = "TableSchema",
    properties = initializeProperties("org.sagebionetworks.repo.model.table.TableEntity", TRUE)
  )
)

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

TableSchema<-function() {
  result<-TableSchema()
  result
}
  