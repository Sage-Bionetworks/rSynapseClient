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
    tableSchema<-initializeTableSchemaSlots(tableSchema)
    for (prop in names(propertiesList))
      tableSchema<-synAnnotSetMethod(tableSchema, prop, propertiesList[[prop]])
    
    propertyValue(tableSchema, "concreteType") <- "org.sagebionetworks.repo.model.TableEntity"
    
    tableSchema
  }
)

# Note this duplicates the 'prototype' for the superclass, Entity
initializeTableSchemaSlots<-function(tableSchema) {
  tableSchema@annotations <- new("SynapseAnnotations")
  tableSchema@synapseWebUrl <- ""
  tableSchema@generatedByChanged <- FALSE
  tableSchema@properties <- SynapseProperties(getEffectivePropertyTypes("org.sagebionetworks.repo.model.Entity"))
  tableSchema
}

TableSchema<-function(name, parent, columns, ...) {
  result<-new("TableSchema")
  result<-initializeTableSchemaSlots(result)
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
  if (missing(columns) || length(columns)==0) {
    stop("'columns' is required.")
  }
  columnIds<-list()
  for (column in columns) {
    if (is(column, "TableColumn")) {
      columnIds<-append(columnIds, column$id)
    } else {
      columnIds<-append(columnIds, column)
    }
  }
  propertyValue(result, "columnIds")<-columnIds
  entityParams<-list(...)
  for (key in names(entityParams)) {
    result<-synAnnotSetMethod(result, key, entityParams[[key]])
  }
  
  result
}


  