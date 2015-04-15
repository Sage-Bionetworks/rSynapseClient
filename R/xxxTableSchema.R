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
    tableSchema
  }
)

getTableSchemaColumns<-function(tableId) {
	if (is.null(tableId)) stop("tableId is required")
	response<-synRestGET(sprintf("/entity/%s/column", tableId))
	createTypedListFromList(response$results, "TableColumnList")
}

# this is called from synGet to do TableSchema-specific work
populateTableSchema<-function(tableSchema) {
	tableSchema@columns<-getTableSchemaColumns(propertyValue(tableSchema, "id"))
	tableSchema
}

# Note this duplicates the 'prototype' for the superclass, Entity
initializeTableSchemaSlots<-function(tableSchema) {
  tableSchema@annotations <- new("SynapseAnnotations")
  tableSchema@synapseWebUrl <- ""
  tableSchema@generatedByChanged <- FALSE
  tableSchema@properties <- initializeProperties("org.sagebionetworks.repo.model.table.TableEntity", TRUE)
  tableSchema
}

TableSchema<-function(name, parent, columns, ...) {
  result<-new("TableSchema")
  result<-initializeTableSchemaSlots(result)
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
      #if (length(column$id)==0) column<-synStore(column)
      result@columns<-append(result@columns, column)
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

setMethod(
		f = "synStore",
		signature = "TableSchema",
		definition = function(entity, activity=NULL, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T, isRestricted=F, contentType=NULL) {
			# first, store any unsaved columns
			idsFromColumns<-list()
			columnsToStoreAsList<-list()
			for (column in entity@columns@content) {
				if (length(column$id)==0) {
					columnsToStoreAsList<-append(columnsToStoreAsList, list(createListFromS4Object(column)))
				} else {
					idsFromColumns<-append(idsFromColumns, column$id)
				}
			}
			if (length(columnsToStoreAsList)>0) {
				response<-synRestPOST("/column/batch", 
						list(list=columnsToStoreAsList, concreteType="org.sagebionetworks.repo.model.table.ColumnModel"))
				storedColumns<-createTypedListFromList(response$list, "TableColumnList")
				for (column in storedColumns@content) {
					idsFromColumns<-append(idsFromColumns, column@id)
				}
			}
			# now make sure the entity's ID list includes those of the columns
			propertyValue(entity, "columnIds")<-unique(append(propertyValue(entity, "columnIds"), idsFromColumns))
			# now do the standard operations for storing an Entity
			storedEntity<-synStoreMethod(entity, activity, used, executed, activityName, activityDescription, createOrUpdate, forceVersion, isRestricted, contentType)
			# finally, get a fresh copy of the columns
			storedEntity@columns<-getTableSchemaColumns(propertyValue(storedEntity, "id"))
			
			storedEntity
		}
)

  