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

getTableSchemaColumnsFromId<-function(tableId) {
	if (is.null(tableId)) stop("tableId is required")
	response<-synRestGET(sprintf("/entity/%s/column", tableId))
	createTypedListFromList(response$results, "TableColumnList")
}

synGetColumns<-function(arg) {
	if (is.null(arg)) stop("table, schema or id is required")
	if (is(arg, "character")) {
		if (!isSynapseId(arg)) stop(sprintf("%s is not a Synapse ID.", arg))
		getTableSchemaColumnsFromId(arg)
	} else if (is(arg, "Table")) {
		synGetColumns(arg@schema)
	} else if (is(arg, "TableSchema")) {
		arg@columns
	} else {
		stop(sprintf("Unexpected type: %s", class(arg)[[1]]))
	}
}

synAddColumn<-function(arg, column) {
	if (is.null(arg)) stop("table or schema is required")
	if (is.null(column)) stop("column is required")
	if (is(arg, "Table")) {
		synAddColumn(arg@schema, column)
	} else if (is(arg, "TableSchema")) {
		if (is(column, "character")) {
			propertyValue(arg, "columnIds")<-append(propertyValue(arg, "columnIds"), column)
		} else if (is(column, "TableColumn")) {
			arg@columns<-append(arg@columns, column)
		} else {
			stop(sprintf("Unexpected type for 'column': %s", class(column)[[1]]))
		}
		arg
	} else {
		stop(sprintf("Unexpected type: %s", class(arg)[[1]]))
	}
}

synRemoveColumn<-function(arg, column) {
	if (is.null(arg)) stop("table or schema is required")
	if (is.null(column)) stop("column is required")
	if (is(arg, "Table")) {
		synRemoveColumn(arg@schema, column)
	} else if (is(arg, "TableSchema")) {
		# remove both from 'columns' and 'columnIds'
		if (is(column, "character")) {
			idToRemove<-column
		} else if (is(column, "TableColumn")) {
			idToRemove<-column@id
		} else {
			stop(sprintf("Unexpected type for 'column': %s", class(column)[[1]]))
		}
		currentColumnIds<-propertyValue(arg, "columnIds")
		reducedIdList<-currentColumnIds[currentColumnIds!=idToRemove]
		reducedColumns<-TableColumnList()
		for (column in arg@columns@content) {
			if (idToRemove!=column@id) reducedColumns<-append(reducedColumns, column)
		}
		propertyValue(arg, "columnIds")<-reducedIdList
		arg@columns<-reducedColumns
		arg
	} else {
		stop(sprintf("Unexpected type: %s", class(arg)[[1]]))
	}
}

# this is called from synGet to do TableSchema-specific work
populateTableSchema<-function(tableSchema) {
	tableSchema@columns<-getTableSchemaColumnsFromId(propertyValue(tableSchema, "id"))
	tableSchema
}

# Note this duplicates the 'prototype' for the superclass, Entity
initializeTableSchemaSlots<-function(tableSchema) {
  tableSchema@annotations <- new("SynapseAnnotations")
  tableSchema@synapseWebUrl <- ""
  tableSchema@generatedByChanged <- FALSE
	tableSchema@columns<-TableColumnList()
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
			storedEntity@columns<-getTableSchemaColumnsFromId(propertyValue(storedEntity, "id"))
			
			storedEntity
		}
)

  