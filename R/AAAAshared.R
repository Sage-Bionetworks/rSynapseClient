# Collection of functions used both by the R client and by
# external scripts that can access the code base but run before
# the package is built
# 
# Author: brucehoff
#########################################################################

readSchema <- function(name, path) { 
  file <- sprintf("%s.json", gsub("[\\.]", "/", name))
  
  fullPath <- file.path(path,file)
  
  if(!file.exists(fullPath))
    stop(sprintf("Could not find file: %s for entity: %s", fullPath, name))
  
  schema <- fromJSON(fullPath, simplifyWithNames = FALSE)
  schema
}


#-----------------------------------
# utilities for parsing schemas

# get the parent class for the given schema or NULL if none
getImplements<-function(schema) {
  if(is.null(schema))
    return(NULL)
  schema$implements
}

# returns TRUE iff the schema defines an interface
isVirtual<-function(schema) {
  type<-schema$type
  !is.null(type) && type=="interface"
}

schemaTypeFromProperty<-function(property) {
  type<-property[["type"]]
  ref<-property[["$ref"]]
  if (!is.null(ref)) {
    ref
  } else {
    type
  }
}

getEffectivePropertySchemas<-function(schemaName, schemaPath) {
  schema<-readSchema(schemaName, schemaPath)
  properties<-schema$properties
  implements <- getAllInterfaces(schema)
  if (length(implements)>0) {
    for (i in length(implements):1) {
      thisProp <- readSchema(implements[i], schemaPath)$properties
      for (n in names(thisProp))
        properties[[n]] <- thisProp[[n]]
    }
  }
  properties
}

getAllInterfaces <- function(schema, schemaPath) {
  if(is.null(schema))
    return(NULL)
  implements <- NULL
  while(!is.null(schema$implements)){
    implements <- c(implements, schema$implements[[1]][[1]])
    try({
        schema <- readSchema(schema$implements[[1]][[1]], schemaPath)
      }, silent = TRUE)
  }
  implements
}

getArraySubSchema<-function(propertySchema) {
  propertySchema$items
}

#-----------------------------------

# This maps the keyword found in the JSON schema to the 
# type used in the S4 class.
TYPEMAP_FOR_ALL_PRIMITIVES <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
  number = "numeric",
  boolean = "logical"
)

isPrimitiveType <- function(rType) {
  !is.na(match(rType, TYPEMAP_FOR_ALL_PRIMITIVES))
}


# This is the our approach to naming typed lists:
# We append "List" to the type and make sure the first character
# is upper case. So a typed list of "character" is "CharacterList"
listClassName<-function(rType) {
  sprintf("%s%sList", toupper(substring(rType, 1, 1)), substring(rType, 2))
}



