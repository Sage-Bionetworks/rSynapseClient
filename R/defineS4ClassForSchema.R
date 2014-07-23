# This module generates S4 classes from JSON schemas.
# Note:  There is similar code in AAAschema, but it specifically generates
# classes which extend Entity.  This code is generic.
# 
# Author: brucehoff
###############################################################################


# This maps the keyword found in the JSON schema to the 
# type used in the S4 class. Note:  'array' maps to 'list'
# not vector. Mapping to vector would have the advantage
# of retaining the type of the field.  However the library
# that maps objects to JSON (for HTTP requests) cannot differentiate
# between scalars and vectors of unit length, which are the
# same in R but very different in JSON.
TYPEMAP_FOR_ALL_PRIMITIVES <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
  number = "numeric",
  array = "list",
  boolean = "logical"
)

# Omit the part of the string preceding the last "." (if any)
getClassNameFromSchemaName<-function(which) {
  if (is.null(which)) return(NULL)
  result<-gsub("^.+[\\.]", "", which)
  names(result)<-names(which)
  result
}

# 'which' is the full class name
# 'name' is the Class name.  If omitted it's the suffix of 'which', e.g. 
# if 'which' is "org.sagebionetworks.repo.model.Folder" and 'name' is omitted,
# then the Class name is "Folder".
defineS4ClassForSchema <- 
  function(which, name, where = parent.frame(), package)
{
  if(missing(name))
    name <- getClassNameFromSchemaName(which)
  
  if(is.null(name) | name == "")
    stop("name must not be null")
  
  schemaDef <- readEntityDef(which)
  
  # make sure all extended classes are defined
  implements <- getImplements(schemaDef)
  if (!is.null(implements)) {
    for (i in implements) {
      implementsName <- getClassNameFromSchemaName(i)
      tryCatch(new(implementsName), 
        error = function(e){
          defineS4ClassForSchema(which=i, where=where, package=package)
        }
      )
    }
  }
  
  propertyTypes<-getPropertyTypes(entityDef=schemaDef)
  propertyTypes<-mapTypesForAllSlots(propertyTypes)
  
  # make sure they're all defined
  for (propertyType in propertyTypes) {
    # check whether the type is one of the known primitives
    if (!isPrimitiveType(propertyType)) {
      # if it is not one of the known primitives then try instantiating it
      propertyTypeName <- getClassNameFromSchemaName(propertyType)
      tryCatch(
        {
          new(propertyTypeName)
        }, 
        error = function(e){
          # if we can't instantiate it, it's not defined yet, so define it!
          defineS4ClassForSchema(which=propertyType, where=where, package=package)
        }
      )
    }
  }
  
  slots<-getClassNameFromSchemaName(propertyTypes)
  slots<-append(slots, list(updateUri="character"))
  if (isVirtual(schemaDef)) {
    slots<-append(slots, "VIRTUAL")
  }
  
  setClass(
    Class = name,
    contains=getClassNameFromSchemaName(implements),
    representation = do.call("representation", slots),
    package=package
  )
  
  # This generic constructor takes the form:
  # ClassName(slot1=value1, slot2=value2, ...)
  assign(name, function(...) {
      args <-list(...)
      obj<-new(name)
      for (slotName in names(args)) {
        slot(obj, slotName)<-args[[slotName]]
      }
      obj      
  })
  
  # If we don't define a 'generic' version of the constructor
  # we get an error when we try to include it as an export in
  # the NAMESPACE file.
  setGeneric(
    name=name,
    def = function(...) {
      do.call(name, list(...))
    },
    package = package
  )
  
  setMethod(
    f = "$",
    signature = name,
    definition = function(x, name){
      x@name
    }
  )
  
  setReplaceMethod("$", 
    signature = name,
    definition = function(x, name, value) {
      x@name<-value
      x
    }
  )
  
}

isPrimitiveType <- function(type) {
  !is.na(match(type, TYPEMAP_FOR_ALL_PRIMITIVES))
}

mapTypesForAllSlots <- function(types)
{ 
  indx <- match(types, names(TYPEMAP_FOR_ALL_PRIMITIVES))
  retval <- TYPEMAP_FOR_ALL_PRIMITIVES[indx]
  
  mk <- sapply(X=retval, FUN=function(x)is.null(x))
  
  if (any(mk)) {
    retval[mk] <-types[mk]
  }
  
  names(retval) <- names(types)
  retval
}

# get the parent class or NULL if none
getImplements<-function(schema) {
  if(is.null(schema))
    return(NULL)
  schema$implements
}

getType<-function(schema) {
  if(is.null(which))
    return(NULL)
  schema$type
}

# returns TRUE iff the schema defines an interface
isVirtual<-function(schema) {
  type<-getType(schema)
  !is.null(type) && type=="interface"
}


