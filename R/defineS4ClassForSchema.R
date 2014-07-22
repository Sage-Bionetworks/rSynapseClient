# TODO: Add comment
# 
# Author: brucehoff
###############################################################################


TYPEMAP_FOR_ALL_PRIMITIVES <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
  number = "numeric",
  array = "list",
  boolean = "logical"
)

getClassNameFromSchemaName<-function(which) {
  if (is.null(which)) return(NULL)
  result<-gsub("^.+[\\.]", "", which)
  names(result)<-names(which)
  result
}

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
        }, 
        silent=TRUE)
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
      #,silent=TRUE
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
      for (slot in names(args)) {
        obj@slot<-args[[slot]]
      }
      obj      
    })
  
  setGeneric(
    name=name,
    def = function(...) {
      do.call(name, list(...))
    },
    package = package
  )
  
}

isPrimitiveType <- function(type) {
  !is.na(match(type, TYPEMAP_FOR_ALL_PRIMITIVES))
}

mapTypesForAllSlots <- 
  function(types)
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

getImplements<-function(entityDef) {
  if(is.null(entityDef))
    return(NULL)
  entityDef$implements
}

getType<-function(entityDef) {
  if(is.null(which))
    return(NULL)
  entityDef$type
}

isVirtual<-function(entityDef) {
  type<-getType(entityDef)
  !is.null(type) && type=="interface"
}


