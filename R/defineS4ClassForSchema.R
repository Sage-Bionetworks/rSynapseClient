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

isClassDefined<-function(className) {
  tryCatch(
    {
      getClass(Class=className)
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

defineS4ClassForSchema <-  function(fullSchemaName)
{
  name <- getClassNameFromSchemaName(fullSchemaName)
  
  if(is.null(name) | name == "")
    stop("name must not be null")
  
  schemaDef <- readEntityDef(fullSchemaName)
  
  # make sure all extended classes are defined
  superClasses<-character()
  implements <- getImplements(schemaDef)
  if (!is.null(implements)) {
    for (i in implements) {
      implementsName <- getClassNameFromSchemaName(i)
      superClasses<-c(superClasses, implementsName)
      if (!isClassDefined(implementsName)) {
        defineS4ClassForSchema(i)
      }
    }
  }
  
  # these are the property types defined by the schema
  propertyTypes<-getPropertyTypes(entityDef=schemaDef)
  # these are the types that will be used for the S4 class slots
  s4PropertyTypes<-mapTypesForAllSlots(propertyTypes)
  
  # make sure they're all defined
  for (propertyType in s4PropertyTypes) {
    # check whether the type is one of the known primitives
    if (!isPrimitiveType(propertyType)) {
      # if it is not one of the known primitives then try instantiating it
      propertyTypeName <- getClassNameFromSchemaName(propertyType)
      if (!isClassDefined(propertyTypeName)) {
        defineS4ClassForSchema(propertyType)
      }
    }
  }
  
  # slots defined by the schema:
  slots<-getClassNameFromSchemaName(s4PropertyTypes)
  # metadata slots required by the client:
  slots<-append(slots, list(
      updateUri="character" # URI for updating objects of this type
  ))

  isVirtualClass <- isVirtual(schemaDef)
  if (isVirtualClass) {
    superClasses<-c(superClasses, "VIRTUAL")
  }
  
  setClass(
    Class = name,
    contains=superClasses,
    slots = slots,
    package="synapseClient"
  )
  
  if (!isClassDefined(name)) stop(sprintf("Class definition failed for %s", name))
  
  if (!isVirtualClass) {
    # This generic constructor takes one of two forms:
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
      }
    )
    
    setMethod(
      f = "$",
      signature = name,
      definition = function(x, name){
        slot(x,name)
      }
    )
    
    setReplaceMethod("$", 
      signature = name,
      definition = function(x, name, value) {
        slot(x, name)<-value
        x
      }
    )
    
    # for backwards compatibility
    setMethod(
      f = "propertyValue",
      signature = signature(name, "character"),
      definition = function(object, which){
        slot(object, which)
      }
    )
    
    # for backwards compatibility
    setReplaceMethod(
      f = "propertyValue",
      signature = signature(name, "character"),
      definition = function(object, which, value) {
        slot(object, which) <- value
        object
      }
    )
  } # end 'if(isVirtualClass)'
}

isPrimitiveType <- function(type) {
  !is.na(match(type, TYPEMAP_FOR_ALL_PRIMITIVES))
}

# the values in 'types' is taken from the range of types in the JSON schema
# the names of 'types' are the names of the properties having the given type
# the returned list maps the properties to their corresponding 'R' type
mapTypesForAllSlots <- function(types) { 
  indx <- match(types, names(TYPEMAP_FOR_ALL_PRIMITIVES))
  retval <- TYPEMAP_FOR_ALL_PRIMITIVES[indx]
  
  # find the indices null entries in 'retval'
  mk <- sapply(X=retval, FUN=function(x)is.null(x))
  
  if (any(mk)) {
    # go through all the non-primitives
    for (i in which(mk)) {
      fieldSchema <- readEntityDef(types[[i]])
      if (is.null(fieldSchema$properties) && !is.null(TYPEMAP_FOR_ALL_PRIMITIVES[fieldSchema$type])) {
        # it's an 'enum' or similar.  use the type of the field's schema
        retval[i] <- TYPEMAP_FOR_ALL_PRIMITIVES[fieldSchema$type]
      } else {
        # use the class name for this schema
        retval[i] <- getClassNameFromSchemaName(types[i])
      }
    }
  }
  
  names(retval) <- names(types)
  retval
}

# for class slots which are lists this gives the type of the list elements
mapTypesForListSlotsFromSchema <- function(schema) { 
  result<-list()
  for (name in names(schema$properties)) {
    prop<-schema$properties[[name]]
    if (prop$type=="array") {
      type<-prop$items[["type"]]
      ref<-prop$items[["$ref"]]
      if (!is.null(ref)) {
        result[[name]]<-ref
      } else {
        result[[name]]<-type
      }
    }
  }
  mapTypesForAllSlots(result)
}

mapTypesForListSlots <- function(className) {
  schema <- getSchemaFromCache(className)
  if (is.null(schema)) stop(sprintf("Missing schema for %s", className))
  mapTypesForListSlotsFromSchema(schema)
}


# given the type (and of the list elements if type==list) 
# and content represented in list form,
# construct and return the object used the auto-generated S4 classes
createS4ObjectFromList<-function(className, listElemType, content) {
  # if the list specifies a concrete type, then use it instead
  if (class(content)=="list") {
    concreteTypeSchemaName<-content$concreteType
    if (!is.null(concreteTypeSchemaName)) {
      concreteTypeClassName<-getClassNameFromSchemaName(concreteTypeSchemaName)
      if (is.null(getClass(concreteTypeClassName)@contains[[className]])) {
        stop(sprintf("concreteType %s specified for class %s", concreteTypeClassName, className))
      }
      className<-concreteTypeClassName
    }
  }
  if (isPrimitiveType(className)) {
    if (className=="list") {
      # recursively call this function for each list element
      lapply(X=content, FUN=function(elem) {
          if (isPrimitiveType(listElemType)) {
            elem
          } else {
            createS4ObjectFromList(listElemType, NULL, elem)
          }
      })
    } else {
      # just return 'content', unmodified
      content
    }
  } else {
    constructorArgs<-list()
    obj<-new(className)
    typesForListSlots <- mapTypesForListSlots(className)
    for (elemName in names(content)) {
      slotType <- class(slot(obj, elemName))
      slotValue <- content[[elemName]]
      # if 'elem' is a primitive, no conversion is needed
      if (isPrimitiveType(slotType)) {
        if (slotType=="list") {
          listElemType <- typesForListSlots[[elemName]]
          if (listElemType=="list") stop(sprintf("Lists of lists not supported. Type: %s, slot: %s", className, elemName))
          constructorArgs[[elemName]]<-createS4ObjectFromList(slotType, listElemType, slotValue)
        } else {
          # it's a simple primitive.  just pass it along
          constructorArgs[[elemName]]<-slotValue
        }
      } else {
        constructorArgs[[elemName]]<-createS4ObjectFromList(slotType, NULL, slotValue)
      }
    }
    do.call(className, constructorArgs)
  }
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


