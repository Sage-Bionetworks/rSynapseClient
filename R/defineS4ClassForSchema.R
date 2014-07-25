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

#getOrCreateUnionOfTypeWithNull<-function(type) {
#  if (missing(type) || is.null(type) || length(type)==0) stop("getOrCreateUnionOfTypeWithNull: type is missing, null, or empty.")
#  unionName<-sprintf("synapseClient_%sOrNull",type)
#  if (!isClassUnion(unionName)) {
#    setClassUnion(unionName, c(type, "NULL"))
#  }
#  unionName
#}

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
  for (propertyTypeName in names(s4PropertyTypes)) {
    propertyType<-propertyTypes[[propertyTypeName]]
    s4PropertyType<-s4PropertyTypes[[propertyTypeName]] # notably this provides the fully qualified schema name
    # check whether the type is one of the known primitives
    if (!isPrimitiveType(s4PropertyType)) {
      # if it is not one of the known primitives then try instantiating it
      if (!isClassDefined(s4PropertyType)) {
        defineS4ClassForSchema(propertyType)
      }
    }
  }
  
  # slots defined by the schema:
  slots<-list()
  for (propertyName in names(s4PropertyTypes)) {
    #unionTypeName<-getOrCreateUnionOfTypeWithNull(s4PropertyTypes[[propertyName]])
    slots[[propertyName]]<-s4PropertyTypes[[propertyName]]
  }
  # metadata slots required by the client:
  #characterAndNullTypeName<-getOrCreateUnionOfTypeWithNull("character")
  slots<-append(slots, list(
    updateUri="character" #characterAndNullTypeName # URI for updating objects of this type
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
  
  if (!isVirtualClass) {
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
  if (is.null(content)) return(NULL)
  # if the list specifies a concrete type, then use it instead
  if (is.list(content)) {
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
    typesForListSlots <- mapTypesForListSlots(className)
    
    schema <- getSchemaFromCache(className)
    if (is.null(schema)) stop(sprintf("Missing schema for %s", className))
    # these are the types that will be used for the S4 class slots
    s4PropertyTypes<-mapTypesForAllSlots(getEffectiveSchemaTypes(schema))
    
    for (elemName in names(s4PropertyTypes)) {
      slotType <- s4PropertyTypes[[elemName]]
      if (is.list(content)) {
        slotValue <- content[[elemName]]
      } else {
        slotValue <- as.list(content)[[elemName]]
      }
      # if 'elem' is a primitive, no conversion is needed
      if (isPrimitiveType(slotType)) {
        if (slotType=="list") {
          subListElemType <- typesForListSlots[[elemName]]
          if (subListElemType=="list") stop(sprintf("Lists of lists not supported. Type: %s, slot: %s", className, elemName))
          constructorArgs[[elemName]]<-createS4ObjectFromList(slotType, subListElemType, slotValue)
        } else {
          # it's a simple primitive.  just pass it along
          # handle some edge cases:
          if (slotType=="integer") {
            # a value may come in as 'numeric'
            constructorArgs[[elemName]]<-as.integer(slotValue)
          } else {
            constructorArgs[[elemName]]<-slotValue
          }
        }
      } else {
        constructorArgs[[elemName]]<-createS4ObjectFromList(slotType, NULL, slotValue)
      }
    }
    do.call(className, constructorArgs)
  }
}

# create a list version suitable for serialization to JSON,
# adhering to the schema
createListFromS4Object<-function(obj) {
  if (is.null(obj) || length(obj)==0) return(NULL)
  result <-list()
  className<-class(obj)
  typesForListSlots <- mapTypesForListSlots(className)
  schemaDef<-getSchemaFromCache(className)
  effectiveSlotTypes<-getEffectiveSchemaTypes(schemaDef)
    
  
  # Note the slots are named after the schema properties, but may be a *superset*
  # so we must make the list based on the schema rather than the slots
  for (slotName in names(effectiveSlotTypes)) {
    value<-slot(obj, slotName)
    if (is.null(value)) {
      # skip this field
    } else {
      slotClass<-class(value)
      if (isPrimitiveType(slotClass)) {
        if (slotClass=="list") {
          elemType<-typesForListSlots[[slotName]]
          if (isPrimitiveType(elemType)) {
            result[[slotName]]<-value
          } else {
            result[[slotName]]<-lapply(value, FUN=function(elem){createListFromS4Object(elem)})
          }
        } else {
          # take care of some edge cases
          if (length(value)>0) {
            result[[slotName]]<-value
          }
        }
      } else {
        result[[slotName]]<-createListFromS4Object(value)
      }
    }
  }
  result
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


