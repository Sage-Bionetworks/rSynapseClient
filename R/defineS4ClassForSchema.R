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
  s4PropertyTypes<-mapSchemaTypeToS4Type(propertyTypes)
  
  
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
    slots[[propertyName]]<-s4PropertyTypes[[propertyName]]
  }
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
    # This is the way to define the slots as of R 3.0.0
    #slots = slots,
    # This is the deprecated way.  We have to use it to support pre-3.0 versions of R
    representation = do.call("representation", slots),
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
mapSchemaTypeToS4Type <- function(types) { 
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

# we define the schema for a slot of an S4 class to be
# the map ({...}) labeled by the property 'propertyName' within the
# 'properties' array of the overarching schema
getElemSchemaFromS4ClassSchema<-function(schema, propertyName) {
  schema$properties[[propertyName]]
}

# given the schema for an element which is an array,
# the array's "subschema" is the map labeled by "items"
getArraySubSchema<-function(schema) {
  schema$items
}

# get all the properties in the effective schema for the given schema
getEffectiveSchemaProperties <- function(schema) {
  # start with the properties for the immediate schema
  properties<-schema$properties
  implements <- getAllInterfaces(schema)
  if (length(implements)>0) {
    for (i in length(implements):1) {
      implementedSchema <- readEntityDef(implements[[i]])
      implementedProperties <- implementedSchema$properties
      for (n in names(implementedProperties))
        properties[[n]] <- implementedProperties[[n]]
    }
  }
  properties
}

getRTypeFromPropertySchema<-function(schema) {
  mapSchemaTypeToS4Type(schemaTypeFromProperty(schema))
}

# given the content represented in list form and the type
# construct and return the object used the auto-generated S4 classes
createS4ObjectFromList<-function(content, className) {
  if (is.null(content)) return(NULL)
  # if the list specifies a concrete type, then use it instead of the given class name
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
  
  schema <- getSchemaFromCache(className)
  if (is.null(schema)) stop(sprintf("Missing schema for %s", className))  
  effectiveProperties <- getEffectiveSchemaProperties(schema)
  # now replace the schema with its effective schema:
  schema <-list(properties=effectiveProperties)
  
  # these are the types that will be used for the S4 class slots
  schemaTypes<-getEffectiveSchemaTypes(schema)
  
  constructorArgs<-list()   
  for (schemaSlotName in names(schemaTypes)) {
    schemaSlotType<-schemaTypes[[schemaSlotName]] # type of the slot in schema 'language'
    s4SlotType <- mapSchemaTypeToS4Type(schemaSlotType) # type of the slot in R
    if (is.list(s4SlotType) && length(s4SlotType)==1) s4SlotType<-s4SlotType[[1]]
    if (is.list(content)) {
      slotValue <- content[[schemaSlotName]]
    } else {
      slotValue <- as.list(content)[[schemaSlotName]]
    }
    if (isPrimitiveType(s4SlotType)) {
      elemSchema<-getElemSchemaFromS4ClassSchema(schema, schemaSlotName)
      constructorArgs[[schemaSlotName]]<-createS4SlotValueFromList(slotValue, s4SlotType, elemSchema)
    } else {
      constructorArgs[[schemaSlotName]]<-createS4ObjectFromList(slotValue, s4SlotType)
    }
  }
  do.call(className, constructorArgs)
}

# This creates the slot content of an S4 object which is primitives or a list
# 'schema' is not needed when 'className' is an auto-generated S4 class name
createS4SlotValueFromList<-function(content, className, schema) {
  if (is.null(content)) return(NULL)
  if (!isPrimitiveType(className)) return(createS4ObjectFromList(content, className))
  if (className=="list") {
    # recursively call 'createS4ObjectFromListIntern' for each list element
    arraySubSchema<-getArraySubSchema(schema)
    listElemType <- getRTypeFromPropertySchema(arraySubSchema)
    if (is.list(listElemType) && length(listElemType)==1) listElemType<-listElemType[[1]]
    lapply(X=content, FUN=function(elem) {
        createS4SlotValueFromList(elem, listElemType, arraySubSchema)
    })
  } else {
    # just return 'content', but do handle some edge cases:
    if (schemaTypeFromProperty(schema)=="integer") {
      # a value may come in as 'numeric'
      as.integer(content)
    } else {
      content
    }
  }
}

# create a list version suitable for serialization to JSON,
# adhering to the schema
createListFromS4Object<-function(obj) {
  if (is.null(obj) || length(obj)==0) return(NULL)
  if (isPrimitiveType(class(obj))) {
    if (is.list(obj)) {
      result<-lapply(X=obj, FUN=createListFromS4Object)
      return(result)
    } else {
      return(obj)
    }
  }
  # at this point we know obj is an S4 class
  result <-list()
  className<-class(obj)
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
      if (isPrimitiveType(slotClass) && slotClass!="list") {
        # take care of some edge cases
        if (length(value)>0) {
          result[[slotName]]<-value
        }
      } else {
        result[[slotName]]<-createListFromS4Object(value)
      }
    }
  }
  # An object with no field values becomes an empty list.
  # To keep RJSONIO from erroneously encoding as an empty _JSON_ list ("[]")
  # we replace it with NULL
  if (length(result)==0) {
    NULL
  } else {
    result
  }
}



