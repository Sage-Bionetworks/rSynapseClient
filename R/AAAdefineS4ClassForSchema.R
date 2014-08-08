# This module generates S4 classes from JSON schemas.
# Note:  There is similar code in AAAschema, but it specifically generates
# classes which extend Entity.  This code is generic.
# 
# Author: brucehoff
###############################################################################


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
getS4MapName<-function() {"s4.map"}

getS4ClassNameFromSchemaName<-function(schemaName) {
  s4MapName <- getS4MapName()
  if (existsPackageVariable(s4MapName)) {
    s4Map<-getPackageVariable(s4MapName)
    result<-s4Map[[schemaName]]
  } else {
    result<-NULL
  }
  if (is.null(result)) {
    stop(sprintf("No S4 class name for %s", schemaName)) 
  } else {
    result
  }
}

setS4ClassNameForSchemaName<-function(schemaName, className) {
  s4MapName <- getS4MapName()
  s4Map <- getPackageVariable(s4MapName)
  if (is.null(s4Map)) s4Map<-list()
  s4Map[[schemaName]]<-className
  setPackageVariable(s4MapName, s4Map)
}

readS4ClassesToGenerate<-function() {
  read.table(
    system.file("resources/s4ClassesToGenerate.txt",package="synapseClient"), 
    header=TRUE, colClasses=c("character", "character", "logical"))
}

populateSchemaToClassMap<-function() {
  s4ClassesToAutoGenerate<-readS4ClassesToGenerate()
  
  for(i in 1:(dim(s4ClassesToAutoGenerate)[1])) { 
    schemaName<-s4ClassesToAutoGenerate[i,"schemaName"]
    className<-s4ClassesToAutoGenerate[i,"className"]
    setS4ClassNameForSchemaName(schemaName, className)
  }
}

# this is actually called in AAAschema, after other classes are defined
defineS4Classes<-function() {
  populateSchemaToClassMap()
  
  s4ClassesToAutoGenerate<-readS4ClassesToGenerate()
  
  for(i in 1:(dim(s4ClassesToAutoGenerate)[1])) { 
    schemaName<-s4ClassesToAutoGenerate[i,"schemaName"]
    defineS4ClassForSchema(schemaName)
  }
}

defineS4ClassForSchema <- function(fullSchemaName) { 
  name<-getS4ClassNameFromSchemaName(fullSchemaName)
  
  schemaDef <- readEntityDef(fullSchemaName, getSchemaPath())
  
  # make sure all extended classes are defined
  superClasses<-character()
  implements <- getImplements(schemaDef)
  if (!is.null(implements)) {
    for (i in implements) {
      implementsName <- getS4ClassNameFromSchemaName(i[["$ref"]])
      superClasses<-c(superClasses, implementsName)
     }
  }
  
  # slots defined by the schema:
  slots<-list()
  prototype<-list()
  for (propertyName in names(schemaDef$properties)) {
    propertySchema<-schemaDef$properties[[propertyName]]
    slotType <- defineRTypeFromPropertySchema(propertySchema)
    if (isPrimitiveType(slotType)) {
      slots[[propertyName]]<-slotType
    } else {
      nullableType <- nullableType(slotType)
      if (!isClassDefined(nullabeType)) {
        setClassUnion(nullableType, c("NullS4Object", slotType))
      }
      slots[[propertyName]]<-nullableType
      prototype[[propertyName]]<-new("NullS4Object")
    }
  }

  isVirtualClass <- isVirtual(schemaDef)
  if (isVirtualClass) {
    superClasses<-c(superClasses, "VIRTUAL")
  } else {
    if (length(implements)>0)
    prototype[["concreteType"]]<-fullSchemaName
  }
  
  setClass(
    Class = name,
    contains=superClasses,
    # This is the way to define the slots as of R 3.0.0
    #slots = slots,
    # This is the deprecated way.  We have to use it to support pre-3.0 versions of R
    representation = do.call("representation", slots),
    prototype = prototype,
    package="synapseClient"
  )
  
  name
}

defineS4ConstructorAndAccessors<-function(name) {
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
}

# define (or just return, for primitives) the class
# for the given property schema. There are three cases:
# 1) type is primitive.  Just return the R type
# 2) type is 'array'.  Define and return an S4 class for a typed list
# 3) type is defined by a schema.  Define and return an S4 class
# return the R class name for the propertySchema
defineRTypeFromPropertySchema <- function(propertySchema) {
  # This is the type 'in the language of the schema'
  schemaPropertyType<-schemaTypeFromProperty(propertySchema)
  primitiveRType<-TYPEMAP_FOR_ALL_PRIMITIVES[[schemaPropertyType]]
  if(length(primitiveRType)>0) {
    # No S4 class to define, just return type name
    primitiveRType
  } else if (schemaPropertyType=="array") {
    elemRType <- defineRTypeFromPropertySchema(getArraySubSchema(propertySchema))
    typeListClassName<-listClassName(elemRType)
    if (!isClassDefined(typeListClassName)) {
      # define the class
      setClass(
        Class=typeListClassName, 
        contains=list("TypedList"), 
        prototype=list(type=elemRType),
        package="synapseClient"
      )
      # define the constructor
      # This generic constructor takes the form:
      # ClassName(value1, value2, ...)
      assign(typeListClassName, function(...) {
          args <-list(...)
          obj<-new(typeListClassName)    
          set(obj, args)     
        })
      # If we don't define a 'generic' version of the constructor
      # we get an error when we try to include it as an export in
      # the NAMESPACE file.
      setGeneric(
        name=typeListClassName,
        def = function(...) {
          do.call(typeListClassName, list(...))
        }
      )
    }
    typeListClassName
  } else {
    # check for an enum
    propertySchema <- readEntityDef(schemaPropertyType, getSchemaPath())
    if (isEnum(propertySchema)) {
      # it's an 'enum' or similar. use the type of the property's schema
      return(TYPEMAP_FOR_ALL_PRIMITIVES[[propertySchema$type]])
    }
    
    getS4ClassNameFromSchemaName(schemaPropertyType)
  }
}

nullableType<-function(type) {
  sprintf("%sOrNull", type)
}

isNullableType<-function(type) {
  extends("NullS4Object", type)
}

getNonNullableType<-function(type) {
  if (!isNullableType(type)) stop(sprintf("%s is not a nullable type", type))
  nullSuffix <-"OrNull"
  nullSuffixLength <- nchar(nullSuffix)
  typeLength <- nchar(type)
  if (substring(type, typeLength-nullSuffixLength+1, typeLength)!=nullSuffix)  
    stop(sprintf("%s does not end with %s", type, nullSuffix))
  result <- substring(type, 1, typeLength-nullSuffixLength)
  if (!extends(result, type)) {
    stop(sprintf("%s is not a subclass of %s", result, type))
  }
  result
}
