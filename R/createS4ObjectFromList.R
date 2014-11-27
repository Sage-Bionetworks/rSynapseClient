# given the content represented in list form and the type,
# construct and return the object using the auto-generated S4 classes
# 
# Author: brucehoff
###############################################################################

createS4ObjectFromList<-function(content, className) {
  if (is.null(content)) return(NULL)
  if (!is.list(content) && length(content)>1) {
    # it's a vector.  Coerce to a list
    content<-as.list(content)
  }
  # if the list specifies a concrete type, then use it instead of the given class name
  if (is.list(content)) {
    concreteTypeSchemaName<-content$concreteType
    if (!is.null(concreteTypeSchemaName)) {
      concreteTypeClassName<-getS4ClassNameFromSchemaName(concreteTypeSchemaName)
      if (!extends(concreteTypeClassName, className)) {
        stop(sprintf("concreteType %s specified for class %s", concreteTypeClassName, className))
      }
      className<-concreteTypeClassName
    }
  }
  
  constructorArgs<-list()   
  slotTypes<-getSlots(className)
  for (slotName in names(content)) {
    s4SlotType <- slotTypes[slotName]
    if (is.na(s4SlotType)) stop(sprintf("No slot %s in %s", slotName, className))
    
    slotValue <- content[[slotName]]
    
    if (isPrimitiveType(s4SlotType)) {
      if (is(slotValue, "list")) {
        slotValue<-unlist(slotValue) 
      }
      if (s4SlotType=="integer") {
        # a value may come in as 'numeric'
        slotValue<-as.integer(slotValue)
      }
      constructorArgs[[slotName]]<-slotValue
    } else {
      if (!isNullableType(s4SlotType)) {
        # something has gone wrong.  Non-primitive slots should extend a nullable type
        stop("%s is not a 'nullable' type.", s4SlotType)
      }
      # want to make one of these:
      s4SlotType <- getNonNullableType(s4SlotType)
      if (extends(s4SlotType, "TypedList")) {
        constructorArgs[[slotName]]<-createTypedListFromList(slotValue, s4SlotType)
      } else {
        constructorArgs[[slotName]]<-createS4ObjectFromList(slotValue, s4SlotType)
      }
    }
  }
  do.call(className, constructorArgs)
}

createTypedListFromList<-function(content, className) {
  if (!extends(className, "TypedList")) 
    stop(sprintf("Expected TypedList subclass but found %s", className))
  result<-new(className)
  listElementType <- result@type
  isPrimitive<-isPrimitiveType(listElementType)
  isTypedList<-extends(listElementType, "TypedList")
  
  for (elem in content) {
    value<-NULL
    if (isPrimitive) {
      value<-elem
    } else if (isTypedList) {
      value<-createTypedListFromList(elem, listElementType)
    } else {
      value<-createS4ObjectFromList(elem, listElementType)
    }
    result<-append(result, value)
  }
  result
}

