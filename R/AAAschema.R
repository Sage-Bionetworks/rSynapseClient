#
# 
# Author: mfuria
###############################################################################

TYPEMAP_FOR_ENTITY_PROPS <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
  number = "numeric",
  object = "character",
  array = "character"
)

TYPEMAP_FOR_ALL_PRIMITIVES <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
  number = "numeric",
  array = "list",
  boolean = "logical"
)

getResources <- 
    function()
{
  RJSONIO::fromJSON(system.file("resources/Register.json", package="synapseClient"))
}

entitiesToLoad <- 
    function(resources = getResources())
{
  paths <- NULL
  
  classesToSkip<-c(
    "org.sagebionetworks.repo.model.FileEntity"
    )
  
  for(i in 1:length(resources$entityTypes)){
    thisOne <- resources$entityTypes[[i]]
    # We skip FileEntity, see .onLoad in zzz.R
    if (!any(thisOne$entityType==classesToSkip)) {
      paths <- unique(c(paths, thisOne$validParentTypes, thisOne$entityType))
    }
  }
  
  setdiff(paths, "DEFAULT")
}

readEntityDef <-
    function(name, path = system.file("resources/schema",package="synapseClient"))
{
  file <- sprintf("%s.json", gsub("[\\.]", "/", name))
  
  fullPath <- file.path(path,file)
  
  if(!file.exists(fullPath))
    stop(sprintf("Could not find file: %s for entity: %s", fullPath, name))

  schema <- fromJSON(fullPath, simplifyWithNames = FALSE)
  
  schema
}

# 'which' is the full class name
# 'name' is the Class name.  If omitted it's the suffix of 'which', e.g. 
# if 'which' is "org.sagebionetworks.repo.model.Folder" and 'name' is omitted,
# then the Class name is "Folder".
defineEntityClass <- 
  function(which, name, where = parent.frame(), package)
{
  entityDef <- readEntityDef(which)
  
  if(missing(name))
    name <- getClassNameFromSchemaName(which)
  
  if(is.null(name) | name == "")
    stop("name must not be null")
  
  implements <- unique(c(entityDef$implements[[1]][[1]], getAllInterfaces(entityDef$implements[[1]][[1]])))
  
  if ("org.sagebionetworks.repo.model.Locationable" %in% implements) {
    contains <- "Locationable"
  } else if ("org.sagebionetworks.repo.model.Entity" %in% implements) {
    contains <- "Entity"
  } else {
    stop(sprintf("%s must contain Entity or Locationable", name))
  }
  
  setClass(
    Class = name,
    contains = contains,
    prototype = prototype(
      synapseEntityKind = name,
      properties = SynapseProperties(getEffectivePropertyTypes(which))
    ),
    package=package
  )
}

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

#  # This generic constructor takes the form:
#  # ClassName(slot1=value1, slot2=value2, ...)
#  setMethod(
#    f = name,
#    signature = "missing",
#    definition = function(...) {
#      args <-list(...)
#      obj<-new(name)
#      for (slot in names(args)) {
#        obj@slot<-args[[slot]]
#      }
#      obj
#    },
#    where = where
#  )


}

defineEntityConstructors <-
  function(which, name, overrideExiting = FALSE, where = parent.frame(), package)
{
  if(missing(name))
    name <- gsub("^.+[\\.]", "", which)
  
  if(is.null(name) | name == "")
    stop("name must not be null")
  ## define the generic

  if(overrideExiting | is.null(getGeneric(name))){
    setGeneric(
      name=name,
      def = function(entity, ...){
        do.call(name, list(...))
      },
      package = package
    )
    setMethod(
      f = name,
      signature = "list",
      definition = function(entity, ...){
        classType <- name
        synapseType <- which
        ## GRAB NAMED ARGUMENTS AND ADD TO ENTITY LIST
        argList <- list(...)
        entity <- c(entity, argList)

        if(length(entity) > 0){
          if(any(names(argList) == ""))
            stop(sprintf("Arguments passed to %s must be named", classType))
        }
        
        ee <- new(classType)
        for(prop in names(entity))
          propertyValue(ee, prop) <- entity[[prop]]
        if (is(ee, "Entity")) propertyValue(ee, "concreteType") <- synapseType
        ee
      },
      where = where
    )

    setMethod(
      f = name,
      signature = "missing",
      definition = function(...){
        classType <- name
        synapseType <- which
        ## GRAB NAMED ARGUMENTS AND ADD TO ENTITY LIST
        entity <- list(...)
        do.call(name, list(entity))
      },
      where = where
    )
  }
}

getPropertyTypes <- function(which, entityDef)
{
  if(!missing(which) && !missing(entityDef))
    stop("must specify either 'which' or 'entityDef', but not both")
  
  if(!missing(which))
    entityDef <- readEntityDef(which)
  
  properties <- lapply(
    X = names(entityDef$properties), 
    FUN = function(prop){
      theprop <- entityDef$properties[[prop]]
      type<-theprop[["type"]]
      ref<-theprop[["$ref"]]
      if (!is.null(ref)) {
        ref
      } else {
        type
      }
    }
  )
  names(properties) <- names(entityDef$properties)
  properties
}

getEffectivePropertyTypes <- function(which)
{
  implements <- c(which, getAllInterfaces(which))
  
  properties <- list()
  i <- length(implements)
  while(i > 0){
    thisProp <- getPropertyTypes(implements[i])
    thisProp<-mapTypesForEntityProps(thisProp)
    for(n in names(thisProp))
      properties[[n]] <- thisProp[[n]]
    i <- i - 1
  }
  
  properties
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

mapTypesForEntityProps <-  function(types) { 
  indx <- match(types, names(TYPEMAP_FOR_ENTITY_PROPS))
  retval <- TYPEMAP_FOR_ENTITY_PROPS[indx]
  
  mk <- sapply(X=retval, FUN=function(x)is.null(x))
  
  if (any(mk)) {
    retval[mk] <- "character"
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

getAllInterfaces <- function(which){
  if(is.null(which))
    return(NULL)
  thisDef <- readEntityDef(which)
  implements <- NULL
  while(!is.null(thisDef$implements)){
    implements <- c(implements, thisDef$implements[[1]][[1]])
    
    try({
        thisDef <- readEntityDef(thisDef$implements[[1]][[1]])
      }, silent = TRUE)
  }
  implements
}

