#
# 
# Author: mfuria
###############################################################################

TYPEMAP <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
  number = "numeric",
  object = "character",
  array = "character"
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

#------------------------------------------------------------
# Utilities for setting and getting package level variables
# 
getPackageEnvironment<-function() {
  parent.env(environment(entitiesToLoad)) # get the package's environment
}

# returns TRUE iff the given name exists in the package environment
existsPackageVariable<-function(name) {
  packageEnv<-getPackageEnvironment()
  exists(x=name, envir=packageEnv)
}

# returns the value for the given name or NULL if the name is not defined
getPackageVariable<-function(name) {
  packageEnv<-getPackageEnvironment()
  if (exists(x=name, envir=packageEnv)) {
    get(x=name, envir=packageEnv)
  } else {
    NULL
  }
}

setPackageVariable<-function(name, value) {
  packageEnv<-getPackageEnvironment()
  assign(x=name, value=value, envir=packageEnv)
}
#------------------------------------------------------------

getSchemaCacheName<-function() {"schema.cache"}

getSchemaFromCache<-function(schemaName) {
  schemaCacheName <- getSchemaCacheName()
  if (existsPackageVariable(schemaCacheName)) {
    schemaCache<-getPackageVariable(schemaCacheName)
    schemaCache[[schemaName]]
  } else {
    NULL
  }
}

putSchemaToCache<-function(key, value) {
  schemaCacheName <- getSchemaCacheName()
  schemaCache <- getPackageVariable(schemaCacheName)
  if (is.null(schemaCache)) schemaCache<-list()
  schemaCache[[key]]<-value
  setPackageVariable(schemaCacheName, schemaCache)
}

# Omit the part of the string preceding the last "." (if any)
getClassNameFromSchemaName<-function(schemaName) {
  if (is.null(schemaName)) return(NULL)
  result<-gsub("^.+[\\.]", "", schemaName)
  names(result)<-names(schemaName)
  result
}

getSchemaPath<-function() {
  system.file("resources/schema",package="synapseClient")
}

readEntityDef <-
    function(name, path)
{ 
  result<-getSchemaFromCache(name)
  if (!is.null(result)) {
    return(result)
  }
  schema<-readSchema(name, path)  
  putSchemaToCache(name, schema)
  schema
}

# 'which' is the full class name
# 'name' is the Class name.  If omitted it's the suffix of 'which', e.g. 
# if 'which' is "org.sagebionetworks.repo.model.Folder" and 'name' is omitted,
# then the Class name is "Folder".
defineEntityClass <- 
  function(which, name, where = parent.frame(), package)
{
  entityDef <- readEntityDef(which, getSchemaPath())
  
  if(missing(name))
    name <- getClassNameFromSchemaName(which)
  
  if(is.null(name) | name == "")
    stop("name must not be null")
  
  implementsSchemaName <-entityDef$implements[[1]][[1]]
  implementsSchema<-readEntityDef(implementsSchemaName, getSchemaPath())
  implements <- unique(c(implementsSchemaName, getAllInterfaces(implementsSchema, getSchemaPath())))
  
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


mapTypes <- function(types) { 
  indx <- match(types, names(TYPEMAP))
  retval <- TYPEMAP[indx]
  
  mk <- sapply(X=retval, FUN=function(x)is.null(x))
  
  if (any(mk)) {
    retval[mk] <- "character"
  }
  
  names(retval) <- names(types)
  retval
}

getEffectivePropertyTypes <-function(schemaName) {
  schema<-readEntityDef(schemaName, getSchemaPath())
  mapTypes(getEffectiveSchemaTypes(schema, getSchemaPath()))
}

getPropertyTypes <- function(entityDef) {
  properties <- lapply(
    X = names(entityDef$properties), 
    FUN = function(prop){
      theprop <- entityDef$properties[[prop]]
      schemaTypeFromProperty(theprop)
    }
  )
  names(properties) <- names(entityDef$properties)
  properties
}

getEffectiveSchemaTypes <- function(schema, schemaPath) {
  # start with the properties for the immediate schema
  properties<-getPropertyTypes(schema)
  implements <- getAllInterfaces(schema, schemaPath)
  if (length(implements)>0) {
     for (i in length(implements):1) {
      thisProp <- getPropertyTypes(readEntityDef(implements[i], getSchemaPath()))
      for (n in names(thisProp))
        properties[[n]] <- thisProp[[n]]
    }
  }
  properties
}






