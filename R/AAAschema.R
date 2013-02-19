# TODO: Add comment
# 
# Author: mfuria
###############################################################################

TYPEMAP <- list(
  string = "character",
  integer = "integer",
  float = "numeric",
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
  
  for(i in 1:length(resources$entityTypes)){
    thisOne <- resources$entityTypes[[i]]
    paths <- unique(c(paths, thisOne$validParentTypes, thisOne$entityType))
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

defineEntityClass <- 
    function(which, name, where = parent.frame(), package)
{
  entityDef <- readEntityDef(which)

  if(missing(name))
    name <- gsub("^.+[\\.]", "", which)
  
  if(is.null(name) | name == "")
    stop("name must not be null")
  
  implements <- unique(c(entityDef$implements[[1]][[1]], synapseClient:::getImplements(entityDef$implements[[1]][[1]])))
  
  if("org.sagebionetworks.repo.model.Locationable" %in% implements) {
    contains <- "Locationable"
  } else if ("org.sagebionetworks.repo.model.FileEntity" %in% implements) {
    contains <- "FileEntity"
  } else{
    contains <- "Entity"
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
        do.call("standardGeneric", list(name))
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
        propertyValue(ee, "entityType") <- synapseType
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

getPropertyTypes <- 
  function(which, entityDef, mapTypes=TRUE)
{
  if(!missing(which) && !missing(entityDef))
    stop("must specifiy either 'which' or 'entityDef', but not both")
  
  if(!missing(which))
    entityDef <- synapseClient:::readEntityDef(which)
  
  properties <- lapply(
    X = names(entityDef$properties), 
    FUN = function(prop){
      entityDef$properties[[prop]][["type"]]
    }
  )
  names(properties) <- names(entityDef$properties)
  if(mapTypes)
    return(mapTypes(properties))
  properties
}

getEffectivePropertyTypes <-
  function(which, mapTypes = TRUE)
{
  implements <- c(which, synapseClient:::getImplements(which))
  
  properties <- list()
  i <- length(implements)
  while(i > 0){
    thisProp <- synapseClient:::getPropertyTypes(implements[i], mapTypes = mapTypes)
    for(n in names(thisProp))
      properties[[n]] <- thisProp[[n]]
    i <- i - 1
  }
  
  properties
}


mapTypes <- 
    function(types)
{ 
  indx <- match(types, names(TYPEMAP))
  retval <- TYPEMAP[indx]
  
  mk <- is.na(retval)
  if(any(mk))
    retval[mk] <- "character"
  
  names(retval) <- names(types)
  retval
}

getImplements <- function(which){
  if(is.null(which))
    return(NULL)
  thisDef <- synapseClient:::readEntityDef(which)
  implements <- NULL
  while(!is.null(thisDef$implements)){
    implements <- c(implements, thisDef$implements[[1]][[1]])
    
    try({
        thisDef <- synapseClient:::readEntityDef(thisDef$implements[[1]][[1]])
      }, silent = TRUE)
  }
  implements
}

