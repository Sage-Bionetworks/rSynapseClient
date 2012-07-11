# TODO: Add comment
# 
# Author: mfuria
###############################################################################

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
    function(entityDef, name, where = parent.frame())
{
  if(missing(name))
    name <- entityDef$title
  
  if(is.null(name))
    stop("name must not be null")
  
  implements <- unique(c(entityDef$implements[[1]][[1]], synapseClient:::getImplements(entityDef$implements[[1]][[1]])))
  
  if("org.sagebionetworks.repo.model.Locationable" %in% implements){
    contains <- "SynapseLocationOwner"
  }else{
    contains <- "SynapseEntity"
  }
  
  propTypes <- synapseClient:::getEffectivePropertyTypes(entityDef = entityDef)
  
  setClass(
      Class = name,
      contains = contains,
      representation = propTypes,
      prototype = prototype(
          synapseEntityKind = name,
          properties = list(names(propTypes))
      ),
      where = where
  )
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
  TYPEMAP <- list(
      string = "character",
      integer = "integer",
      float = "numeric",
      object = "character",
      array = "character"
  )
  
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

