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
    function(name, path = system.file("resources/schema",package="synapseClient"), fixTypes = TRUE)
{
  file <- sprintf("%s.json", gsub("[\\.]", "/", name))
  
  fullPath <- file.path(path,file)
  
  if(!file.exists(fullPath))
    stop(sprintf("Could not find file: %s for entity: %s", fullPath, name))

  schema <- fromJSON(fullPath, simplifyWithNames = FALSE)
  if(fixTypes)
    schema <- fixTypes(schema)
  
  schema
}

defineEntityClass <- 
    function(entityDef, name = entityDef$title)
{
  if(is.null(name))
    stop("name must not be null")
  contains <- switch(
      entityDef$implements[[1]][[1]],
      org.sagebionetworks.repo.model.GenericData = "SynapseLocationOwner",
      "SynapseEntity"
  )
  
  properties <- lapply(
      X = names(entityDef$properties), 
      FUN = function(prop){
        entityDef$properties[[prop]][["type"]]
      }
  )
  names(properties) <- names(entityDef$properties)
  
  setClass(
      Class = name,
      contains = contains,
      representation = representation(properties),
      prototype = prototype(
          synapseEntityKind = name,
          description = entityDef$description
      )
  )
}


fixTypes <- 
    function(entityDef)
{
  TYPEMAP <- list(
      string = "character",
      integer = "long",
      float = "numeric"
  )
  
  entityDef
}


