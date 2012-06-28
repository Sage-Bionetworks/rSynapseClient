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

  fromJSON(fullPath, simplifyWithNames = FALSE)
}

defineEntityClass <- 
    function(name, entityDef)
{
  setClass(
      entityDef$title,
      contains = "SynapseEntity",
      prototype = prototype(
          synapseEntityKind = name,
          description = entityDef$description
      )
  )
}

