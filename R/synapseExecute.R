# synapseExecute
# convenience function for executing code, saving it in Synapse, and creating provenance record
#
# Author:  Adam Margolin
# adapted by: Bruce Hoff
#
# adaptations made for inclusion in synapseClient:
# - defined isSynapseId (check for numeric suffix)
# - createUsedEntities recurses on vectors as well as lists
# - entity name 'scrubbing' converts all illegal characters
# - TODO code project is an input parameter
# - TODO unit, integration tests

# TODO rGithubClient isn't 'required', need allow synapseExecute to work without it
require(rGithubClient)
require(synapseClient)
require(devtools)
require(RCurl)

## returns TRUE iff 's' is Synapse ID, e.g. "syn123456"
## returns FALSE for vectors, lists (anything not a string)
## a string is considered to be a Synapse ID if it begins with 'syn' and ends with an integer
isSynapseId<-function(s) {
  if (!(class(s)=="character" && length(s)==1 && substr(s, 1, 3)=="syn" && nchar(s)>3)) {
    F
  } else {
    # now check whether the suffix is numeric
    coerced <-try(as.numeric(substr(s, 4, nchar(s))), silent=T)
    (class(coerced)!="try-error")
   }
}

# creates a list of synapse IDs extracted from the argument in the form required
# for the 'used entity' list of an Activity
createUsedEntitiesList <- function(args) {  
  usedEntitiesList <- list()
  for (argVal in args) {
    if (is.vector(argVal)) {
      usedEntitiesList <- append(usedEntityList, createUsedEntitiesList(argVal))
    } else if (isSynapseId(argVal)) {
      usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=argVal, wasExecuted=FALSE)
    } else if (class(argVal) %in% c("Data", "Code", "Folder") ){ # TODO what if it's some other Synapse type
      usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=argVal$properties$id, wasExecuted=FALSE)
    }
  }
  usedEntitiesList
}


# Retrieve the entity having the given name and parentId
# If the entity does not exist, then create it (using entityType as the type of object to create)
# Note: The query and creation are not in the same transaction.  However, if the object is created
# in another thread between the two requests, an error will be raised during the create operation
# since name uniqueness under a parent is enforced by Synapse
getOrCreateEntity <- function(name, parentId, entityType) {
  entityId <- synapseQuery(sprintf("select id from entity where entity.parentId =='%s' AND entity.name='%s'", parentId, name))
  if (is.null(entityId)) {
    entity <- do.call(entityType, list(name=name, parentId=parentId))
    storeEntity(entity)
  } else {
    getEntity(entityId$entity.id)
  }
}

# Like getOrCreateEntity, but only for projects and does not require a parentId
getOrCreateProject <- function(name) {
  entityId <- synapseQuery(sprintf("select id from Project where entity.name='%s'", name))
  if (is.null(entityId)) {
    project <- Project(list(name=name))
    storeEntity(project)
  } else {
    getEntity(entityId$entity.id)
  }
  
}

# Entity names may only contain: letters, numbers, spaces, underscores, hypens, periods, plus signs, and parentheses
# any other characters are replaced by '+' signs
scrubEntityName<-function(s) {
  gsub("[^a-zA-Z0-9_.+() -]", "+", s)
}

isGithubClientInstalled<-function() {
  "rGithubClient" %in% rownames(installed.packages())
}

#
# uses this synapse organization:
#     RootFolder > GitRepoName > CurrentGitCommit > SourceCode
# creates any entities that don't exist, constructs the URL, 
# then returns the created or updated Code entity
#
# TODO:  allow a default code project, e.g. define a default name and getOrCreate it
#
createGithubCodeEntity <- function(repoName, sourceFile, githubCodeProjectId) {
  githubRepo <- getRepo(repository=repoName)
  
  synapseRepoName <- scrubEntityName(repoName)
  synapseSourceFile <- scrubEntityName(sourceFile)
  
  repoEntity <- getOrCreateEntity(name=synapseRepoName, parentId=githubCodeProjectId, entityType="Folder")
  commitEntity <- getOrCreateEntity(name=as.character(githubRepo@commit), parentId=repoEntity$properties$id, entityType="Folder")
  sourceFileEntity <- getOrCreateEntity(name=synapseSourceFile, parentId=commitEntity$properties$id, entityType="Code")
  
  githubURL <- paste("https://raw.github.com", githubRepo@user, githubRepo@repo, githubRepo@commit, sourceFile, sep="/")
  
  # TODO instead of an annotation this should be the location
  sourceFileEntity$annotations$githubURL <- githubURL
  # TODO delineate code with 'markdown' code tags
  sourceFileEntity$properties$description <- getURLContent(githubURL)
  # TODO: is new version generated if code changes?
  sourceFileEntity <- storeEntity(sourceFileEntity)
  
  return(sourceFileEntity)
}

hasRSuffix<-function(fileName) {
  ".R"==toupper(substr(fileName, nchar(fileName)-1, nchar(fileName)))
}

# TODO finish this
#
# executable - what to execute.  choices are:
#			- Code in github: list(repoName="...", sourceFile="...")  (rGithubClient package required)
#	    - a file path whose file contains a matching function (exeutable = <folder path>/<function name>.R)
#			- a function (no Code object created)
#	args - arguments for the given function
# resultParentId - ID of the Synapse container (e.g. Project or Folder) where the result shall go
# resultEntityProperties - annotations to be added to the resulting entity
# resultEntityName - name of the Data object containing the result
#
# Execute the given function with the given arguments. 
# Store the resulting R object as a new Data object (or a new revision of an existing Data object)
# having the given name and in the given parent container
# Set the given properties on the object.
#
# Also, store the given function as Code in Synapse and
# create a provenance record connecting the result to 
# the executed code and the input arguments.
#
synapseExecute <- function(executable, args, resultParentId, resultEntityProperties = NULL,  resultEntityName=NULL) {

  usedEntitiesList <- createUsedEntitiesList(args)
  
  if (is.list(executable)) {
    ## check list contents and check that rGithubRepo is there
    executionCodeEntity <- createGithubCodeEntity(repoName = executable$repoName, sourceFile = executable$sourceFile)
    executionCode <- source_url(executionCodeEntity$annotations$githubURL)
    functionResult <- do.call(executionCode$value, args)
    
    usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=executionCodeEntity$properties$id, wasExecuted=TRUE)
    
    activity <- Activity(list(name = executable$sourceFile, used = usedEntitiesList))
    activity <- createEntity(activity)
    
  } else if (is.character(executable)) {
    if (!hasRSuffix(executable)) stop(sprintf("executable file %s does have '.R' ending.", executable))
    fileName <- basename(executable)
    functionName <- substr(fileName, 1, nchar(fileName)-2)
    source(executable)
    functionResult <- do.call(functionName, args)
  } else if (is.function(executable)) {
    functionResult <- do.call(executable, args)
  } else {
    stop("Unexpected executable type.") # TODO give more information
  }
  
  resultEntity <- Data(name=resultEntityName, parentId = resultParentId)
  generatedBy(resultEntity) <- activity
  
  resultEntity <- addObject(resultEntity, functionResult, name="functionResult")
  # TODO move these to annotations (can an annotation be JSON???) in a separate entity resultEntity <- addObject(resultEntity, args, name="functionArgs")
  if(!is.null(resultEntityProperties)){
  # TODO move these to annotations resultEntity <- addObject(resultEntity, resultEntityProperties)
  }
  
  resultEntity <- storeEntity(resultEntity)
  
  return(resultEntity)
}
