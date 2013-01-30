# synapseExecute
# convenience function for executing code, saving it in Synapse, and creating provenance record
#
# Author:  Adam Margolin
# adapted by: Bruce Hoff
#
# adaptations made for inclusion in synapseClient:
# - redefined isSynapseId
# - createUsedEntities recurses on vectors as well as lists
# - made entity name 'scrubbing' comphrehensive
# - code project is an input parameter


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
      usedEntitiesList <- append(usedEntityList, createUsedEntitiesList(argVal)
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
  if (is.null(entityId)){
    entity <- do.call(entityType, list(name=name, parentId=parentId))
    storeEntity(entity)
  }else{
    getEntity(entityId$entity.id)
  }
}

# Entity names may only contain: letters, numbers, spaces, underscores, hypens, periods, plus signs, and parentheses
# any other characters are replaced by '+' signs
scrubEntityName<-function(s) {
  gsub("[^a-zA-Z0-9_.+() -]", "+", s)
}

createGithubCodeEntity <- function(repoName, sourceFile){
  githubRepo <- getRepo(repository=repoName)
  
  githubCodeProjectId <- "syn1583141"
  
  synapseRepoName <- gsub("/", "+", repoName)
  synapseSourceFile <- gsub("/", "+", sourceFile)
  
  ##### is there a standard R client function like getOrCreateEntity?
  repoEntity <- getOrCreateEntity(name=synapseRepoName, parentId=githubCodeProjectId, entityType="Folder")
  commitEntity <- getOrCreateEntity(name=as.character(githubRepo@commit), parentId=repoEntity$properties$id, entityType="Folder")
  sourceFileEntity <- getOrCreateEntity(name=synapseSourceFile, parentId=commitEntity$properties$id, entityType="Code")
  
  githubURL <- paste("https://raw.github.com", githubRepo@user, githubRepo@repo, githubRepo@commit, sourceFile, sep="/")
  
  ##### need a more robust way of handling Code entities pointing to GitHub
  sourceFileEntity$annotations$githubURL <- githubURL
  sourceFileEntity$properties$description <- getURLContent(githubURL)
  sourceFileEntity <- storeEntity(sourceFileEntity)
  
  return(sourceFileEntity)
}


synapseExecute <- function(activityFunctionRef, args, resultParentId, resultEntityProperties = NULL, 
  resultEntityName=NULL, functionResult=NULL){
#   resultEntityName <- makeProvenanceEntityName(activityFunctionRef, args)
  usedEntitiesList <- createUsedEntitiesList(args)
  
  print(paste("Executing function"))
  
  #### should probably support activityFunctionRef being a GitHubRepo, a function name (both handled below)
  #### of a file containing Code, which it would copy to a Synapse Code entity.
  
  ##### would be better to use Brian's GitHub client to represent a file and not just a repo so we can check for class of type
  ##### GithubFile rather than checking for a list #############
  if (is.list(activityFunctionRef)){
    executionCodeEntity <- createGithubCodeEntity(repoName = activityFunctionRef$repoName, sourceFile = activityFunctionRef$sourceFile)
    executionCode <- source_url(executionCodeEntity$annotations$githubURL)
    functionResult <- do.call(executionCode$value, args)
    
    usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=executionCodeEntity$properties$id, wasExecuted=TRUE)
    
    activity <- Activity(list(name = activityFunctionRef$sourceFile, used = usedEntitiesList))
    activity <- createEntity(activity)
    
  }else if (is.character(activityFunctionRef) || is.function(activityFunctionRef)){
    
    functionResult <- do.call(activityFunctionRef, args)
  }
#   executionCode <- loadEntity(executionCodeId)
#   functionResult <- do.call(executionCode$objects[[1]], args)
  
  print(paste("Executing of function complete"))
  
  resultEntity <- Data(name=resultEntityName, parentId = resultParentId)
  generatedBy(resultEntity) <- activity
  
  resultEntity <- addObject(resultEntity, functionResult, name="functionResult")
  resultEntity <- addObject(resultEntity, args, name="functionArgs")
  if(!is.null(resultEntityProperties)){
    resultEntity <- addObject(resultEntity, resultEntityProperties)
  }
  
  resultEntity <- storeEntity(resultEntity)
  
  return(resultEntity)
}
