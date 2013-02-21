# synapseExecute
# convenience function for executing code, saving it in Synapse, and creating provenance record
#
# Author:  Adam Margolin
# adapted by: Bruce Hoff
#
# adaptations made for inclusion in synapseClient:
# - function may either (1) have any name, if the only thing in the file or (2) have a name matching the file name
# - defined isSynapseId (check for numeric suffix)
# - createUsedEntities recurses on vectors as well as lists
# - entity name 'scrubbing' converts all illegal characters
# - local file, as well as github file, gets stored as Code object
# - check whether arg is Entity using S4 methods 'isClass' and 'extends'
# - uses markdown formatting to add code to entity description
# - args and properties are added to result entity as individual annotations, for visibility in Web UI
# - documented
# - tested

## returns TRUE iff 's' is Synapse ID, e.g. "syn123456"
## returns FALSE for vectors, lists (anything not a string)
## a string is considered to be a Synapse ID if it begins with 'syn' and ends with an integer
isSynapseId<-function(s) {
  if (!(class(s)=="character" && length(s)==1 && substr(s, 1, 3)=="syn" && nchar(s)>3)) {
    F
  } else {
    # now check whether the suffix is numeric
    any(grep("^[[:digit:]]*$", substr(s, 4, nchar(s))))
   }
}

# creates a list of synapse IDs extracted from the argument in the form required
# for the 'used entity' list of an Activity
createUsedEntitiesList <- function(args) {  
  usedEntitiesList <- list()
  for (argVal in args) {
    if (!is.scalar(argVal)) {
      usedEntitiesList <- append(usedEntitiesList, createUsedEntitiesList(argVal))
    } else if (isSynapseId(argVal)) {
      usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=argVal, wasExecuted=FALSE)
    } else if (extends(class(argVal), "Entity") && !is.null(propertyValue(argVal, "id"))) {
      usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=propertyValue(argVal, "id"), wasExecuted=FALSE)
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
  entityId <- synapseQuery(sprintf("select id from entity where entity.parentId=='%s' AND entity.name=='%s'", parentId, name))
  if (is.null(entityId)) {
    entity <- do.call(entityType, list(name=name, parentId=parentId))
    storeEntity(entity)
  } else {
    getEntity(entityId$entity.id)
  }
}

# Like getOrCreateEntity, but only for projects and does not require a parentId
getOrCreateParentlessContainerEntity <- function(name, entityType="Project") {
  tryCatch({
      # try to create the entity
      entity <- do.call(entityType, list(name=name))
      storeEntity(entity)
    },error=function(e){
      # if creation fails, then retrieve.  this assumes that creation failed because entity exists
      queryResult <- synapseQuery(sprintf("select id from entity where entity.name=='%s'", name))
      # if creation failed for some other reason, then we would get zero results back
      if (nrow(queryResult)!=1) stop(sprintf("Expected one result but found %d", nrow(queryResult)))
      getEntity(queryResult$entity.id)
    })
}

# Entity names may only contain: letters, numbers, spaces, underscores, hypens, periods, plus signs, and parentheses
# any other characters are replaced by '+' signs
scrubEntityName<-function(s, replaceChar=".") {
  # first check that 'replaceChar' is legal
  illegalChars <- "[^a-zA-Z0-9_.+() -]"
  if (regexpr(illegalChars, replaceChar)>0) stop(sprintf("%s is illegal in an entity name", replaceChar))
  # ok now replace illegal characters
  gsub(illegalChars, replaceChar, s)
}

isGithubClientInstalled<-function() {
  "rGithubClient" %in% rownames(installed.packages())
}

# split text into lines regardless or whether the line separator is "\r", "\n", or "\r\n"
splitByLines<-function(text) {
  splitByRN <-strsplit(text, "\r\n", fixed=TRUE)[[1]]
  unlist(lapply(splitByRN, function(x) {if (x=="") {""} else {strsplit(x, "[\r\n]")[[1]]}}))
}

# add the given prefix to each line in the given text, separating lines by the given linesep
indent<-function(text, prefix="\t", linesep="\n") {
  splitByLines<-splitByLines(text)
  sprintf("%s%s", prefix, paste(splitByLines, collapse = sprintf("%s%s", linesep, prefix)))
}

#calculate the MD-5 checksum for a string
stringMd5<-function(s) {
  digest(s, algo="md5", serialize=FALSE)
}

rGithubClientPackageIsAvailable<-function() {
  any(.packages(all.available=T)=="rGithubClient")
}

hasRSuffix<-function(fileName) {
  ".R"==toupper(substr(fileName, nchar(fileName)-1, nchar(fileName)))
}

# Create a Code entity for a file on the local file system
#
# uses this synapse organization:
#     RootFolder > SourceCode
# creates any entities that don't exist, adds the code file
# then returns the created or updated Code entity
#
createFileCodeEntity <- function(sourceFile, codeFolderId, replChar=".") {
  synapseSourceFile <- scrubEntityName(sourceFile, replChar)
  
  if (missing(codeFolderId)) {
    codeFolder <-getOrCreateParentlessContainerEntity(name="Code", entityType="Folder")
    codeFolderId <- propertyValue(codeFolder, "id")
  }
  sourceFileEntity <- getOrCreateEntity(name=synapseSourceFile, parentId=codeFolderId, entityType="Code")
  
  sourceFileEntity<-addFile(sourceFileEntity, sourceFile)
  
  # delineate code with Synapse 'markdown' code tags. This is done by prefixing each line with a tab
  fileContent<-readChar(sourceFile, file.info(sourceFile)$size)
  propertyValue(sourceFileEntity, "description") <- indent(fileContent)
  
  # store the entity and return it
  storeEntity(sourceFileEntity)
}

# Create a Code entity for a file in Github
#
# uses this synapse organization:
#     RootFolder > GitRepoName > CurrentGitCommit > SourceCode
# creates any entities that don't exist, constructs the URL, 
# then returns the created or updated Code entity
#
createGithubCodeEntity <- function(repoName, sourceFile, githubCodeFolderId, replChar=".") {
  ## check that rGithubClient package is installed
  if (!rGithubClientPackageIsAvailable()) stop("Github repo specified but rGithubClient pacakge not installed.  Please install and try again.")

  githubRepo <- getRepo(repository=repoName)
  
  synapseRepoName <- scrubEntityName(repoName, replChar)
  synapseSourceFile <- scrubEntityName(sourceFile, replChar)
  
  if (missing(githubCodeFolderId)) {
    repoEntity <-getOrCreateParentlessContainerEntity(name=synapseRepoName, entityType="Folder")
  } else {
    repoEntity <- getOrCreateEntity(name=synapseRepoName, parentId=githubCodeFolderId, entityType="Folder")
  }
  commitEntity <- getOrCreateEntity(name=as.character(githubRepo@commit), parentId=repoEntity$properties$id, entityType="Folder")
  sourceFileEntity <- getOrCreateEntity(name=synapseSourceFile, parentId=commitEntity$properties$id, entityType="Code")
  
  githubURL <- paste("https://raw.github.com", githubRepo@user, githubRepo@repo, githubRepo@commit, sourceFile, sep="/")
  
  # TODO this will change with the new file service
  propertyValue(sourceFileEntity, "locations")<-list(list(type="external", path=githubURL))
  urlContent<-getURLContent(githubURL)
  propertyValue(sourceFileEntity, "md5")<-stringMd5(urlContent)
  
  # delineate code with Synapse 'markdown' code tags. This is done by prefixing each line with a tab
  propertyValue(sourceFileEntity, "description") <- indent(urlContent)
  
  # store the entity and return it
  storeEntity(sourceFileEntity)
}

# executable - what to execute.  choices are:
#			- Code in github: list(repoName="...", sourceFile="...")  (rGithubClient package required)
#	    - a file path whose file contains a matching function (exeutable = <folder path>/<function name>.R)
#			- a function (no Code object created)
#	args - arguments for the specified function
# resultParentId - ID of the Synapse container (e.g. Project or Folder) where the entity containing the result shall go
# resultEntityProperties - annotations to be added to the resulting entity
# resultEntityName - name of the resulting entity
# replChar - the replacement character to use when illegal characters are encountered while creating entity names (default is ".")
#
# Execute the given code with the given arguments. 
# Store the resulting R object as a new Data object (or a new revision of an existing Data object)
# having the given name and in the given parent container
# Set the given properties on the entity.
# Returns the result entity
#
# Since a Code file can contain multiple functions, synapseExecute disambiguates the function to call as follows:
# If there's just one function, that is the one called.  If there are multiple functions and one is named
# according to the containing file, that is the one called.  Otherwise, synapseExecute stops.
#
# Also, store the given function as Code in Synapse and
# create a provenance record connecting the result to 
# the executed code and the input arguments.
#
synapseExecute <- function(executable, args, resultParentId, resultEntityProperties = NULL,  resultEntityName=NULL, replChar=".") {
  
  if (!is.list(args)) stop("args must be a list.")
  if (!is.null(resultEntityProperties) && !is.list(resultEntityProperties)) stop("resultEntityProperties must be a list.")
  if (missing(resultParentId)) stop("In synapseExecute, resultParentId is required.")
    
  usedEntitiesList <- createUsedEntitiesList(args)
  
  executableIsGitHubRepoFile <- is.list(executable)
  executableIsLocalFile <- is.character(executable)
  executableIsFunction <- is.function(executable)
  
  if (executableIsGitHubRepoFile || executableIsLocalFile) {
    filePath<-NULL
    executionCodeEntity<-NULL
    if (executableIsGitHubRepoFile) {
      ## 'executable' is a list() containing repoName and sourceFile
      if (is.null(executable$repoName)) stop("Missing repoName in githubRepo code descriptor.")
      if (is.null(executable$sourceFile)) stop("Missing sourceFile in githubRepo code descriptor.")
      filePath<-executable$sourceFile
      executionCodeEntity <- createGithubCodeEntity(repoName = executable$repoName, sourceFile = executable$sourceFile, replChar)
    } else {# it's a local file
      ## 'executable' is a the full file path to a ".R" file, containing a function whose name matches the file name
      if (!hasRSuffix(executable)) stop(sprintf("Executable file %s does have '.R' ending.", executable))
      filePath <- executable
      executionCodeEntity <- createFileCodeEntity(sourceFile=executable, replChar)
    }
    
    # now we need to find the function added to the code entity
    # TODO test file containing data item rather than function
    if (is.null(executionCode$objects)) stop("executionCode$objects is null.")
    if (length(executionCode$objects)==0) stop(sprintf("File %s contains no function to execute.", filePath))
    executableFunction<-NULL
    if (length(executionCode$objects)==1) {
      executableFunction <- executionCode$objects[[1]]
    } else { # more than one function
      fileName <- basename(filePath)
      functionName <- substr(fileName, 1, nchar(fileName)-2)
      executableFunction <- executionCode$objects[[functionName]]
      if (is.null(executableFunction)) stop(sprintf("File %s has multiple functions but none named %s", filePath, functionName))
    }
    functionResult <- do.call(executableFunction, args)
    
    usedEntitiesList[[length(usedEntitiesList)+1]] <- list(entity=executionCodeEntity, wasExecuted=TRUE)
    
    activity <- Activity(list(name = filePath, used = usedEntitiesList))
    activity <- createEntity(activity)
  } else if (is.function(executable)) {
    ## 'executable' is a function in the workspace, but does not come from a file
    functionResult <- do.call(executable, args)
    activity <- Activity(list(used = usedEntitiesList))
    activity <- createEntity(activity)
  } else {
    stop("Executable is neither a github repo descriptor, a local file, nor a function.")
  }
  
  # Finally, we create the result entity

  resultEntity <- Data(name=resultEntityName, parentId = resultParentId)
  generatedBy(resultEntity) <- activity
  
  resultEntity <- addObject(resultEntity, functionResult, name="functionResult")
  # TODO test with an argument which is a list or vector (not a primitive)
  for (name in args) {
    annotValue(args, name)<-args[[name]]
  }
  if(!is.null(resultEntityProperties)){
    for (name in names(resultEntityProperties)) {
      annotValue(resultEntity, name)<-resultEntityProperties[[name]]
    }
  }
  
  # store result entity and return it
  storeEntity(resultEntity)
}
