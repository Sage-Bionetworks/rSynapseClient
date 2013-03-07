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
getOrCreateEntity <- function(name, parentId, entityType, load=F) {
  queryResult <- synapseQuery(sprintf("select id from entity where entity.parentId=='%s' AND entity.name=='%s'", parentId, name))
  if (is.null(queryResult)) {
    entity <- do.call(entityType, list(name=name, parentId=parentId))
    storeEntity(entity)
  } else {
    if (load) {
      loadEntity(queryResult$entity.id)
    } else {
      getEntity(queryResult$entity.id)
    }
  }
}

# Entity names may only contain: letters, numbers, spaces, underscores, hypens, periods, plus signs, and parentheses
# any other characters are replaced by <replaceChar>
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

isMap<-function(json) {nchar(json)>0 & substr(json,1,1)=="{"}
isArray<-function(json) {nchar(json)>0 & substr(json,1,1)=="["}
isScalar<-function(x) {length(x)<2 & is.null(names(x)) & !is.list(x)}

# ensures that 'a' is a valid Synapse annotation value
# if a is a primitive (string or number) or an array of primitives, 
# then a is returned.  Otherwise a is serialized to make it allowable:
# if a is a map (a named list) then it is serialized
# if a is an array, then each element that is not a primitive is serialized
convertAnnotation<-function(a) {
  if (isScalar(a)) {
    a
  } else {
    jsonA<-toJSON(a)
    if (isMap(jsonA)) {
      jsonA
    } else {
      # must be an array
      if (!isArray(jsonA)) stop(sprintf("Expected %s to be a JSON array.", jsonA))
      convertedArray<-list()
      for (elem in a) {
        if (isScalar(elem)) {
          convertedArray[[length(convertedArray)+1]]<-elem
        } else {
          convertedArray[[length(convertedArray)+1]]<-toJSON(elem)
        }
      }
      convertedArray
    }
  }
}

# create a Code entity for a function in memory
# f: the function in memory
createMemoryCodeEntity <-function(f, codeFolderId, codeEntityName, replChar=".") {
  synapseEntityName <- scrubEntityName(codeEntityName, replChar)
  
  sourceFileEntity <- getOrCreateEntity(name=synapseEntityName, parentId=codeFolderId, entityType="Code")
  
  sourceFileEntity<-addObject(sourceFileEntity, f)
  
  # delineate code with Synapse 'markdown' code tags. This is done by prefixing each line with a tab
  functionContent<-paste(format(f), collapse="\n")
  propertyValue(sourceFileEntity, "description") <- indent(functionContent)
  
  # store the entity and return it
  storeEntity(sourceFileEntity)
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
  
  sourceFileEntity <- getOrCreateEntity(name=synapseSourceFile, parentId=codeFolderId, entityType="Code")
  
  sourceFileEntity<-addFile(sourceFileEntity, sourceFile)
  
  # delineate code with Synapse 'markdown' code tags. This is done by prefixing each line with a tab
  fileContent<-readChar(sourceFile, file.info(sourceFile)$size)
  propertyValue(sourceFileEntity, "description") <- indent(fileContent)
  
  # store the entity and return it
  storeEntity(sourceFileEntity)
}

# on some systems RCurl needs an SSL certificate bundle to be passed to make https requests work
configureRGithubClientCertBundle<-function() {
  githubCurlOpts<-rGithubClient:::.getGithubCache("opts")
  if (is.null(githubCurlOpts$cainfo)) {
    synapseClientCurlOpts<-.getCache("curlOpts")
    if (!is.null(synapseClientCurlOpts)) {
      githubCurlOpts$cainfo<-synapseClientCurlOpts$cainfo
      rGithubClient:::.setGithubCache("opts", githubCurlOpts)
    }
  }  
}

# Create a Code entity for a file in Github
#
# uses this synapse organization:
#     RootFolder > GitRepoName > CurrentGitCommit > SourceCode
# creates any entities that don't exist, constructs the URL, 
# then returns the created or updated Code entity
#
createGithubCodeEntity <- function(repoName, sourceFile, codeFolderId, replChar=".") {
  ## check that rGithubClient package is installed
  if (!rGithubClientPackageIsAvailable()) stop("Github repo specified but rGithubClient pacakge not installed.  Please install and try again.")
  configureRGithubClientCertBundle()
  
  githubRepo <- getRepo(repository=repoName)
  
  synapseRepoName <- scrubEntityName(repoName, replChar)
  synapseCommitName <- scrubEntityName(githubRepo@commit, replChar)
  synapseSourceFile <- scrubEntityName(sourceFile, replChar)
  
  repoEntity <- getOrCreateEntity(name=synapseRepoName, parentId=codeFolderId, entityType="Folder")

  commitEntity <- getOrCreateEntity(name=synapseCommitName, parentId=repoEntity$properties$id, entityType="Folder")
  sourceFileEntity <- getOrCreateEntity(name=synapseSourceFile, parentId=commitEntity$properties$id, entityType="Code")
  
  githubURL <- getPermlink(repository=githubRepo, repositoryPath=sourceFile, type="raw")
  
  # TODO this will change with the new file service
  propertyValue(sourceFileEntity, "locations")<-list(list(type="external", path=githubURL))
  urlContent<-getURLContent(githubURL)
  propertyValue(sourceFileEntity, "md5")<-stringMd5(urlContent)
  
  # delineate code with Synapse 'markdown' code tags. This is done by prefixing each line with a tab
  # TODO this will change when we introduce the wiki object
  propertyValue(sourceFileEntity, "description") <- indent(urlContent)
  
  # store the entity and return it
  # NOTE, after setting an external URL you must call 'updateEntity', not 'storeEntity' lest the locations are deleted!!
  updateEntity(sourceFileEntity)
}

# executable - what to execute.  choices are:
#			- Code in github: list(repoName="...", sourceFile="...")  (rGithubClient package required)
#	    - a file path whose file contains a matching function (exeutable = <folder path>/<function name>.R)
#			- a function (no Code object created)
#	args - arguments for the specified function
# resultParentId - ID of the Synapse container (e.g. Project or Folder) where the entity containing the result shall go
# codeParentId - ID of the root container (e.g. Project or Folder) for code
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
#
synapseExecute <- function(executable, args, resultParentId, codeParentId, resultEntityProperties = NULL,  resultEntityName=NULL, replChar=".") {
  
  if (!is.list(args)) stop("args must be a list.")
  if (!is.null(resultEntityProperties) && !is.list(resultEntityProperties)) stop("resultEntityProperties must be a list.")
  if (missing(resultParentId)) stop("In synapseExecute, resultParentId is required.")
    
  usedEntitiesList <- createUsedEntitiesList(args)
  
  executableIsGitHubRepoFile <- is.list(executable)
  executableIsLocalFile <- is.character(executable)
  executableIsFunction <- is.function(executable)
  
  executionCodeEntity <- NULL
  
  if (executableIsGitHubRepoFile || executableIsLocalFile) {
    filePath<-NULL
    if (executableIsGitHubRepoFile) {
      ## 'executable' is a list() containing repoName and sourceFile
      if (is.null(executable$repoName)) stop("Missing repoName in githubRepo code descriptor.")
      if (is.null(executable$sourceFile)) stop("Missing sourceFile in githubRepo code descriptor.")
      filePath<-executable$sourceFile
      executionCodeEntity <- createGithubCodeEntity(repoName = executable$repoName, sourceFile = executable$sourceFile, codeParentId, replChar)
    } else {# it's a local file
      ## 'executable' is a the full file path to a ".R" file, containing a function whose name matches the file name
      if (!hasRSuffix(executable)) stop(sprintf("Executable file %s does have '.R' ending.", executable))
      filePath <- executable
      executionCodeEntity <- createFileCodeEntity(sourceFile=executable, codeParentId, replChar=replChar)
    }
    
    # now we need to find the function added to the code entity
    executionCodeEntity<-loadEntity(executionCodeEntity)
    if (is.null(executionCodeEntity$objects)) stop("executionCode$objects is null.")
    if (length(executionCodeEntity$objects)==0) stop(sprintf("File %s contains no function to execute.", filePath))
    executableFunction<-NULL
    if (length(executionCodeEntity$objects)==1) {
      executableFunction <- executionCodeEntity$objects[[1]]
    } else { # more than one function
      fileName <- basename(filePath)
      functionName <- substr(fileName, 1, nchar(fileName)-2)
      executableFunction <- executionCodeEntity$objects[[functionName]]
      if (is.null(executableFunction)) stop(sprintf("File %s has multiple functions but none named %s", filePath, functionName))
    }
    activityName<-filePath
    
  } else if (is.function(executable)) {
    ## 'executable' is a function in the workspace, but does not come from a file
    codeEntityName <- "InMemoryFunction"
    executionCodeEntity <- createMemoryCodeEntity(executable, codeParentId, codeEntityName, replChar=replChar)
    executableFunction <- executable
    activityName <- NULL
  } else {
    stop("Executable is neither a github repo descriptor, a local file, nor a function.")
  }
  
  functionResult <- do.call(executableFunction, args)
  
  usedEntitiesList[[length(usedEntitiesList)+1]] <- 
    list(reference=list(targetId=propertyValue(executionCodeEntity, "id"), 
        targetVersionNumber=propertyValue(executionCodeEntity, "versionNumber")), 
      wasExecuted=TRUE)
  
  activity <- Activity(list(name = activityName, used = usedEntitiesList))
  activity <- createEntity(activity)
  # Finally, we create the result entity
  # If the entity already exists we create a new version
  resultEntity <- getOrCreateEntity(name=resultEntityName, parentId = resultParentId, "Data", load=T) 
  
  # if this is an existing version, then 'rev' it
  version <- propertyValue(resultEntity, "versionNumber")
  if (!is.null(version)) propertyValue(resultEntity, "versionNumber")<-(version+1)
  
  generatedBy(resultEntity) <- activity
  
  # Adding a new object causes the version to increment upon storage
  resultEntity <- addObject(resultEntity, functionResult, name="functionResult")

  for (name in names(args)) {
    annotValue(resultEntity, name)<-convertAnnotation(args[[name]])
  }
  if(!is.null(resultEntityProperties)){
    for (name in names(resultEntityProperties)) {
      annotValue(resultEntity, name)<-convertAnnotation(resultEntityProperties[[name]])
    }
  }
  
  # store result entity and return it
  storeEntity(resultEntity)
}
