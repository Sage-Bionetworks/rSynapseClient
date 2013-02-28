\name{synapseExecute}
\alias{synapseExecute}
\title{
  Execute Code and Upload Results to Synapse
}
\description{
  Function for executing code, uploading results, and creating a provenance record in Synapse
}
\usage{
  synapseExecute(executable, args, resultParentId, codeProjectId, resultEntityProperties,  resultEntityName, replChar=".")
}
\arguments{
	\item{executable}{
    a function, file, or github reference
	}
	\item{args}{
    a list of arguments passed to the function via 'do.call'
	}
	\item{resultParentId}{
    the Synapse ID of the project or folder in which the result entity is created
	}
	\item{codeProjectId}{
    the Synapse ID of the project or folder in which the executed code is placed
	}
	\item{resultEntityProperties}{
    an optional list of named values to be added to the result entity as annotations
	}
	\item{resultEntityName}{
    the name for the result entity
	}
	\item{replChar}{
    an optional replacement character used when 'scrubbing' illegal characters from entity names.  Default is "."
	}
}
\details{
    Executable may be one of the following:
    \itemize{
    \item{a function defined in the current session.}
    \item{a file on the local file system, including its full path and ending in ".R"}
    \item{a Github Repository reference in the form of \code{list(repoName=<repository name>, sourceFile=<source File>)}.  
   		The repo name is of the form \code{/owner/reponame}.  The source file is the full path to the R source file.}
    }
    
    A Code entity is created for the given Code.  If there is an existing Code entity in the target Synapse location (described below), 
    and if the code file differs from the existing, then a new revision of the existing Code entity is created.
    
    If the Code entity is a file or an in-memory function, then then the Code entity created is placed in the folder or project 
    given by \code{codeProjectId} parameter.  If the Code entity is a file, then then the Code entity created is named using 
    the file path of the code file.
    
    If the Code entity is a github reference then an entity hierarchy is created under the folder/project given by \code{codeProjectId} having
    the structure <codeProjectId>/<githubrepo>/<commit>/<filepath>.  
    
    The if the code is in a (local or github) file, the file is loaded.  Since a Code file can contain multiple functions, 
    synapseExecute disambiguates the function to call as follows: If there's just one function, that is the one called.  If 
    there are multiple functions and one is named according to the containing file, that is the one called.  Otherwise, 
    synapseExecute stops.
    
    An Activity (provenance record) is created which 'generates' the output entity and whose 'used' entities are (1) the aforementioned Code 
    object, (2) any Synapse entities included in \code{args}.
    
    The \code{args} and \code{resultEntityProperties} lists are added as annotations of the result entity.
    The result entity is created in the given parent project/folder and having the given name.  If such an
    entity already exists then a new revision of the entity is created.
}
\value{
  The Synapse Entity containing the result.
}
\author{
  Bruce Hoff <bruce.hoff@sagebase.org>
}
\examples{
  ## execute code in a local file
  resultEntity <- synapseExecute(
  		executable="/path/to/file.R", 
  		args=list(x=1, y="syn102987"), 
  		resultParentId="syn123456", 
  		codeProjectId="syn987654", 
  		resultEntityProperties=list(a="foo", b="bar),  
  		resultEntityName="exampleOutput")
  
  ## execute code in Github
  resultEntity <- synapseExecute(
  		executable=list(repoName="/githubUser/ProjectRepo", sourceFile="path/to/file.R"), 
  		args=list(x=1, y="syn102987"), 
  		resultParentId="syn123456", 
  		codeProjectId="syn987654", 
  		resultEntityProperties=list(a="foo", b="bar),  
  		resultEntityName="exampleOutput")
}

