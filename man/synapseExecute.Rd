\name{synapseExecute}
\title{
  Execute Code and Upload Results to Synapse
}
\description{
  Function for executing code and uploading results, and creating a provenance record in Synapse
}
\usage{
  synapseExecute(executable, args, resultParentId, resultEntityProperties,  resultEntityName)
}
\arguments{
  \item{executable}{
    
  }
  \item{args}{
    
  }
  \item{resultParentId}{
    
  }
  \item{resultEntityProperties}{
    
  }
  \item{resultEntityName}{
    
  }
}
\details{
}
\value{
  The Synapse Entity containing the result.
}
\author{
  Bruce Hoff <bruce.hoff@sagebase.org>
}
\examples{
  ## execute code in a local file
  entity <- synapseExecute(executable, args, resultParentId, resultEntityProperties,  resultEntityName)
  
  ## execute code in Github
  entity <- synapseExecute(executable, args, resultParentId, resultEntityProperties,  resultEntityName)
}

