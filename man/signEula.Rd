\name{signEula}
\alias{signEula}
\title{
    Sign a EULA
}
\description{
    Signs the EULA for the given entity in Synapse
}
\usage{
signEula(entity)
}
\arguments{
  \item{entity}{
    The entity whose EULA should be signed. Can be passes as either
    an entity ID or an instance of a S4 SynapseEntity class.
}
}
\value{
    This method is called for it's side effect, which is a signed
    EULA in Synapse
}
\author{
Matthew D. Furia <matt.furia@sagebase.org
}
\examples{

## The function is currently defined as
function (entity) 
{
    .signEula(entity)
  }
}
