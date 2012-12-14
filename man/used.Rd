\name{used}
\alias{used}
\title{
	\code{used}
}
\description{
Function for getting the entities used to generate an entity
}
\usage{
used(entity)
}
\arguments{
  \item{entity}{
  The entity of interest.
}
}
\details{
This method returns information already in the session.  There is no server interaction.
}
\value{
A list of entities involved in generating the given entity, and whether each entity was executed to generate the given entity.
}

\author{
Bruce Hoff
}


