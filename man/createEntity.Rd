\name{createEntity}
\alias{createEntity}
\alias{getEntity}
\alias{deleteEntity}
\alias{updateEntity}
\alias{refreshEntity}
\alias{storeEntity}
\alias{storeEntityFiles}
\alias{storeEntityObjects}
\alias{downloadEntity}
\alias{loadEntity}
\alias{getParentEntity}
\title{
	Entity CRUD Operations
}
\description{
	Functions for creating, retrieving, updating and deleting entities in Synapse.
}
\usage{
	createEntity(entity, ...)
	getEntity(entity)
	deleteEntity(entity)
	updateEntity(entity)
	refreshEntity(entity)
	storeEntity(entity)
	downloadEntity(entity)
	loadEntity(entity)
	storeEntityFiles(entity)
	storeEntityObjects(entity)
	getParentEntity(entity)
}
\arguments{
  \item{entity}{
		The entity being operated on. Depending on the function can be either an id (represented as a character or number) or
		a object whose class inherits from SynapseEntity. (see Details)
	}
  \item{\dots}{
		additional args passed to S3 method
	}
}
\details{
	
}
\value{
	for getParentEntity returns the parent of the entity provided.
	
	for deleteEntity, NULL.
	
	All other methods return a copy of the entity after performing the desired action.
}
\author{
	Matt Furia
}
\note{
%%  ~~further notes~~
}

\seealso{
	\code{\link{SynapseEntity}}
}
