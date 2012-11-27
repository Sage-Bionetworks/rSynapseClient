\name{used<--methods}
\alias{used<-}
\title{
	\code{used<-}
}
\description{
	Function for setting the entities used to generate an entity.
}
\usage{
used(entity)<-list(entity1, entity2, entity3)
used(entity)<-list(list(entity=codeEntity, wasExecuted=T), list(entity=dataEntity, wasExecuted=F))
}
\details{
Sets the list of entities involved in generating the given entity, optionally specifying 
whether each entity was executed to generate the given entity. (Default is False.)    No server interaction takes place. (That is done when \code{storeEntity} is called.)
}


\author{
Bruce Hoff
}


