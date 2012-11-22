\name{used<--methods}
\docType{methods}
\alias{used<--methods}
\alias{used<-,SynapseEntity,list}
\title{ ~~ Methods for Function \code{used<-}  ~~}
\description{
 ~~ Methods for function \code{used<-}  ~~
}
\usage{
used(entity)<-list(entity1, entity2, entity3)
used(entity)<-list(list(entity=codeEntity, wasExecuted=T), list(entity=dataEntity, wasExecuted=F))
}
\section{Methods}{
\describe{

\item{\code{signature(object = "SynapseEntity", list = "list")}}{
Sets the list of entities involved in generating the given entity, optionally specifying 
whether each entity was executed to generate the given entity. (Default is False.)

}
}}
