\name{copyEntity}
\alias{copyEntity}
\title{
	Make a Copy of an Entity
}
\description{
	Copies environment Objects.
}
\usage{
	copyEntity(entity)
}
\arguments{
  \item{entity}{
		The entity to copy
	}
}
\details{
	Entities that hold objects do so in an R environment. In R, environments are passed by reference,
	so modifying the environment of an entity will also modify the contents of all copies made using
	the assignment operator. This function returns a copy of the passed entity that holds copies of all
	the objects held by that entity in a new environment, separate from the original.
}
\value{
	A copy of the entity detached from the original
}
\author{
	Matt Furia
}
