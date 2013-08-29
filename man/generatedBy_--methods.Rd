\name{generatedBy<--methods}
\alias{generatedBy<-}
\title{
\code{generatedBy<-}
}
\description{
 	Method for setting the Activity which generated an Entity
}
\usage{
	generatedBy(locationable)<-activity
}
\arguments{
  \item{locationable}{
the Locationable entity of interest
}
 \item{activity}{
the activity which generated the Locationable entity
}
}
\details{
Sets the Activity which generated a Locationable entity.  NULL is allowed.  No server interaction takes place. (That is done when \code{storeEntity} is called.)
}

\author{
Bruce Hoff
}