\name{synGetWiki}
\alias{synGetWiki}
\title{
synGetWiki
}
\description{
Function to retrieve a Wiki Page.
}
\usage{
synGetWiki(parent)
synGetWiki(parent, id)
}
\details{
Retrieves a WikiPage given its parent (Entity or Evaluation) and, optionally, its ID.  If no ID is specified then 
the root ID is returned.   To get the IDs of all the WikiPages for a parent, see getWikiHeaders.
}
\examples{
entity<-synGet("syn123456")
wikiPage<-synGetWiki(entity)
}
\seealso{
\code{\link{getWikiHeaders}}
\code{\link{synStore}}
}
