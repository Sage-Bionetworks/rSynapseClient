\name{WikiPage}
\alias{WikiPage}
\docType{data}
\title{
Constructor for WikiPage
}
\description{
Constructor for WikiPage
}
\usage{
WikiPage(entity, title="Wiki Page for Entity", markdown="markdown goes here")
WikiPage(entity, title="Sub-Wiki Page for Entity", markdown="sub-page markdown goes here", parentWikiId="123456")
}
\details{
This creates a wiki page (in memory), or sub-page beneath an existing page for the given entity.
}
\examples{
entity<-synGet("syn123456")
page<-WikiPage(entity, title="Wiki Page for Entity", markdown="markdown goes here")
page<-synStore(page)
}
\seealso{
\code{\link{synGetWiki}}
\code{\link{synStore}}
}
