\name{WikiPage}
\alias{WikiPage}
\docType{methods}
\title{
Constructor for WikiPage
}
\description{
Constructor for WikiPage
}
\usage{
WikiPage(owner, title="Wiki Page for Entity", markdown="markdown goes here", attachments=list(file1, file2, ...) )
WikiPage(owner, title="Sub-Wiki Page for Entity", markdown="sub-page markdown goes here", parentWikiId="123456")
}
\details{
This creates a wiki page (in memory), or sub-page beneath an existing page for the given entity.
}
\examples{
entity<-synGet("syn123456")
page<-WikiPage(owner=entity, title="Wiki Page for Entity", markdown="markdown goes here")
page<-synStore(page)
}
\seealso{
\code{\link{synGetWiki}}
\code{\link{synStore}}
}
