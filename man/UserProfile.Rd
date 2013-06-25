\name{UserProfile}
\alias{UserProfile}
\docType{methods}
\title{
Constructor for UserProfile
}
\description{
Constructor for UserProfile
}
\usage{UserProfile()}
\details{
This constructor is not called directly, rather an instance is returned by synGetUserProfile(principalId).
}
\examples{
# get my own user profile
profile<-synGetUserProfile()
propertyValue(profile, "position")<-"Imperial Vice Duke of all Engineering"
profile<-synStore(profile)
# get someone else's profile
principalId<-"123456"
profile<-synGetUserProfile(principalId)
}
\seealso{
\code{\link{synGetUserProfile}}
\code{\link{synStore}}
}
