\name{synapseLogin}
\alias{synapseLogin}
\alias{synapseLogout}
\title{
Authentication
}
\description{
Functions for handling Synapse authentication.
}
\usage{
synapseLogin(username, password)
synapseLogin(username = ..., apiKey = ...)
synapseLogin(sessionToken = ...)
synapseLogin(username = ...)
synapseLogin()
synapseLogout(localOnly=FALSE)
}

\arguments{
  \item{username}{Synapse username, usually an email}
  \item{password}{Password associated with the username}
  \item{apiKey}{Can be found online by logging in and going to https://www.synapse.org/#!Settings:0}
  \item{sessionToken}{Usage not recommended}
  \item{localOnly}{Boolean controlling whether logout is done locally only, or propagated to Synapse}
}
\details{
If insufficient arguments are provided to login, either cached or configuration information may be used to supplement.
The configuration file is by default located in ``~/.synapseConfig''.  

If logout is propagated to Synapse (ie. localOnly = TRUE), 
it is likely that the user will be logged out for all clients currently in use.
}

\author{Matt Furia, Joseph Wu}