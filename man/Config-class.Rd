\name{Config-class}
\docType{class}
\alias{Config}
\alias{Config-class}

\title{Class \code{"Config"}}
\description{A parser for INI-format configuration files}
\author{Joseph Wu}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{get.option(section, option)}:}{
      Returns the value of the given section and option.  
    }
    \item{\code{has.option(section, option)}:}{
      Returns TRUE if the given option exists within the given section.
    }
    \item{\code{initialize(configFile)}:}{
      Reads and parses the given configuration file.
    }
  }
}

\seealso{
  \code{\link{synapseLogin}}
}

\examples{conf
  # Logging into Synapse with loaded credentials
  config <- new("Config", "~/.synapseConfig")
  
  if (config$has.option("authentication", "username") 
      && config$has.option("authentication", "password")) {
    username <- config$get.option("authentication", "username") 
    password <- config$get.option("authentication", "password")
    synapseLogin(username, password)
  }
}