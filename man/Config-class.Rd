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
    \item{\code{ConfigParser(path)}:}{
      Returns a Config object with parsed INI data.  
    }
    \item{\code{Config.hasOption(config, section, option)}:}{
      Returns TRUE if the given option exists within the given section.
    }
    \item{\code{Config.getOption(config, section, option)}:}{
      Returns the value of the given section and option.  
    }
  }
}

\seealso{
  \code{\link{synapseLogin}}
}

\examples{
\dontrun{
  # Logging into Synapse with loaded credentials
  config <- ConfigParser("~/.synapseConfig")
  
  if (Config.hasOption(config, "authentication", "username") 
      && Config.hasOption(config, "authentication", "password")) {
    username <- Config.getOption(config, "authentication", "username") 
    password <- Config.getOption(config, "authentication", "password")
    synapseLogin(username, password)
  }
 }
}