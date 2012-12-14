\name{Activity}
\alias{Activity}
\title{
Activity Constructor
}
\description{
Constructor for Activity
}
\usage{
Activity()
}
\arguments{
  \item{properties}{
list(name="an activity", description="my description", used=list(e1, e2, ...))
where each of e1, e2, ... can be either an <entity-elem> or list(entity=<entity-elem>, wasExecuted=<boolean>),  
and <entity-elem> can be either a SynapseEntity or an entity ID ("syn1234").
}
}

\value{
The created Activity is returned.
}
\author{
Bruce Hoff
}
