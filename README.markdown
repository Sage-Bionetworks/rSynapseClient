This is an R client for the open-biology platform "Synapse". Visit https://www.synapse.org or http://www.sagebase.org for more info. To view documentation visit http://r-docs.synapse.org/. To file bug reports and feature requests visit http://sagebionetworks.jira.com/browse/SYNR.

## Installation

The R Synapse Client can be installed by typing these commands into the R prompt:

```R
source("http://depot.sagebase.org/CRAN.R")
pkgInstall(c("synapseClient"))
```

For early access to new and possibly not-ready-for-production features, you can install directly from GitHub using the `devtools` package. The _master_ branch will usually mirror the released version of the package. If you'd like to install the _develop_ branch, you can do so like this:

```R
library(devtools)
install_github('Sage-Bionetworks/rSynapseClient', ref='develop')
```

## Getting Started

The [Getting Started](docs.synapse.org/articles/getting_started.html) guide for Synapse includes examples for the R client.

Full documentation for the R client is also available at https://docs.synapse.org/r/.


