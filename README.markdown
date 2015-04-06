This is an R client for the open-biology platform "Synapse". visit https://synapse.sagebase.org or http://www.sagebase.org for more info. To view documentation or to file bug reports and feature requests visit http://sagebionetworks.jira.com/browse/SYNR.

## Installation

The R Synase Client can be installed by typing these commands into the R prompt:

```R
source('http://depot.sagebase.org/CRAN.R')
pkgInstall(c("synapseClient"))
```

For early access to new and possibly not-ready-for-production features, you can install directly from GitHub using the `devtools` package. The _master_ branch will usually mirror the released version of the package. If you'd like to install the _develop_ branch, you can do so like this:

```R
library(devtools)
install_github('Sage-Bionetworks/rSynapseClient', ref='develop')
```

## Getting Started

The Getting started with the R synapseClient guide is hosted in Synapse itself. See [syn1834618](https://www.synapse.org/#!Synapse:syn1834618).


