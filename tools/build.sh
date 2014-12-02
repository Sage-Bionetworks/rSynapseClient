# First, do everything that needs to be done before building the package.
tools/prebuild.sh
# Now build the package
R CMD build .
R CMD install synapseClient_1.5-0.tar.gz
