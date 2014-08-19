# Everything that needs to be done before building the package goes here:
#
# download schemas from artifactory
tools/retrieveDependencies.R .

# build the .Rd files
tools/docGen/createRdFiles.R .

