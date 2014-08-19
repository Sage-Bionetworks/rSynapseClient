# download schemas from artifactory
tools/retrieveDependencies.R .
# build the .Rd files
tools/docGen/createRdFiles.R .
# Now build the package
R CMD INSTALL .
