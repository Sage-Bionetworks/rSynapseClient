# get the schemas
# TODO download schemas from artifactory
# build the .Rd files
tools/docGen/createRdFiles.R .
# Now build the package
R CMD INSTALL .
