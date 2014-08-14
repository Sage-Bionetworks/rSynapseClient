# get the schemas
# TODO
# build the .Rd files
# TODO how do we specify the path?
inst/docGen/createRdFiles.R .
# Now build the package
#R CMD INSTALL . --no-test-load
R CMD INSTALL .
