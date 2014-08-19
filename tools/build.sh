# First, do everything that needs to be done before building the package.
prebuild.sh
# Now build the package
R CMD INSTALL .
