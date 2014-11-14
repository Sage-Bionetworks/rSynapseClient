#!/bin/sh
# Everything that needs to be done before building the package goes here:

# download jar file from artifactory
# note, this gets version 67.0.  To change versions requires changing the following line in two places
export srcjar=http://sagebionetworks.artifactoryonline.com/sagebionetworks/libs-releases-local/org/sagebionetworks/lib-auto-generated/67.0/lib-auto-generated-67.0.jar
wget -O temp.jar $srcjar
if [ "$?" -ne "0" ]; then
	curl -o temp.jar $srcjar
fi
if [ "$?" -ne "0" ]; then
  echo "Failed to download $srcjar"
  exit 1
fi

cd inst/resources
# extract schema and Register.json into inst/resources
jar -xf ../../temp.jar Register.json
jar -xf ../../temp.jar schema
# return to original directory and clean up downloaded jar file
cd ../..
rm temp.jar

# build the .Rd files
# Note:  We want to just call 'tools/docGen/createRdFiles.R .' but it doesn't work on cygwin (Windows)
Rscript tools/docGen/createRdFiles.R .


