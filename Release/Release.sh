#! /bin/sh

cd $APSIM
. $APSIM/Model/Build/VersionInfo.sh

dest=Apsim$MAJOR_VERSION$MINOR_VERSION-r$BUILD_NUMBER

tar cfz $dest.tar.gz --transform=s,^,$dest/,  Model/*.xml Model/*.so Model/*.x Model/*.exe Model/TclLink/CIDataTypes.tcl Model/TclLink/lib UserInterface/*.xml UserInterface/ToolBoxes/*.xml 


