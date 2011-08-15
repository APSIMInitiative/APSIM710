#! /bin/sh

cd $APSIM
. $APSIM/Model/Build/VersionInfo.sh

dest=Apsim$MAJOR_VERSION$MINOR_VERSION-r$BUILD_NUMBER

mkdir $dest
cp    ../Apsim.xml $dest/Apsim.xml
mkdir $dest/Model
cp    ../Model/*.xml $dest/Model
cp    ../Model/*.so $dest/Model
cp    ../Model/*.exe $dest/Model
cp    ../Model/*.x $dest/Model
mkdir $dest/Model/TclLink
cp    ../Model/TclLink/CIDataTypes.tcl $dest/Model/TclLink
cp -r ../Model/TclLink/lib $dest/Model/TclLink
mkdir $dest/UserInterface
cp    ../UserInterface/*.xml $dest/UserInterface
mkdir $dest/UserInterface/ToolBoxes
cp    ../UserInterface/ToolBoxes/*.xml $dest/UserInterface/ToolBoxes

7zr a -sfx $dest.binaries.Linux.x $dest

rm -rf $dest
