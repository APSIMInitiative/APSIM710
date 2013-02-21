#! /bin/sh

dest=Temp
mkdir $dest
cp    $APSIM/Apsim.xml   $dest/Apsim.xml
mkdir $dest/Model
cp    $APSIM/Model/ApsimRun.sh $dest/Model
cp    $APSIM/Model/*.xml $dest/Model
cp    $APSIM/Model/*.so  $dest/Model
cp    $APSIM/Model/*.dll $dest/Model
cp    $APSIM/Model/*.exe $dest/Model
cp    $APSIM/Model/*.x   $dest/Model
mkdir $dest/Model/TclLink
cp    $APSIM/Model/TclLink/CIDataTypes.tcl $dest/Model/TclLink
mkdir $dest/UserInterface
cp    $APSIM/UserInterface/*.xml $dest/UserInterface
mkdir $dest/UserInterface/ToolBoxes
cp    $APSIM/UserInterface/ToolBoxes/*.xml $dest/UserInterface/ToolBoxes

if [ "$1" = "" ] ; then
  . $APSIM/Model/Build/VersionInfo.sh
  7zr a -sfx Apsim${MAJOR_VERSION}${MINOR_VERSION}-r${BUILD_NUMBER}.binaries.Linux.exe $dest
else  
  7zr a -sfx `basename $1 .zip`.binaries.Linux.exe Temp
fi

rm -rf Temp
