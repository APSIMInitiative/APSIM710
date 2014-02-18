#!/bin/bash

dest=Temp
mkdir $dest
cp    $APSIM/Apsim.xml   $dest/Apsim.xml
mkdir $dest/Model
cp    $APSIM/Model/ApsimRun.sh $dest/Model
cp    $APSIM/Model/*.xml $dest/Model
cp    $APSIM/Model/*.so  $dest/Model
cp    $APSIM/Model/*.dll $dest/Model
if [ -f $dest/Model/APSIM.Tests.dll ]; then rm -f $dest/Model/APSIM.Tests.dll; fi
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
  if [ $(uname -m) == 'x86_64' ]; then
     7zr a -sfx Apsim${MAJOR_VERSION}${MINOR_VERSION}-r${BUILD_NUMBER}.binaries.LINUX.X86_64.exe $dest
  else
     7zr a -sfx Apsim${MAJOR_VERSION}${MINOR_VERSION}-r${BUILD_NUMBER}.binaries.LINUX.INTEL.exe $dest
  fi
else  
  if [ $(uname -m) == 'x86_64' ]; then
     7zr a -sfx $1.binaries.LINUX.X86_64.exe Temp
  else
     7zr a -sfx $1.binaries.LINUX.INTEL.exe Temp
  fi
fi

rm -rf Temp
