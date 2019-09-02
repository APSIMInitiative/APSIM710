#!/bin/bash

dest=Temp
/bin/mkdir $dest
/bin/cp    $APSIM/Apsim.xml   $dest/Apsim.xml
/bin/mkdir $dest/Model
/bin/cp    $APSIM/Model/ApsimRun.sh $dest/Model
/bin/cp    $APSIM/Model/*.xml $dest/Model
/bin/cp    $APSIM/Model/*.so  $dest/Model
/bin/cp    $APSIM/Model/*.dll $dest/Model
if [ -f $dest/Model/APSIM.Tests.dll ]; then /bin/rm -f $dest/Model/APSIM.Tests.dll; fi
/bin/cp    $APSIM/Model/*.exe $dest/Model
/bin/cp    $APSIM/Model/*.x   $dest/Model
/bin/mkdir $dest/Model/TclLink
/bin/cp    $APSIM/Model/TclLink/CIDataTypes.tcl $dest/Model/TclLink
/bin/mkdir $dest/UserInterface
/bin/cp    $APSIM/UserInterface/*.xml $dest/UserInterface
/bin/mkdir $dest/UserInterface/ToolBoxes
/bin/cp    $APSIM/UserInterface/ToolBoxes/*.xml $dest/UserInterface/ToolBoxes

if [ "$1" = "" ] ; then
  . $APSIM/Model/Build/VersionInfo.sh
  if [ $(/bin/uname -m) == 'x86_64' ]; then
     7z a Apsim${MAJOR_VERSION}${MINOR_VERSION}-r${BUILD_NUMBER}.binaries.LINUX.X86_64.zip $dest
  else
     7z a Apsim${MAJOR_VERSION}${MINOR_VERSION}-r${BUILD_NUMBER}.binaries.LINUX.INTEL.zip $dest
  fi
else  
  if [ $(/bin/uname -m) == 'x86_64' ]; then
     7z a $1.binaries.LINUX.X86_64.zip Temp
  else
     7z a -sfx $1.binaries.LINUX.INTEL.zip Temp
  fi
fi

/bin/rm -rf Temp
