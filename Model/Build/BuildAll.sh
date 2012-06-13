#! /bin/sh

if [ -n "${APSIM:-x}" ] ; then
   APSIM=`readlink -f $0`  ;# runmake
   APSIM=`dirname $APSIM`  ;# build
   APSIM=`dirname $APSIM`  ;# model
   export APSIM=`dirname $APSIM`
fi


$APSIM/Model/Build/CleanAll.sh
$APSIM/Model/Build/RunMake.sh $APSIM/Model/RunTime
$APSIM/Model/Build/RunMake.sh $APSIM/Model/JobScheduler

tclsh $APSIM/Model/Build/VersionStamper.tcl
if [ -f "$APSIM/Model/Build/VersionInfo.sh" ] ; then 
  . "$APSIM/Model/Build/VersionInfo.sh"
fi

$APSIM/Model/JobScheduler.exe $APSIM/Model/Build/BuildAll.xml
