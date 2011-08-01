#! /bin/sh

if [ -n "${APSIM:-x}" ] ; then
   APSIM=`readlink -f $0`  ;# runmake
   APSIM=`dirname $APSIM`  ;# build
   APSIM=`dirname $APSIM`  ;# model
   export APSIM=`dirname $APSIM`
fi

if [ -f "$APSIM/Model/Build/VersionInfo.sh" ] ; then 
  . "$APSIM/Model/Build/VersionInfo.sh"
fi

make $1 $2 $3 $4

