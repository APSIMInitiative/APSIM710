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

# ----- The working directory may be specified by $1 (or blank)
if [ $# -eq 0 ] ; then
  make 
else
  make -C "$1" $2 $3 
fi

