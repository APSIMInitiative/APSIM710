#! /bin/sh

if [ -n "${APSIM:-x}" ] ; then
   APSIM=`readlink -f $0`  ;# runmake
   APSIM=`dirname $APSIM`  ;# build
   APSIM=`dirname $APSIM`  ;# model
   export APSIM=`dirname $APSIM`
fi

if [ -f "$MONO_PREFIX/mono/jit.h" ] ; then
   export MONO_PREFIX=/usr
fi
if [ -f "$MONO_PREFIX/mono/jit.h" ] ; then
   echo Cant find mono/jit.h - expect trouble
fi

$APSIM/Model/Build/RunMake.sh $APSIM/Model/JobScheduler clean
$APSIM/Model/Build/RunMake.sh $APSIM/Model/JobScheduler

$APSIM/Model/JobScheduler.exe $APSIM/Model/Build/BuildAll.xml  Target=BuildAll
