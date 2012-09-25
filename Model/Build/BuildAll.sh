#! /bin/sh

if [ -n "${APSIM:-x}" ] ; then
   APSIM=`readlink -f $0`  ;# runmake
   APSIM=`dirname $APSIM`  ;# build
   APSIM=`dirname $APSIM`  ;# model
   export APSIM=`dirname $APSIM`
fi

if [ ! -f "$MONO_PREFIX/include/mono-2.0/mono/jit/jit.h" ] ; then
   export MONO_PREFIX=/usr
fi
if [ ! -f "$MONO_PREFIX/include/mono-2.0/mono/jit/jit.h" ] ; then
   echo Cant find mono/jit.h - expect trouble
fi

export MONO_XMLSERIALIZER_THS=no

$APSIM/Model/cscs.exe $APSIM/Model/Build/VersionStamper.cs Directory=$APSIM 

$APSIM/Model/Build/RunMake.sh $APSIM/Model/JobScheduler clean
$APSIM/Model/Build/RunMake.sh $APSIM/Model/JobScheduler

$APSIM/Model/JobScheduler.exe $APSIM/Model/Build/BuildAll.xml  Target=BuildAll
