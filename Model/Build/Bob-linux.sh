#! /bin/bash

export MONO_PREFIX=/usr

# Work out install dir
me="`readlink -f $0`"
APSIMBUILD=`dirname "$me"`
APSIMMODEL=`dirname "$APSIMBUILD"`
export APSIM=`dirname "$APSIMMODEL"`

export LD_LIBRARY_PATH=$APSIM/Model
export LANG=en_AU.UTF-8
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export SHELL=/bin/bash

export MONO_XMLSERIALIZER_THS=no

cd $APSIM

$APSIM/Model/cscs.exe $APSIM/Model/Build/Bob.cs
