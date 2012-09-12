#! /bin/bash

export MONO_PREFIX=/usr
export APSIM=/home/bob/apsim
export LD_LIBRARY_PATH=$APSIM/Model
export LANG=en_AU.UTF-8

export MONO_XMLSERIALIZER_THS=no

cd $APSIM

$APSIM/Model/cscs.exe $APSIM/Model/Build/Bob.cs   $APSIM/Model/Build/BobMain.cs

