#! /bin/sh

cd ../..
export APSIM=`pwd`
cd $APSIM/Model/Build

ulimit -s unlimited
rm $APSIM/Model/Build/Build.out

./MakeProject.sh General
./MakeProject.sh ApsimShared
./MakeProject.sh Protocol
./MakeProject.sh ComponentInterface
./MakeProject.sh ComponentInterface2
./MakeProject.sh FortranComponentInterface
./MakeProject.sh FortranInfrastructure
./MakeProject.sh Apsim
./MakeProject.sh ProtocolManager

./MakeProject.sh Clock
./MakeProject.sh Report

