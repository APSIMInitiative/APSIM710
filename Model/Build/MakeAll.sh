#! /bin/sh

cd ../..
export APSIM=`pwd`
cd $APSIM/Model/Build

ulimit -s unlimited
rm -f $APSIM/Model/Build/Build.out

./MakeProject.sh General
./MakeProject.sh ApsimShared
./MakeProject.sh Protocol
./MakeProject.sh ComponentInterface
./MakeProject.sh ComponentInterface2
./MakeProject.sh FortranComponentInterface
./MakeProject.sh FortranComponentInterface2
./MakeProject.sh FortranInfrastructure
./MakeProject.sh Apsim
./MakeProject.sh ProtocolManager

./MakeProject.sh Clock
./MakeProject.sh Report
./MakeProject.sh Input
./MakeProject.sh Manager
./MakeProject.sh SoilWat
./MakeProject.sh SoilN
./MakeProject.sh SurfaceOM
./MakeProject.sh Fertiliser
./MakeProject.sh Irrigation
./MakeProject.sh Plant
./MakeProject.sh Sorghum

