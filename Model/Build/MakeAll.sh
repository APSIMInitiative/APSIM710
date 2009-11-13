#! /bin/sh

cd ../..
export APSIM=`pwd`
cd $APSIM/Model/Build

ulimit -s unlimited
rm -f $APSIM/Model/Build/Build.out

./MakeProject.sh CSGeneral $1
./MakeProject.sh ApsimFile $1
./MakeProject.sh DataTypes $1
./MakeProject.sh General $1
./MakeProject.sh ApsimShared $1
./MakeProject.sh Protocol $1
./MakeProject.sh ComponentInterface $1
./MakeProject.sh ComponentInterface2 $1
./MakeProject.sh FortranComponentInterface $1
./MakeProject.sh FortranComponentInterface2 $1
./MakeProject.sh FortranInfrastructure $1
./MakeProject.sh Apsim $1
./MakeProject.sh ProtocolManager $1

./MakeProject.sh Clock $1
./MakeProject.sh Report $1
./MakeProject.sh Input $1
./MakeProject.sh Manager $1
./MakeProject.sh SoilWat $1
./MakeProject.sh SoilN $1
./MakeProject.sh SurfaceOM $1
./MakeProject.sh Fertiliser $1
./MakeProject.sh Irrigation $1
./MakeProject.sh Plant $1
./MakeProject.sh Sorghum $1

./MakeProject.sh ConToSim $1
./MakeProject.sh ApsimToSim $1

