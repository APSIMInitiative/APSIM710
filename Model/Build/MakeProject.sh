#! /bin/sh

cd $APSIM/Model/$1

echo ----------------------------------------------------- >> $APSIM/Model/Build/Build.out
echo Compiling  $1 >> $APSIM/Model/Build/Build.out
echo ----------------------------------------------------- >> $APSIM/Model/Build/Build.out
make $2 1>> $APSIM/Model/Build/Build.out 2>&1

if [ $? -gt 0 ]
then
   echo ERRORS FOUND >> $APSIM/Model/Build/Build.out
fi
echo >> $APSIM/Model/Build/Build.out
echo >> $APSIM/Model/Build/Build.out
