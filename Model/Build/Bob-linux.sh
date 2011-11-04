#! /bin/sh

export APSIM=/home/bob/apsim

svn revert -R $APSIM
svn update $APSIM

cd $APSIM/Model
mono ./RunTime/cscs.exe ./Build/RemoveUnwantedFiles.cs $APSIM

# At this point the development tree will be clean
cp $APSIM/Model/RunTime/ICSharpCode.SharpZipLib.dll $APSIM/Model
$APSIM/Model/Build/RunMake.sh JobScheduler

cd $APSIM/Model/Build
../JobScheduler.exe Bob-linux.xml
