#! /bin/sh

export APSIM=/home/bob/apsim

svn revert -R $APSIM
svn update $APSIM

cd $APSIM/Model
mono ./RunTime/cscs.exe ./Build/RemoveUnwantedFiles.cs $APSIM

# At this point the development tree will be clean
cp $APSIM/Model/RunTime/ICSharpCode.SharpZipLib.dll $APSIM/Model
$APSIM/Model/Build/RunMake.sh JobScheduler


??????????for /f "tokens=1,2" %%i in ('%APSIM%\Model\JobSchedulerWaitForPatch.exe C:\Upload') do set %%i=%%j

echo Patch file: %PatchFileName%

cd $APSIM/Model/Build
../JobScheduler.exe Bob-linux.xml

