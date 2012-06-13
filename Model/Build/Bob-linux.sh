#! /bin/sh

while 1 ; do
  export APSIM=/home/bob/apsim
  svn revert -R $APSIM
  svn update $APSIM
  cd $APSIM/Model
  mono ./RunTime/cscs.exe ./Build/RemoveUnwantedFiles.cs $APSIM

  # At this point the development tree will be clean
  $APSIM/Model/Build/RunMake.sh RunTime
  $APSIM/Model/Build/RunMake.sh JobScheduler
  $APSIM/Model/JobSchedulerWaitForPatch.exe |  -d =

  #??????????for /f "tokens=1,2" %%i in ('%APSIM%\Model\JobSchedulerWaitForPatch.exe') do set %%i=%%j
  echo Patch file: $PatchFileName
  $APSIM/Model/JobSchedulerApplyPatch.exe $APSIM http://bob.apsim.info/Files/${PatchFileName}.zip

  cd $APSIM/Model/Build
  $APSIM/Model/JobScheduler.exe Bob-linux.xml
done

