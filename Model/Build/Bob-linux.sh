#! /bin/bash

export MONO_PREFIX=/usr

while [ : ] ; do
  export APSIM=/home/bob/apsim
  export LD_LIBRARY_PATH=$APSIM/Model
  svn revert -R $APSIM
  svn update $APSIM
  cd $APSIM/Model
  mono ./RunTime/cscs.exe ./Build/RemoveUnwantedFiles.cs $APSIM

  # At this point the development tree will be clean
  $APSIM/Model/Build/RunMake.sh RunTime
  $APSIM/Model/Build/RunMake.sh JobScheduler
  $APSIM/Model/JobSchedulerWaitForPatch.exe | while read line; do
    nv=( $line )
    export ${nv[0]}="${nv[@]:1}"
  done

  echo Patch file: $PatchFileName
  $APSIM/Model/JobSchedulerApplyPatch.exe $APSIM http://bob.apsim.info/Files/Upload/${PatchFileName}.zip

  cd $APSIM/Model/Build
  $APSIM/Model/JobScheduler.exe Bob-linux.xml
done

