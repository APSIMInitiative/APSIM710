#! /bin/bash

export MONO_PREFIX=/usr

while [ : ] ; do
  echo \<Junk\>\<StdOut\>\<\!\[CDATA\[           > /tmp/Bootstrap.xml

  export APSIM=/home/bob/apsim
  export LD_LIBRARY_PATH=$APSIM/Model
  echo ----- SVN revert -----                    >> /tmp/Bootstrap.xml
  svn revert -R $APSIM >> /tmp/Bootstrap.xml
  echo ----- SVN update -----                    >> /tmp/Bootstrap.xml
  svn update $APSIM
  cd $APSIM/Model
  echo ----- RemoveUnwantedFiles -----           >> /tmp/Bootstrap.xml
  mono ./RunTime/cscs.exe ./Build/RemoveUnwantedFiles.cs $APSIM >> /tmp/Bootstrap.xml

  # At this point the development tree will be clean
  echo ----- Runtime -----                       >> /tmp/Bootstrap.xml
  $APSIM/Model/Build/RunMake.sh RunTime          >> /tmp/Bootstrap.xml
  echo ----- JobScheduler -----                  >> /tmp/Bootstrap.xml
  $APSIM/Model/Build/RunMake.sh JobScheduler     >> /tmp/Bootstrap.xml
  echo ----- JobSchedulerWaitForPatch -----      >> /tmp/Bootstrap.xml
  $APSIM/Model/JobSchedulerWaitForPatch.exe | while read line; do
    nv=( $line )
    export ${nv[0]}="${nv[@]:1}"
  done

  echo Downloading patch file = $PatchFileName   >> /tmp/Bootstrap.xml
  wget -nd "http://bob.apsim.info/Files/Upload/${PatchFileName}.zip" >> /tmp/Bootstrap.xml
  echo ----- JobSchedulerApplyPatch -----        >> /tmp/Bootstrap.xml
  $APSIM/Model/JobSchedulerApplyPatch.exe $APSIM "${PatchFileName}.zip"  >> /tmp/Bootstrap.xml

  echo \]\]\>\</StdOut\>\</Junk\>                >> /tmp/Bootstrap.xml
  cd $APSIM/Model/Build
  $APSIM/Model/JobScheduler.exe Bob-linux.xml
done

