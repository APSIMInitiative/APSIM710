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
  unset PatchFileName; unset JobID
  patchInfo=`$APSIM/Model/JobSchedulerWaitForPatch.exe`
  while read -a nv; do
    vname=${nv[0]}
    value=${nv[@]:1}
    eval ${vname}=\"${value}\"
    echo Patch : ${vname}=${value}
  done <<< "$patchInfo"

  echo Downloading patch ${PatchFileName}.
  echo Downloading patch ${PatchFileName}   >> /tmp/Bootstrap.xml
  wget -nd "http://bob.apsim.info/Files/Upload/${PatchFileName}.zip" >> /tmp/Bootstrap.xml
  echo ----- JobSchedulerApplyPatch -----        >> /tmp/Bootstrap.xml
  $APSIM/Model/JobSchedulerApplyPatch.exe $APSIM "${PatchFileName}.zip"  >> /tmp/Bootstrap.xml

  echo \]\]\>\</StdOut\>\</Junk\>                >> /tmp/Bootstrap.xml
  cd $APSIM/Model/Build
  $APSIM/Model/JobScheduler.exe Bob-linux.xml PatchFileName=${PatchFileName}
done

