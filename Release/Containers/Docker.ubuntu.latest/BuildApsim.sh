#!/bin/bash

# Build apsim 
APSIM=/usr/local/src/apsim/
rm -rf $APSIM
mkdir -p $APSIM/ 
cd $APSIM/..

git clone https://github.com/APSIMInitiative/APSIMClassic apsim

cd $APSIM/Model/Build
sh ./BuildAll.sh

chmod +x $APSIM/Model/Build/VersionInfo.sh
. $APSIM/Model/Build/VersionInfo.sh

# Binaries are installed here
dest=/usr/local/lib/Apsim$MAJOR_VERSION.$MINOR_VERSION-r$BUILD_NUMBER
mkdir $dest
cp    $APSIM/Apsim.xml   $dest/Apsim.xml
mkdir $dest/Model
cp    $APSIM/Model/*.xml $dest/Model
cp    $APSIM/Model/*.so  $dest/Model

cd $dest/Model
for so in ApsimShared General Protocol ComponentInterface ComponentInterface2 FortranComponentInterface FortranComponentInterface2; do
  rm lib$so.so
  ln -s $so.so lib$so.so
done
cp    $APSIM/Model/*.dll $dest/Model
if [ -f $dest/Model/APSIM.Tests.dll ]; then /bin/rm -f $dest/Model/APSIM.Tests.dll; fi
cp    $APSIM/Model/*.exe $dest/Model
mkdir $dest/Model/TclLink
cp    $APSIM/Model/TclLink/CIDataTypes.tcl $dest/Model/TclLink
mkdir $dest/UserInterface
cp    $APSIM/UserInterface/*.xml $dest/UserInterface
mkdir $dest/UserInterface/ToolBoxes
cp    $APSIM/UserInterface/ToolBoxes/*.xml $dest/UserInterface/ToolBoxes

# Wrappers for the above 
for i in ApsimUI.exe ApsimToSim.exe ConToSim.exe BundleApsim.exe cscs.exe Apsim.exe JobScheduler.exe; do 
cat > /usr/local/bin/$i << EOF 
#!/bin/sh
mono $dest/Model/$i "\$@"
EOF
chmod +x /usr/local/bin/$i
done

for i in ApsimModel.exe; do
cat > /usr/local/bin/$i << EOF2 
#!/bin/sh
export LD_LIBRARY_PATH=$dest/Model:$LD_LIBRARY_PATH
$dest/Model/$i "\$@"
EOF2
chmod +x /usr/local/bin/$i
done

# workaround https://github.com/APSIMInitiative/APSIMClassic/issues/1755#issuecomment-619705163
mkdir $dest/lib
ln -s /usr/lib/libmono-native.so $dest/lib/libSystem.Native.so

# This is where the installation is archived (mapped to a docker volume)
dist=/opt/apsim

cd /
tar cfz $dist/Apsim$MAJOR_VERSION.$MINOR_VERSION-r$BUILD_NUMBER.ubuntu.tar.gz  usr/local/bin/*.exe $dest

#if [ -f $dist/Apsim$MAJOR_VERSION.$MINOR_VERSION-r$BUILD_NUMBER.ubuntu.tar.gz ]; then
#  rm -f $dist/Apsim$MAJOR_VERSION.$MINOR_VERSION-r$BUILD_NUMBER.ubuntu.tar.gz
#fi
#ln -s $dist/Apsim$MAJOR_VERSION.$MINOR_VERSION-r$BUILD_NUMBER.ubuntu.tar.gz $dist/Apsim$MAJOR_VERSION.$MINOR_VERSION-latest.ubuntu.tar.gz
