@echo off
cd %APSIM%\Model\Build

if EXIST VersionInfo.bat call VersionInfo

pushd %APSIM%\Model\%1

if EXIST makefile (
   call %APSIM%\Model\Build\RunMake %2 %3 %4 %5 
)

popd


