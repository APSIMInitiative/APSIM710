@echo off
cd %APSIM%\Model\Build

echo ----------------------------------------------------- >> Build.out
echo Compiling  %1 >> Build.out
echo ----------------------------------------------------- >> Build.out

pushd %APSIM%\Model\%1

if EXIST makefile (
   call %APSIM%\Model\Build\RunMake >> %APSIM%\Model\Build\Build.out
)

popd
if ERRORLEVEL 1 echo ERRORS FOUND >> Build.out
if ERRORLEVEL 1 echo ERRORS FOUND in %1 >> Build.log
echo. >> Build.out
echo. >> Build.out


