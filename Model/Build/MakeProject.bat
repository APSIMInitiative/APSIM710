@echo off
cd %APSIM%\Model\Build

echo ----------------------------------------------------- >> Build.out
echo Compiling  %1 >> Build.out
echo ----------------------------------------------------- >> Build.out

pushd %APSIM%\Model\%1

if EXIST makefile (
   %APSIM%\Model\Build\make >> %APSIM%\Model\Build\Build.out
)

for %%f in (*.sln) do "C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv" %%f /build release >> %APSIM%\Model\Build\Build.out

popd
if ERRORLEVEL 1 echo ERRORS FOUND >> Build.out
if ERRORLEVEL 1 echo ERRORS FOUND in %1 >> Build.log
echo. >> Build.out
echo. >> Build.out


