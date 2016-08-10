@echo off
setlocal enabledelayedexpansion
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

rem ----- This will tell the versionstamper not to increment the revision number.
%APSIM%\Model\cscs.exe %APSIM%\Model\Build\VersionStamper.cs Directory=%APSIM% Increment=no

rem ----- Change to model directory
pushd %APSIM%\Model

if NOT "%1" == "" (
  SET VSTOOLPATH=VS%10COMNTOOLS
  goto forceVersion
)

rem ----- Determine the highest installed version of Visual Studio on the current system
rem ----- The set of numbers in the FOR statement indicate the VS version that we will look for
rem ----- 10 = VS 2010
rem ----- 11 = VS 2012
rem ----- 12 = VS 2013
rem ----- 14 = VS 2015
rem ----- (There is no 13!)
rem ----- If you want to force building with a specific version, just delete the numbers of the
rem ----- versions you don't want to consider
rem ----- The .NET components are built against 4.5 (VS 2012 or later)
set VSTOOLPATH=""
for %%a in (10 11 12 14) DO IF DEFINED VS%%a0COMNTOOLS (
  SET VSTOOLPATH=VS%%a0COMNTOOLS
)
if "%VSTOOLPATH%" == "" (
  echo Cannot locate the Visual Studio compiler!
  popd
  exit /B
) 
:forceVersion
set VSTOOLPATH=!%VSTOOLPATH%!

rem ----- Set up the Visual Studio compiler tools
if "%LIBPATH%" == "" call "%VSTOOLPATH%..\..\VC\vcvarsall.bat"

rem ----- Compile the job scheduler.
echo Compiling the JobScheduler
IF EXIST "%VSTOOLPATH%\..\IDE\devenv.exe" (
 "%VSTOOLPATH%\..\IDE\devenv.exe" "%APSIM%\Model\JobScheduler\JobScheduler.sln" /build debug
) ELSE IF EXIST "%VSTOOLPATH%\..\IDE\vcsexpress.exe" (
 "%VSTOOLPATH%\..\IDE\vcsexpress.exe" "%APSIM%\Model\JobScheduler\JobScheduler.sln" /build debug
) ELSE (
  echo "Cannot locate the Visual Studio compilers!"
  popd
  exit /B
)

echo.

rem ----- Run the job scheduler.
JobScheduler Build\BuildAll.xml Target=BuildAll

rem ----- Change back to Build directory
popd