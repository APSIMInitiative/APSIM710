@echo off
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

rem ----- This will tell the versionstamper not to increment the revision number.
%APSIM%\Model\cscs.exe %APSIM%\Model\Build\VersionStamper.cs Directory=%APSIM% Increment=no

rem ----- Change to model directory
pushd %APSIM%\Model

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- Compile the job scheduler.
echo Compiling the JobScheduler
IF EXIST "%VS100COMNTOOLS%\..\IDE\devenv.exe" (
 "%VS100COMNTOOLS%\..\IDE\devenv.exe" "%APSIM%\Model\JobScheduler\JobScheduler.sln" /build debug
) ELSE IF EXIST "%VS100COMNTOOLS%\..\IDE\vcsexpress.exe" (
 SET VS_EXPRESS=1
 "%VS100COMNTOOLS%\..\IDE\vcsexpress.exe" "%APSIM%\Model\JobScheduler\JobScheduler.sln" /build debug
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