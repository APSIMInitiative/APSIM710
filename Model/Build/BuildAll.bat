@echo off
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

rem ----- Change to model directory
pushd %APSIM%\Model

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- This will tell the versionstamper not to increment the revision number.
set INCREMENT=

rem ----- Compile the job scheduler.
echo Compiling the JobScheduler
call Build\RunMake %APSIM%\Model\JobScheduler
echo.

rem ----- Run the job scheduler.
JobScheduler Build\BuildAll.xml Target=BuildAll

rem ----- Change back to Build directory
popd