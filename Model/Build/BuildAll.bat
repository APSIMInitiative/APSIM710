rem @echo off
setlocal enabledelayedexpansion
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

rem ----- This will tell the versionstamper not to increment the revision number.
%APSIM%\Model\cscs.exe %APSIM%\Model\Build\VersionStamper.cs Directory=%APSIM% Increment=no

rem ----- Change to model directory
pushd %APSIM%\Model

rem ----- Set up the Visual Studio compiler tools
call "C:\BuildTools\Common7\Tools\VsDevCmd.bat"

rem ----- Compile the job scheduler.
echo Compiling the JobScheduler
msbuild "%APSIM%\Model\JobScheduler\JobScheduler.sln"

rem ----- Run the job scheduler.
JobScheduler Build\BuildAll.xml Target=Release

rem ----- Change back to Build directory
popd
