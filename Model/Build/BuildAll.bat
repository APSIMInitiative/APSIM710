@echo off
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

call CleanAll.bat
call %APSIM%\Model\Build\RunMake %APSIM%\Model\RunTime
call %APSIM%\Model\Build\RunMake %APSIM%\Model\JobScheduler

%APSIM%\Model\TclLink\bin\tclsh85.exe VersionStamper.tcl
if EXIST "%APSIM%\Model\Build\VersionInfo.bat" call "%APSIM%\Model\Build\VersionInfo.bat"

rem Now go and do full build and run.
%APSIM%\Model\JobScheduler.exe %APSIM%\Model\Build\BuildAll.xml
