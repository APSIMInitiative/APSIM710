@echo off
rem -------------------------------------------------------------
rem This batch file compiles everything
rem -------------------------------------------------------------

rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

call RunMake %APSIM%\Model\RunTime
call RunMake %APSIM%\Model\JobScheduler

rem Now go and do full build and run.
..\JobScheduler.exe BuildAll.xml
