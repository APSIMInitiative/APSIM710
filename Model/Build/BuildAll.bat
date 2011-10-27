@echo off
rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd

call CleanAll.bat
call %APSIM%\Model\Build\RunMake %APSIM%\Model\RunTime
call %APSIM%\Model\Build\RunMake %APSIM%\Model\JobScheduler

rem Now go and do full build and run.
%APSIM%\Model\JobScheduler.exe %APSIM%\Model\Build\BuildAll.xml
