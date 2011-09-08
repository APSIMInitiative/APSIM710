@echo off
rem -------------------------------------------------------------
rem This batch file compiles everything
rem -------------------------------------------------------------
pushd ..\..
set APSIM=%CD%
popd

call MakeProject RunTime
call MakeProject JobScheduler

rem Now go and do full build and run.
..\JobScheduler.exe BuildAll.xml
