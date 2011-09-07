@echo off
pushd ..\..
set APSIM=%CD%
popd

call CleanAll.bat
call MakeProject RunTime
call MakeProject JobScheduler

rem Now go and do full build and run.
..\JobScheduler.exe BuildAll.xml
