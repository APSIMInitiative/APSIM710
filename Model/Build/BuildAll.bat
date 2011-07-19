@echo off

pushd ..\..
set APSIM=%CD%
popd

%Apsim%\..\BuildLibraries\Tcl\ASTcl\bin\tclsh.exe WriteVersionNumber.tcl

..\JobScheduler BuildAll.xml

