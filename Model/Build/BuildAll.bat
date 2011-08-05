@echo off
pushd ..\..
set APSIM=%CD%
popd

rem Remove any previous readonly attributes from the jobscheduler binaries.
for %%f in (JobScheduler*.exe) do attrib -R %%f
for %%f in (JobScheduler*.dll) do attrib -R %%f

rem Need to compile the JobScheduler first.
cd ..\JobScheduler

rem Force a rebuild by deleting the .exe
del ..\JobScheduler.exe
call ..\Build\RunMake.bat APSIM=%APSIM%

rem Make the JobScheduler binary files readonly so that CleanAll doesn't remove them.
for %%f in (JobScheduler*.exe) do attrib +R %%f
for %%f in (JobScheduler*.dll) do attrib +R %%f

cd ..\Build
%Apsim%\..\BuildLibraries\Tcl\ASTcl\bin\tclsh.exe WriteVersionNumber.tcl

rem Now go and do full build and run.
..\JobScheduler BuildAll.xml

set APSIM=
