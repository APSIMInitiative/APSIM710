@echo off
pushd ..\..
set APSIM=%CD%
popd

rem Remove any previous readonly attributes from the jobscheduler binaries.
for %%f in (JobScheduler*.exe) do attrib -R %%f
for %%f in (JobScheduler*.dll) do attrib -R %%f

rem Need to compile the JobScheduler first.
cd ..\JobScheduler
call ..\Build\RunMake.bat APSIM=%APSIM%

rem Make the JobScheduler binary files readonly so that CleanAll doesn't remove them.
for %%f in (JobScheduler*.exe) do attrib +R %%f
for %%f in (JobScheduler*.dll) do attrib +R %%f

rem Now go and do full build and run.
cd ..\Build
..\JobScheduler Bob.xml

set APSIM=
