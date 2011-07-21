@echo off
pushd ..\..
set APSIM=%CD%
popd

rem Need to compile the JobScheduler first.
rem Force a rebuild by deleting the .exe
cd %APSIM%\Model\JobScheduler
del ..\JobScheduler.exe
set PATHSAVED=%PATH%
call ..\Build\RunMake.bat APSIM=%APSIM%
set PATH=%PATHSAVED%
set LIBPATH=

rem Make the JobScheduler binary files readonly so that CleanAll doesn't remove them.
for %%f in (%APSIM%\Model\JobScheduler*.exe) do attrib +R %%f
for %%f in (%APSIM%\Model\JobScheduler*.dll) do attrib +R %%f

rem Now go and do full build and run.
cd %APSIM%\Model\Build
..\JobScheduler Bob.xml

rem Remove any previous readonly attributes from the jobscheduler binaries.
for %%f in (%APSIM%\Model\JobScheduler*.exe) do attrib -R %%f
for %%f in (%APSIM%\Model\JobScheduler*.dll) do attrib -R %%f

set APSIM=
