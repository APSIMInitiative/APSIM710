@echo off

rem Change DateTime format inside the docker container, otherwise unit tests will fail.
reg add "HKCU\Control Panel\International" /V sShortDate /T REG_SZ /D dd/MM/yyyy /F

set APSIM=C:\APSIM
cd %APSIM%

rem ----- This will prevent the git hash from being inserted into the output files.
set IncludeBuildNumberInOutSumFile=No

rem ----- Change to Build directory.
cd %APSIM%\Model\Build

rem ----- This will tell the versionstamper not to increment the revision number.
%APSIM%\Model\cscs.exe %APSIM%\Model\Build\VersionStamper.cs Directory=%APSIM% Increment=no

rem ----- Set up the Visual Studio compiler tools
call "C:\BuildTools\Common7\Tools\VsDevCmd.bat"

rem ----- Compile the job scheduler.
echo Compiling the JobScheduler
msbuild "%APSIM%\Model\JobScheduler\JobScheduler.sln" /v:m

rem ----- Run the job scheduler.
cd %APSIM%\Model
JobScheduler Build\BuildAll.xml Target=BuildAll
