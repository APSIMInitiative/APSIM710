rem ----- Set the %APSIM% variable based on the directory where this batch file is located
:Start
pushd %~dp0..\..
set APSIM=%CD%
popd
cd %APSIM%\Model

rem ----- Save the path 
path > %TEMP%\SavedPath.bat

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

echo Compile the JobScheduler
call Build\RunMake.bat %APSIM%\Model\JobScheduler
if ERRORLEVEL 1 goto Error

rem ----- Clean the development tree-----
JobScheduler Build\BuildAll.xml Target=FullClean CreateRunner=No
if ERRORLEVEL 1 goto Error 
ren Build\BuildAllOutput.xml CleanOutput.xml
if ERRORLEVEL 1 goto Error

rem ------------------------------------------------------------------------
rem At this point the development tree will be clean
rem 
rem Go wait for a patch. WaitForPatch needs to
rem return 2 variables (PatchFileName and JobID) to this batch file. It 
rem can't use environment variables because a child process cannot alter
rem a parent's environment. Instead, it has writen the values to stdout.
rem The for loop below reads the values from stdout
rem and creates environment variables.
rem ------------------------------------------------------------------------

echo Re-compile the JobScheduler - files were previously cleaned out.
call Build\RunMake.bat %APSIM%\Model\JobScheduler
if ERRORLEVEL 1 goto Error

echo Wait for patch...
for /f "tokens=1,2" %%i in ('WaitForPatch.exe C:\Upload') do set %%i=%%j
if ERRORLEVEL 1 goto Error

echo Apply patch file: %PatchFileName%
ApplyPatch.exe %APSIM% "C:\inetpub\wwwroot\Files\Upload\%PatchFileName%.zip"

echo Re-compile the JobScheduler a third time, may have changed in patch.
call Build\RunMake.bat %APSIM%\Model\JobScheduler
if ERRORLEVEL 1 goto Error

echo Increment revision number in case of green build
set INCREMENT=Yes

echo Do full build and run
JobScheduler Build\BuildAll.xml Target=Bob CreateRunner=No
if ERRORLEVEL 1 goto Error

rem ------------------------------------------------------------------------
rem ----- Build and run has finished - clean up.
rem ------------------------------------------------------------------------
:CleanUp

rem ----- Reset the environment -----
set APSIM=
set LIBPATH=
set INCLUDE=
set LIB=
set PatchFileName=
set JobID=
%TEMP%\SavedPath.bat

goto Start

:Error
rem Don't clean up as it may be useful to see environment for debugging purposes.
