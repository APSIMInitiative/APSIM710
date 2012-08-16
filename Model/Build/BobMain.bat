rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd
cd %APSIM%

rem ----- Save the path 
path > %TEMP%\SavedPath.bat

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

echo ----- Install runtimes         -----                                          > %APSIM%\Model\Build\Bootstrap.xml
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\RunTime                         >> %APSIM%\Model\Build\Bootstrap.xml
if ERRORLEVEL 1 goto CleanUp

echo ----- Compile the JobScheduler -----                                          >> %APSIM%\Model\Build\Bootstrap.xml
del /Q %APSIM%\Model\*.exe
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                    >> %APSIM%\Model\Build\Bootstrap.xml
if ERRORLEVEL 1 goto CleanUp

rem ------------------------------------------------------------------------
rem Go wait for a patch. JobSchedulerWaitForPatch needs to
rem return 2 variables (PatchFileName and JobID) to this batch file. It 
rem can't use environment variables because a child process cannot alter
rem a parent's environment. Instead, it has writen the values to stdout.
rem The for loop below reads the values from stdout
rem and creates environment variables.
rem ------------------------------------------------------------------------
echo Patch file: %PatchFileName%

echo ----- JobSchedulerApplyPatch -----                                            >> %APSIM%\Model\Build\Bootstrap.xml
%APSIM%\Model\JobSchedulerApplyPatch.exe %APSIM% "C:\inetpub\wwwroot\Files\Upload\%PatchFileName%.zip"   >> %APSIM%\Model\Build\Bootstrap.xml

echo ----- Re-compile the JobScheduler -----                                       >> %APSIM%\Model\Build\Bootstrap.xml
del /Q %APSIM%\Model\*.exe
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                    >> %APSIM%\Model\Build\Bootstrap.xml

echo ----- Set the (fake incremented) version numbers -----                        >> %APSIM%\Model\Build\Bootstrap.xml
cd %APSIM%\Model\Build
%APSIM%\Model\TclLink\bin\tclsh85.exe VersionStamper.tcl Increment                 >> %APSIM%\Model\Build\Bootstrap.xml
call %APSIM%\Model\Build\VersionInfo.bat                                           >> %APSIM%\Model\Build\Bootstrap.xml
cd %APSIM%

rem ----- Run the JobScheduler -----
echo ^</StdOut^>^</StdOut^>                                                        >> %APSIM%\Model\Build\Bootstrap.xml
%APSIM%\Model\JobScheduler %APSIM%\Model\Build\Bob.xml                             

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