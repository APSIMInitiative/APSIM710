@echo off

rem ----- Endless loop to continually restart the JobScheduler.
:Start

rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd
cd %APSIM%

rem ----- Setup the bootstrap.xml
echo ^<StdOut^>^<StdOut^>                                                           > %TEMP%\Bootstrap.xml

echo ----- SVN revert -----                                                        >> %TEMP%\Bootstrap.xml
svn.exe revert -R %APSIM%                                                          >> %TEMP%\Bootstrap.xml

echo ----- SVN update -----                                                        >> %TEMP%\Bootstrap.xml
svn.exe update %APSIM%                                                             >> %TEMP%\Bootstrap.xml

echo ----- Remove unwanted files -----                                             >> %TEMP%\Bootstrap.xml
cd %APSIM%\Model
RunTime\cscs Build\RemoveUnwantedFiles.cs %APSIM%                                  >> %TEMP%\Bootstrap.xml

rem ------------------------------------------------------------------------
rem At this point the development tree will be clean
rem ------------------------------------------------------------------------

echo ----- Copy the Bootstrap.xml file to the build directory                      >> %TEMP%\Bootstrap.xml
copy %TEMP%\Bootstrap.xml %APSIM%\Model\Build

echo ----- Compile the JobScheduler -----                                          >> %APSIM%\Model\Build\Bootstrap.xml
copy %APSIM%\Model\RunTime\ICSharpCode.SharpZipLib.dll %APSIM%\Model               >> %APSIM%\Model\Build\Bootstrap.xml
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                    >> %APSIM%\Model\Build\Bootstrap.xml

echo ----- JobSchedulerWaitForPatch -----                                          >> %APSIM%\Model\Build\Bootstrap.xml
%APSIM%\Model\JobSchedulerWaitForPatch.exe C:\Upload                               >> %APSIM%\Model\Build\Bootstrap.xml
if ERRORLEVEL 1 goto CleanUp

echo ----- JobSchedulerApplyPatch -----                                            >> %APSIM%\Model\Build\Bootstrap.xml
%APSIM%\Model\JobSchedulerApplyPatch.exe %APSIM% "C:\Upload\%PatchFileName%.zip"   >> %APSIM%\Model\Build\Bootstrap.xml
if ERRORLEVEL 1 goto CleanUp

echo ----- Re-compile the JobScheduler -----                                       >> %APSIM%\Model\Build\Bootstrap.xml
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                    >> %APSIM%\Model\Build\Bootstrap.xml

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

rem ----- Go back to start if JobScheduler returns code 0
if ERRORLEVEL 1 goto End
if ERRORLEVEL 0 goto Start

:End
rem Stop at end so that we can see screen.
pause