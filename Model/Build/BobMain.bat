rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd
cd %APSIM%

rem ----- Save the path 
path > %TEMP%\SavedPath.bat

rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- Setup the bootstrap.xml
echo ^<StdOut^>^<StdOut^>                                                           > %TEMP%\Bootstrap.xml

echo ----- SVN revert -----                                                        >> %TEMP%\Bootstrap.xml
svn.exe revert -R %APSIM%                                                          >> %TEMP%\Bootstrap.xml

echo ----- SVN update -----                                                        >> %TEMP%\Bootstrap.xml
svn.exe update %APSIM%                                                             >> %TEMP%\Bootstrap.xml

echo ----- Remove unwanted files -----                                             >> %TEMP%\Bootstrap.xml
cd %APSIM%\Model
RunTime\cscs Build\RemoveUnwantedFiles.cs %APSIM%                                  >> %TEMP%\Bootstrap.xml
if ERRORLEVEL 1 goto CleanUp 

rem ------------------------------------------------------------------------
rem At this point the development tree will be clean
rem ------------------------------------------------------------------------

echo ----- Copy the Bootstrap.xml file to the build directory                      >> %TEMP%\Bootstrap.xml
copy %TEMP%\Bootstrap.xml %APSIM%\Model\Build

echo ----- Compile the JobScheduler -----                                          >> %APSIM%\Model\Build\Bootstrap.xml
copy %APSIM%\Model\RunTime\ICSharpCode.SharpZipLib.dll %APSIM%\Model               >> %APSIM%\Model\Build\Bootstrap.xml
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                    >> %APSIM%\Model\Build\Bootstrap.xml

rem ------------------------------------------------------------------------
rem Go wait for a patch. JobSchedulerWaitForPatch needs to
rem return 2 variables (PatchFileName and JobID) to this batch file. It 
rem can't use environment variables because a child process cannot alter
rem a parent's environment. Instead, it has writen the values to stdout.
rem The for loop below reads the values from stdout
rem and creates environment variables.
rem ------------------------------------------------------------------------
echo ----- JobSchedulerWaitForPatch -----                                          >> %APSIM%\Model\Build\Bootstrap.xml
for /f "tokens=1,2" %%i in ('%APSIM%\Model\JobSchedulerWaitForPatch.exe C:\Upload') do set %%i=%%j
if ERRORLEVEL 1 goto CleanUp
echo Patch file: %PatchFileName%

echo ----- Re-compile the JobScheduler -----                                       >> %APSIM%\Model\Build\Bootstrap.xml
del /Q %APSIM%\Model\*.exe
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                    >> %APSIM%\Model\Build\Bootstrap.xml

echo ----- Set the (fake incremented) version numbers -----                        >> %APSIM%\Model\Build\Bootstrap.xml
%APSIM%\Model\TclLink\bin\tclsh85.exe VersionStamper.tcl Increment                 >> %APSIM%\Model\Build\Bootstrap.xml
call %APSIM%\Model\Build\VersionInfo.bat                                           >> %APSIM%\Model\Build\Bootstrap.xml

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