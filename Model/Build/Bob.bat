@echo off

rem ----- Endless loop to continually restart the JobScheduler.
:Start

rem ----- Set the %APSIM% variable based on the directory where this batch file is located
pushd %~dp0..\..
set APSIM=%CD%
popd
cd %APSIM%


echo ----- SVN revert -----                                                            > %APSIM%\Model\Build\Bootstrap.txt
svn.exe revert -R %APSIM%                                                             >> %APSIM%\Model\Build\Bootstrap.txt

echo ----- SVN update -----                                                           >> %APSIM%\Model\Build\Bootstrap.txt
svn.exe update %APSIM%                                                                >> %APSIM%\Model\Build\Bootstrap.txt

echo ----- Remove unwanted files -----                                                >> %APSIM%\Model\Build\Bootstrap.txt
cd %APSIM%\Model
RunTime\cscs Build\RemoveUnwantedFiles.cs %APSIM%                                     >> %APSIM%\Model\Build\Bootstrap.txt

rem ------------------------------------------------------------------------
rem At this point the development tree will be clean
rem ------------------------------------------------------------------------

echo ----- Compile the JobScheduler -----                                             >> %APSIM%\Model\Build\Bootstrap.txt
copy %APSIM%\Model\RunTime\ICSharpCode.SharpZipLib.dll %APSIM%\Model
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                       >> %APSIM%\Model\Build\Bootstrap.txt

echo ----- JobSchedulerWaitForPatch -----                                             >> %APSIM%\Model\Build\Bootstrap.txt
%APSIM%\Model\JobSchedulerWaitForPatch.exe C:\Upload                                  >> %APSIM%\Model\Build\Bootstrap.txt

echo ----- JobSchedulerApplyPatch -----                                               >> %APSIM%\Model\Build\Bootstrap.txt
%Apsim%\Model\JobSchedulerApplyPatch.exe %Apsim% "C:\Upload\%PatchFileName%.zip"      >> %APSIM%\Model\Build\Bootstrap.txt

echo ----- Re-compile the JobScheduler -----                                          >> %APSIM%\Model\Build\Bootstrap.txt
call %APSIM%\Model\Build\RunMake.bat %APSIM%\Model\JobScheduler                       >> %APSIM%\Model\Build\Bootstrap.txt

echo ----- Run the JobScheduler -----                                                 >> %APSIM%\Model\Build\Bootstrap.txt
..\JobScheduler Bob.xml

echo ----- Reset the environment -----                                                >> %APSIM%\Model\Build\Bootstrap.txt
set APSIM=
set PATH=%PATHSAVED%
set LIBPATH=
set INCLUDE=
set LIB=

rem ----- Go back to start if JobScheduler returns code 0
if ERRORLEVEL 1 goto End
if ERRORLEVEL 0 goto Start

:End