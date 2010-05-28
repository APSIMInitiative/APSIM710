@echo off
rem ----------------------------------------------
rem Create a release installation.
rem ----------------------------------------------
echo Making release msi >> %APSIM%\Model\Build\build.out
cd %APSIM%\Release

rem Compile our tool first.
"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" InsertFilesIntoSetup\InsertFilesIntoSetup.sln /build release >> %APSIM%\Model\Build\build.out

set InsertFiles=InsertFilesIntoSetup\bin\release\InsertFilesIntoSetup

rem Make a copy of the setup template before putting files into it.
copy ApsimSetupTemplate.vdproj ApsimSetup.vdproj

rem Now use the tool to insert filenames into the .vdproj file
%InsertFiles% ApsimSetup.vdproj ..\Apsim.xml                               %%Apsim%% >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\UserInterface                           %%Apsim%%\UserInterface >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Examples                                %%Apsim%%\Examples >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Documentation                           %%Apsim%%\Documentation >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\*.lnk                             %%Apsim%%\Model >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\*.xml                             %%Apsim%%\Model >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\*.exe                             %%Apsim%%\Model >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\*.dll                             %%Apsim%%\Model >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\RunTime\*.*                       %%Apsim%%\Model >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\*.dll                 %%Apsim%%\Model\TclLink\bin >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\*.exe                 %%Apsim%%\Model\TclLink\bin >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\lib\                   %%Apsim%%\Model\TclLink\lib >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\Tcl8.4.13.license.terms   %%Apsim%%\Model\TclLink >> %APSIM%\Model\Build\build.out
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink >> %APSIM%\Model\Build\build.out

"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" ApsimSetup.sln /build release >> %APSIM%\Model\Build\build.out
