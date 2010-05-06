@echo off
rem ----------------------------------------------
rem Create a release installation.
rem ----------------------------------------------

cd %APSIM%\Release

rem Compile our tool first.
"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" InsertFilesIntoSetup\InsertFilesIntoSetup.sln /build release

set InsertFiles=InsertFilesIntoSetup\bin\release\InsertFilesIntoSetup

rem Make a copy of the setup template before putting files into it.
copy ApsimSetupTemplate.vdproj ApsimSetup.vdproj

rem Now use the tool to insert filenames into the .vdproj file
%InsertFiles% ApsimSetup.vdproj ..\Apsim.xml                               %%Apsim%%
%InsertFiles% ApsimSetup.vdproj ..\UserInterface                           %%Apsim%%\UserInterface
%InsertFiles% ApsimSetup.vdproj ..\Examples                                %%Apsim%%\Examples
%InsertFiles% ApsimSetup.vdproj ..\Documentation                           %%Apsim%%\Documentation
%InsertFiles% ApsimSetup.vdproj ..\Model\*.lnk                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\*.xml                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\*.exe                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\*.dll                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\RunTime\*.*                       %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\*.dll                 %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\*.exe                 %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\lib\*.*                   %%Apsim%%\Model\TclLink\lib
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\Tcl8.4.13.license.terms   %%Apsim%%\Model\TclLink
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" ApsimSetup.sln /build release
