@echo off
rem ----------------------------------------------
rem Create a release installation. 
rem    %1 = Name of file to create.
rem    %2 = Name of .sed file.
rem    %3 (optional) = /WithExclusions
rem ----------------------------------------------

set InsertFiles=InsertFilesIntoSetup\bin\release\InsertFilesIntoSetup

rem ------------------------------------------------------------------------------------------------------------
rem ------- Create a release that has all files in it
rem ------------------------------------------------------------------------------------------------------------

rem Make a copy of the setup template before putting files into it.
copy ApsimSetupTemplate.vdproj %2.vdproj

rem Now use the tool to insert filenames into the .vdproj file
%InsertFiles% %2.vdproj %3 ..\Apsim.xml                               %%Apsim%%
%InsertFiles% %2.vdproj %3 ..\UserInterface                           %%Apsim%%\UserInterface
%InsertFiles% %2.vdproj %3 ..\Examples                                %%Apsim%%\Examples
%InsertFiles% %2.vdproj %3 ..\Documentation                           %%Apsim%%\Documentation
%InsertFiles% %2.vdproj %3 ..\Model\*.lnk                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\Model\*.xml                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\Model\*.exe                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\Model\*.dll                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\Model\RunTime\*.dll                     %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\Model\TclLink\bin\                      %%Apsim%%\Model\TclLink\bin
%InsertFiles% %2.vdproj %3 ..\Model\TclLink\lib\                      %%Apsim%%\Model\TclLink\lib
%InsertFiles% %2.vdproj %3 ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

"%VS100COMNTOOLS%\..\IDE\devenv" %2.vdproj /build release

rem Now combine apsimsetup.msi and setup.exe into a self extracting installation.
rem This uses IExpress described here:
rem http://www.itscodingtime.com/post/Combine-Setup-MSI-and-EXE-into-a-single-package-with-IExpress.aspx
..\..\BuildLibraries\IExpress32\iexpress /N /Q %2.sed

rem Now copy the releases to the right directory - with the revision number.
copy %2.exe C:\inetpub\wwwroot\Files\%1.exe

