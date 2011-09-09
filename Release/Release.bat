@echo off
rem ----------------------------------------------
rem Create a release installation.
rem ----------------------------------------------
echo Making release msi

rem Compile our tool first.
"%VS100COMNTOOLS%\..\IDE\devenv" InsertFilesIntoSetup\InsertFilesIntoSetup.sln /build release

set InsertFiles=InsertFilesIntoSetup\bin\release\InsertFilesIntoSetup

rem ------------------------------------------------------------------------------------------------------------
rem ------- Create a release that has all files in it
rem ------------------------------------------------------------------------------------------------------------

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
%InsertFiles% ApsimSetup.vdproj ..\Model\RunTime\*.dll                     %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\                      %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\lib\                      %%Apsim%%\Model\TclLink\lib
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

"%VS100COMNTOOLS%\..\IDE\devenv" ApsimSetup.sln /build release

rem Now combine apsimsetup.msi and setup.exe into a self extracting installation.
rem This uses IExpress described here:
rem http://www.itscodingtime.com/post/Combine-Setup-MSI-and-EXE-into-a-single-package-with-IExpress.aspx
..\..\BuildLibraries\IExpress32\iexpress /N /Q ApsimSetup.sed

rem ------------------------------------------------------------------------------------------------------------
rem ------- Now do the whole lot again this time with some files (Reference Panel unapproved) excluded
rem ------------------------------------------------------------------------------------------------------------

rem Make a copy of the setup template before putting files into it.
copy ApsimSetupTemplate.vdproj ApsimSetup.vdproj

rem Now use the tool to insert filenames into the .vdproj file
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Apsim.xml                               %%Apsim%%
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\UserInterface                           %%Apsim%%\UserInterface
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Examples                                %%Apsim%%\Examples
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Documentation                           %%Apsim%%\Documentation
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\*.lnk                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\*.xml                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\*.exe                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\*.dll                             %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\RunTime\*.dll                     %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\bin\                      %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\lib\                      %%Apsim%%\Model\TclLink\lib
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

"%VS100COMNTOOLS%\..\IDE\devenv" ApsimSetup.sln /build release

rem Now combine apsimsetup.msi and setup.exe into a self extracting installation.
rem This uses IExpress described here:
rem http://www.itscodingtime.com/post/Combine-Setup-MSI-and-EXE-into-a-single-package-with-IExpress.aspx
..\..\BuildLibraries\IExpress32\iexpress /N /Q ApsimSetupForRelease.sed

rem Now copy the releases to the right directory - with the revision number.
copy ApsimSetup.exe C:\inetpub\wwwroot\Files\%1.ApsimSetup.exe
copy ApsimSetupForRelease.exe C:\inetpub\wwwroot\Files\%1.ApsimSetupForRelease.exe


rem Now a self extracting "binaries only" for cluster runs
set dest=Temp
mkdir %dest%
copy  ..\Apsim.xml %dest%\Apsim.xml
mkdir %dest%\Model
copy  ..\Model\*.xml %dest%\Model
copy  ..\Model\*.dll %dest%\Model
copy  ..\Model\*.exe %dest%\Model
mkdir %dest%\Model\TclLink
copy  ..\Model\TclLink\CIDataTypes.tcl %dest%\Model\TclLink
xcopy /s ..\Model\TclLink\bin %dest%\Model\TclLink\bin
xcopy /s ..\Model\TclLink\lib %dest%\Model\TclLink\lib
mkdir %dest%\UserInterface
copy ..\UserInterface\*.xml %dest%\UserInterface
mkdir %dest%\UserInterface\ToolBoxes
copy ..\UserInterface\ToolBoxes\*.xml %dest%\UserInterface\ToolBoxes

"c:\Program Files\7-Zip\7z.exe" a -sfx -mx=9 -mmt=on %1.binaries.Win32.exe %dest%

rmdir /s /q %dest%

copy %1.binaries.Win32.exe C:\inetpub\wwwroot\Files\%1.binaries.Win32.exe
