@echo off
rem ----------------------------------------------
rem Create a release installation.
rem ----------------------------------------------
echo Making release msi
cd %APSIM%\Release

rem Compile our tool first.
"%VS90COMNTOOLS%\..\IDE\devenv" InsertFilesIntoSetup\InsertFilesIntoSetup.sln /build release

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
%InsertFiles% ApsimSetup.vdproj ..\Model\RunTime\*.*                       %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\*.dll                 %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\bin\*.exe                 %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\lib\                      %%Apsim%%\Model\TclLink\lib
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\Tcl8.4.13.license.terms   %%Apsim%%\Model\TclLink
%InsertFiles% ApsimSetup.vdproj ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

"%VS90COMNTOOLS%\..\IDE\devenv" ApsimSetup.sln /build release

rem Now combine apsimsetup.msi and setup.exe into a self extracting installation.
rem This uses IExpress described here:
rem http://www.itscodingtime.com/post/Combine-Setup-MSI-and-EXE-into-a-single-package-with-IExpress.aspx
c:\iexpress32\iexpress /N /Q ApsimSetup.sed

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
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\RunTime\*.*                       %%Apsim%%\Model
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\bin\*.dll                 %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\bin\*.exe                 %%Apsim%%\Model\TclLink\bin
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\lib\                      %%Apsim%%\Model\TclLink\lib
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\Tcl8.4.13.license.terms   %%Apsim%%\Model\TclLink
%InsertFiles% ApsimSetup.vdproj /WithExclusions ..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

"%VS90COMNTOOLS%\..\IDE\devenv" ApsimSetup.sln /build release

rem Now combine apsimsetup.msi and setup.exe into a self extracting installation.
rem This uses IExpress described here:
rem http://www.itscodingtime.com/post/Combine-Setup-MSI-and-EXE-into-a-single-package-with-IExpress.aspx
c:\iexpress32\iexpress /N /Q ApsimSetupForRelease.sed

rem Now copy the 2 releases to the right directory - with the revision number.
copy ApsimSetup.exe C:\inetpub\wwwroot\Files\%1.ApsimSetup.exe
copy ApsimSetupForRelease.exe C:\inetpub\wwwroot\Files\%1.ApsimSetupForRelease.exe
