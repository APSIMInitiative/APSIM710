rem @echo off
rem ----------------------------------------------
rem Create a release installation. 
rem    %1 = Name of file to create.
rem    %2 = Name of .sed file.
rem    %3 (optional) = /WithExclusions
rem ----------------------------------------------

rem ----- Set the %APSIM% variable based on the directory where this batch file is located
cd %~dp0..
set APSIM=%CD%
cd %APSIM%\Release

set InsertFiles=%APSIM%\Release\InsertFilesIntoSetup\bin\Release\InsertFilesIntoSetup.exe

rem Make a copy of the setup template before putting files into it. Also copy the .sed file 
mkdir %2
copy Files\ApsimSetupTemplate.vdproj %2\%2.vdproj
copy %2.sed %2\%2.sed
copy Exclusions.txt %2
copy Install.bat %2
cd %2

rem Now use the tool to insert filenames into the .vdproj file
%InsertFiles% %2.vdproj %3 ..\..\Apsim.xml                               %%Apsim%%
%InsertFiles% %2.vdproj %3 ..\..\UserInterface                           %%Apsim%%\UserInterface
%InsertFiles% %2.vdproj %3 ..\..\Examples                                %%Apsim%%\Examples
%InsertFiles% %2.vdproj %3 ..\..\Documentation                           %%Apsim%%\Documentation
%InsertFiles% %2.vdproj %3 ..\..\Model\*.lnk                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\..\Model\*.xml                             %%Apsim%%\Model
del ..\..\Model\APSIM.Tests.exe
%InsertFiles% %2.vdproj %3 ..\..\Model\*.exe                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\..\Model\*.dll                             %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\..\Model\RunTime\*.dll                     %%Apsim%%\Model
%InsertFiles% %2.vdproj %3 ..\..\Model\TclLink\bin\                      %%Apsim%%\Model\TclLink\bin
%InsertFiles% %2.vdproj %3 ..\..\Model\TclLink\lib\                      %%Apsim%%\Model\TclLink\lib
%InsertFiles% %2.vdproj %3 ..\..\Model\TclLink\CIDataTypes.tcl           %%Apsim%%\Model\TclLink

rem this requires the "VS2015 installer projects" extension from https://visualstudiogallery.msdn.microsoft.com/f1cc3f3e-c300-40a7-8797-c509fb8933b9
"%VS140COMNTOOLS%..\IDE\devenv" %2.vdproj /build Release

rem Now combine apsimsetup.msi and setup.exe into a self extracting installation.
rem This uses IExpress described here:
rem http://www.itscodingtime.com/post/Combine-Setup-MSI-and-EXE-into-a-single-package-with-IExpress.aspx
start /w iexpress /N /Q %2.sed

rem Now copy the releases to the right directory - with the revision number.
copy %2.exe C:\inetpub\wwwroot\Files\%1.exe

cd ..
rmdir /S /Q %2
