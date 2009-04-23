@echo off
set HERE=%CD%
set YPBIN=\hol353\ApsimWork\YieldProphet\Bin

rem --------------------------------------------------------------
rem Extract the necessary paddocks from PaddockDetails.xml and put
rem into YieldProphet.xml. The names to extract are listed in
rem PaddockNames.txt
rem --------------------------------------------------------------
rem cd %APSROOT%\Tools\ExtractXMLElements\bin\Release\
rem ExtractXMLElements.exe %HERE%\PaddockDetails.xml %HERE%\PaddockNames.txt %HERE%\YieldProphet.xml

rem --------------------------------------------------------------
rem Turn the YieldProphet.xml file, created in the previous step,
rem into a YieldProphet.apsim file that is ready to run
rem --------------------------------------------------------------
cd %YPBIN%
YPCreateAPSIM.exe %HERE%\YieldProphet.xml

rem --------------------------------------------------------------
rem Remove all references to SILO from the YieldProphet.apsim file
rem that was created in the previous step. NB the year on the
rem end of the next line.
rem --------------------------------------------------------------
ConvertSimsFromSILOToMetFile.exe %HERE%\YieldProphet.apsim 2005

set HERE=
set YPBIN=
pause
