@echo off

cd 2004
call CreateAllOut.bat

cd ..\2005
call CreateAllOut.bat

cd ..\2006
call CreateAllOut.bat

cd ..\2007
call CreateAllOut.bat

cd ..\2008
call CreateAllOut.bat

cd ..\

rem -----------------------------------------------
rem Produce all Wheat validation graphs.
rem -----------------------------------------------

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2004"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Wheat\2004.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2005"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Wheat\2005.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2006"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Wheat\2006.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2007"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Wheat\2007.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat' and year=2008"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Wheat\2008.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Wheat'"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Wheat\All.gif

rem -----------------------------------------------
rem Produce all Barley validation graphs.
rem -----------------------------------------------

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2005"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Barley\2005.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2006"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Barley\2006.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2007"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Barley\2007.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley' and year=2008"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Barley\2008.gif

%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Barley'"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Barley\All.gif

rem -----------------------------------------------
rem Produce all Canola validation graphs.
rem -----------------------------------------------
%APSIM%\Model\SetXMLValue YieldProphet.report Data/Filter/FilterString "Crop='Canola'"
"C:\Program Files\ApsimReport\ApsimReport.exe" YieldProphet.report %APSIM%\Graphs\YieldProphet\Validation\Canola\All.gif

rem -----------------------------------------------
rem Restore original YieldProphet.report
rem -----------------------------------------------
svn revert YieldProphet.report
