@echo off
pushd .

rem -------------------------------------------------
rem Probes a DLL and creates documentation + 
rem a manager helper.
rem %1 = ModelDLL
rem %2 = Module name e.g. wheat
rem -------------------------------------------------

rem -------------------------------------------------
rem Probe the DLL and get a Probe XML file for output
rem -------------------------------------------------
rem                            ModelDLL                ModelXML                       ProbeXMLOutput
%APSIM%\Model\ProbeDLL     %APSIM%\Model\%1        %APSIM%\Model\%2.xml       %APSIM%\Documentation\ModelInfo\%2.xml

rem -------------------------------------------------
rem Take the Probe XML file and generate some
rem Manager Helper source code
rem -------------------------------------------------
cd %APSIM%\Model\ManagerHelpers
rem                                                    ProbeXMLOutput                       ManagerHelperMacro
%APSIM%\Model\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\%2.xml   %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro

rem -------------------------------------------------
rem Take the ManagerHelperMacro source code and 
rem compile to a ManagerHelper .dll
rem -------------------------------------------------
%APSIM%\Model\SetXMLValue ManagerHelper.vcproj "@Name" %2ManagerHelper
"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" ManagerHelper.vcproj /build release

popd