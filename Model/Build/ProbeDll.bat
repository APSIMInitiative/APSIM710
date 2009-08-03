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

popd