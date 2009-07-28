@echo off
pushd .
cd %APSIM%\Model\ManagerHelper

%APSIM%\Model\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\%1.xml          %APSIM%\Model\ManagerHelper\ManagerHelpers.macro
%APSIM%\Model\SetXmlValue                    ManagerHelper.vcproj @Name %1ManagerHelper
call %APSIM%\Model\Build\MakeProject         ManagerHelper

popd