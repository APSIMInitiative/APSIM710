@echo off
pushd .
cd %APSIM%\Model\ManagerHelper

%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\%1.xml          %APSIM%\Model\ManagerHelper\ManagerHelpers.macro
..\SetXmlValue ManagerHelper.vcproj @Name %1ManagerHelper
..\Build\MakeProject ManagerHelper

popd