@echo off
pushd .
cd %APSIM%\Model\ManagerHelpers

set MODEL=%APSIM%\Model
set MODELINFO=%APSIM%\Documentation\ModelInfo

copy /Y %APSIM%\Model\ManagerHelpers\ManagerHelper.sl %APSIM%\Model\ManagerHelpers\ManagerHelper.sln

%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Barley.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Broccoli.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\ButterflyPea.xml    %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Canola.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Centro.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Chickpea.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Cowpea.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Chickpea.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Fertiliser.xml      %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Fieldpea.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Horsegram.xml       %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Lablab.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Lettuce.xml         %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Lucerne.xml         %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Lupin.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Maize.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Mucuna.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Mungbean.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Navybean.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Oats.xml            %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Ozcot.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Mungbean.xml        %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Peanut.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Pigeonpea.xml       %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Potato.xml          %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Slurp.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\SoilN.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\SoilWat.xml         %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Sorghum.xml         %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Soybean.xml         %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Stylo.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Sunflower.xml       %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\SweetCorn.xml       %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\SweetSorghum.xml    %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Vine.xml            %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Weed.xml            %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Wheat.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro


cd "c:\program files\ausfarm"
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Stock.xml           %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Pasture.xml         %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro
%MODEL%\ProcessDataTypesInterface     %APSIM%\Documentation\ModelInfo\Supplement.xml      %APSIM%\Model\ManagerHelpers\ManagerHelpers.macro

:end

del %APSIM%\Model\ManagerHelpers\ManagerHelper.sln
popd
pause