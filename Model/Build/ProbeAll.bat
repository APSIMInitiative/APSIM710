@echo on
pushd .

set MODEL=%APSIM%\Model
set MODELINFO=%APSIM%\Documentation\ModelInfo

rem Bambatsi
rem Broccoli
rem Clock
rem EGrandis
rem EMelliodora
rem EPopulnea
rem Erosion
rem Grasp
rem Maize
rem MicroMet
rem Millet
rem Orobanche
rem Oryza
rem Rice
rem SoilP
rem SoilTemp
rem Solute
rem StockScienceConverter
rem Sugar
rem Sunflower
rem SurfaceOM
rem SWIM2
rem Tree
rem WaterSupply



%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Barley.xml          %MODELINFO%\Barley.xml
%MODEL%\ProbeDLL %MODEL%\Plant2.dll       %MODEL%\Broccoli.xml        %MODELINFO%\Broccoli.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\ButterflyPea.xml    %MODELINFO%\ButterflyPea.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Canola.xml          %MODELINFO%\Canola.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Centro.xml          %MODELINFO%\Centro.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Chickpea.xml        %MODELINFO%\Chickpea.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Cowpea.xml          %MODELINFO%\Cowpea.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Chickpea.xml        %MODELINFO%\Chickpea.xml
%MODEL%\ProbeDLL %MODEL%\Fertiliser.dll   %MODEL%\Fertiliser.xml      %MODELINFO%\Fertiliser.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Fieldpea.xml        %MODELINFO%\Fieldpea.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Horsegram.xml       %MODELINFO%\Horsegram.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Lablab.xml          %MODELINFO%\Lablab.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Lettuce.xml         %MODELINFO%\Lettuce.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Lucerne.xml         %MODELINFO%\Lucerne.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Lupin.xml           %MODELINFO%\Lupin.xml
%MODEL%\ProbeDLL %MODEL%\Maize.dll        %MODEL%\Maize.xml           %MODELINFO%\Maize.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Mucuna.xml          %MODELINFO%\Mucuna.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Mungbean.xml        %MODELINFO%\Mungbean.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Navybean.xml        %MODELINFO%\Navybean.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Oats.xml            %MODELINFO%\Oats.xml
%MODEL%\ProbeDLL %MODEL%\Ozcot.dll        %MODEL%\Ozcot.xml           %MODELINFO%\Ozcot.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Mungbean.xml        %MODELINFO%\Mungbean.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Peanut.xml          %MODELINFO%\Peanut.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Pigeonpea.xml       %MODELINFO%\Pigeonpea.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Potato.xml          %MODELINFO%\Potato.xml
%MODEL%\ProbeDLL %MODEL%\Plant2.dll       %MODEL%\Slurp.xml           %MODELINFO%\Slurp.xml
%MODEL%\ProbeDLL %MODEL%\SoilN.dll        %MODEL%\SoilN.xml           %MODELINFO%\SoilN.xml
%MODEL%\ProbeDLL %MODEL%\SoilWat.dll      %MODEL%\SoilWat.xml         %MODELINFO%\SoilWat.xml
%MODEL%\ProbeDLL %MODEL%\Sorghum.dll      %MODEL%\Sorghum.xml         %MODELINFO%\Sorghum.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Soybean.xml         %MODELINFO%\Soybean.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Stylo.xml           %MODELINFO%\Stylo.xml
%MODEL%\ProbeDLL %MODEL%\CropMod.dll      %MODEL%\Sunflower.xml       %MODELINFO%\Sunflower.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\SweetCorn.xml       %MODELINFO%\SweetCorn.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\SweetSorghum.xml    %MODELINFO%\SweetSorghum.xml
%MODEL%\ProbeDLL %MODEL%\Plant2.dll       %MODEL%\Vine.xml            %MODELINFO%\Vine.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Weed.xml            %MODELINFO%\Weed.xml
%MODEL%\ProbeDLL %MODEL%\Plant.dll        %MODEL%\Wheat.xml           %MODELINFO%\Wheat.xml


cd "c:\program files\ausfarm"
%MODEL%\ProbeDLL Stock.dll                %MODEL%\Stock.xml           %MODELINFO%\Stock.xml
%MODEL%\ProbeDLL Pasture.dll              %MODEL%\Pasture.xml         %MODELINFO%\Pasture.xml
%MODEL%\ProbeDLL Supplement.dll           %MODEL%\Supplement.xml      %MODELINFO%\Supplement.xml


set MODEL=
set MODELINFO=
popd