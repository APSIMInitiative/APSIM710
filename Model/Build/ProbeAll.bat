@echo off
pushd .

call ProbeDLL Plant.dll        Barley
call ProbeDLL Plant2.dll       Broccoli
call ProbeDLL Plant.dll        ButterflyPea
call ProbeDLL Plant.dll        Canola
call ProbeDLL Plant.dll        Centro
call ProbeDLL Plant.dll        Chickpea
call ProbeDLL Plant.dll        Cowpea
call ProbeDLL Plant.dll        Chickpea
call ProbeDLL Fertiliser.dll   Fertiliser
call ProbeDLL Plant.dll        Fieldpea
call ProbeDLL Plant.dll        Horsegram
call ProbeDLL Plant.dll        Lablab
call ProbeDLL Plant.dll        Lettuce
call ProbeDLL Plant.dll        Lucerne
call ProbeDLL Plant.dll        Lupin
call ProbeDLL Maize.dll        Maize
call ProbeDLL Plant.dll        Mucuna
call ProbeDLL Plant.dll        Mungbean
call ProbeDLL Plant.dll        Navybean
call ProbeDLL Plant.dll        Oats
call ProbeDLL Ozcot.dll        Ozcot
call ProbeDLL Plant.dll        Mungbean
call ProbeDLL Plant.dll        Peanut
call ProbeDLL Plant.dll        Pigeonpea
call ProbeDLL Plant.dll        Potato
call ProbeDLL Plant2.dll       Slurp
call ProbeDLL SoilN.dll        SoilN
call ProbeDLL SoilWat.dll      SoilWat
call ProbeDLL Sorghum.dll      Sorghum
call ProbeDLL Plant.dll        Soybean
call ProbeDLL Plant.dll        Stylo
call ProbeDLL CropMod.dll      Sunflower
call ProbeDLL Plant.dll        SweetCorn
call ProbeDLL Plant.dll        SweetSorghum
call ProbeDLL Plant2.dll       Vine
call ProbeDLL Plant.dll        Weed
call ProbeDLL Plant.dll        Wheat

cd "c:\program files\ausfarm"
call %APSIM%\Model\Build\ProbeDLL Stock.dll        Stock
call %APSIM%\Model\Build\ProbeDLL Pasture.dll      Pasture
call %APSIM%\Model\Build\ProbeDLL Supplement.dll   Supplement.xml

popd