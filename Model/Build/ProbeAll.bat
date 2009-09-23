@echo off
pushd .

cd ..\
ProbeDLL Barley.xml
ProbeDLL Broccoli.xml
ProbeDLL ButterflyPea.xml
ProbeDLL Canola.xml
ProbeDLL Centro.xml
ProbeDLL Chickpea.xml
ProbeDLL Cowpea.xml
ProbeDLL Fertiliser.xml
ProbeDLL Fieldpea.xml
ProbeDLL Horsegram.xml
ProbeDLL Lablab.xml
ProbeDLL Lettuce.xml
ProbeDLL Lucerne.xml
ProbeDLL Lupin.xml
ProbeDLL Maize.xml
ProbeDLL Mucuna.xml
ProbeDLL Mungbean.xml
ProbeDLL Navybean.xml
ProbeDLL Oats.xml
ProbeDLL Ozcot.xml
ProbeDLL Peanut.xml
ProbeDLL Phos.xml
ProbeDLL Pigeonpea.xml
ProbeDLL Potato.xml
ProbeDLL Slurp.xml
ProbeDLL Soil.xml
ProbeDLL Sorghum.xml
ProbeDLL Soybean.xml
ProbeDLL Stylo.xml
ProbeDLL Sugar.xml
ProbeDLL Sunflower.xml
ProbeDLL SweetCorn.xml
ProbeDLL SweetSorghum.xml
ProbeDLL Vine.xml
ProbeDLL Weed.xml
ProbeDLL Wheat.xml

cd "c:\program files\ausfarm"
%APSIM%\Model\ProbeDLL %APSIM%\UserInterface\Ausfarm.xml

popd