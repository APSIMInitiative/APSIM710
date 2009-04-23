@echo off
cd Hermitage\HE1-8

%APSIM%\Model\ApsimReport.exe HE1.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE1.gif

%APSIM%\Model\ApsimReport.exe HE2.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE2.gif

%APSIM%\Model\ApsimReport.exe HE3.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE3.gif

%APSIM%\Model\ApsimReport.exe HE4.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE4.gif

%APSIM%\Model\ApsimReport.exe HE5.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE5.gif

%APSIM%\Model\ApsimReport.exe HE6.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE6.gif

%APSIM%\Model\ApsimReport.exe HE7.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE7.gif

%APSIM%\Model\ApsimReport.exe HE8.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE8.gif

cd Icrisat\BW

%APSIM%\Model\ApsimReport.exe BW5GxET1-4.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW5GxET1-4.gif
%APSIM%\Model\ApsimReport.exe BW5GxET5-8.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW5GxET5-8.gif
%APSIM%\Model\ApsimReport.exe BW5GxET9-12.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW5GxET9-12.gif
%APSIM%\Model\ApsimReport.exe BW8GxET1-4.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW8GxET1-4.gif
%APSIM%\Model\ApsimReport.exe BW8GxET5-8.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW8GxET5-8.gif
%APSIM%\Model\ApsimReport.exe BW8GxET9-12.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW8GxET9-12.gif

cd Lawes\LE
%APSIM%\Model\ApsimReport.exe Sorghum_LE13_Buster.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE13_Buster.gif
%APSIM%\Model\ApsimReport.exe Sorghum_LE13_M35-1.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE13_M35-1.gif
%APSIM%\Model\ApsimReport.exe Sorghum_LE14.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE14.gif
%APSIM%\Model\ApsimReport.exe Sorghum_LE17.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE17.gif
%APSIM%\Model\ApsimReport.exe LE19_Buster.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE19_Buster.gif
%APSIM%\Model\ApsimReport.exe LE19_CSH13R.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE19_CSH13R.gif
%APSIM%\Model\ApsimReport.exe LE21_A35xQL36.report  %APSIM%\Graphs\Sorghum\Validation\Lawes\LE21_A35xQL36.gif
%APSIM%\Model\ApsimReport.exe LE21_CSH13R.report    %APSIM%\Graphs\Sorghum\Validation\Lawes\LE21_CSH13R.gif
%APSIM%\Model\ApsimReport.exe LE21_QL39xQL36.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE21_QL39xQL36.gif

cd ..\..\

md Predicted
copy /Y Hermitage\HE1-8\*.out Predicted
copy /Y Icrisat\BW\*.out Predicted
copy /Y Lawes\LE\*.out Predicted

%APSIM%\Model\ApsimReport.exe SorghumValidation.report %APSIM%\Graphs\Sorghum\Validation\SorghumValidation.gif

