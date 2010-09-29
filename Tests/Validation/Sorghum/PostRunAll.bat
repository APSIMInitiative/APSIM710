@echo off
cd Hermitage\HE1-8

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE1.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE1.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE2.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE2.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE3.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE3.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE4.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE4.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE5.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE5.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE6.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE6.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE7.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE7.gif

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" HE8.report %APSIM%\Graphs\Sorghum\Validation\Hermitage\HE8.gif

cd Icrisat\BW

"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" BW5GxET1-4.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW5GxET1-4.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" BW5GxET5-8.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW5GxET5-8.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" BW5GxET9-12.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW5GxET9-12.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" BW8GxET1-4.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW8GxET1-4.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" BW8GxET5-8.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW8GxET5-8.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" BW8GxET9-12.report %APSIM%\Graphs\Sorghum\Validation\Icrisat\BW8GxET9-12.gif

cd Lawes\LE
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" Sorghum_LE13_Buster.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE13_Buster.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" Sorghum_LE13_M35-1.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE13_M35-1.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" Sorghum_LE14.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE14.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" Sorghum_LE17.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE17.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" LE19_Buster.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE19_Buster.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" LE19_CSH13R.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE19_CSH13R.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" LE21_A35xQL36.report  %APSIM%\Graphs\Sorghum\Validation\Lawes\LE21_A35xQL36.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" LE21_CSH13R.report    %APSIM%\Graphs\Sorghum\Validation\Lawes\LE21_CSH13R.gif
"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" LE21_QL39xQL36.report %APSIM%\Graphs\Sorghum\Validation\Lawes\LE21_QL39xQL36.gif

cd ..\..\



"C:\Program Files (x86)\ApsimReport\ApsimReport.exe" SorghumValidation.report %APSIM%\Graphs\Sorghum\Validation\SorghumValidation.gif

