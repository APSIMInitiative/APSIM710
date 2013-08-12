REM a "wrapper" for R's gcc toolchain. 

set PATH=\Rtools30\bin;\Rtools30\gcc-4.6.3\bin;%PATH%
set R_HOME=/R-3.0.1
set TMPDIR=/tmp

\Rtools30\bin\make.exe %1 %2 %3 %4
