@echo off
REM a "wrapper" for R's gcc toolchain. 

set PATH=\Rtools30\bin;\Rtools30\gcc-4.6.3\bin;%windir%;%windir%\system32
set R_HOME=d:/R-3.0.1
set APSIM=d:/apsim
set PLATFORM=WIN32VS
set TMPDIR=/tmp
REM  -f Makefile.rgcc APSIM=$(APSIM) PLATFORM=$(PLATFORM) R_HOME=$(R_HOME) $@
\Rtools30\bin\make.exe %1 %2 %9
