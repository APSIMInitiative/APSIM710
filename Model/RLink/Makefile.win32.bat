@rem Build the "embedding" dll that in turn loads R.dll on windows. 
@rem Requires RTools (from CRAN) to compile.
@rem I've never managed to build this easily on windows. 

set PATH=c:\Rtools\bin;C:\Rtools\mingw_32\bin;%windir%;%windir%\system32
set TMPDIR=c:/tmp

g++ -c -IC:/R-3.5.1/include -IC:/R-3.5.1/library/Rcpp/include -IC:/R-3.5.1/library/RInside/include -Ic:/apsim/Model REmbed.cpp 

g++ -c -IC:/R-3.5.1/include -IC:/R-3.5.1/library/Rcpp/include -IC:/R-3.5.1/library/RInside/include -Ic:/apsim/Model dllProcAddressWrapper.cpp

dlltool -d RLink.def -l libRLink.a

gcc -shared -s -static-libgcc -o ../REmbed.dll REmbed.o dllProcAddressWrapper.o libRlink.a "C:/R-3.5.1/bin/i386/R.dll" "C:/R-3.5.1/library/Rcpp/libs/i386/Rcpp.dll" "C:/R-3.5.1/library/RInside/lib/i386/libRInside.a" -lstdc++