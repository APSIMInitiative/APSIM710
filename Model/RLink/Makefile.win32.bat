@rem Build the "embedding" dll that in turn loads R.dll on windows. 
@rem Requires RTools (from CRAN) to compile.
@rem I've never managed to build this easily on windows. 

set PATH=c:\Rtools\bin;c:\Rtools\gcc-4.6.3\bin;%windir%;%windir%\system32
set TMPDIR=c:/tmp

g++ -c -IC:/R-3.2.0/include -IC:/R-3.2.0/library/Rcpp/include -IC:/R-3.2.0/library/RInside/include -Ic:/apsim/Model REmbed.cpp 

g++ -c -IC:/R-3.2.0/include -IC:/R-3.2.0/library/Rcpp/include -IC:/R-3.2.0/library/RInside/include -Ic:/apsim/Model dllProcAddressWrapper.cpp

gcc -shared -s -static-libgcc -o ../REmbed.dll REmbed.o dllProcAddressWrapper.o libRLink.a  "C:/R-3.2.0/bin/i386/R.dll" "C:/R-3.2.0/library/Rcpp/libs/i386/Rcpp.dll" "C:/R-3.2.0/library/RInside/lib/i386/libRInside.a" -lstdc++