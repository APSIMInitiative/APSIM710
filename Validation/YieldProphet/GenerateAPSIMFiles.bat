rem ===================================================
rem This file will generate a new set of .apsim
rem files from the template file:
rem    C:\Users\hol353\Work\YieldProphet\Template.apsim
rem ===================================================

set YPBin=C:\Users\hol353\Work\YieldProphet\bin2
set HERE=%CD%

%YPBIN%\YPCreateAPSIM2.exe %HERE%\2004\YieldProphet.xml
%YPBIN%\YPCreateAPSIM2.exe %HERE%\2005\YieldProphet.xml
%YPBIN%\YPCreateAPSIM2.exe %HERE%\2006\YieldProphet.xml
%YPBIN%\YPCreateAPSIM2.exe %HERE%\2007\YieldProphet.xml
%YPBIN%\YPCreateAPSIM2.exe %HERE%\2008\YieldProphet.xml
%YPBIN%\YPCreateAPSIM2.exe %HERE%\2009\YieldProphet.xml

%YPBIN%\ConvertSimsFromSILOToMetFile.exe %HERE%\2004\YieldProphet.apsim 2004
%YPBIN%\ConvertSimsFromSILOToMetFile.exe %HERE%\2005\YieldProphet.apsim 2005
%YPBIN%\ConvertSimsFromSILOToMetFile.exe %HERE%\2006\YieldProphet.apsim 2006
%YPBIN%\ConvertSimsFromSILOToMetFile.exe %HERE%\2007\YieldProphet.apsim 2007
%YPBIN%\ConvertSimsFromSILOToMetFile.exe %HERE%\2008\YieldProphet.apsim 2008
%YPBIN%\ConvertSimsFromSILOToMetFile.exe %HERE%\2009\YieldProphet.apsim 2009
