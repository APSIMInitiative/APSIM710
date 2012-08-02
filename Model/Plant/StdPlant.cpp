#include "StdPlant.h"

#include <stdio.h>
#include <stdarg.h>
char szBuffer[512]; 
ofstream dbg;
void XTrace(const char* lpszFormat, ...)
{
   string fmt = lpszFormat;
   replaceAll(fmt, "%f0", "%.0f");
   replaceAll(fmt, "%f2", "%.2f");
   replaceAll(fmt, "%f", "%.3f");
   if (fmt.find("oil.") == string::npos)
      {
      if (!dbg.is_open())
         dbg.open("plant.debug");
       va_list args;
       va_start(args, lpszFormat);
       int nBuf;
       nBuf = vsprintf(szBuffer, fmt.c_str(), args);
       strcat(szBuffer, "\n");
       dbg << szBuffer;
       //::OutputDebugString(szBuffer);
       va_end(args);
      }
}