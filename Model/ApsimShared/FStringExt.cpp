#include <stdlib.h>
#include <string>
#include <stdexcept>
#include "FString.h"
#include <general/platform.h>
#include "FStringExt.h"
std::string EXPORT STDCALL asString(const FString& st)
   {
   return std::string(st.f_str(), st.length());
   }

std::string EXPORT STDCALL asString(const char *fstring, unsigned int stringlength)
   {
   unsigned int realLen;
   for (realLen = stringlength; 
        realLen > 0 && fstring[realLen-1] == ' '; 
        realLen--);

   return std::string(fstring, realLen);
   }
