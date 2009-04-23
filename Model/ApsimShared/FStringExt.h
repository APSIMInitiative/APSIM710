//---------------------------------------------------------------------------
#ifndef FStringExtH
#define FStringExtH

#include <ApsimShared/FString.h>
#include <general/platform.h>
std::string EXPORT STDCALL asString(const FString& st);
std::string EXPORT STDCALL asString(const char *fstring, unsigned int stringlength);

#endif
