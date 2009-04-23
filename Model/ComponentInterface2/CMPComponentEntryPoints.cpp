#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <general/platform.h>


extern "C" void EXPORT STDCALL wrapperDLL(char* wrapperDll)
   {
   #ifdef __WIN32__
   strcpy(wrapperDll, "ComponentInterface2.dll");
   #else
   strcpy(wrapperDll, "");
   #endif
   }

// ------------------------------------------------------------------
// The following forward declarations are all in entrypoints.cpp
// in the componentinterface.dll
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript, char* description);
extern "C" void EXPORT STDCALL getDescriptionLengthInternal(char* initScript, char* description, int* length);


// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }

extern "C" void EXPORT STDCALL getDescriptionLength(char* initScript, int* length)
//=======================================================================================
// Return component description info.
   {
   char* buffer = new char[500000];
   getDescriptionInternal(initScript, buffer);
   *length = strlen(buffer);
   delete [] buffer;
   }
