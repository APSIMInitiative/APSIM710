#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <General/platform.h>

struct Instance
   {
   const char* id;
   unsigned int idSize;
   const char* g;
   unsigned int gSize;
   const char* p;
   unsigned int pSize;
   const char* c;
   unsigned int cSize;
   unsigned int dummy1;
   unsigned int dummy2;
   unsigned int dummy3;
   unsigned int dummy4;
   unsigned int dummy5;
   unsigned int dummy6;
   unsigned int dummy7;
   unsigned int dummy8;
   unsigned int dummy9;
   unsigned int dummy10;
   };

// Statically linked to fortran code:
extern Instance instancepointers_;

// Simple stub to be linked with fortran dlls. This will cause
// Computation::loadComponent(computation.cpp) to firstly load
// the fortran wrapper, followed by the fortran after.
extern "C" void EXPORT STDCALL wrapperDLL(char* wrapperDll)
   {
#ifdef __WIN32__
   strcpy(wrapperDll, "ComponentInterface.dll");
#else
   strcpy(wrapperDll, "");
#endif
   }

// Accessor to instance pointers
extern "C" void EXPORT STDCALL getInstance(Instance ** p)
   {
   *p = &instancepointers_;
   }


extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);
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
