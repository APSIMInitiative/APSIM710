#include <windows.h>
#include <general/platform.h>

extern "C" void EXPORT STDCALL wrapperDLL(char* wrapperDll)
   {
#ifdef __WIN32__
   strcpy(wrapperDll, "tcllink\\bin\\TclComponent.dll");
#else
   strcpy(wrapperDll, "");
#endif
   }


// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescription(char* initScript, char* description)
   {
   initScript[0] = '\0';
   }

extern "C" void EXPORT STDCALL getDescriptionLength(char* initScript, int* length)
//=======================================================================================
// Return component description info.
   {
   *length = 0;
   }
