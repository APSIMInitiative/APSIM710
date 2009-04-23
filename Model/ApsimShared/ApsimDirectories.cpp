//---------------------------------------------------------------------------

#include <string>
#include <stdexcept>
#include <General/platform.h>
#include <General/string_functions.h>
#include <General/path.h>
#include "ApsimDirectories.h"

using namespace std;

#ifdef __WIN32__
   #include <windows.h>
   extern HINSTANCE hInstance;
#else
   // gnu libc specific version
   #include <dlfcn.h>
   extern "C" void EXPORT dummyFnPtr(void) {;};
#endif

string EXPORT getExecutableFileName()
   {
   // ------------------------------------------------------------------
   // This routine returns the filename with path to the
   // current executable.
   // ------------------------------------------------------------------
   #ifdef __WIN32__
      char moduleFileName[MAX_PATH];
      GetModuleFileName(hInstance, moduleFileName, sizeof moduleFileName);
      return &moduleFileName[0];
   #else
      Dl_info dlinfo;
      if (dladdr((void *)dummyFnPtr, &dlinfo) != 0)
         return dlinfo.dli_fname;
      else
         return "";
   #endif
   }

string EXPORT getApsimDirectory(void)
   {
// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------
   string dll = getExecutableFileName();
   dll = fileDirName(dll);
   return fileDirName(dll);
   }



