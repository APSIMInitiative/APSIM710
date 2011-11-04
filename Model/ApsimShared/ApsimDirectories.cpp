#include <../General/pch.h>
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
   #include <shlobj.h>
#else
   // gnu libc specific version
   #include <dlfcn.h>
   extern "C" void EXPORT dummyFnPtr(void) {;};
#endif

std::string EXPORT getExecutableFileName(void)
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

std::string EXPORT getExecutableDirectory(void)
   {
   // ------------------------------------------------------------------
   // This routine returns the directory where the
   // current executable is located.
   // ------------------------------------------------------------------
   return fileDirName(getExecutableFileName());
   }

std::string EXPORT getApsimDirectory(void)
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

static std::string AusFarmDir;

std::string EXPORT getAusFarmDirectory(void)
   {
      if (AusFarmDir == "")
	  {
#ifdef __WIN32__
		HKEY hKey = 0;
		DWORD dataType;
		DWORD dataSize = MAX_PATH;
		char data[MAX_PATH];
		RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\CSIRO\\AusFarm\\ModLibs\\CSIRO\\Output", 0, KEY_READ, &hKey);
		if (hKey && RegQueryValueEx(hKey, "Path", 0, &dataType, (LPBYTE)data, &dataSize) == ERROR_SUCCESS)
		{
			*(strrchr(data, '\\')) = '\0'; //strip \Output.exe off path.
			AusFarmDir = data;
		}
        if (AusFarmDir == "")
		{
          if (SHGetSpecialFolderPath(0, (LPSTR)data, CSIDL_PROGRAM_FILESX86, false) ||  // should work for 64-bit Windows
             (SHGetSpecialFolderPath(0, (LPSTR)data, CSIDL_PROGRAM_FILES, false)))
			AusFarmDir = std::string(data) + "\\AusFarm";
        }
#endif
	  }
      return AusFarmDir;
   }

