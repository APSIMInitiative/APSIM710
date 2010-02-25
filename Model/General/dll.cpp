//---------------------------------------------------------------------------
#include <stdexcept>

#ifdef __WIN32__
   #include <windows.h>
   #include <direct.h>
   
#else
   #include <dlfcn.h>
   #ifndef RTLD_LOCAL
    #define RTLD_LOCAL 0
   #endif 
#endif
#include <General/path.h>

#include "dll.h"

using namespace std;
void *loadDLL(const string& filename) throw (std::runtime_error)
   {
   // ------------------------------------------------------------------
   // Loads a dll into memory and returns a handle to it.
   // ------------------------------------------------------------------
   void* result;
   #ifdef __WIN32__
      char oldwd[4096];
      GetCurrentDirectory(sizeof(oldwd), oldwd);

      std::string newwd = fileDirName(filename);
      SetCurrentDirectory(newwd.c_str());

      result = LoadLibrary(filename.c_str());
      SetCurrentDirectory(oldwd);

      if (result == NULL )
         {
         // Get windows error message.
         LPVOID lpMsgBuf;
         FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                       NULL,
                       GetLastError(),
                       MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                       (LPTSTR) &lpMsgBuf,
                       0,
                       NULL
                       );


         ::string errorMessage = ("Cannot load DLL: " + filename + ".\n  " + (LPTSTR) lpMsgBuf);
         LocalFree( lpMsgBuf );
         throw std::runtime_error(errorMessage);
         }
   #else
      result = dlopen(filename.c_str(), RTLD_NOW|RTLD_LOCAL);
      char *dllerr = dlerror();
      if (dllerr != NULL)
	     {
             std::string errorMessage = string("Cannot load DLL: ") + filename + ".\n" + dllerr;
             throw std::runtime_error(errorMessage);
	     }
   #endif
   return result;
   }

void closeDLL(void* handle)
   {
   // ------------------------------------------------------------------
   // Close the specified dll handle.
   // ------------------------------------------------------------------
   #ifdef __WIN32__
      FreeLibrary((HINSTANCE)handle);
   #else
      //dlclose(handle);
   #endif
   }

void* dllProcAddress(void* dllHandle, const char* name)
   {
   // ------------------------------------------------------------------
   // Return the address of a function in the dll specified by dllHandle.
   // ------------------------------------------------------------------
   #ifdef __WIN32__
      return GetProcAddress((HINSTANCE)dllHandle, name);
   #else
      return dlsym(dllHandle, name);
   #endif
   }

