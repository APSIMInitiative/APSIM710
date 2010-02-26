#ifdef __WIN32__
   #include <windows.h>
   #include <direct.h>
#else
   #include <dlfcn.h>
#endif


#include <list>
#include <functional>
#include <General/platform.h>
#include <General/dll.h>
#include <General/path.h>
#include <ApsimShared/ApsimDirectories.h>
#include "Computation.h"
#include "Transport.h"

using namespace std;
using namespace protocol;

typedef EXPORT STDCALL void (wrapperDll_t) (char* dllFileName);

// ------------------------------------------------------------------
//  Short description:
//     Callback routine that all components call when sending a message.

//  Notes:

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
void EXPORT STDCALL messageCallback(const unsigned int* dummy, Message* message)
   {
   Transport::getTransport().deliverMessage(message);
   }

CallbackType* callback = &messageCallback;

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Computation::Computation(const string& name,
                         const string& fileName,
                         const string& componentInterfaceExecutable,
                         unsigned int componentId,
                         unsigned int parentId) throw (runtime_error)
   {
   // need to give the component to the transport layer.  Need a better
   // way of doing this.
   Transport::getTransport().addComponent(componentId, name, this);

   if (loadComponent(fileName, componentInterfaceExecutable)) {
     createInstance(fileName, componentId, parentId);
   }
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Computation::~Computation(void)
   {
   if (isOk())
      deleteInstance();
   unloadComponent();
   }

// ------------------------------------------------------------------
//  Short description:
//    call the CREATE entry point.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::createInstance(const std::string& filename,
                                 unsigned int componentId,
                                 unsigned int parentId)
   {
   static int dummy = 0;
   (*createInstanceProc) (filename.c_str(),
                          &componentId,
                          &parentId,
                          &instanceNo,
                          &dummy,
                          callback);
   }

// ------------------------------------------------------------------
//  Short description:
//    call the TERMINATE entry point

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::deleteInstance(void) const
   {
   try
      {
      (*deleteInstanceProc) (&instanceNo);
      }
   catch (...)
      {
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Load the DLL and find pointers to all the entry points.
//    An exception is thrown if the dll cannot be loaded.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool Computation::loadComponent(const std::string& filename,
                                std::string componentInterfaceExecutable) throw (std::runtime_error)
   {
   executableFileName = filename;

   createInstanceProc = NULL;
   deleteInstanceProc = NULL;
   messageToLogicProc = NULL;
   string componentInterface;
   if (componentInterfaceExecutable != "")
      {
      componentInterface = componentInterfaceExecutable;
      handle = loadDLL(componentInterface.c_str());
      }
   else
      {
       handle = loadDLL(executableFileName);

       wrapperDll_t *wrapperDll;
       wrapperDll = (wrapperDll_t *) dllProcAddress(handle, "wrapperDLL");
       if (wrapperDll == NULL)
          throw std::runtime_error("Cannot find entry point 'wrapperDll' in dll: " + executableFileName);

       // Go get the wrapperDll filename.
       char wrapperFileName[1024];
       (*wrapperDll)(&wrapperFileName[0]);
       componentInterface = wrapperFileName;

       if (componentInterface != "")
          {
          // This is a wrapped dll - it has no "entry points". Load the wrapper instead.
          closeDLL(handle);

#ifdef __WIN32__
          if (Str_i_Eq(fileTail(componentInterface), "piwrapper.dll"))
             {
             // AUSFarm dlls are treated differently - need to be loaded with the ausfarm 
             // infrastructure in the same working dir
             char oldwd[MAX_PATH];
             getcwd(oldwd, MAX_PATH);
             chdir(fileDirName(executableFileName).c_str());
             handle = LoadLibrary(componentInterface.c_str());
             if (handle == NULL)
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
               string errorMessage = ("Cannot load DLL: " + componentInterface + ".\n  " + (LPTSTR) lpMsgBuf);
               LocalFree( lpMsgBuf );
               throw runtime_error(errorMessage);
               }
             chdir(oldwd);
             }
          else 
             {
             componentInterface = getExecutableDirectory() + "/" + componentInterface;
             handle = loadDLL(componentInterface.c_str());
             }
#else
          componentInterface = getExecutableDirectory() + "/" + componentInterface;
          handle = loadDLL(componentInterface.c_str());
#endif
          }
       else
          {
          // This is not a wrapped dll - it will provide entrypoints itself
          }
      }

#ifdef __WIN32__
 #ifdef _MSC_VER
   createInstanceProc = (void (*)(const char*,
                                  const unsigned int*,
                                  const unsigned int*,
                                  const int*,
                                  const int*,
                                  void(*)(const unsigned int*, protocol::Message*))) GetProcAddress((HMODULE)handle, "createInstance");
   deleteInstanceProc = (void (*)(const int*)) GetProcAddress((HMODULE)handle, "deleteInstance");
   messageToLogicProc = (void (*)(const int*, const protocol::Message*, bool*)) GetProcAddress((HMODULE)handle, "messageToLogic");
 #else
   (FARPROC) createInstanceProc = GetProcAddress(handle, "createInstance");
   (FARPROC) deleteInstanceProc = GetProcAddress(handle, "deleteInstance");
   (FARPROC) messageToLogicProc = GetProcAddress(handle, "messageToLogic");
 #endif
#else
   createInstanceProc = (void (*)(const char*,
                                  const unsigned int*,
                                  const unsigned int*,
                                  const int*,
                                  const int*,
                                  void(*)(const unsigned int*, protocol::Message*)))
              dlsym(handle, "createInstance");
   deleteInstanceProc = (void (*)(const int*))dlsym(handle, "deleteInstance");
   messageToLogicProc = (void (*)(const int*, const protocol::Message*, bool*))dlsym(handle, "messageToLogic");
#endif

   if (createInstanceProc == NULL ||
       deleteInstanceProc == NULL ||
       messageToLogicProc == NULL)
      {
      string msg = "Not a valid APSIM DLL.  Missing 1 or more entry points.  DLL="
            + filename + ", wrapper=" + componentInterface;
      throw runtime_error(msg);
      }

   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Unload the specified dll.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::unloadComponent(void)
   {
   if (handle) 
     closeDLL(handle);
   }
