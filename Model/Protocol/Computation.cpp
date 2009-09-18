#ifdef __WIN32__
   #include <windows.h>
   #include <dir.h>
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

       void EXPORT STDCALL (*wrapperDll)(char* dllFileName);
#ifdef __WIN32__
       (FARPROC) wrapperDll = GetProcAddress(handle, "wrapperDLL");
#else
       wrapperDll = (void (*)(char *))dlsym(handle, "wrapperDLL");
#endif

       if (wrapperDll == NULL)
          throw std::runtime_error("Cannot find entry point 'wrapperDll' in dll: " + executableFileName);

       // Go get the wrapperDll filename.
       char wrapperFileName[1024];
       (*wrapperDll)(&wrapperFileName[0]);
       componentInterface = wrapperFileName;

       if (componentInterface != "")
          {
          // This is a wrapped dll - it has no "entry points". Load the wrapper.
          closeDLL(handle);

          componentInterface = getExecutableDirectory() + "/" + componentInterface;
#ifdef __WIN32__
          if (Str_i_Eq(fileTail(componentInterface), "piwrapper.dll"))
             {
             //chdir(fileDirName(executableFileName).c_str());?? Make sure this still works in 7.1!!!! FIXME
             }
#endif
          handle = loadDLL(componentInterface.c_str());
          }
       else
          {
          // This is not a wrapped dll - it will provide entrypoints itself
          }
      }

#ifdef __WIN32__
   (FARPROC) createInstanceProc = GetProcAddress(handle, "createInstance");
   (FARPROC) deleteInstanceProc = GetProcAddress(handle, "deleteInstance");
   (FARPROC) messageToLogicProc = GetProcAddress(handle, "messageToLogic");
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
