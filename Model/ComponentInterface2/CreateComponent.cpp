
#include <General/dll.h>
#include "ScienceAPI.h"

class CMPComponentInterface;
typedef  unsigned (STDCALL createComponentType)(ScienceAPI* scienceAPI);
typedef  void (STDCALL deleteComponentType)(unsigned component);

unsigned CreateComponent(ScienceAPI* scienceAPI, CMPComponentInterface*, const char* dllFileName, void* dllHandle)
   {
   createComponentType *createComponent = (createComponentType *) dllProcAddress(dllHandle, "createComponent");
   return (unsigned) createComponent(scienceAPI);
   }
void DeleteComponent(unsigned component, void* dllHandle)
   {
   deleteComponentType *deleteComponent = (deleteComponentType *) dllProcAddress(dllHandle, "deleteComponent");
   deleteComponent(component);
   }
