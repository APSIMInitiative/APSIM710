
#include <General/dll.h>
#include "ScienceAPI.h"

class CMPComponentInterface;
unsigned CreateComponent(ScienceAPI* scienceAPI, CMPComponentInterface*, const char* dllFileName, void* dllHandle)
   {
   unsigned STDCALL (*createComponent)(ScienceAPI* scienceAPI);
   createComponent = (unsigned STDCALL(*)(ScienceAPI* scienceAPI)) dllProcAddress(dllHandle, "createComponent");
   return (unsigned) createComponent(scienceAPI);
   }
void DeleteComponent(unsigned component, void* dllHandle)
   {
   void STDCALL (*deleteComponent)(unsigned component);
   deleteComponent = (void STDCALL(*)(unsigned component)) dllProcAddress(dllHandle, "deleteComponent");
   deleteComponent(component);
   }
