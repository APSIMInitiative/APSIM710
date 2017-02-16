
#include <General/dll.h>
#include "ScienceAPI2.h"

class CMPComponentInterface;
typedef  uintptr_t (STDCALL createComponentType)(ScienceAPI2* scienceAPI);
typedef  void (STDCALL deleteComponentType)(uintptr_t component);

uintptr_t CreateComponent(ScienceAPI2* scienceAPI, CMPComponentInterface*, const char* dllFileName, void* dllHandle)
   {
   createComponentType *createComponent = (createComponentType *) dllProcAddress(dllHandle, "createComponent");
   if (createComponent == NULL) throw std::runtime_error("Missing createComponent() in " + std::string(dllFileName));
   return (uintptr_t) createComponent(scienceAPI);
   }
void DeleteComponent(uintptr_t component, void* dllHandle)
   {
   deleteComponentType *deleteComponent = (deleteComponentType *) dllProcAddress(dllHandle, "deleteComponent");
   if (deleteComponent == NULL) throw std::runtime_error("Missing deleteComponent()");
   deleteComponent(component);
   }
