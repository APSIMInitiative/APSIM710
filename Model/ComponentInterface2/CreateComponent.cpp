
#include <General/dll.h>
#include "ScienceAPI2.h"

class CMPComponentInterface;
typedef  unsigned (STDCALL createComponentType)(ScienceAPI2* scienceAPI);
typedef  void (STDCALL deleteComponentType)(unsigned component);

unsigned CreateComponent(ScienceAPI2* scienceAPI, CMPComponentInterface*, const char* dllFileName, void* dllHandle)
   {
   createComponentType *createComponent = (createComponentType *) dllProcAddress(dllHandle, "createComponent");
   if (createComponent == NULL) throw std::runtime_error("Missing createComponent() in " + std::string(dllFileName));
   return (unsigned) createComponent(scienceAPI);
   }
void DeleteComponent(unsigned component, void* dllHandle)
   {
   deleteComponentType *deleteComponent = (deleteComponentType *) dllProcAddress(dllHandle, "deleteComponent");
   if (deleteComponent == NULL) throw std::runtime_error("Missing deleteComponent()");
   deleteComponent(component);
   }
