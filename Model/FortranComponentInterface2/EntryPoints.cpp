#include <stdexcept>
#include <string>

#include <General/platform.h>
#include <General/dll.h>
#include <ComponentInterface2/ScienceAPI2Impl.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <map>
#include "FortranComponentWrapper.h"
#include <ComponentInterface2/Messages.h>

using namespace std;

uintptr_t CreateFortranComponent(ScienceAPI2* scienceAPI, CMPComponentInterface* componentInterface,
                         const char* dllFileName, void* dllHandle)
   {
   return (uintptr_t) new FortranComponentWrapper(scienceAPI, componentInterface, dllHandle);
   }
void DeleteFortranComponent(uintptr_t component, void* dllHandle)
   {
   delete (FortranComponentWrapper*) component;
   }
struct Bit
   {
   CMPComponentInterface* componentInterface;
   ScienceAPI2Impl* scienceAPI;
   void* dllHandle;
   uintptr_t component;

   ~Bit()
      {
      delete componentInterface;
      delete scienceAPI;

      DeleteFortranComponent(component, dllHandle);

      closeDLL(dllHandle);
      }
   };


// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL createInstance
   (const char* dllFileName,
    unsigned int* componentID,
    unsigned int* parentID,
    uintptr_t* instanceNumber,
    uintptr_t* callbackArg,
    CallbackType* callback)
   {
   Bit* bit = new Bit;

   // create a component interface and a science api that the component
   // will talk to.
   bit->componentInterface = new CMPComponentInterface(callbackArg, callback, *componentID, *parentID, dllFileName);
   bit->scienceAPI = new ScienceAPI2Impl(*bit->componentInterface);

   // go create an instance of our component by loading the correct dll
   // and calling a createComponent entry point.
   bit->dllHandle = loadDLL(dllFileName);
   bit->component = CreateFortranComponent(bit->scienceAPI, bit->componentInterface,
                                    dllFileName, bit->dllHandle);

   // The instance number we return to the PM is a pointer to the component
   // object we just created.
   *instanceNumber = (uintptr_t) bit;
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL deleteInstance (uintptr_t* instanceNumber)
   {
   Bit* bit = (Bit*) *instanceNumber;
   delete bit;
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL messageToLogic (uintptr_t* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   Bit* bit = (Bit*) *instanceNumber;
   bit->componentInterface->messageToLogic(*message);
   *processed = true; // ???? not sure why we need this.
   }

// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL FCI2_getDescriptionInternal(char* initScript,
                                                      char* description)
   {
   XMLDocument* Doc = new XMLDocument(initScript, XMLDocument::xmlContents);
   std::string dllFileName = Doc->documentElement().getAttribute("executable");
   std::string instanceName = Doc->documentElement().getAttribute("name");

   // create an instance of the module.
   unsigned dummy = 0;
   uintptr_t instanceNumber = 0;
   uintptr_t dummy2 = 0;
   createInstance(dllFileName.c_str(), &dummy, &dummy, &instanceNumber, &dummy2, NULL);

   Init1Type init1;
   init1.sdml = initScript;
   init1.fqn = instanceName;
   init1.inStartup = true;

   // call init1.
   Message& init1Message = newMessage(Message::Init1, 0, 0, false, init1);
   bool processed;
   messageToLogic(&instanceNumber, &init1Message, &processed);

   Bit* bit = (Bit*) instanceNumber;

   std::string compDescription =  bit->componentInterface->getDescription(dllFileName);
   strcpy(description, compDescription.c_str());

   // delete the instance.
   deleteInstance(&instanceNumber);
   }
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL FCI2_getDescriptionLengthInternal(char* initScript,
                                                            int* length)
   {
   char* buffer = new char[500000];
   FCI2_getDescriptionInternal(initScript, buffer);
   *length = strlen(buffer);
   delete [] buffer;
   }

