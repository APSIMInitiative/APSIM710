#include <stdexcept>
#include <string>

#include <General/platform.h>
#include <General/dll.h>
#include <ComponentInterface2/ScienceAPIImpl.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <map>
#include "FortranComponentWrapper.h"
#include <ComponentInterface2/Messages.h>

using namespace std;

unsigned CreateComponent(ScienceAPI* scienceAPI, CMPComponentInterface* componentInterface,
                         const char* dllFileName, void* dllHandle)
   {
   return (unsigned) new FortranComponentWrapper(scienceAPI, componentInterface, dllHandle);
   }
void DeleteComponent(unsigned component, void* dllHandle)
   {
   delete (FortranComponentWrapper*) component;
   }
struct Bit
   {
   CMPComponentInterface* componentInterface;
   ScienceAPIImpl* scienceAPI;
   void* dllHandle;
   unsigned component;

   ~Bit()
      {
      delete componentInterface;
      delete scienceAPI;

      DeleteComponent(component, dllHandle);

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
    unsigned int* instanceNumber,
    unsigned int* callbackArg,
    CallbackType* callback)
   {
   Bit* bit = new Bit;

   // create a component interface and a science api that the component
   // will talk to.
   bit->componentInterface = new CMPComponentInterface(callbackArg, callback, *componentID, *parentID, dllFileName);
   bit->scienceAPI = new ScienceAPIImpl(*bit->componentInterface);

   // go create an instance of our component by loading the correct dll
   // and calling a createComponent entry point.
   bit->dllHandle = loadDLL(dllFileName);
   bit->component = CreateComponent(bit->scienceAPI, bit->componentInterface,
                                    dllFileName, bit->dllHandle);

   // The instance number we return to the PM is a pointer to the component
   // object we just created.
   *instanceNumber = (unsigned) bit;
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL deleteInstance (unsigned* instanceNumber)
   {
   Bit* bit = (Bit*) *instanceNumber;
   delete bit;
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL messageToLogic (unsigned* instanceNumber,
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
extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript,
                                                      char* description)
   {
   XMLDocument* Doc = new XMLDocument(initScript, XMLDocument::xmlContents);
   std::string dllFileName = Doc->documentElement().getAttribute("executable");
   std::string instanceName = Doc->documentElement().getAttribute("name");

   // create an instance of the module.
   unsigned dummy = 0;
   unsigned instanceNumber = 0;
   createInstance(dllFileName.c_str(), &dummy, &dummy, &instanceNumber, &dummy, NULL);

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
extern "C" void EXPORT STDCALL getDescriptionLengthInternal(char* initScript,
                                                            int* length)
   {
   char* buffer = new char[500000];
   getDescriptionInternal(initScript, buffer);
   *length = strlen(buffer);
   delete [] buffer;
   }

