#ifdef __WIN32__
 #include <windows.h>
#endif

#include <stdexcept>
#include <string>

#include <ComponentInterface/Component.h>
#include <General/platform.h>

namespace protocol {
// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL createInstance
   (const char* dllFileName,
    const unsigned int* compID,
    const unsigned int* parentID,
    unsigned int* instanceNumber,
    const unsigned int* callbackArg,
    protocol::CallbackType* callback)
   {
   protocol::Component* component;
   component = ::createComponent();
   component->setup(dllFileName, *compID, *parentID, callbackArg, callback);
   *instanceNumber = (unsigned) component;
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL deleteInstance (unsigned* instanceNumber)
   {
   delete (protocol::Component*) *instanceNumber;
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   ((protocol::Component*) *instanceNumber)->messageToLogic(message);
   *processed = true; // ???? not sure why we need this.
   }

// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript,
                                                         char* description)
   {
   ApsimComponentData componentData(initScript);
   std::string dllFileName = componentData.getExecutableFileName();
   std::string instanceName = componentData.getName();

   // create an instance of the module.
   unsigned dummy = 0;
   unsigned instanceNumber = 0;
   createInstance(dllFileName.c_str(), &dummy, &dummy, &instanceNumber, &dummy, NULL);

   // call init1.
   Message* init1Message = newInit1Message(0, 0, initScript, instanceName.c_str(), true);
   bool processed;
   messageToLogic(&instanceNumber, init1Message, &processed);

   std::string compDescription = ((protocol::Component*) instanceNumber)->getDescription();
   strcpy(description, compDescription.c_str());

   // delete the instance.
   deleteInstance(&instanceNumber);

   ApsimRegistry::getApsimRegistry().reset();
   }

extern "C" void EXPORT STDCALL getDescriptionLengthInternal(char* initScript, int* length)
//=======================================================================================
// Return component description info.
   {
   char* buffer = new char[500000];
   getDescriptionInternal(initScript, buffer);
   *length = strlen(buffer);
   delete [] buffer;
   }

} // namespace protocol
