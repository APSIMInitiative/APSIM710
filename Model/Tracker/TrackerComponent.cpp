#include <General/platform.h>
#include <ComponentInterface/Component.h>
#include "TrackerVariable.h"
#include "TrackerComponent.h"

using namespace std;
// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }

extern "C" void STDCALL getDescriptionLengthInternal(char* initScript,
                                                 int* length);
// ------------------------------------------------------------------
// Return component description length.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescriptionLength(char* initScript, int* length)
   {
   getDescriptionLengthInternal(initScript, length);
   }
// ------------------------------------------------------------------
// Create an instance of the TRACKER module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new TrackerComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
TrackerComponent::TrackerComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
TrackerComponent::~TrackerComponent(void)
   {
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void TrackerComponent::doInit2(void)
   {
   vector<string> vars;
   componentData->getVariables(vars);

   // only keep the values of variables that have a name of trackervariable
   writeString("Tracker variables:");
   for (unsigned i = 0; i != vars.size(); i++)
      {
      try
         {
         string msg = "   " + vars[i];
         writeString(msg.c_str());
         variables.push_back(TrackerVariable(this, vars[i]));
         }
      catch (const exception& err)
         {
         error(err.what(), true);
         }
      }

   for (unsigned i = 0; i != variables.size(); i++)
      variables[i].doRegistrations();
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void TrackerComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   for (unsigned i = 0; i != variables.size(); i++)
      variables[i].respondToEvent((int)fromID, eventID);
   }
// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void TrackerComponent::respondToGet(unsigned int& fromID,
                                     protocol::QueryValueData& queryData)
   {
   for (unsigned i = 0; i != variables.size(); i++)
      variables[i].respondToGet(fromID, queryData);
   }

