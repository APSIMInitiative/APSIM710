//---------------------------------------------------------------------------
#include <General\pch.h>
#pragma hdrstop

#include <General\string_functions.h>
#include <General\stristr.h>
#include <General\Path.h>

#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimDirectories.h>

#include <ComponentInterface\Component.h>
#include "VensimComponent.h"

#pragma package(smart_init)
using namespace std;
using namespace protocol;

static const char* MES_Prepare = "prepare";
static const char* MES_Process = "process";

/* variable types  for vensim_get_varnames */
#define VARTYPE_CONSTANT 5
#define VARTYPE_AUXILIARY 2
#define VARTYPE_GAME 12
#define VARTYPE_ALL 0
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
// Create an instance of the VENLINK module
// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new VensimComponent;
   }
// ------------------------------------------------------------------
// Initialises the Vensim component.
// ------------------------------------------------------------------
VensimComponent::VensimComponent()
   {
   vensimLibrary = NULL;
   }

// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
VensimComponent::~VensimComponent(void)
   {
   vensim_command("GAME>ENDGAME");
   if (vensimLibrary) FreeLibrary(vensimLibrary);
   }

// ------------------------------------------------------------------
// Stage 1 initialisation.
// ------------------------------------------------------------------
void VensimComponent::doInit1(const protocol::Init1Data& initData)
   {
   Component::doInit1(initData);
   processID = addRegistration(::respondToEvent, 0, "prepare", "");   // XX Astounding!! XX
   }

// ------------------------------------------------------------------
// Find the VENSIM dll.
// ------------------------------------------------------------------
bool VensimComponent::findLibrary(void)
   {
   string dllName = componentData->getProperty("parameters", "dll_filename");
   if (dllName == "")
      {
      // APSIM installation puts one in in %apsuite/bin
      dllName = getApsimDirectory() + "/Model/VenDLM32.dll";
      }

   writeString(string("DLL filename: " + fileTail(dllName)).c_str());

   vensimLibrary = LoadLibrary(dllName.c_str());

   if (vensimLibrary == NULL)
      {
      string msg = "Cannot load VENSIM dll \"" + dllName + "\"";
      error(msg.c_str(), true);
      return false;
      }
   return true;
   }


// ------------------------------------------------------------------
// Initialise the VENSIM component.
// ------------------------------------------------------------------
void VensimComponent::doInit2(void)
   {
   Component::doInit2();

   // write copyright notice.
   writeString("Portions Copyright (c) 1987-1999 Ventana Systems, Inc.");

   if (findLibrary() == false) return;

   // get the model filename.
   fileName = componentData->getProperty("parameters", "model_filename");

   // Tell the vensim dll to load the simulation
   string command = "SPECIAL>LOADMODEL|" + fileName;
   if (vensim_command(command) == 0)
      {
      string msg = "Cannot load VENSIM model: " + fileName;
      error(msg.c_str(), true);
      }
   writeString(string("Model filename: " + fileName).c_str());

   // Find the variables in this file. (These functions are 'disabled' in most dlls.)
   char buf[8192];
   if (vensim_get_varnames("*", VARTYPE_CONSTANT, buf, 8192) > 0) {
     writeString("Constants:");
     for (char *ptr = buf; *ptr; ptr+=strlen(ptr)) {
        writeString(ptr);
     }
   }

   if (vensim_get_varnames("*", VARTYPE_GAME, buf, 8192) > 0) {
      writeString("Gaming Variables:");
      for (char *ptr = buf; *ptr; ptr+=strlen(ptr)) {
         writeString(ptr);
      }
   }

   // register all model variables.
   registerModelVariables();

   // Loop through all groups.  Read the model filename from the .parameters
   // group, the model inputs from the .inputs group and the model outputs
   // from the .outputs group.
   writeString("Vensim constants:");
   std::vector<string> names, values;
   componentData->getProperties("constants", names, values);
   for (unsigned g = 0; g != names.size(); g++)
      {
      if (!Str_i_Eq(names[g], "model_filename") && !Str_i_Eq(names[g], "variable"))
         {
         writeString(string("   " + names[g] + " = " + values[g]).c_str());

         // pass constant to VENSIM.
         Replace_all(names[g], "_", " ");
         string command = "SIMULATE>SETVAL|" + names[g] + " = " + values[g];
         if (vensim_command(command) == 0)
            {
            string msg = "Cannot set VENSIM variable value.  Variable=" + names[g];
            error(msg.c_str(), true);
            }
         }
      }

   vensim_command("MENU>GAME");
   if (vensim_command("GAME>GAMEINTERVAL|1") == 0)
      {
      string msg = "Cannot start VENSIM simulation: " + fileName;
      error(msg.c_str(), true);
      }
   }

   // ------------------------------------------------------------------
// Register all model variables.
// ------------------------------------------------------------------
void VensimComponent::registerModelVariables(void)
   {
   string floatDDML = "<type kind=\"single\"/>";

   writeString("VENSIM model variables:");
   std::vector<string> names;
   componentData->getVariables(names);
   for (unsigned variable = 0; variable != names.size(); variable++)
      {
      if (names[variable] != "")
         {
         string nameWithUnderscore = names[variable];
         Replace_all(nameWithUnderscore, " ", "_");
         unsigned id = addRegistration(::respondToGetSet,
                                       0,
                                       nameWithUnderscore,
                                       floatDDML);
         variables.insert(VariableMap::value_type(id, names[variable]));
         writeString(string("   " + names[variable]).c_str());
         }
      }
   }
// ------------------------------------------------------------------
// Look for a process message.
// ------------------------------------------------------------------
void VensimComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == processID)
      doTimestep();
   }
// ------------------------------------------------------------------
// Return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void VensimComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   if (variables.find(queryData.ID) != variables.end())
   {
     string name = variables[queryData.ID];
     float value = 0.0;
     if (vensim_get_val(name, &value) == 1)
        sendVariable(queryData, value);
   }
   else
      protocol::Component::respondToGet(fromID, queryData);
   }
// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool VensimComponent::respondToSet(unsigned int& fromID, QuerySetValueData& setValueData)
   {
   string name = variables[setValueData.ID];
   string stringValue;
   setValueData.variant.unpack(stringValue);
   float value = atof(stringValue.c_str());

   string command = "SIMULATE>SETVAL|" + name + " = " + ftoa(value, 5);
   if (vensim_command(command) == 0)
      return false;
   return true;
   }
// ------------------------------------------------------------------
// Perform the next iteration of the VENSIM model.
// ------------------------------------------------------------------
void VensimComponent::doTimestep(void)
   {
   // run VENSIM one more timestep.
   int result = vensim_command("GAME>GAMEON");
   if (result != 1)
      error("VENSIM has prematurely finished its simulation", true);
   }

//////////////////////////Wrappers//////////////////////////////////
// These routines are wrapppers that look up procedure addresses in
// whatever dll is loaded and calls them.

////extern "C" int __stdcall vensim_command(const char* command);
////extern "C" int __stdcall vensim_start_simulation(int, int, int);
////extern "C" int __stdcall vensim_get_val(const char* name, float* value);
////extern "C" int __stdcall vensim_continue_simulation(int numIterations);
////extern "C" int __stdcall vensim_finish_simulation(void);
////extern "C" int __stdcall vensim_check_status(void);
////extern "C" int __stdcall vensim_get_varnames(const char* filter,
////                                             int vartype,
////                                             char* buf,
////                                             int buflength);
////extern "C" unsigned long __stdcall vensim_get_varoff(const char* varname);
////extern "C" int __stdcall vensim_get_data(char *filename,
////                                         char *varname,
////                                         char *tname,
////                                         float *vval,
////                                         float *tval,
////                                         int maxn) ;

// ------------------------------------------------------------------
// Run a command in the vensim application.
// ------------------------------------------------------------------
int VensimComponent::vensim_command(string &command)
   {
   if (vensimLibrary != NULL)
      {
      typedef int  (STDCALL PF) (const char *);
      PF *pf = (PF*) GetProcAddress(vensimLibrary, "vensim_command");
      if (pf) return (*pf)(command.c_str());
      }
   return 0;
   }

int VensimComponent::vensim_command(const char *command)
   {
   if (vensimLibrary != NULL)
      {
      typedef int  (STDCALL PF) (const char *);
      PF *pf = (PF*) GetProcAddress(vensimLibrary, "vensim_command");
      if (pf) return (*pf)(command);
      }
   return 0;
   }

int VensimComponent::vensim_get_varnames(const char* filter,
                                         int vartype,
                                         char* buf,
                                         int buflength)
   {
   *buf = '\0';
   if (vensimLibrary != NULL)
      {
      typedef int  (STDCALL PF) (const char*, int, char*, int);
      PF *pf = (PF*) GetProcAddress(vensimLibrary, "vensim_get_varnames");
      if (pf) return (*pf)(filter, vartype, buf, buflength);
      }
   return 0;
   }

int VensimComponent::vensim_get_val(string &name, float* value)
   {
   if (vensimLibrary != NULL)
      {
      typedef int  (STDCALL PF) (const char *, float *);
      PF *pf = (PF*) GetProcAddress(vensimLibrary, "vensim_get_val");
      if (pf) return (*pf)(name.c_str(), value);
      }
   return 0;
   }

