//---------------------------------------------------------------------------
// Wrapper dll for fortran routines.
// Keeps pointers to fortran entry points (Main(), do_init1() etc..
// and calls them when reqd.
#include <stdio.h>
#include <ComponentInterface/ScienceAPI.h>
#include <General/dll.h>
#include "FORTRANComponentWrapper.h"

static const char* nullType = "<type/>";
static const char* integerType = "<type kind=\"integer4\"/>";
static const char* integerArrayType = "<type kind=\"integer4\" array=\"T\"/>";
static const char* realType = "<type kind=\"single\"/>";
static const char* realArrayType = "<type kind=\"single\" array=\"T\"/>";
static const char* doubleType = "<type kind=\"double\"/>";
static const char* doubleArrayType = "<type kind=\"double\" array=\"T\"/>";
static const char* stringType = "<type kind=\"string\"/>";
static const char* stringArrayType = "<type kind=\"string\" array=\"T\"/>";
static const char* logicalType = "<type kind=\"boolean\"/>";

static const int ERR_internal = 1;
static const int ERR_user = 2;

FortranWrapper* FortranWrapper::currentInstance = NULL;
#ifdef __WIN32__
CRITICAL_SECTION swapMutex;
bool swapMutexInited = false;
#endif

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
FortranWrapper::FortranWrapper(void)
   : outgoingApsimVariant(this), incomingApsimVariant(this), queryData((unsigned)-1, (unsigned)-1)
   {
   vars = NULL;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
FortranWrapper::~FortranWrapper(void)
   {
   // get FORTRAN to release memory blocks.
   const unsigned int doAllocate = false;
   swapInstanceIn();
   alloc_dealloc_instance(&doAllocate);
   swapInstanceOut();

   if (libraryHandle)
      {
      closeDLL (libraryHandle);
      libraryHandle = NULL;
      }
   }

void FortranWrapper::setup(void)
   {
   setupFortranDll();

   // get FORTRAN to create new memory blocks.
   const unsigned int doAllocate = true;
   alloc_dealloc_instance(&doAllocate);
   // save pointers and contents of these memory blocks.
   setupInstancePointers();
   }
// ------------------------------------------------------------------
// Find entry points. Leave NULL if not found..
// ------------------------------------------------------------------
void FortranWrapper::setupFortranDll(void)
   {
   my_Main = NULL;
   my_alloc_dealloc_instance = NULL;
   my_respondToEvent = NULL;

   libraryHandle = loadDLL(this->getDllName().c_str());

   if (libraryHandle != NULL)
      {
      my_Main = (Main_t*) dllProcAddress(libraryHandle, "Main");
      my_alloc_dealloc_instance = (alloc_dealloc_instance_t*) dllProcAddress(libraryHandle, "alloc_dealloc_instance");
      my_do_init1 = (do_init1_t*) dllProcAddress(libraryHandle, "doInit1");
      my_respondToEvent = (respondToEvent_t*) dllProcAddress(libraryHandle, "respondToEvent");
      }
   }
// ------------------------------------------------------------------
// Find pointers to fortran common block, and keep a copy.
// ------------------------------------------------------------------
void FortranWrapper::setupInstancePointers(void)
   {
   instance = NULL;
   memset(&myInstance, 0, sizeof(Instance));

   if (libraryHandle != NULL)
      {

      getInstance_t *proc = (getInstance_t *) dllProcAddress(libraryHandle, "getInstance");

      if (proc != NULL)
         {
         instance = (*proc) ();
         myInstance = *instance;
         }
      else
         {
         throw std::runtime_error("Missing getInstance()");
         }
      }
   }

void FortranWrapper::Main(const char* action, const char *data)
   {
   if (my_Main) {(*my_Main) (action, data, strlen(action), strlen(data));}
   }
void FortranWrapper::Main(const char* action, FString &data)
   {
   if (my_Main) {(*my_Main) (action, data.f_str(), strlen(action), data.length());}
   }
void FortranWrapper::Main(FString &action, FString &data)
   {
   if (my_Main) {(*my_Main) (action.f_str(), data.f_str(), action.length(), data.length());}
   }
void FortranWrapper::Main(FString &action, const char *data)
   {
   if (my_Main) {(*my_Main) (action.f_str(), data, action.length(), strlen(data));}
   }

void FortranWrapper::alloc_dealloc_instance(const unsigned int* doAllocate)
   {
   if (my_alloc_dealloc_instance) {(*my_alloc_dealloc_instance) (doAllocate);}
   }

// ------------------------------------------------------------------
// do init1 stuff
// ------------------------------------------------------------------
void FortranWrapper::doInit1(const protocol::Init1Data& init1Data)
   {
   // Set up the dll pointers
   setup();

   protocol::Component::doInit1(init1Data);
   swapInstanceIn();

   if (my_do_init1)
      (*my_do_init1)();
   else
      Main("create", asString(init1Data.sdml).c_str());

   swapInstanceOut();
   }
// ------------------------------------------------------------------
// do init2 stuff
// ------------------------------------------------------------------
void FortranWrapper::doInit2(void)
   {
   swapInstanceIn();

   Main("init2", "");

   swapInstanceOut();
   }
// ------------------------------------------------------------------
// do commence stuff
// ------------------------------------------------------------------
void FortranWrapper::doCommence(void)
   {
   swapInstanceIn();

   Main("start", "");

   swapInstanceOut();
   }
// ------------------------------------------------------------------
// respond to a get request.
// ------------------------------------------------------------------
void FortranWrapper::respondToGet(unsigned int& fromID, protocol::QueryValueData& qData)
   {
   swapInstanceIn();

   string name = getRegistration(componentID, qData.ID)->getNameWithoutBrackets();
   inApsimGetQuery = false;
   queryData = qData;
   messageWasUsed = true;
   Main("get", name.c_str());
   swapInstanceOut();
   if (!messageWasUsed)
      protocol::Component::respondToGet(fromID, qData);
   }
// ------------------------------------------------------------------
// respond to a set request.
// ------------------------------------------------------------------
bool FortranWrapper::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& querySetData)
   {
   swapInstanceIn();

   string name = getRegistration(componentID, querySetData.ID)->getName();

   incomingVariant.aliasTo(querySetData.variant);
   inRespondToSet = true;
   messageWasUsed = true;

   Main("set", name.c_str());

   swapInstanceOut();

   return messageWasUsed;
   }
// ------------------------------------------------------------------
// respond to a notification of termination
// ------------------------------------------------------------------
void FortranWrapper::notifyTermination(void)
   {
   swapInstanceIn();

   Main("end_run", "");

   swapInstanceOut();
   }
// ------------------------------------------------------------------
// respond to an event.
// ------------------------------------------------------------------
void FortranWrapper::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& var)
   {
   swapInstanceIn();

   incomingApsimVariant.aliasTo(* var.getMessageData());
   inRespondToSet = false;
   messageWasUsed = true;
//   unsigned int sender = var.getFromId();

   string eventName = getRegistration(componentID, eventID)->getName();
   Main(eventName.c_str(), " ");
   if (!messageWasUsed)
      if (my_respondToEvent) {(*my_respondToEvent)(fromID, eventID, &var);}

   swapInstanceOut();
   }
// ------------------------------------------------------------------
// respond to an onApsimGetQuery message
// ------------------------------------------------------------------
void FortranWrapper::onApsimGetQuery(unsigned int fromID, protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   swapInstanceIn();

   inApsimGetQuery = true;

   string qualifiedName = asString(apsimGetQueryData.name);
   unsigned pos = qualifiedName.find("(");
   if (pos != string::npos)
      qualifiedName = qualifiedName.substr(0,pos);

   Main("get", qualifiedName.c_str());

   swapInstanceOut();
   }
// ------------------------------------------------------------------
// respond to an onApsimSetQuery message
// ------------------------------------------------------------------
bool FortranWrapper::onApsimSetQuery(protocol::ApsimSetQueryData& apsimSetQueryData)
   {
   swapInstanceIn();

   incomingVariant.aliasTo(apsimSetQueryData.variant);
   messageWasUsed = true;
   inRespondToSet = true;

   Main("set", apsimSetQueryData.name);

   swapInstanceOut();

   return messageWasUsed;
   }
// ------------------------------------------------------------------
// swap an instance in.
// ------------------------------------------------------------------
void FortranWrapper::swapInstanceIn(void)
{
#ifdef __WIN32__
   if (!swapMutexInited) {
      InitializeCriticalSectionAndSpinCount(&swapMutex, 0x80000400);
      swapMutexInited = true;
   }
   EnterCriticalSection(&swapMutex);
#endif
   instanceStack.push(*instance);
   *instance = myInstance;
   callStack.push(currentInstance);
   currentInstance = this;
}

void FortranWrapper::swapInstanceOut(void)
{
   *instance = instanceStack.top();
   instanceStack.pop();
   currentInstance = callStack.top();
   callStack.pop();
#ifdef __WIN32__
   LeaveCriticalSection(&swapMutex);
#endif
}

void FortranWrapper::event_send(int destID, const FString& eventName)
   {
   unsigned eventID = addRegistration(::event,
                                      destID,
                                      asString(eventName),
                                      DDML(outgoingApsimVariant));
   publish(eventID, outgoingApsimVariant);
   }


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// DLL Exports
// ------------------------------------------------------------------

// ------------------------------------------------------------------
//  Short description:
//    add a registration to the system.  Return it's registration ID
//    to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL add_registration
   (EventTypeCode* kind, const char* name, const char* type,
    const char* alias,
    unsigned nameLength, unsigned typeLength, unsigned aliasLength)
   {
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(
      FortranWrapper::currentInstance->getId(),
      asString(name,nameLength), destID, regName);

   return FortranWrapper::currentInstance->addRegistration(*kind,
                                                           destID,
                                                           regName,
                                                           asString(type, typeLength),
                                                           asString(alias, aliasLength));
   }

/*string addUnitsToDDML(const string& ddml, const string& units)
   {
   string returnString = ddml;
   unsigned pos = returnString.find("/>");
   if (pos != string::npos)
      returnString.insert(pos, " unit=\"" + units + "\"/>");
   return returnString;
   }
*/

// ------------------------------------------------------------------
//  Short description:
//    add a registration to the system.  Return it's registration ID
//    to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL add_registration_with_units
   (EventTypeCode* kind, const char* name, const char* type,
    const char* units,
    unsigned nameLength, unsigned typeLength, unsigned unitsLength)
   {
   string ddml = addUnitsToDDML(asString(type, typeLength),
                                asString(units, unitsLength));
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(
       FortranWrapper::currentInstance->getId(),
       asString(name,nameLength), destID, regName);

   return FortranWrapper::currentInstance->addRegistration
      (*kind, destID, regName, ddml);
   }
// ------------------------------------------------------------------
//  Short description:
//    add a registration to the system.  Return it's registration ID
//    to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL add_reg
   (EventTypeCode* kind, const char* name, const char* type,
    const char* units, const char* desc,
    unsigned nameLength, unsigned typeLength, unsigned unitsLength, unsigned descLength)


   {
   string ddml = addUnitsToDDML(asString(type, typeLength),
                                asString(units, unitsLength));
   ddml = addDescToDDML(ddml, asString(desc, descLength));
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(
       FortranWrapper::currentInstance->getId(),
       asString(name,nameLength), destID, regName);

   return FortranWrapper::currentInstance->addRegistration
      (*kind, destID, regName, ddml);
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to issue an error

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL Fatal(const char* msg, unsigned int msgLength)
   {
   FortranWrapper::currentInstance->error(FString(msg, msgLength, FORString), true);
   }
extern "C" void EXPORT STDCALL Warning(const char* msg, unsigned int msgLength)
   {
   FortranWrapper::currentInstance->error(FString(msg, msgLength, FORString), false);
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to terminate a simulation

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL terminate_simulation(void)
   {
   FortranWrapper::currentInstance->terminateSimulation();
   }

// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to write a string to the summary file

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL write_string(const char* msg, unsigned int msgLength)
   {
   FortranWrapper::currentInstance->writeString(asString(msg, msgLength));
   }


// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to write a string to the std err stream.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL writeStdErr(const char* msg, unsigned int msgLength)
   {
   FortranWrapper::currentInstance->writeStdErr(asString(msg, msgLength));
   }

// ------------------------------------------------------------------
//  Short description:
//   Send a message to system

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL send_message(protocol::Message* message)
   {
   FortranWrapper::currentInstance->send_message(message);
   }

// ------------------------------------------------------------------
//  Short description:
//    return the componentID to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL get_componentID(void)
   {
   return FortranWrapper::currentInstance->get_componentID();
   }

// ------------------------------------------------------------------
//  Short description:
//    return the parentID to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL get_parentID(void)
   {
   return FortranWrapper::currentInstance->get_parentID();
   }

// ------------------------------------------------------------------
//  Short description:
//    return the componentdata object to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL get_componentData(void)
   {
   return FortranWrapper::currentInstance->get_componentData();
   }

// ------------------------------------------------------------------
//  Short description:
//    return the component's name to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void  EXPORT STDCALL get_name(char* name, unsigned nameLength)
   {
   FortranWrapper::currentInstance->get_name(name, nameLength);
   }

extern "C" void  EXPORT STDCALL get_fq_name(char* name, unsigned nameLength)
   {
   FortranWrapper::currentInstance->get_fq_name(name, nameLength);
   }

// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to push a routine onto stack

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL push_routine(const char* routineName,
                                       unsigned int routineNameLength)
   {
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to pop a routine from the stack

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL pop_routine(const char* routineName,
                                       unsigned int routineNameLength)
   {

   }

// ------------------------------------------------------------------
//  Short description:
//    Perform a case insensitive string comparison and return true
//    if strings are equal.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" bool EXPORT STDCALL strings_equal(const char* st1, const char* st2,
                                        unsigned st1Length, unsigned st2Length)
   {
   return (FString(st1, st1Length, FORString) == FString(st2, st2Length, FORString));
   }


// ------------------------------------------------------------------
//  Short description:
//    return a type string for the specified registration ID.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL get_setVariableSuccess(void)
   {
   return FortranWrapper::currentInstance->get_set_variable_success();
   }
// ------------------------------------------------------------------
//  Short description:
//    display a setvariable error message.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_variable_error(unsigned int* regID)
   {
   FortranWrapper::currentInstance->set_variable_error(*regID);
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to indicate a message was not used.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL message_unused(void)
   {
   FortranWrapper::currentInstance->message_unused();
   }

// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_integer_var
   (const char* variableName, const char* units, const int* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), integerType, *value);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_integer_array
   (const char* variableName, const char* units, int* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<int> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), integerArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_real_var
   (const char* variableName, const char* units, float* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), realType, *value);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_real_array
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), realArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_double_var
   (const char* variableName, const char* units, double* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), doubleType, *value);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_double_array
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<double> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), doubleArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_logical_var
   (const char* variableName, const char* units, int* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   bool b = *value != 0;
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), logicalType, b);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_char_var
   (const char* variableName, const char* units, const char* value,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), stringType, FString(value, valueLength, FORString));
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_char_array
   (const char* variableName, const char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FStrings values(value, valueLength, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), stringArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_time_var
   (const char* variableName, const char* units, const protocol::TimeType* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   string timeDDML = DDML(protocol::TimeType());
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), timeDDML.c_str(), *value);
   }
// ------------------------------------------------------------------
// Bound check a real variable and produce an error if out of bound.
// ------------------------------------------------------------------
void boundCheckVar(double value, double lower, double upper,
                   const FString& variableName)
   {
   if (value < lower || value > upper)
      {
      char varName[100];
      strncpy(varName, variableName.f_str(), variableName.length());
      varName[variableName.length()] = 0;

      char msg[500];
      sprintf(msg, "Value of variable is out of bounds.\n"
                   "Variable: %s\n"
                   "Value: %16.7f\n"
                   "Bounds: %16.7f to %16.7f",
              varName, value, lower, upper);
      FortranWrapper::currentInstance->error(FString(msg), false);
      }
   }
// ------------------------------------------------------------------
// Bound check a real variable and produce an error if out of bound.
// ------------------------------------------------------------------
void boundCheckRealArray(float* values, unsigned numvals, float lower, float upper,
                         const FString& variableName)
   {
   for (unsigned i = 0; i != numvals; i++)
      {
      //string z = asString(variableName) + "(" + itoa(i+1) + ")";
      boundCheckVar(values[i], lower, upper, variableName);
      }
   }
// ------------------------------------------------------------------
// Bound check a double variable and produce an error if out of bound.
// ------------------------------------------------------------------
void boundCheckDoubleArray(double* values, unsigned numvals, double lower, double upper,
                           const FString& variableName)
   {
   for (unsigned i = 0; i != numvals; i++)
      {
      //string z = asString(variableName) + "(" + itoa(i+1) + ")";
	  boundCheckVar(values[i], lower, upper, variableName);
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_integer_var
   (int* componentID, const char* variableName, const char* units,
    int* value, unsigned* numvals, int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), integerType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_integer_vars
   (int* requestNo, const char* variableName, const char* units,
    int* value, unsigned* numvals, int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), integerType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }

// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_var
   (int* componentID, const char* variableName, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
	  boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_array
   (int* componentID, const char* variableName, unsigned* arraySize, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realArrayType, values, *numvals);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckRealArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_array_optional
   (int* componentID, const char* variableName, unsigned* arraySize, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realArrayType, values, *numvals, true);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckRealArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_arrays
   (int* requestNo, const char* variableName, unsigned* arraySize, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), realArrayType, values, *numvals);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckRealArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_var_optional
   (int* componentID, const char* variableName, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realType, *value, *numvals, true);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_vars
   (int* requestNo, const char* variableName, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), realType, *value, *numvals);
   if (*numvals == 0)
	  *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_var
   (int* componentID, const char* variableName, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_var_optional
   (int* componentID, const char* variableName, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleType, *value, *numvals, true);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_vars
   (int* requestNo, const char* variableName, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), doubleType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
	  boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_array
   (int* componentID, const char* variableName, unsigned* arraySize, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<double> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleArrayType, values, *numvals);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckDoubleArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_char_var
   (int* componentID, const char* variableName, const char* units,
    char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength,
    unsigned valueLength)
   {
   FString valueString;
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), stringType,
       valueString, *numvals);
   FString(value, valueLength, FORString) = valueString;
   if (*numvals == 0)
      memset((char*)value, ' ', valueLength);
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_char_var_optional
   (int* componentID, const char* variableName, const char* units,
    char* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString valueString(value, valueLength, FORString);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), stringType,
       valueString, *numvals, true);
   FString(value, valueLength, FORString) = valueString;
   if (*numvals == 0)
      memset((char*)value, ' ', valueLength);
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_char_vars
   (int* requestNo, const char* variableName, const char* units,
    char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString valueString;
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), stringType,
       valueString, *numvals);
   FString(value, valueLength, FORString) = valueString;
   if (*numvals == 0)
      memset((char*)value, ' ', valueLength);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_real_var(int* componentID, const char* variableName,
                                       const char* units, const float* value,
                                       unsigned variableNameLength,
                                       unsigned unitsLength )
   {
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), realType,
       *value);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_real_array(int* componentID, const char* variableName,
                                         const char* units, float* value, unsigned* numvals,
                                         unsigned variableNameLength,
                                         unsigned unitsLength)
   {
   protocol::vector<float> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), realArrayType, values);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_double_var(int* componentID, const char* variableName,
                                         const char* units, const double* value,
                                         unsigned variableNameLength,
                                         unsigned unitsLength )
   {
   FortranWrapper::currentInstance->set_var
	  (*componentID, FString(variableName, variableNameLength, FORString), doubleType,
       *value);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_double_array(int* componentID, const char* variableName,
                                           const char* units, double* value, unsigned* numvals,
                                           unsigned variableNameLength,
                                           unsigned unitsLength)
   {
   protocol::vector<double> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleArrayType, values);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_char_var(int* componentID, const char* variableName,
									   const char* units, const char* value,
									   unsigned variableNameLength,
									   unsigned unitsLength, unsigned valueLength)
   {
   FortranWrapper::currentInstance->set_var
	  (*componentID, FString(variableName, variableNameLength, FORString), stringType,
	   FString(value, valueLength, FORString));
   }

extern "C" void EXPORT STDCALL set_char_var_optional(
									   int* componentID,
									   const char* variableName,
									   const char* units,
									   int *num_set,
									   const char* value,
									   unsigned variableNameLength,
									   unsigned unitsLength, unsigned valueLength)
   {
   bool result = FortranWrapper::currentInstance->set_var_optional
	  (*componentID,
	   FString(variableName, variableNameLength, FORString),
	   stringType,
	   FString(value, valueLength, FORString));
   *num_set = result ? 1 : 0;
   }

// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_char_array(int* componentID, const char* variableName,
										 const char* units, char* value, unsigned* numvals,
										 unsigned variableNameLength, unsigned unitsLength,
										 unsigned valueLength)
   {
   FStrings values(value, valueLength, *numvals, *numvals);
   FortranWrapper::currentInstance->set_var
	  (*componentID, FString(variableName, variableNameLength, FORString), stringArrayType, values);
   }
// ------------------------------------------------------------------
// Module is requesting a new postbox.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL new_postbox( )
   {
   FortranWrapper::currentInstance->new_postbox();
   }
// ------------------------------------------------------------------
// Module is requesting to delete the postbox.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL delete_postbox( )
   {
   }
// ------------------------------------------------------------------
// Module wants to send an event.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL event_send(int *id, const char* eventName, unsigned eventNameLength)
   {
   FortranWrapper::currentInstance->event_send(*id, FString(eventName, eventNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is posting a string array into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_char_array
   (const char* variableName, const char* units, const char* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FStrings values(const_cast<char*>(value), valueLength, *numvals, *numvals);
   FortranWrapper::currentInstance->post_var
	  (FString(variableName, variableNameLength, FORString), protocol::DTstring, true,
	   values);
   }
// ------------------------------------------------------------------
// Module is posting a string into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_char_var
   (const char* variableName, const char* units, const char* value,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   unsigned numvals = 1;
   post_char_array(variableName, units, value, &numvals, variableNameLength, unitsLength, valueLength);
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_integer_var
   (const char* variableName, const char* units, int* value,
	unsigned variableNameLength, unsigned unitsLength)
   {
   char tmp[40];   //temp char string
   sprintf(tmp, "%d", *value);
   post_char_var(variableName, units, tmp,
	 variableNameLength, unitsLength, strlen(tmp));
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_real_var
   (const char* variableName, const char* units, float* value,
	unsigned variableNameLength, unsigned unitsLength)
   {
   char tmp[40];   //temp char string
   sprintf(tmp, "%.12g", *value);
   post_char_var(variableName, units, tmp,
	 variableNameLength, unitsLength, strlen(tmp));
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_real_array
   (const char* variableName, const char* units, float* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, *numvals, *numvals);
   char* temp = (char*)malloc(*numvals*24);
   memset(temp, ' ', *numvals*24);
   char tmp[40];
   for (unsigned int i=0; i < *numvals; i++) {
	 int nChars = sprintf(tmp, "%.12g", values[i]);
	 strncpy(temp + 24 * i, tmp, nChars);
   }
   post_char_array(variableName, units, temp, numvals,
	 variableNameLength, unitsLength, 24);
   free(temp);
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_double_var
   (const char* variableName, const char* units, double* value,
	unsigned variableNameLength, unsigned unitsLength)
   {
   char tmp[40];   //temp char string
   sprintf(tmp, "%.12g", *value);
   post_char_var(variableName, units, tmp,
	 variableNameLength, unitsLength, strlen(tmp));
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_double_array
   (const char* variableName, const char* units, double* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<double> values(value, *numvals, *numvals);
   char* temp = (char*)malloc(*numvals*24);
   memset(temp, ' ', *numvals*24);
   char tmp[40];
   for (unsigned int i=0; i < *numvals; i++) {
	 int nChars = sprintf(tmp, "%.12g", values[i]);
	 strncpy(temp + 24 * i, tmp, nChars);
   }
   post_char_array(variableName, units, temp, numvals,
	 variableNameLength, unitsLength, 24);
   free(temp);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_var
   (const char* variableName, const char* units, char* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString newValue;
   FortranWrapper::currentInstance->collect_var
	  (FString(variableName, variableNameLength, FORString), protocol::DTstring, false,
	   newValue, *numvals, false);
   FString(value, valueLength, FORString) = newValue;
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_var_optional
   (const char* variableName, const char* units, char* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString newValue;
   FortranWrapper::currentInstance->collect_var
	  (FString(variableName, variableNameLength, FORString), protocol::DTstring, false,
	   newValue, *numvals, true);
   FString(value, valueLength, FORString) = newValue;
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_array
   (const char* variableName, unsigned* arraySize, char* units, char* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FStrings values(value, valueLength, *arraySize, 0);
   FortranWrapper::currentInstance->collect_var
	  (name, protocol::DTstring, true, values, *numvals, false);
   *numvals = values.getNumElements();
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_array_optional
   (const char* variableName, unsigned* arraySize, char* units, char* value, unsigned* numvals,
	unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FStrings values(value, valueLength, *arraySize, 0);
   FortranWrapper::currentInstance->collect_var
	  (name, protocol::DTstring, true, values, *numvals, true);
   *numvals = values.getNumElements();
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_var
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, false, *value, *numvals, false);
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_var_optional
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, false, *value, *numvals, true);
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_array
   (const char* variableName, unsigned* arraySize, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, true, values, *numvals, false);
   *numvals = values.size();
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_array_optional
   (const char* variableName, unsigned* arraySize, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, true, values, *numvals, true);
   *numvals = values.size();
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_integer_var
   (const char* variableName, const char* units, int* value, unsigned* numvals,
    int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTint4, false, *value, *numvals, false);
   boundCheckVar(*value, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_integer_var_optional
   (const char* variableName, const char* units, int* value, unsigned* numvals,
    int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTint4, false, *value, *numvals, true);
   boundCheckVar(*value, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_double_var
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTdouble, false, *value, *numvals, false);
   boundCheckVar(*value, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_double_var_optional
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTdouble, false, *value, *numvals, true);
   boundCheckDoubleArray(value, *numvals, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_double_array
   (const char* variableName, unsigned* arraySize, const char* units, double* value, unsigned* numvals,
    double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   protocol::vector<double> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTdouble, true, values, *numvals, false);
   *numvals = values.size();
   boundCheckDoubleArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// remove leading spaces
// ------------------------------------------------------------------
void stripBlanks(FString& str)
   {
   unsigned firstNonBlank = 0;
   while (str[firstNonBlank] == ' ')
      firstNonBlank++;

   unsigned lastNonBlank = str.length()-1;
   while (str[lastNonBlank] == ' ')
      lastNonBlank--;
   str = str.substr(firstNonBlank, lastNonBlank-firstNonBlank+1);
   }

// ------------------------------------------------------------------
// Store the specified string into the 'postbox'
// ------------------------------------------------------------------
extern "C" bool EXPORT STDCALL store_in_postbox(const char* str, unsigned strLength)
   {
   FString line(str, strLength, FORString);

   unsigned posStart = 0;

   while (posStart < line.length())
      {
      unsigned posComma = line.find(",", posStart);
      if (posComma == FString::npos)
         posComma = line.length();

      unsigned posEquals = line.find("=", posStart);
      if (posEquals != FString::npos)
         {
         FString name = line.substr(posStart, posEquals-posStart);
         stripBlanks(name);

         FString value = line.substr(posEquals+1, posComma-posEquals-1);
         unsigned posOpenBracket = value.find("(");
         if (posOpenBracket != FString::npos)
            {
            unsigned posCloseBracket = value.find(")", posOpenBracket);
            if (posCloseBracket != FString::npos)
               value.erase(posOpenBracket, posCloseBracket-posOpenBracket+1);
            }
         stripBlanks(value);

         post_char_var(name.f_str(), " ", value.f_str(),
                       name.length(), 1, value.length());
         }
      else
         return false;
      posStart = posComma+1;
      }
   return true;
   }
// ------------------------------------------------------------------
// return the posting module to caller.
// ------------------------------------------------------------------
extern "C" int EXPORT STDCALL get_posting_module(void)
   {
   return FortranWrapper::currentInstance->getFromID();
   }
// ------------------------------------------------------------------
// return the posting module to caller.
// ------------------------------------------------------------------
extern "C" bool EXPORT STDCALL component_id_to_name(unsigned* id, char* name,
												   unsigned nameLength)
   {
   string nameString = asString(name, nameLength);
   bool ok = FortranWrapper::currentInstance->componentIDToName(*id, nameString);
   if (ok)
	  FString(name, nameLength, FORString) = nameString.c_str();
   return ok;
   }
// ------------------------------------------------------------------
// return the posting module to caller.
// ------------------------------------------------------------------
extern "C" bool EXPORT STDCALL component_name_to_id(char* name, int* id,
												   unsigned nameLength)
   {
   int iid = -1;
   bool ok = FortranWrapper::currentInstance->componentNameToID
					(asString(name, nameLength), iid);

   if (ok)
	 *id = iid;
   else
	 *id = -1;

   return ok;
   }
// ------------------------------------------------------------------
// Module is reading a string from a file.
// ------------------------------------------------------------------
extern "C" int EXPORT STDCALL read_parameter
   (const char* parameterName, const char* sectionName, char* value, int* optional,
    unsigned parameterNameLength, unsigned sectionNameLength, unsigned valueLength)
   {
   FString fsect = FString(sectionName, sectionNameLength, FORString);
   FString fpar = FString(parameterName, parameterNameLength, FORString);
   FString fvar = FString(value, valueLength, FORString);
   bool found = FortranWrapper::currentInstance->readParameter(fsect, fpar, fvar, *optional != 0);

   if (found)
      return true;

   else
      {
      memset((char*)value, ' ', valueLength);
      return false;
      }
   }
// ------------------------------------------------------------------
// Change the component order.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL change_component_order
   (char* moduleList, unsigned* numModules,
    unsigned moduleListLength)
   {
   FStrings names(moduleList, moduleListLength, *numModules, *numModules);
   FortranWrapper::currentInstance->changeComponentOrder(names);
   }

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl


