#include <string.h>

#include <General/platform.h>
#include <General/string_functions.h>
#include <General/stristr.h>
#include <General/path.h>
#include <General/dll.h>

#include <ApsimShared/ApsimDirectories.h>
#include <ApsimShared/ApsimRegistry.h>

#include <ComponentInterface2/ScienceAPI2.h>

// Ensure we dont link against the tcl library. We pull it in via LoadLibrary
#define USE_TCL_STUBS
#include <tcl.h>

#ifndef __WIN32__
// will need to hunt for the tcl library on Linux platform
#include <glob.h>
#endif

typedef Tcl_Interp *(*CREATEINTERPFN)();

#include "TclComponent.h"

using namespace std;
int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimGetOptionalProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendMessageProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendRawMessageProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimWriteToSummaryFileProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimSubscribeNull(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimSubscribeVariant(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
//int apsimUnRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
//int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetComponentXML(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetChildren(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetFQName(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);

// This use of global variables is almost certainly NOT threadsafe...
static int hasWrittenCopyright = 0;
static Tcl_Interp *MainInterpreter = NULL;

//--------------------------- Tcl DLL initialisation
// Initialise the TCL dll. Call once.
//   pointers from http://wiki.tcl.tk/2074
void StartTcl (const std::string &exeName)
{
	void *hTcl = loadDLL(exeName);
	if (hTcl == NULL) 
		throw std::runtime_error("Can't load DLL " + exeName);     

	CREATEINTERPFN CreateInterpFn;
	CreateInterpFn = (CREATEINTERPFN) dllProcAddress(hTcl, "Tcl_CreateInterp");
	if (CreateInterpFn == NULL) 
		throw std::runtime_error("Can't find Tcl_CreateInterp in " + exeName);     

	MainInterpreter = CreateInterpFn();
	if (MainInterpreter == NULL) 
		throw std::runtime_error("Tcl_CreateInterp failed in " + exeName);     

	Tcl_InitStubs(MainInterpreter, "8.5", 0);
	Tcl_FindExecutable(exeName.c_str());
	Tcl_InitMemory(MainInterpreter);
	Tcl_SetVar(MainInterpreter, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
	Tcl_Init(MainInterpreter);
}

// Create a new interpreter
Tcl_Interp *NewInterp (ClientData cd, const std::string &interpName)
   {
  if (MainInterpreter == NULL) {throw std::runtime_error("MainInterpreter is NULL in NewInterp"); }

   Tcl_Interp *interp;
   if (interpName == "") {
      interp = MainInterpreter;
   } else {
      if ((interp = Tcl_CreateSlave(MainInterpreter, interpName.c_str(), 0))== NULL) { 
         char msg[1024];
         strcpy(msg,"CreateSlave '");
         strcat(msg,interpName.c_str());
         strcat(msg,"' failed.");
         throw std::runtime_error(msg);
      }
   }
 
   Tcl_CreateObjCommand(interp, "apsimGet", apsimGetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetOptional", apsimGetOptionalProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSet", apsimSetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimRegisterGetSet", apsimRegisterGetSetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSendMessage", apsimSendMessageProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSendRawMessage", apsimSendRawMessageProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimWriteToSummaryFile", apsimWriteToSummaryFileProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimRegisterEvent", apsimRegisterEvent, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSubscribeNull", apsimSubscribeNull, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSubscribeVariant", apsimSubscribeVariant, cd, NULL);
//   Tcl_CreateObjCommand(interp, "apsimUnRegisterEvent", apsimUnRegisterEvent, cd, NULL);
//   Tcl_CreateObjCommand(interp, "apsimCatchMessages", apsimCatchMessages, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetComponentXML", apsimGetComponentXML, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetChildren", apsimGetChildren, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetFQName", apsimGetFQName, cd, NULL);
   return interp;
   }

// Delete an interpreter
void StopTcl(Tcl_Interp *interp)
{
	if (!Tcl_InterpDeleted(interp) && interp != MainInterpreter)
		Tcl_DeleteInterp(interp);
}

#ifndef __WIN32__
// find tcl shared library (Linux only)
static const char *tclSharedLibraryPathname()
{
  static glob_t glob_result;
  static const char *path = NULL;

  if (path == NULL) {
    // library could be in /usr/lib or /usr/lib64,
    // and who know's what version may be installed, so:
    const char *glob_pattern = "/usr/lib*/libtcl?*.so";
    // this allocates memory, but we only ever call it once
    int rc = glob(glob_pattern,
                  0,              // flags
                  NULL,           // errorfunc,
                  &glob_result);
    if (rc == 0) {
      // found one or more, choose the last, which will be the latest
      path = glob_result.gl_pathv[glob_result.gl_pathc - 1];
    } else if (rc == GLOB_NOMATCH) {
      throw std::runtime_error("Can't glob TCL library " + string(glob_pattern));
    } else {
      // some other error
      throw std::runtime_error("Glob TCL library failed");
    }
  }
  return path;
}
#endif


//---------------------------APSIM dll entrypoints
extern "C" EXPORT TclComponent  * STDCALL createComponent(ScienceAPI2& scienceAPI)
   {
   string apsimDLL = scienceAPI.getExecutableFileName();
   replaceAll(apsimDLL, "\\", "/"); 
#ifdef __WIN32__
   string tclEXE = fileDirName(apsimDLL) + "/TclLink/bin/tcl85.dll";
#else
   string tclEXE = tclSharedLibraryPathname();
#endif
   StartTcl(tclEXE);

   return new TclComponent(scienceAPI);
   }

extern "C" void EXPORT STDCALL deleteComponent(TclComponent* component)
   {
   if (component)
     delete component;
   }

// ------------------------------------------------------------------
// Initialise and destroy the Apsim/Tcl component. 
// ------------------------------------------------------------------
TclComponent::TclComponent(ScienceAPI2 & api) : apsimAPI(api)
   {
   Interp = NewInterp(this, apsimAPI.name().c_str());
   hasFatalError = false;

   apsimAPI.subscribe("error", nullFunction(&TclComponent::onError));
   apsimAPI.subscribe("init2", nullFunction(&TclComponent::onInit2));
   }

TclComponent::~TclComponent(void)
   {
   if (Interp != NULL) 
      {
      StopTcl(Interp);
      Interp = NULL;
      }
   }

// ------------------------------------------------------------------
// Initialise the Tcl component.
// ------------------------------------------------------------------
void TclComponent::onInit2(void)
   {
   if (hasWrittenCopyright == 0) {
      // write copyright notice(s).
      apsimAPI.write("Copyright (C) 1991-1994 The Regents of the University of California.\n");
      apsimAPI.write("Copyright (C) 1996-1997 Sun Microsystems, Inc.\n");
      apsimAPI.write("Copyright (C) 2001      ActiveState.\n");
      hasWrittenCopyright++;
   } 

   Tcl_SetVar(Interp, "apsuite", getApsimDirectory().c_str(), TCL_GLOBAL_ONLY);
   Tcl_SetVar(Interp, "apsim", getApsimDirectory().c_str(), TCL_GLOBAL_ONLY);
   
   rules.clear();
   apsimAPI.readScripts(rules);
 
   for (map<string,string>::iterator i = rules.begin(); i != rules.end(); i++) 
       {
       string msg;
       msg = "--->Section: "; msg += i->first; msg += "\n";
       msg += i->second; msg += "\n";
       apsimAPI.write(msg);
       if (i->first != "init") 
          apsimAPI.subscribe(i->first, namedNullFunction(i->first, &TclComponent::onNullEventCallback));
       }

   apsimAPI.write("--->End\n");

   // Do the init rule if specified..
   string initRule = rules["init"];
   if (initRule != "")
      if (Tcl_Eval(Interp, initRule.c_str()) != TCL_OK)
          throw std::runtime_error(string(Tcl_GetStringResult(Interp)));

   //char buf[80]; sprintf(buf, "this=%x", this);
   //MessageBox(0,  buf, "Init", MB_ICONSTOP);
   }

void TclComponent::onError(void)
   {
   hasFatalError = true;
   }
   
// Allow apsim to read (and/ or write) one of our variables. 
void TclComponent::exposeReadable(const std::string &variableName, const std::string &units)
  {
  apsimAPI.exposeFunction2(variableName, units, "", StringArrayFunction2(variableName, &TclComponent::respondToGet));
  }

void TclComponent::exposeReadWrite(const std::string &variableName, const std::string &units)
  {
  apsimAPI.exposeFunction2(variableName, units, "", 
                           StringArrayFunction2(variableName, &TclComponent::respondToGet),
                           StringArrayFunction2(variableName, &TclComponent::respondToSet));
  }

// Subscribe to an event. Append to the event->script map so we can run it later.
void TclComponent::subscribeVariant(const std::string &event, const std::string &script) 
	{
    rules[event] += script;
    apsimAPI.subscribe(event, VariantFunction2(event, &TclComponent::onVariantEventCallback));
	}

void TclComponent::subscribeNull(const std::string &event, const std::string &script) 
	{
    rules[event] += script;
    apsimAPI.subscribe(event, namedNullFunction(event, &TclComponent::onNullEventCallback));
	}

// Deal with an event coming from the system to us
void TclComponent::onNullEventCallback(const std::string &s)
{
   string rule = rules[s];
   if (rule != "") 
   	  {
      if (Tcl_Eval(Interp, rule.c_str()) != TCL_OK)
           throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
      }
}

void TclComponent::onVariantEventCallback(const std::string &s, Variant &v)
{
   string rule = rules[s];
   // Do something with Variant args here, eg {sow hartog} {plants 100} etc
   if (rule != "") 
   	  {
      // Expose the raw message 
   	  Tcl_ObjSetVar2(Interp, Tcl_NewStringObj("incomingVariant",-1), NULL, 
                     Tcl_NewByteArrayObj((const unsigned char *)v.bufStart, v.bufLen), TCL_GLOBAL_ONLY);

      if (Tcl_Eval(Interp, rule.c_str()) != TCL_OK)
           throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
      }
}

// ------------------------------------------------------------------
// Return a variable to caller. Everything is a string.
// ------------------------------------------------------------------
void TclComponent::respondToGet(const std::string &variableName, std::vector<std::string> &result)
   {
   result.clear();
   Tcl_Obj *var = Tcl_GetVar2Ex(Interp, variableName.c_str(), NULL, TCL_LEAVE_ERR_MSG );

   if (var != NULL) 
      {
      // Find what we have. Is it a list? (APSIM arrays <=> TCL lists)
      Tcl_ObjType *listType = Tcl_GetObjType("list");
      if (var->typePtr == listType) 
         {
      	 Tcl_Obj **list = NULL; int len = 0;
         if (Tcl_ListObjGetElements(Interp, var, &len, &list) != TCL_OK) 
            throw std::runtime_error(string(Tcl_GetStringResult(Interp)));

         for (int i = 0; i < len; i++)
            result.push_back(Tcl_GetStringFromObj(list[i], NULL));
         }
      else 
         { 
      	 // Scalar. 
         result.push_back(Tcl_GetStringFromObj(var, NULL));
         }  
      }
   }

// ------------------------------------------------------------------
// Set the value of a variable for the specified variable name.
// ------------------------------------------------------------------
void TclComponent::respondToSet(const std::string &variableName, std::vector<std::string> &value)
   {
   if (value.size() > 1) 
      {	
      Tcl_Obj *result = Tcl_GetObjResult(Interp);
      Tcl_SetListObj(result, 0, NULL);

      for (std::vector<string>::iterator p = value.begin(); p != value.end(); p++)
          Tcl_ListObjAppendElement(Interp, result, Tcl_NewStringObj((char *)(*p).c_str(), -1));
      }
   else    
      Tcl_SetVar(Interp, variableName.c_str(), value[0].c_str(), TCL_GLOBAL_ONLY);
   }

// (Called from TCL interpreter). Find component() and ask apsim for a variable
int apsimGetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName(Tcl_GetStringFromObj(objv[1], NULL));
      return (component->apsimGet(interp, apsimName, false));
      }
   Tcl_SetResult(interp, "Wrong num args: apsimGet <variableName>", NULL);
   return TCL_ERROR;
   }
   
int apsimGetOptionalProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName(Tcl_GetStringFromObj(objv[1], NULL));
      return (component->apsimGet(interp, apsimName, true));
      }
   Tcl_SetResult(interp,"Wrong num args: apsimGetOptional <variableName>", NULL);
   return TCL_ERROR;
   }

// Get an apsim variable into interp->result. 
int TclComponent::apsimGet(Tcl_Interp *interp, const string &varName, bool optional)
   {
   vector<string> resultArray;

   apsimAPI.get(varName, "", optional, resultArray);

   if (resultArray.size() > 1) 
      {
      Tcl_Obj *result = Tcl_GetObjResult(interp);
      Tcl_SetListObj(result, 0, NULL);
      for (std::vector<string>::iterator p = resultArray.begin(); p != resultArray.end(); p++)
         Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj((char *)(*p).c_str(), -1));
      return TCL_OK;
      }
   else if (resultArray.size() == 1) 
      {
      string s = resultArray[0]; 
      Tcl_SetObjResult(interp, Tcl_NewStringObj((char *)s.c_str(),-1));
      return TCL_OK;
      }    
   return (optional ? TCL_OK : TCL_ERROR);
   }

// (Called from TCL interpreter.) Set an apsim value
int apsimSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 3)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName = Tcl_GetStringFromObj(objv[1], NULL);
      if (component->apsimSet(interp, apsimName, objv[2]))
         return TCL_OK;
      Tcl_AppendResult(interp, "Can't set the apsim variable ", apsimName.c_str(), NULL);
      return TCL_ERROR;
      }
   Tcl_SetResult(interp,"Wrong num args: apsimSet <variableName> <value>", NULL);
   return TCL_ERROR;
   }

// Set an apsim variable
bool TclComponent::apsimSet(Tcl_Interp *interp, const string &varName, Tcl_Obj *value)
   {
   Tcl_ObjType *listType = Tcl_GetObjType("list");

   // Find what we have. Is it an array (ie tcl list.)?
   if (value->typePtr==listType)
      {
      std::vector<string> outValue;
      Tcl_Obj *listElement;
      int      listLength;
      Tcl_ListObjLength(interp, value, &listLength);
      for (int idx = 0; idx < listLength; idx++)
           {
           Tcl_ListObjIndex(interp, value, idx, &listElement);
           string scratch = string(Tcl_GetStringFromObj(listElement, NULL));
           outValue.push_back(scratch);
           }
      apsimAPI.set(varName, "", outValue);
      }
   else
      {
      string outValue = string(Tcl_GetStringFromObj(value, NULL));
      apsimAPI.set(varName, "", outValue);
      }

   return(hasFatalError ==  false);
   }

// Called from TCL script. Tell apsim of something we own
int apsimRegisterGetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   string units = "";
   if (objc == 3)
       units = Tcl_GetStringFromObj(objv[2], NULL);
   if (objc == 2 || objc == 3)
      {
      TclComponent *component = (TclComponent *) cd;
      string name = Tcl_GetStringFromObj(objv[1], NULL);
      component->exposeReadable(name, units);
      return TCL_OK;
      }
   Tcl_SetResult(interp, "Wrong num args: apsimRegisterGet <variableName> [<units>]", NULL);
   return TCL_ERROR;
   }

int apsimRegisterGetSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   string units = "";
   if (objc == 3)
       units = Tcl_GetStringFromObj(objv[2], NULL);
   if (objc == 2 || objc == 3)
      {
      TclComponent *component = (TclComponent *) cd;
      string name = Tcl_GetStringFromObj(objv[1], NULL);
      component->exposeReadWrite(name, units);
      return TCL_OK;
      }
   Tcl_SetResult(interp, "Wrong num args: apsimRegisterGetSet <variableName> [<units>]", NULL);
   return TCL_ERROR;
   }

// Called from TCL script. Send a Variant message to the system, eg:
// apsimSendMessage wheat sow {plants 100} {depth 20}
// Todo: work out how to send other data structures FIXME!
int apsimSendMessageProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
      TclComponent *component = (TclComponent *) cd;

      if (objc < 3)
         {
         Tcl_SetResult(interp,"Wrong num args: apsimSendMessage <moduleName> <message> {<name> <value>} ...]", NULL);
         return TCL_ERROR;
         }

      // 1. destination
      string moduleName = Tcl_GetStringFromObj(objv[1], NULL);
      string actionName = Tcl_GetStringFromObj(objv[2], NULL);

      // 2. build variant
      Variant outgoingVariant;

      for (int i = 3; i < objc; i++)
         {
         int check = 0;
         if (Tcl_ListObjLength(interp, objv[i], &check) != TCL_OK) {
            Tcl_SetResult(interp,"arg not a list??", NULL);
            return TCL_ERROR;
         }
         
         if (check != 2) {
            Tcl_SetResult(interp,"arg format error: apsimSendEvent <moduleName> <message> {<name> <value>} ...", NULL);
            return TCL_ERROR;
         }

         Tcl_Obj *firstListElement, *secondListElement;
         Tcl_ListObjIndex(interp, objv[i], 0, &firstListElement);
         Tcl_ListObjIndex(interp, objv[i], 1, &secondListElement);
         
         if (firstListElement==NULL || secondListElement==NULL)
            {
            Tcl_SetStringObj(Tcl_GetObjResult(interp), "LE is NULL??? ", -1);
            return TCL_ERROR;
            }

         const string variableName = Tcl_GetStringFromObj(firstListElement,NULL);

         std::vector<std::string> variableValue;
         int numInList = 0;
         Tcl_ListObjLength(interp, secondListElement, &numInList);

         for (int j = 0; j < numInList; j++)  
            {
            Tcl_Obj *datum;
            Tcl_ListObjIndex(interp, secondListElement, j, &datum);
            variableValue.push_back(Tcl_GetStringFromObj(datum,NULL));
            }   

         outgoingVariant.pack(variableName, variableValue);
         }

      string destination;
      if (moduleName != "") 
      	 destination = moduleName + "." + actionName;
      else 
      	 destination = actionName;

      component->apsimAPI.publish(destination, outgoingVariant);
      return TCL_OK;         
   }

// Send a raw message (created by tcl's binary command). [de]constructors for apsim data structures can be found in
// %apsim/Model/TclLink/CIDataTypes.tcl
// Onus is on the user to get it right.
int apsimSendRawMessageProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
      TclComponent *component = (TclComponent *) cd;

      if (objc != 4)
         {
         Tcl_SetResult(interp,"Wrong num args: apsimSendRawMessage <moduleName> <message> ", NULL);
         return TCL_ERROR;
         }

      // 1. destination
      string moduleName = Tcl_GetStringFromObj(objv[1], NULL);
      string actionName = Tcl_GetStringFromObj(objv[2], NULL);

      // 2. build variant from raw byte array. This is highly dangerous, not for the faint hearted!!
      Variant outgoingVariant;
      unsigned char *buf;
      int bufLen;
      buf = Tcl_GetByteArrayFromObj(objv[3], &bufLen);
      if (buf != NULL) {
         outgoingVariant.bufStart = (char *) malloc(bufLen);
         if (outgoingVariant.bufStart == NULL) throw std::runtime_error("Out of memory");
         memcpy(buf, outgoingVariant.bufStart, bufLen);
         outgoingVariant.bufLen = bufLen;

         string destination;
         if (moduleName != "") 
           destination = moduleName + "." + actionName;
         else 
      	   destination = actionName;

         component->apsimAPI.publish(destination, outgoingVariant);
      }
      return TCL_OK;         
   }

int apsimWriteToSummaryFileProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 2)
         {
         Tcl_SetResult(interp,"Wrong num args: apsimWriteToSummaryFile <message>", NULL);
         return TCL_ERROR;
         }
   string message = Tcl_GetStringFromObj(objv[1], NULL);
   component->apsimAPI.write(message + "\n");
   return TCL_OK;
}

// Tell apsim we are interested in an event
int apsimSubscribeNull(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 3)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimSubscribeNull <eventName> <script>", NULL);
         return TCL_ERROR;
         }

   component->subscribeNull(string(Tcl_GetStringFromObj(objv[1], NULL)),
                            string(Tcl_GetStringFromObj(objv[2], NULL)));

   return TCL_OK;
}

// Same deal; but we'll expect a Variant structure inside the message
int apsimSubscribeVariant(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 3)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimSubscribeVariant <eventName> <script>", NULL);
         return TCL_ERROR;
         }

   component->subscribeVariant(string(Tcl_GetStringFromObj(objv[1], NULL)),
                               string(Tcl_GetStringFromObj(objv[2], NULL)));

   return TCL_OK;
}

int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
	 return (apsimSubscribeNull(cd, interp, objc, objv));
}

int apsimGetComponentXML(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   Tcl_SetObjResult(interp, Tcl_NewStringObj(component->apsimAPI.getInitData().c_str(), -1));
   return TCL_OK;
} 

int apsimGetChildren(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   if (objc != 2)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimGetChildren componentname", NULL);
         return TCL_ERROR;
         }

   Tcl_Obj *result = Tcl_GetObjResult(interp);
   Tcl_SetListObj(result, 0, NULL);

   string ComponentName = Tcl_GetStringFromObj(objv[1], NULL);
   int componentID = ApsimRegistry::getApsimRegistry().componentByName(ComponentName);
   if (componentID > 0)
      {
      // Return a list of child components for the specified componentID
      vector<int> childIDs;
      ApsimRegistry::getApsimRegistry().getChildren(componentID, childIDs);

      for (unsigned i = 0; i != childIDs.size(); i++)
         {
         string ChildName = ApsimRegistry::getApsimRegistry().componentByID(childIDs[i]);
         Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj((char *)ChildName.c_str(), -1));
         }
      return TCL_OK;
	    }
	 string emsg = "No component called: " + ComponentName ;
   Tcl_SetResult(interp, (char *) emsg.c_str(), NULL);
   return TCL_ERROR;
} 

int apsimGetFQName(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   Tcl_SetObjResult(interp, Tcl_NewStringObj(component->apsimAPI.FQName().c_str(), -1));
   return TCL_OK;
}

