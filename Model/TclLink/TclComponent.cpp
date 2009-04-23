#include <string.h>

#include <map>
#include <general/string_functions.h>
#include <general/stristr.h>

#include <ApsimShared/ApsimDirectories.h>
#include <ApsimShared/FStringExt.h>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Variants.h>
#include <ComponentInterface/MessageTypes.h>
#include <Protocol/Transport.h>

#include <tcl.h>
#include <tclInt.h>
#include "TclComponent.h"

using namespace std;
using namespace protocol;

extern void StartTcl (const char *);
extern Tcl_Interp *NewInterp (Tcl_Interp *, ClientData, const char *);
extern void StopTcl(Tcl_Interp *);

static int initialisationState = 0;

#define nullDDML       "<type/>"
#define intString      "<type kind=\"integer4\" array=\"F\"/>"
#define intStringArray "<type kind=\"integer4\" array=\"T\"/>"
#define fltString      "<type kind=\"single\" array=\"F\"/>"
#define fltStringArray "<type kind=\"single\" array=\"T\"/>"
#define dblString      "<type kind=\"double\" array=\"F\"/>"
#define dblStringArray "<type kind=\"double\" array=\"T\"/>"
#define strString      "<type kind=\"string\" array=\"F\"/>"
#define strStringArray "<type kind=\"string\" array=\"T\"/>"

static const char* messageNames[45] =
     {"ActivateComponent", "AddComponent", "Checkpoint", "Commence",
      "Complete", "DeactivateComponent", "DeleteComponent", "Deregister",
      "Event", "GetValue", "Init1", "Init2",
      "NotifyAboutToDelete", "NotifyRegistrationChange", "NotifySetValueSuccess",
      "NotifyTermination", "PauseSimulation", "PublishEvent", "QueryInfo",
      "QuerySetValue", "QueryValue", "Register", "ReinstateCheckpoint",
      "ReplySetValueSuccess", "ReplyValue",
      "RequestComponentID", "RequestSetValue", "ResumeSimulation",
      "ReturnComponentID", "ReturnInfo", "ReturnValue",
      "TerminateSimulation", "<unused>", "<unused>", "<unused>", "<unused>", "<unused>", "<unused>", "<unused>",
      "ApsimGetQuery", "ApsimSetQuery", "ApsimChangeOrder"};

int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendEventProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetComponentXML(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);

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
// ------------------------------------------------------------------
// Create an instance of the TclLink module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new TclComponent;
   }
// ------------------------------------------------------------------
// Initialises the Tcl component.
// ------------------------------------------------------------------
TclComponent::TclComponent()
   {
   Interp = NULL;
   hasFatalError = false;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
TclComponent::~TclComponent(void)
   {
   if (Interp != NULL) 
      {
      if (!hasFatalError) 
         {
         if (!terminationRule.empty()) { Tcl_Eval(Interp, terminationRule.c_str()); }
         }
      
      StopTcl(Interp);
      Interp = NULL;
      }
   }
// ------------------------------------------------------------------
// Stage 1 initialisation. We can initialise the TCL world now that we know where we are..
// ------------------------------------------------------------------
void TclComponent::doInit1(const protocol::Init1Data& initData)
   {
   protocol::Component::doInit1(initData);

   if (initialisationState == 0) {
     StartTcl(Component::componentData->getExecutableFileName().c_str());
     initialisationState = 1;
   }
   //MessageBox(0,  Component::componentData->getExecutableFileName().c_str(), "Init", MB_ICONSTOP);

   errorID = addRegistration(::respondToEvent,
                             -1,
                             string("error"),
                             DDML(ErrorType()));

   }

// ------------------------------------------------------------------
// Initialise the Tcl component.
// ------------------------------------------------------------------
void TclComponent::doInit2(void)
   {
   protocol::Component::doInit2();

   if (initialisationState == 1) {
      // write copyright notice(s).
      writeString("Copyright (C) 1991-1994 The Regents of the University of California.");
      writeString("Copyright (C) 1996-1997 Sun Microsystems, Inc.");
      writeString("Copyright (C) 2001      ActiveState.");
      Interp = NewInterp(NULL, this, getName().c_str());
   } 

   Tcl_SetVar(Interp, "apsuite", getApsimDirectory().c_str(), TCL_GLOBAL_ONLY);
   
   string initRule;
   std::vector<string> ruleNames;
   componentData->getRuleNames(ruleNames);
   rules.clear();

   for (unsigned int i = 0; i != ruleNames.size(); i++)
      {
         string condition, rule;
         componentData->getRule(ruleNames[i], condition, rule);
         unsigned id = addRegistration(::respondToEvent, -1, condition, "");
         rules.insert(UInt2StringMap::value_type(id, rule));

         string msg = "--->Section: " + condition;
         writeString(msg.c_str());
         writeString(rule.c_str());
         if (condition == string("init")) {initRule.append(rule);}
         if (condition == string("exit")) {terminationRule.append(rule);}
      }
   writeString("--->End");

   // Do the init rule if specified..
   if (!initRule.empty())
      {
      int result = Tcl_Eval(Interp, initRule.c_str());
      if (result != TCL_OK)
          {
          throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
          }
      }
      //char buf[80]; sprintf(buf, "this=%x", this);
      //MessageBox(0,  buf, "Init", MB_ICONSTOP);
   }
// ------------------------------------------------------------------
// Look for messages.
// ------------------------------------------------------------------
void TclComponent::respondToEvent(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant &variant)
   {
   UInt2StringMap::iterator ip, ip1, ip2;

   if (eventID == errorID) hasFatalError = true;

   ip1 = rules.lower_bound(eventID);
   ip2 = rules.upper_bound(eventID);

   for (ip = ip1; ip != ip2; ip++)
     {
     string rule = ip->second;
     if (!rule.empty())
        {
        // Set the global variable "incomingApsimVariant" to the variant's binary data
        MessageData message = variant.getMessageData();
        Tcl_ObjSetVar2(Interp, Tcl_NewStringObj("incomingApsimVariant",-1), NULL, 
                       Tcl_NewByteArrayObj((const unsigned char *)message.start(), message.totalBytes()), TCL_GLOBAL_ONLY);

        int result = Tcl_Eval(Interp, rule.c_str());
        if (result != TCL_OK)
           throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
        }
     }  
   }
// ------------------------------------------------------------------
// Return a variable to caller.
// ------------------------------------------------------------------
void TclComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   string variableName = getRegistration(fromID, queryData.ID)->getName();   
   string infoCmd, getCmd;

   infoCmd = "array exists " + variableName;
   if (Tcl_Eval(Interp, infoCmd.c_str()) != TCL_OK)
       throw std::runtime_error(string(Tcl_GetStringResult(Interp)));

   int isArray = 0;
   if (Tcl_GetIntFromObj(Interp, Tcl_GetObjResult(Interp), &isArray) != TCL_OK)
       throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
   
   if (isArray) 
      getCmd = "array get " + variableName;
   else 
      getCmd = "set " + variableName;

   if (Tcl_Eval(Interp, getCmd.c_str()) != TCL_OK)
       throw std::runtime_error(string(Tcl_GetStringResult(Interp)));

   const char *result = Tcl_GetStringFromObj(Tcl_GetObjResult(Interp), NULL);

   if (result != NULL)
      sendVariable(queryData, string(result));
   }
// ------------------------------------------------------------------
// Something is asking whether we know about a variable. If we do, register it.
// ------------------------------------------------------------------
void TclComponent::onApsimGetQuery(unsigned int fromID, protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   if (Interp != NULL) 
      {
      string varCmd = "info exists " + asString(apsimGetQueryData.name);

      int result = Tcl_Eval(Interp, varCmd.c_str());
      if (result == TCL_OK)
         if (Str_i_Eq("1", Tcl_GetStringFromObj(Tcl_GetObjResult(Interp), NULL)))
            registerApsimVariable(Interp, asString(apsimGetQueryData.name));
      }
   }   
// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool TclComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   string newValue;
   setValueData.variant.unpack(newValue);

   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   ApsimRegistration *reg = registry.find(::respondToSet, fromID, setValueData.ID);
   if (reg == NULL)
        {
        string msg = "Invalid registration ID in TclComponent::respondToSet ";
                     msg += itoa(fromID);
                     msg += ".";
                     msg += itoa(setValueData.ID);
        throw std::runtime_error(msg);
        }

   string name = reg->getName();

   const char *result = Tcl_SetVar(Interp, name.c_str(), newValue.c_str(), TCL_GLOBAL_ONLY);
   if (result != NULL)
      return true;
   return false;
   }

// (Called from TCL interpreter). Find component() and ask protoman for a variable
int apsimGetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc >= 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName(Tcl_GetStringFromObj(objv[1], NULL));
      return (component->apsimGet(interp, apsimName, false));
      }
   Tcl_SetResult(interp,"Wrong num args: apsimGet <variableName>", NULL);
   return TCL_ERROR;
   }
   
int apsimGetOptionalProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc >= 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName(Tcl_GetStringFromObj(objv[1], NULL));
      return (component->apsimGet(interp, apsimName, true));
      }
   Tcl_SetResult(interp,"Wrong num args: apsimGetOptional <variableName>", NULL);
   return TCL_ERROR;
   }

// Get an apsim variable into interp->result. 
int TclComponent::apsimGet(Tcl_Interp *interp, const string &varname, bool optional)
   {

   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(componentID, varname, destID, regName);

   ApsimRegistration *regItem = (ApsimRegistration *)
       addRegistration(::get,
                       destID, 
                       regName,
                       strStringArray);

   protocol::Variant* variant = NULL;
   
   if (getVariable((unsigned int)regItem, &variant, optional)) 
      {
      Tcl_Obj *result = Tcl_GetObjResult(interp);
      Tcl_SetListObj(result, 0, NULL);

      std::vector<string> scratch; 
      protocol::TypeConverter* typeConverter = NULL;
      getTypeConverter(regItem->getName().c_str(),
                       variant->getType(),
                       regItem->getDDML().c_str(),
                       typeConverter);

      protocol::ArraySpecifier* arraySpec = protocol::ArraySpecifier::create(regItem);

      bool ok = variant->unpack(typeConverter, 
                                arraySpec, 
                                scratch);

      if (arraySpec) delete arraySpec;

      if (!ok)
         {
         string msg= "Unpack failed.";
         msg += "\nVariableName: ";
         msg += regItem->getName();
         throw std::runtime_error(msg);
         }

      for (std::vector<string>::iterator p = scratch.begin(); p != scratch.end(); p++)
          Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj((char *)(*p).c_str(), -1));
      }    
   return TCL_OK;
   }

// (Called from TCL interpreter.) Find component() and set apsim value
int apsimSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 3)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName = Tcl_GetStringFromObj(objv[1], NULL);
      if (component->apsimSet(interp, apsimName, objv[2]))
         {
         return TCL_OK;
         }
      Tcl_AppendResult(interp, "Can't set the apsim variable ", apsimName.c_str(), NULL);
      return TCL_ERROR;
      }
   Tcl_SetResult(interp,"Wrong num args: apsimSet <variableName> <value>", NULL);
   return TCL_ERROR;
   }

// Set an apsim variable
bool TclComponent::apsimSet(Tcl_Interp *interp, const string &varname, Tcl_Obj *value)
   {
   Tcl_ObjType *listType = Tcl_GetObjType("list");

   // Find what we have. Is it an array?
   bool isArray = false;
   if (value->typePtr==listType)
      {
      isArray = true;
      }

   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(componentID, varname, destID, regName);

   unsigned variableID = addRegistration(::set,
                                         destID,
                                         regName, 
                                         isArray?strStringArray:strString);
   if (isArray)
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
       return (setVariable(variableID, outValue));
       }
   else
       {
       string outValue = string(Tcl_GetStringFromObj(value, NULL));
       return (setVariable(variableID, outValue));
       }
   /* notreached*/
   }

void TclComponent::registerApsimVariable(Tcl_Interp *interp, const string &name)
   {
   // Find what we have. Is it an array?
   bool varExists = false;

   string varCmd = "info exists " + name;
   int result = Tcl_Eval(interp, varCmd.c_str());
   if (result != TCL_OK) throw std::runtime_error(string(Tcl_GetStringResult(interp)));
   if (Str_i_Eq("1", Tcl_GetStringFromObj(Tcl_GetObjResult(interp), NULL)))
       varExists = true;

   if (varExists)
      {
      addRegistration(::respondToSet, -1, name, strString);
      addRegistration(::respondToGet, -1, name, strString);
      }
   }

// Called from TCL script. Tell protoman of something we own
int apsimRegisterGetSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string name(Tcl_GetStringFromObj(objv[1], NULL));
      component->registerApsimVariable(interp, name);
      return TCL_OK;
      }
   Tcl_SetResult(interp, "Wrong num args: apsimRegisterGetSet <variableName>", NULL);
   return TCL_ERROR;
   }

// Called from TCL script. Send a message to the system, eg:
// apsimSendMessage wheat sow {plants 100} {depth 20}
int apsimSendMessageProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
      TclComponent *component = (TclComponent *) cd;

      if (objc < 3)
         {
         Tcl_SetResult(interp,"Wrong num args: apsimSendEvent <moduleName> <message> {<name> <value>} ...]", NULL);
         return TCL_ERROR;
         }

      // 1. destination
      const char *moduleName = Tcl_GetStringFromObj(objv[1], NULL);
      const char *actionName = Tcl_GetStringFromObj(objv[2], NULL);

      // 2. build variant
      protocol::ApsimVariant outgoingApsimVariant(component);
      outgoingApsimVariant.store(FString("sender"), protocol::DTstring, false, FString(component->getName().c_str()));
      outgoingApsimVariant.store(FString("sender_id"), protocol::DTint4, false, (int)component->getId());

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

         const char *v1 = Tcl_GetStringFromObj(firstListElement,NULL);
         FString variableName = FString(v1);

         const char *v2 = Tcl_GetStringFromObj(secondListElement,NULL);
         FString variableValue = FString(v2);

         if (Tcl_ListObjLength(interp, secondListElement, &check) != TCL_OK) {
            check = 0;
         }

         bool isArray = check > 1;

         outgoingApsimVariant.store(variableName, protocol::DTstring, isArray, variableValue);
         }

      // Send it
      int destID;
      if (! component->componentNameToID(moduleName, destID)) {
         //Tcl_SetStringObj(Tcl_GetObjResult(interp), "Unknown module", -1);
         //return TCL_ERROR;         
         destID = 0;
      }
      unsigned int eventID = component->addRegistration(::event,
                                                        destID,
                                                        actionName,
                                                        (const char *)"<variant/>");
      component->publish(eventID, outgoingApsimVariant);
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
   const char *message = Tcl_GetStringFromObj(objv[1], NULL);
   component->writeString(message);
   return TCL_OK;
}

int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 3)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimRegisterEvent <eventName> <script>", NULL);
         return TCL_ERROR;
         }
   string eventName = string(Tcl_GetStringFromObj(objv[1], NULL));
   string script = string(Tcl_GetStringFromObj(objv[2], NULL));
   
   unsigned int id = component->registerEvent(eventName, script);
   Tcl_SetObjResult(interp, Tcl_NewIntObj((int) id));
   return TCL_OK;
}

int apsimUnRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 2)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimUnRegisterEvent <id>", NULL);
         return TCL_ERROR;
         }
   unsigned int id = Tcl_GetIntFromObj(interp, objv[1], NULL);
   
   component->unRegisterEvent(id);
   return TCL_OK;
}

int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 2)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimCatchMessages <command>", NULL);
         return TCL_ERROR;
         }
   string command = string(Tcl_GetStringFromObj(objv[1], NULL));
   
   component->catchMessages(command);
   return TCL_OK;
}

unsigned int TclComponent::registerEvent(string &eventName, string &script)
   {
   unsigned id = addRegistration(::respondToEvent, 
                                 -1,
                                 eventName, 
                                 "<variant/>");
   rules.insert(UInt2StringMap::value_type(id, script));

   if (eventName == string("exit")) {terminationRule.append(script);}
   return id;
   }

void TclComponent::unRegisterEvent(unsigned int id) 
   {
   throw("TclComponent::unRegisterEvent not implemented");
   }

void TclComponent::catchMessages(string &command)
   {
   messageCallbackCommand = command;
   if (command == "")
      setMessageHook(NULL);
   else
      setMessageHook(this);
   }

void TclComponent::callback(const protocol::Message* message)
   {
   if (messageCallbackCommand != "") 
      {
      int ncmd = 1;
      Tcl_Obj **cmd = (Tcl_Obj**)malloc(100*sizeof(Tcl_Obj*));
      cmd[0] = Tcl_NewStringObj(messageCallbackCommand.c_str(), -1);

      string toName;
      componentIDToName(message->to, toName);

      if (toName != "")
         {
         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("toName",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(toName.c_str(),-1));
         ncmd++;
         } 

      if (message != NULL) 
         {
         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("from",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewIntObj(message->from));
         ncmd++;

         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("to",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewIntObj(message->to));
         ncmd++;

         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("id",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewIntObj(message->messageID));
         ncmd++;

         if (message->messageType >= 45) {throw "messageType oob in TclComponent::callback";}
         if (message->messageType > 0) 
            {
            cmd[ncmd] = Tcl_NewListObj(0, NULL);
            Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("type",-1));
            Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(messageNames[message->messageType-1],-1));
            ncmd++;
            }
    
         switch(message->messageType)
            {
            case protocol::Event:
               {
               protocol::MessageData messageData(message->dataPtr, message->nDataBytes);
               protocol::EventData eventData;
               messageData >> eventData;

               ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
               ApsimRegistration *reg = registry.find(::event, eventData.publishedByID, eventData.ID);
               if (reg == NULL)
                  {
                  string msg = "Invalid registration ID in TclComponent::callback ";
                               msg += itoa(eventData.publishedByID);
                               msg += ".";
                               msg += itoa(eventData.ID);
                  cout << msg << endl;
                  }
               else 
                  {
                  string name = reg->getName();
                  cmd[ncmd] = Tcl_NewListObj(0, NULL);
                  Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("name",-1));
                  Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(name.c_str(),-1));
                  ncmd++;
                  break;
                  }
               }
            case protocol::GetValue:
               {
               protocol::MessageData messageData(message->dataPtr, message->nDataBytes);
               protocol::GetValueData getValueData;
               messageData >> getValueData;
               ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
               ApsimRegistration *reg = registry.find(::get, message->from, getValueData.ID); // fixme!!
               if (reg == NULL)
                   {
                   string msg = "Invalid registration ID in TclComponent::callback ";
                                msg += itoa(message->from);
                                msg += ".";
                                msg += itoa(getValueData.ID);
                   throw std::runtime_error(msg);
                   }
               else
                   {
                   string name = reg->getName();
                   cmd[ncmd] = Tcl_NewListObj(0, NULL);
                   Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("name",-1));
                   Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(name.c_str(),-1));
                   ncmd++;
                   break;
                   }
               }
            }

         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("data",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewByteArrayObj((const unsigned char *)message->dataPtr,message->nDataBytes));
         ncmd++;
         }

      int res = Tcl_EvalObjv(Interp, ncmd, cmd, TCL_EVAL_GLOBAL);

      if (res != TCL_OK) 
         fprintf(stdout, "messageHook error, result=%s\n", Tcl_GetStringResult(Interp));

      //for (int obj = 0; obj < ncmd; obj++)
      //  Tcl_DecrRefCount(cmd[obj]);

      free(cmd);
      }
   }


int apsimGetComponentXML(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   string xml = component->getXML();
   Tcl_SetObjResult(interp, Tcl_NewStringObj(xml.c_str(), -1));
   return TCL_OK;
} 

