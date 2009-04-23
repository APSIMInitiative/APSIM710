#include <general/pch.h>
#pragma hdrstop

#include <list>
#include <fstream>
#include <sstream>
#include <stdexcept>

#include <math.h>
#include <boost/date_time/gregorian/gregorian.hpp>

#include <general/string_functions.h>
#include <general/date_class.h>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimRegistry.h>
#include <ComponentInterface/Component.h>
#include <Protocol/Transport.h>

#include <ComponentInterface/DataTypes.h>
#include "LogComponent.h"

using namespace std;
using namespace protocol;
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
// constructor
// ------------------------------------------------------------------
LogComponent::LogComponent(void)
   {
   nesting = 3;
   previousNesting = -1;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
LogComponent::~LogComponent(void)
   {
   out << "/>" << endl;
   for (int i = previousNesting-1; i > 0; i--)
      {
      out.width(i*3);
      out << ' ';
      out << "</message" << i << '>';
      out << endl;
      }
   out << "</messages>\n";
   }
// ------------------------------------------------------------------
//  Short description:
//     createComponent

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new LogComponent;
   }

// ------------------------------------------------------------------
//  Short description:
//     INIT1 method handler.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void LogComponent::doInit1(const protocol::Init1Data& initData)
   {
   protocol::Component::doInit1(initData);

   debug_outputID = addRegistration(::respondToSet, -1, "debug_output", "");
   string filename = componentData->getProperty("parameters", "logfile");
   if (filename == "")
      filename = "log.xml";
   out.open(filename.c_str());
   if (!out)
      {
      string msg = "Cannot open log file: " + filename;
      throw std::runtime_error(msg);
      }
   string debugOutputString = componentData->getProperty("parameters", "debug_output");
   bool doOutput = (debugOutputString == "" || Str_i_Eq(debugOutputString, "on"));
   if (doOutput)
      setMessageHook(this);
   }
// ------------------------------------------------------------------
// set the value of one of our variables.
// ------------------------------------------------------------------
bool LogComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   if (setValueData.ID == debug_outputID)
   {
      string stringValue;
      setValueData.variant.unpack(stringValue);
      if (stringValue == "off")
      {
//         out << "<DEBUG OUTPUT = OFF>" << endl;
         setMessageHook(NULL);
      }
      else
      {
//         out << "<DEBUG OUTPUT = ON>" << endl;
         setMessageHook(this);
      }
   }
  return true;
   }
// ------------------------------------------------------------------
//  Short description:
//     message callback routine.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void LogComponent::callback(const protocol::Message* message)
   {
   static bool firstTimeThrough = true;

   string toName;
   if (message != NULL) componentIDToName(message->to, toName);

   if (firstTimeThrough)
      {
      firstTimeThrough = false;
      out << "<messages>" << endl;
      out << "   <message1 to=\"MasterPM\" msgtype=\"Init1\" ack=\"false\"";
      }
   else
      {
      if (message != NULL)
         {
         nesting++;
         if (nesting > previousNesting)
            {
            out << '>' << endl;

            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
         else if (nesting == previousNesting)
            {
            out << "/>" << endl;
            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
         else
            {
            out << "/>" << endl;
            for (int i = previousNesting-1; i >= nesting; i--)
               {
               out.width(i*3);
               out << ' ';
               out << "</message" << i << '>';
               out << endl;
               }
            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
         previousNesting = nesting;
         out << flush;
         }
      else
         nesting--;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Get message log string

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeMessage(const string& toName,
                                const protocol::Message* message,
                                ostream& out)
   {
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

//   if (message->messageType == protocol::Register)
//      storeRegistration(message);
   out << "to=\"" << toName << "\"";
   out << " msgtype=\"" << messageNames[message->messageType-1] << "\"";
   out << " ack=\"";
   if (message->toAcknowledge)
      out << "true\"";
   else
      out << "false\"";
//   out << " id=\"" << message->messageID << "\"";
   writeMessageData(message);
   }

// ------------------------------------------------------------------
//  Short description:
//    Write message data

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeMessageData(const protocol::Message* message)
   {
   protocol::MessageData messageData(message->dataPtr, message->nDataBytes);
   switch(message->messageType)
      {
      case protocol::Register:
         {
         protocol::RegisterData registerData;
         messageData >> registerData;
         out << " kind=\"" << typeCodeToString((EventTypeCode)registerData.kind) << "\"";
         out << " name=\"" << asString(registerData.name) << "\"";
         if (registerData.destID > 0)
            {
            string compName;
            componentIDToName(registerData.destID, compName);
            out << " directedToComponent=\"" << compName << "\"";
            }
         out << " type=\"" << formatType(asString(registerData.type)) << "\"";
         break;
         }
      case protocol::RequestSetValue:
         {
         protocol::RequestSetValueData requestSetValueData;
         messageData >> requestSetValueData;
         writeVariant(requestSetValueData.variant);
         break;
         }
      case protocol::QuerySetValue:
         {
         protocol::QuerySetValueData querySetValueData;
         messageData >> querySetValueData;
         writeVariant(querySetValueData.variant);
         break;
         }
      case protocol::GetValue:
         {
         writeRegistrationData(message, ::get);
         break;
         }
      case protocol::ReturnValue:
         {
         protocol::ReturnValueData returnValueData;
         messageData >> returnValueData;
         string compName;
         componentIDToName(returnValueData.fromID, compName);
         out << " component = \"" << compName << "\"";
         break;
         }
      case protocol::PublishEvent:
         {
         protocol::PublishEventData eventData;
         messageData >> eventData;
         writeRegistrationData(message,  ::event);
         writeVariant(eventData.variant);
         break;
         }
      case protocol::ApsimGetQuery:
         {
         protocol::ApsimGetQueryData apsimGetQuery;
         messageData >> apsimGetQuery;
         out << " name=\"" << asString(apsimGetQuery.name) << "\"";
         break;
         }
      case protocol::ApsimSetQuery:
         {
         protocol::ApsimSetQueryData apsimSetQuery;
         messageData >> apsimSetQuery;
         out << " name=\"" << asString(apsimSetQuery.name) << "\"";
         writeVariant(apsimSetQuery.variant);
         break;
         }
      case protocol::QueryInfo:
         {
         protocol::QueryInfoData queryInfo;
         messageData >> queryInfo;
         out << " name=\"" << asString(queryInfo.name) << "\"";
         out << " kind=\"" << typeCodeToString((EventTypeCode)queryInfo.kind) << "\"";
         break;
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    Store this registration

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::storeRegistration(const Message* message)
   {
//   MessageData messageData((Message*)message);
//   RegisterData registerData;
//   messageData >> registerData;
//   components[message->from].registrations.insert(
//      LogComponent::Registrations::value_type(make_pair(registerData.ID, (unsigned)registerData.kind), asString(registerData.name)));
   }
// ------------------------------------------------------------------
//  Short description:
//    write the registration data for the specified message.

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeRegistrationData(const Message* message, EventTypeCode kind)
   {
   MessageData messageData((Message*) message);
   unsigned int regID;
   messageData >> regID;
   ApsimRegistration *reg = ApsimRegistry::getApsimRegistry().find(kind, message->from, regID);
   if (reg == NULL) throw std::runtime_error("NULL in LogComponent::writeRegistrationData");
   out << " regName=\"" << reg->getName() << "\"";
   }
// ------------------------------------------------------------------
//  Short description:
//    write the registration data for the specified message.

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeVariant(const protocol::Variant& variant)
   {
   out << " type=\"" << formatType(asString(variant.getType().getTypeString())) << "\"";
   }

// ------------------------------------------------------------------
// Format a ddml string ready for outputting.
// ------------------------------------------------------------------
string LogComponent::formatType(string RegistrationTypeString)
   {
   replaceAll(RegistrationTypeString, "><", "   ");
   replaceAll(RegistrationTypeString, "<", "");
   replaceAll(RegistrationTypeString, ">", "");
   replaceAll(RegistrationTypeString, "/", "");
   replaceAll(RegistrationTypeString, "\"", "'");
   return RegistrationTypeString;
   }
