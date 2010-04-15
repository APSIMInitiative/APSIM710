#include <iostream>
#include <General/xml.h>
#include <General/date_class.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/path.h>
#include "DataTypes.h"
#include "CMPData.h"
#include "ScienceAPI.h"
#include "CMPComponentInterface.h"
#include "ArraySpecifier.h"
#include "Messages.h"

using namespace std;

string componentType = "APSRU";
string version = "2.0";
string author = "APSRU";
int active = 1;
string state = "";

// =============================================================================
// =============================================================================
// =============================================================================
CMPComponentInterface::CMPComponentInterface(unsigned* callbackarg, CallbackType* callback,
                                             unsigned componentid, unsigned parentid)
   // -----------------------------------------------------------------------
   // constructor
   // -----------------------------------------------------------------------
   {
   callbackArg = callbackarg;
   messageCallback = callback;
   componentID = componentid;
   parentID = parentid;
   initMessageFactory();
   errorHasOccurred = false;
   simScript = NULL;
   init1 = NULL;
   tick.startday = tick.startsec = 0; tickID = 0;
   haveWrittenToStdOutToday = false;
   }

CMPComponentInterface::~CMPComponentInterface()
   // -----------------------------------------------------------------------
   // destructor
   // -----------------------------------------------------------------------
   {
   shutDownMessageFactory();
   for (NameToRegMap::iterator i = regNames.begin();
                               i != regNames.end();
                               i++)
	  if (i->second != NULL) {
		delete i->second->data;
		delete i->second;
	  }

   clearMessages();
   delete simScript;
   }


void CMPComponentInterface::messageToLogic(const Message& message)
   // -----------------------------------------------------------------------
   // Called for all incoming messages.
   // -----------------------------------------------------------------------
   {
   // We need to keep track of bits of the message because the FARMWI$E infrastructure
   // doesn't guarantee that a message is still valid at the end of this method.
   // eg. it deletes the Init1 message before we get to test the ack flag at the bottom.
   bool ack = message.toAcknowledge;
   //unsigned fromID = message.from;
   unsigned msgID = message.messageID;

	try
		{
		switch (message.messageType)
			{
         case Message::Event:                         onEvent(message); break;
         case Message::Init1:                         onInit1(message); break;
         case Message::Init2:                         onInit2(message); break;
			case Message::QueryValue:                    onQueryValue(message); break;
			case Message::QuerySetValue:                 onQuerySetValue(message); break;
			case Message::ReturnValue:
			case Message::ReturnInfo:
			case Message::NotifySetValueSuccess:
			case Message::ReplySetValueSuccess:		      messages.push_back(cloneMessage(message)); break;
			}

      // if acknowledgement is required, then give it.
      if (ack)
         {
         CompleteType complete;
         complete.ackID = msgID;
         sendMessage(newMessage(Message::Complete, componentID, parentID, false, complete));
         }

		}
	catch (const exception& err)
		{
		error(err.what(), true);
		}
   }

bool CMPComponentInterface::get(const std::string& name, const std::string& units, bool optional, Packable* data)
   // -----------------------------------------------------------------------
   // Get the value of a variable from another component.
   // -----------------------------------------------------------------------
   {
	clearMessages();

   // see if we have an array specifier.
   ArraySpecifier* arraySpecifier = ArraySpecifier::create(name);
   string nameWithoutArraySpec = name;
   if (arraySpecifier != NULL)
      nameWithoutArraySpec = arraySpecifier->variableName();

   int id = nameToRegistrationID(nameWithoutArraySpec, getReg);
   bool alreadyRegistered = (id != 0);
   if (!alreadyRegistered)
      id = RegisterWithPM(nameWithoutArraySpec, units, "", getReg, data);

	GetValueType getValue;
	getValue.ID = id;
   sendMessage(newMessage(Message::GetValue, componentID, parentID, false,
                          getValue));

   string errorMsg;
   if (messages.size() == 0)
		errorMsg = "No component responded to a 'get' for variable: " + name;

	else if (messages.size() == 1)
      {
		ReturnValueType returnValue;
      MessageData returnMessageData(*messages[0]);
		unpack(returnMessageData, returnValue);
      if (arraySpecifier != NULL)
         arraySpecifier->summariseData(returnMessageData, returnValue.ddml);
      data->unpack(returnMessageData, returnValue.ddml);
      }

   else if (messages.size() > 1)
      errorMsg = "Too many components responded to a 'get' for variable: " + name;
   if (alreadyRegistered)
      {
      delete data;
      data = NULL;
      }

   delete arraySpecifier;
   if (errorMsg != "")
      {
      if (!optional)
         throw runtime_error(errorMsg);
      return false;
      }
   else
      return true;
   }

void CMPComponentInterface::set(const std::string& name,
                                const std::string& units,
                                Packable* data)
   {
   // -----------------------------------------------------------------------
   // Set the value of a variable in another component.
   // -----------------------------------------------------------------------
	clearMessages();
   int id = nameToRegistrationID(name, setReg);
   if (id == 0)
      id = RegisterWithPM(name, units, "", setReg, data);

	RequestSetValueType requestSetValue;
	requestSetValue.ID = id;
	requestSetValue.ddml = data->ddml();

	Message& requestSetValueMessage = constructMessage(Message::RequestSetValue, componentID, parentID, false,
                                                      memorySize(requestSetValue) + data->memorySize());
   MessageData requestSetValueMessageData(requestSetValueMessage);
   pack(requestSetValueMessageData, requestSetValue);
   data->pack(requestSetValueMessageData);
	sendMessage(requestSetValueMessage);
   }

bool CMPComponentInterface::readFiltered(const string& parName, vector<string> &values)
   {
   // -----------------------------------------------------------------------
   // return the raw data for this component. Used to discover manager rules etc.
   // -----------------------------------------------------------------------
   XMLNode::iterator initData = find_if(simScript->documentElement().begin(),
                                        simScript->documentElement().end(),
                                        EqualToName<XMLNode>("initdata"));

   for_each_if(initData->begin(), initData->end(),
               GetValueFunction<vector<string>, XMLNode>(values),
               EqualToName<XMLNode>(parName));

   return (values.size() > 0);
   }
bool CMPComponentInterface::readAll(std::vector<std::string> &names, std::vector<std::string> &values)
   {
   // -----------------------------------------------------------------------
   // return the raw data for this component. Used to discover manager rules etc.
   // -----------------------------------------------------------------------
   XMLNode::iterator initData = find_if(simScript->documentElement().begin(),
                                        simScript->documentElement().end(),
                                        EqualToName<XMLNode>("initdata"));

   for (XMLNode::iterator i = initData->begin();
                          i != initData->end();
                          i++)
      {
      names.push_back(i->getName());
      values.push_back(i->getValue());
      }

   return (names.size() > 0);
   }


bool CMPComponentInterface::read(const std::string& parName, Convertable* value, bool optional)
   {
   // -----------------------------------------------------------------------
   // Read a parameter from the initial SIM script.
   // 1. read each "section" (in reverse order)
   // 2.  check for a "derived_from" in each section
   // 3. read <initdata> finally.
   // -----------------------------------------------------------------------
   XMLNode::iterator initData = find_if(simScript->documentElement().begin(),
                                        simScript->documentElement().end(),
                                        EqualToName<XMLNode>("initdata"));

   if (initData == simScript->documentElement().end())
      throw runtime_error("Cannot find <initdata> element for component: " + name);

   // Look in each section
   for (vector<string>::reverse_iterator section = simSectionsToSearch.rbegin();
        section != simSectionsToSearch.rend();
        section++)
      {
      XMLNode::iterator sectionData = findNode(*initData, *section);
      if (sectionData == initData->end())
         sectionData = findNodeWithName(*initData, *section);

      if (sectionData != initData->end())
         if (readFromSection(initData, sectionData, parName, value))
            return true;
      }

   // Look in the "global" section
   if (readFromSection(initData, initData, parName, value))
      return true;

   if (!optional)
      throw runtime_error("Cannot read parameter \"" + parName + "\" for module " + name + ".");
   return false;
   }


bool CMPComponentInterface::readFromSection(XMLNode::iterator initData,
                                            XMLNode::iterator sectionData,
                                            const std::string& parName,
                                            Convertable* returnValue)
   {
   // Read a parameter from a section.
   // If there's a "derived_from" member, try it.
   XMLNode::iterator match = find_if(sectionData->begin(), sectionData->end(),
                                     EqualToName<XMLNode>(parName));
   if (match == sectionData->end())
      match = find_if(sectionData->begin(), sectionData->end(),
                      AttributeEquals<XMLNode>("name", parName));

   bool found = false;
   vector <string> result;
   while (match != sectionData->end())
      {
      found = true;
      if (match->begin() == match->end())
         {
         string value = match->getValue();
         // Remove any units specifier "(..)":
         unsigned int posBracket = value.find('(');
         if (posBracket != std::string::npos)
            value = value.substr(0,posBracket);

         // And any whitespace...
         stripLeadingTrailing(value, " \t");

         // Split into list if it can
         split(value, " ", result);
         //result.push_back(value);
         }
      else
         result.push_back(match->write());

      match++;
      match = find_if(match, sectionData->end(), EqualToName<XMLNode>(parName));
      }
   if (found)
      {
      // OK. Typeconverter kicks in here.
      returnValue->from(result);
      return true;
      }

   if (sectionData == initData) return false;     // Don't go circular

   // Handle any "derived_from" directives
   vector<string> sections;
   for_each_if(sectionData->begin(), sectionData->end(),
               GetValueFunction<vector<string>, XMLNode>(sections),
               EqualToName<XMLNode>("derived_from"));

   for (vector<string>::iterator section = sections.begin();
        section != sections.end();
        section++)
       {
       XMLNode::iterator parentData = find_if(initData->begin(),
                                              initData->end(),
                                              EqualToName<XMLNode>(*section));
       if (parentData != initData->end())
          if (readFromSection(initData, parentData, parName, returnValue))
             return true;
       }
   return false;
   }


void CMPComponentInterface::expose(const std::string& name,
                                   const std::string& units,
                                   const std::string& description,
                                   bool writable,
                                   Packable* variable)
   {
   // -----------------------------------------------------------------------
   // Export a variable. The variable passed in is stored directly
   // in our map so the assumption is that we are now owners.
   // ie. don't delete this data object elsewhere!
   // -----------------------------------------------------------------------
   RegistrationKind kind = respondToGetReg;
   if (writable)
      kind = respondToGetSetReg;
   if (nameToRegistrationID(name, respondToGetReg) == 0)
      RegisterWithPM(name, units, description, kind, variable);
   else
      delete variable;
   }

void CMPComponentInterface::subscribe(const std::string& name, Packable* handler)
   // -----------------------------------------------------------------------
   // Subscribe to an event
   // -----------------------------------------------------------------------
   {
   if (Str_i_Eq(name, "init1"))
      init1 = handler;
   else if (Str_i_Eq(name, "init2"))
      init2.push_back( handler );

   RegisterWithPM(name, "", "", respondToEventReg, handler);
   }

void CMPComponentInterface::publish(const std::string& name, Packable* data)
   // -----------------------------------------------------------------------
   // Publish an event
   // -----------------------------------------------------------------------
   {
   int id = nameToRegistrationID(name, eventReg);
   bool alreadyRegistered = (id != 0);
   if (!alreadyRegistered)
      id = RegisterWithPM(name, "", "", eventReg, data);

	PublishEventType publishEvent;
	publishEvent.ID = id;
	publishEvent.ddml = data->ddml();
	Message& publishEventMessage = constructMessage(Message::PublishEvent, componentID, parentID, false,
                                                   memorySize(publishEvent) + data->memorySize());
   MessageData publishEventMessageData(publishEventMessage);
   pack(publishEventMessageData, publishEvent);
   data->pack(publishEventMessageData);
	sendMessage(publishEventMessage);

   if (alreadyRegistered)
      delete data;
   }

void CMPComponentInterface::query(const std::string& pattern, std::vector<QueryMatch>& matches)
   {
   // -----------------------------------------------------------------------
   // Return a list of all variables or components (fully qualified)
   // that match the specified pattern.
   // e.g. * will return a list of all components.
   //      wheat.* will return a list of all variables for the wheat module
   //      *.lai will return a list of all lai variables for all modules.
   // -----------------------------------------------------------------------

   clearMessages();

   // the report component sometimes passes through an array specifier
   // e.g. tt_tot() - need to remove that before sending message to PM.
   ArraySpecifier* arraySpecifier = ArraySpecifier::create(pattern);
   string nameWithoutArraySpec = pattern;
   if (arraySpecifier != NULL)
      nameWithoutArraySpec = arraySpecifier->variableName();

   QueryInfoType queryInfo;
   queryInfo.name = nameWithoutArraySpec;

   // work out if we dealing with a list of components or a list of variables.
   unsigned posPeriod = pattern.find('.');
   if (posPeriod != string::npos)
      queryInfo.kind = 2;
   else
      queryInfo.kind = 7;
   sendMessage(newMessage(Message::QueryInfo, componentID, parentID, false, queryInfo));

   matches.erase(matches.begin(), matches.end());
	for (unsigned i = 0; i != messages.size(); i++)
      {
		ReturnInfoType returnInfo;
      MessageData returnInfoData(*messages[i]);
		unpack(returnInfoData, returnInfo);
      QueryMatch queryMatch;
      queryMatch.name = returnInfo.name;
      queryMatch.ddml = returnInfo.type;

      if (getAttributeFromXML(queryMatch.ddml, "array") == "T" &&
          arraySpecifier != NULL)
         arraySpecifier->adornVariableName(queryMatch.name);

      matches.push_back(queryMatch);
      }

   if (arraySpecifier != NULL)
      delete arraySpecifier;
   }

void CMPComponentInterface::write(const std::string& msg)
   {
   // -----------------------------------------------------------------------
   // write a message to the summary stream.
   // -----------------------------------------------------------------------
   if (!haveWrittenToStdOutToday)
      {
      if (tick.startday != 0)
         {
         GDate gDate;
         gDate.Set(tick.startday);
         gDate.Set_write_format("D MMMMMM YYYY");
         gDate.Write(cout);
         cout << "(Day of year=" << gDate.Get_day_of_year() << ")" << ", ";
         }
      cout << getName();
      cout << ": " << endl;
      haveWrittenToStdOutToday = true;
      }
   cout <<  "     " << msg;
   }


void CMPComponentInterface::clearMessages()
	{
   // -----------------------------------------------------------------------
   // Clear all messages in our message list.
   // -----------------------------------------------------------------------
	for (unsigned i = 0; i != messages.size(); i++)
		deleteClonedMessage(messages[i]);
	messages.erase(messages.begin(), messages.end());
	}

void CMPComponentInterface::sendMessage(Message& message)
   {
   // -----------------------------------------------------------------------
   // Send a message to PM
   // -----------------------------------------------------------------------
   if (messageCallback != NULL)
      (*messageCallback)(callbackArg, message);
   deleteMessage(message);
   }


int CMPComponentInterface::nameToRegistrationID(const std::string& name,
                                                RegistrationKind regKind)
   // -----------------------------------------------------------------------
   // Return a registration id for the specified
   // name.
   // -----------------------------------------------------------------------
   {
   string FullRegName = name + itoa(regKind);
   NameToRegMap::iterator reg = regNames.find(FullRegName);
   if (reg == regNames.end())
      return 0;
   else
      return (int) reg->second;
   }
int CMPComponentInterface::RegisterWithPM(const string& name, const string& units,
                                          const string& description,
                                          RegistrationKind regKind,
                                          Packable* data)
   // -----------------------------------------------------------------------
   // Register something with our PM given the specified information.
   // The data passed in is stored directly in our map so the assumption
   // is that we are now owners.
   // ie. don't delete this data object elsewhere!
   // -----------------------------------------------------------------------
   {
   string ddml = data->ddml();
   if (units != "")
      addAttributeToXML(ddml, "unit=\"" + units + "\"");
   if (description != "")
      addAttributeToXML(ddml, "description=\"" + description + "\"");

   // Add new object to our map.
   string fullRegName = name + itoa(regKind);
   Reg* reg = new Reg;
   reg->data = data;
   reg->kind = regKind;
   reg->ddml = ddml;
   regNames.insert(make_pair(fullRegName, reg));
   int ID = (int) reg;

   // send register message to PM.
   RegisterType registerData;
   registerData.kind = regKind;
   registerData.ID = ID;
   registerData.destID = 0;
   registerData.name = name;
   registerData.ddml = ddml;

   sendMessage(newMessage(Message::Register, componentID, parentID, false,
                          registerData));
   return ID;
   }
void CMPComponentInterface::deRegisterAll()
   // -----------------------------------------------------------------------
   // -----------------------------------------------------------------------
   {
   for (NameToRegMap::iterator reg = regNames.begin();
                               reg != regNames.end();
                               reg++)
      {
      DeRegisterType deregistration;
      deregistration.ID = (int) reg->second;
      deregistration.kind = reg->second->kind;
      sendMessage(newMessage(Message::Deregister, componentID, parentID, false,
                             deregistration));
      delete reg->second;
      }
   regNames.clear();
   clearMessages();
   }


void CMPComponentInterface::error(const string& errorMessage, bool isFatal)
   // -----------------------------------------------------------------------
   // Called to signal an error to the PM.
   // -----------------------------------------------------------------------
	{
	string msg;
	msg += errorMessage + "\n";
	msg += "Component name: " + name + "\n";

   ErrorType errorData;
	errorData.msg = msg;
	errorData.isFatal = isFatal;
	publish("error", new PackableWrapper< ErrorType >(errorData));
	if (isFatal)
		{
		terminate();
		errorHasOccurred = true;
		}
	}

void CMPComponentInterface::onInit1(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for Init1 message.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
   Init1Type init1;
   unpack(messageData, init1);
   // get instance name from fqn.
   unsigned posPeriod = init1.fqn.rfind('.');
   if (posPeriod == string::npos)
      {
      name = init1.fqn;
      pathName = ".";
      }
   else
      {
      name = init1.fqn.substr(posPeriod+1);
      pathName = init1.fqn.substr(0, posPeriod);
      }
   expose("name", "", "", false, new CMPBuiltIn<string&>(name));
   expose("type", "", "", false, new CMPBuiltIn<string&>(componentType));
   expose("version", "", "", false, new CMPBuiltIn<string&>(version));
   expose("author", "", "", false, new CMPBuiltIn<string&>(author));
   expose("active", "", "", false, new CMPBuiltIn<int&>(active));
   expose("state", "", "", false, new CMPBuiltIn<string&>(state));

   simScript = new XMLDocument(init1.sdml, XMLDocument::xmlContents);
   if (this->init1 != NULL)
      this->init1->unpack(messageData, "");
   }

void CMPComponentInterface::onInit2(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for Init2 message.
   // -----------------------------------------------------------------------
   {
   for (unsigned i = 0; i != init2.size(); i++)
      {
      MessageData Data(message);
      init2[i]->unpack(Data, "");
      }
   tickID = nameToRegistrationID("tick", respondToEventReg);
   }

void CMPComponentInterface::onQueryValue(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for all QueryValue messages.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
	QueryValueType queryValue;
   unpack(messageData, queryValue);
   Reg* reg = (Reg*) queryValue.ID;
   Packable& data = *(reg->data);

	ReplyValueType replyValue;
	replyValue.queryID = message.messageID;
	replyValue.ddml = data.ddml();

	Message& replyValueMessage = constructMessage(Message::ReplyValue,
	                                              componentID,
	                                              message.from,
	                                              false,
                                                 memorySize(replyValue) + data.memorySize());
   MessageData replyValueMessageData(replyValueMessage);
   pack(replyValueMessageData, replyValue);
   data.pack(replyValueMessageData);
	sendMessage(replyValueMessage);
   }


void CMPComponentInterface::onQuerySetValue(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for all QuerySetValue messages.
   // -----------------------------------------------------------------------
   {
   unsigned int fromID = message.from;
   MessageData messageData(message);
	QuerySetValueType querySetValue;
   unpack(messageData, querySetValue);
   Reg* reg = (Reg*) querySetValue.ID;
   Packable& data = *(reg->data);

   // change the value of the variable.
   data.unpack(messageData, querySetValue.ddml);

   // now send back a replySetValueSuccess message.
   //NBNBNB can't seem to build a ReplySetValueSuccessType structure, so use a similar one YUCK!!
   NotifySetValueSuccessType replySetValueSuccess;
   replySetValueSuccess.ID = message.messageID;
   replySetValueSuccess.success = true;
   sendMessage(newMessage(Message::ReplySetValueSuccess,
                          componentID, fromID, false,
                          replySetValueSuccess));
   }
void CMPComponentInterface::onEvent(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for all Event messages.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
	EventType event;
   unpack(messageData, event);
   Reg* reg = (Reg*) event.ID;
   Packable& data = *(reg->data);

   if (event.ID == tickID)
      {
      unpack(messageData, tick);
      messageData.reset();
      unpack(messageData, event);
      haveWrittenToStdOutToday = false;
      }
   // unpack the data - this will unpack and then call the function.
   data.unpack(messageData, event.ddml);
   }

void CMPComponentInterface::terminate(void)
   {
   // -----------------------------------------------------------------------
   // Terminate the simulation.
   // -----------------------------------------------------------------------
   Null n;
   sendMessage(newMessage(Message::TerminateSimulation, componentID, parentID, false, n));
   }

std::string CMPComponentInterface::getName() {return name;}
std::string CMPComponentInterface::getFQName() {return (pathName + "." + name);}

// Fake a system registration (that we will publish an event later) so that
// the variable name probing works
void CMPComponentInterface::notifyFutureEvent(const std::string& name)
   {
   RegisterWithPM(name, "", "", eventReg, new PackableWrapper< Null >(*new Null));
   }


std::string CMPComponentInterface::getPropertyDescription(NameToRegMap::iterator reg, const string& access)
   {
   string returnString = "   <property name=\"";
   returnString += getRegName(reg);
   returnString += "\" access=\"" + access + "\" init=\"F\">\n";
   if (reg->second != NULL) {returnString += reg->second->ddml;}
   returnString += "\n</property>\n";
   return returnString;
   }

std::string CMPComponentInterface::getRegName(NameToRegMap::iterator reg)
   {
   string name = reg->first;
   name.erase(name.length() - 1);
   return name;
   }
std::string CMPComponentInterface::getEventDescription(NameToRegMap::iterator reg, const string& published)
   {
   string returnString = "   <event name=\"";
   returnString += getRegName(reg);
   returnString += "\" kind=\"" + published + "\">\n";
   if (reg->second != NULL)
      {
      XMLDocument* doc = new XMLDocument(reg->second->ddml, XMLDocument::xmlContents);
      returnString += doc->documentElement().innerXML();
      delete doc;
      }
   returnString += "\n</event>\n";
   return returnString;
   }

std::string CMPComponentInterface::getDescription(const std::string& dllName)
   {
   std::string returnString;
   try
      {
      returnString = "<describecomp>\n";

      returnString += string("<executable>") + dllName + "</executable>\n";
      returnString += string("<class>") + name + "</class>\n";
	  returnString += "<version>1.0</version>\n";
      returnString += "<author>APSRU</author>\n";

	  for (NameToRegMap::iterator reg = regNames.begin();
								  reg != regNames.end();
                                  reg++)
         {
         RegistrationKind Kind = reg->second->kind;
         if (Kind == respondToGetReg)
            returnString += getPropertyDescription(reg, "read");
         else if (Kind == respondToSetReg)
            returnString += getPropertyDescription(reg, "write");
         else if (Kind == respondToGetSetReg)
            returnString += getPropertyDescription(reg, "both");
         else if (Kind == respondToEventReg)
            returnString += getEventDescription(reg, "subscribed");
         else if (Kind == eventReg)
            returnString += getEventDescription(reg, "published");
         else if (Kind == getReg)
            {
            returnString += "   <driver name=\"";
            returnString += getRegName(reg);
            returnString += "\">\n";
            if (reg->second != NULL) {returnString += reg->second->ddml;}
            returnString += "\n</driver>\n";
            }
         }
      }
   catch (const exception& err)
      {
      returnString += string("<ERROR>") + err.what() + "</ERROR>\n";
      }
   returnString += "</describecomp>\n";
   return returnString;
   }

