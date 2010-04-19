#include <General/pch.h>

#include <assert.h>

#include <sstream>
#include <iomanip>
#include <list>
#include <functional>
#include <memory>
#include <stack>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/path.h>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimDataTypeData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimRegistry.h>
#include <ApsimShared/ApsimRegistration.h>
#include <ApsimShared/ApsimSimulationFile.h>
#include <ApsimShared/ApsimVersion.h>
#include <ApsimShared/ApsimDirectories.h>
#include <ApsimShared/ApsimSettings.h>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/Variant.h>

#include "ComponentAlias.h"
#include "Coordinator.h"

vector<int> Coordinator::componentOrders;

using namespace std;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }

// ------------------------------------------------------------------
//  Short description:
//    Create an instance of our PM.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new Coordinator();
   }

// ------------------------------------------------------------------
//  Short description:
//    Constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Coordinator::Coordinator(void)
   {
   runningMessageID = 0;
   afterInit2 = false;
   doTerminate = false;
   printReport = false;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Coordinator::~Coordinator(void)
   {
   for (Components::iterator i = components.begin();
                             i != components.end();
                             i++)
      delete i->second;
   }
// ------------------------------------------------------------------
//  Short description:
//    do init1 stuff.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::doInit1(const protocol::Init1Data &init1Data)
   {
   if (componentID == parentID) {cout << "Version                = " + getApsimVersion() << endl;}

   protocol::Component::doInit1(init1Data);

   string sdmlString = string(init1Data.sdml.f_str(), init1Data.sdml.length());
   ApsimSimulationFile simulationData(sdmlString, true);


   if (componentID == parentID)
      {
      title = simulationData.getTitle();
      cout << "Title                  = " + title << endl;
      titleID = addRegistration(::respondToGet, componentID,  "title", "<type kind=\"string\"/>");
      componentsID = addRegistration(::respondToGet, componentID, "components", "<type kind=\"string\" array=\"T\"/>");
      }
   else
      {
      //cout << "Paddock \"" << getName() << "\":" << endl;
      cout << "Paddock:" << endl;
      }
   printReport = simulationData.doPrintReport();
   readAllRegistrations();

   // loop through all components specified in SDML and create
   // and add a componentAlias object to our list of components.
   std::vector<string> componentNames;
   simulationData.getComponentNames(componentNames);
   for (std::vector<string>::iterator componentI = componentNames.begin();
                                      componentI != componentNames.end();
                                      componentI++)
      {
      ApsimComponentData component = simulationData.getComponent(*componentI);

      string dll = component.getExecutableFileName();
#ifdef __WIN32__
      // Convert unix style paths to native DOS format
      Replace_all(dll, "/", "\\");
#endif
      ApsimSettings::addMacro(dll);
      cout << "   Component " << setw(30) << ("\"" + component.getName() + "\"") << " = " + dll << endl;

      addComponent(component.getName(),
                   component.getExecutableFileName(),
                   component.getComponentInterfaceFileName(),
                   component.getXML());
      }

   // loop through all systems specified in SDML and create
   // and add a componentAlias object to our list of components.
   std::vector<string> systemNames;
   simulationData.getSystemNames(systemNames);
   for (std::vector<string>::iterator systemI = systemNames.begin();
                                      systemI != systemNames.end();
                                      systemI++)
      {
      ApsimSystemData system = simulationData.getSystem(*systemI);
      addComponent(system.getName(),
                   system.getExecutableFileName(),
                   "",
                   system.getXML());
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    initialise this coordinator.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::doInit2(void)
   {
   protocol::Component::doInit2();

   // resolve all registrations.
   afterInit2 = true;

   // initialise all components.
   if (componentID == parentID)
      ApsimRegistry::getApsimRegistry().getComponents(componentOrders);

   for (Components::iterator componentI = components.begin();
                             componentI != components.end() && !doTerminate;
                             componentI++)
      {
      if (componentI->second->ID != componentID &&
          componentI->second->ID != parentID)
         sendMessage(protocol::newInit2Message(componentID, componentI->second->ID));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Send start message to sequencer

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::doCommence(void)
   {
   if (componentID == parentID)
      {
      // only as top level PM
      cout << "------- Start of simulation  --------------------------------------------------" << endl;

      // send the commence message on to the sequencer.
      sendMessage(protocol::newCommenceMessage(componentID, sequencerID));
      }
   }


// ------------------------------------------------------------------
//  Short description:
//    Add a component to this coordinator (system)

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::addComponent(const string& compName,
                               const string& compExecutable,
                               const string& componentInterfaceExecutable,
                               const string& compSdml)
   {
   // get a unique id for the component we're about to create.
   sendMessage(protocol::newRequestComponentIDMessage(componentID,
                                                      parentID,
                                                      componentID,
                                                      compName.c_str()));

   /* the system sets childComponentID and name here */

   ComponentAlias* componentAlias = NULL;
   try
      {
      componentAlias = new ComponentAlias(compName,
                                          compExecutable,
                                          componentInterfaceExecutable,
                                          childComponentID,
                                          componentID);

      components.insert(Components::value_type(childComponentID, componentAlias));

      // Tell the registry about this component.
      ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
      string fqn = getFQName();
      fqn += ".";
      fqn += compName;

      // assume it's foreign until we know more about it..
      string componentType = compExecutable;
      replaceAll(componentType, "/", "\\");
      componentType = fileTail(fileRoot(componentType));
      registry.addComponent(componentID, childComponentID, fqn, componentType);
      registry.setForeignTaint(childComponentID);

      // send component an init1 message.
      sendMessage(protocol::newInit1Message(componentID,
                                            childComponentID,
                                            compSdml.c_str(),
                                            fqn.c_str(),
                                            true));

      }
   catch (const runtime_error& error)
      {
      if (componentAlias) delete componentAlias;
      components.erase(childComponentID);
      /*registry.deleteComponent(childComponentID);*/
      throw;
      }

   // dph hack - shouldn't hardwire clock as sequencer.
   if (Str_i_Eq(compName, "clock"))
      sequencerID = childComponentID;

   }

// ------------------------------------------------------------------
//  Short description:
//    we have requested a unique ID from the system, and
//    a returnComponentID message has come in.  Store ID and name.
// ------------------------------------------------------------------
void Coordinator::onReturnComponentIDMessage(protocol::ReturnComponentIDData& data)
   {
   childComponentID = data.ID;
   childComponentName = asString(data.fqdn);
   }
// ------------------------------------------------------------------
//  Short description:
//    A child has asked us for a unique identifier

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onRequestComponentIDMessage(unsigned int fromID,
                                              protocol::RequestComponentIDData& data)
   {
   static unsigned newID = 1/* masterPMID*/;
   sendMessage(protocol::newReturnComponentIDMessage(componentID,
                                           fromID,
                                           "", // Wrong. This should be the FQ nameof the new component FIXME!!
                                           ++newID));
   }

// ------------------------------------------------------------------
//  Short description:
//    handle incoming registration messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onRegisterMessage(unsigned int fromID,
                                    protocol::RegisterData& registerData)
   {
   // A Component is registering an ID with the system. An "apsim"
   // component will have already created a global registry entry,
   // and we can safely ignore this message.
   // Other components are passing in a unique ID that relates
   // to some unknown internal mechanism. Create a foreign
   // entry for these, and mark that component for special
   // treatment elsewhere.

   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();

   string regName = asString(registerData.name);
   size_t pos = regName.rfind(".");

   if (pos != string::npos)
      {
      // strip off any module name at the start, convert it to an ID
      string comp = regName.substr(0,pos);
      if (comp[0] != '.') comp = getFQName() + "." + comp;

      int id = -1;
      componentNameToID(comp, id);

      if (id <= 0)
         throw std::runtime_error(string ("Unknown module \"") + comp + "\"");

      if (registerData.destID > 0 && id != registerData.destID)
        {
        throw std::runtime_error(string ("Mismatched destID in Coordinator::onRegisterMessage (") +
                                         regName +
                                         " from " +
                                         registry.componentByID(fromID) +
                                         ")") ;
        }
      registerData.destID = id;
      regName = regName.substr(pos+1);
      }

   if (registry.find((EventTypeCode)registerData.kind, fromID, registerData.ID) == NULL)
      {
      ApsimRegistration *newReg = registry.createForeignRegistration(
                            (EventTypeCode)registerData.kind,
                             regName,
                             asString(registerData.type),
                             registerData.destID,
                             fromID,
                             registerData.ID);
      registry.add(newReg);
      }
    else
      {
      /* nothing to do */
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    handle incoming deregistration messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onDeregisterMessage(unsigned int fromID, protocol::DeregisterData& deregisterData)
   {
   ApsimRegistry &r = ApsimRegistry::getApsimRegistry();
   r.erase((EventTypeCode)deregisterData.kind, fromID, deregisterData.ID);
   }

// ------------------------------------------------------------------
// Handle incoming publish event messages.
// ------------------------------------------------------------------
void Coordinator::onPublishEventMessage(unsigned int fromID, protocol::PublishEventData& publishEventData)
   {
   if (!doTerminate)
      {
      ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
      ApsimRegistration* regItem = registry.find(::event, fromID, publishEventData.ID);
      if (regItem == NULL) {
         string msg = "Invalid registration. offender=";
          msg += registry.componentByID(fromID);
          msg += ",";
          msg += publishEventData.ID;
         throw std::runtime_error(msg);
      }
      if (Str_i_Eq(regItem->getName(), "error"))
         {
         protocol::ErrorData errorData;
         publishEventData.variant.unpack(NULL, NULL, errorData);
         string fromComponentName;
         if (components.find(fromID) != components.end())
            fromComponentName = components[fromID]->getName();
         onError(fromComponentName,
                 asString(errorData.errorMessage),
                 errorData.isFatal);
         }
      propogateEvent(fromID, publishEventData);
      }
   }
// ------------------------------------------------------------------
// Handle incoming publish event messages.
// ------------------------------------------------------------------
void Coordinator::propogateEvent(unsigned int fromID, protocol::PublishEventData& publishEventData)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   ApsimRegistration *reg = registry.find(::event, fromID, publishEventData.ID);
   if (reg == NULL) throw std::runtime_error("NULL in Coordinator::propogateEvent ?");

   vector<ApsimRegistration *> subscriptions;
   registry.lookup(reg, subscriptions);

   reorderSubscriptions(subscriptions);

   for (vector<ApsimRegistration *>::iterator s = subscriptions.begin();
                                              s != subscriptions.end() && !doTerminate;
                                              s++)
      {
      // if the event is going to our parent then we need to say that it is
      // coming from us rather than our child.
      if ((*s)->getComponentID() == parentID)
         fromID = componentID;
      //cout << "propogate " << reg->getName() << " to " << registry.componentByID((*s)->getComponentID()) << endl;
      sendMessage(protocol::newEventMessage(componentID,
                                  (*s)->getComponentID(),
                                  (*s)->getRegID(),
                                  fromID,
                                  publishEventData.variant));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    handle incoming terminate simulation messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onTerminateSimulationMessage(void)
   {
   if (componentID != parentID)
      sendMessage(protocol::newTerminateSimulationMessage(componentID, parentID));
   else
      notifyTermination();
   doTerminate = true;
   }
// ------------------------------------------------------------------
// handle incoming getValue messages.
// ------------------------------------------------------------------
void Coordinator::onGetValueMessage(unsigned int fromID, protocol::GetValueData& getValueData)
   {
/*   if (!afterInit2)
      {
      string msg = "Cannot do GET's before the INIT2.\n";
      msg += "Variable name: ";
      ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
      ApsimRegistration *reg = registry.find(fromID, getValueData.ID);
      if (reg)
        msg += reg->getName();

      error(msg, true);
      }
   else
      {
*/
//    cout << "onGetValueMessage, id=" << getValueData.ID << endl;
      sendQueryValueMessage(fromID, getValueData.ID);
/*      }*/
   }
// ------------------------------------------------------------------
// Send queryValue messages to all subscribed components.
// ------------------------------------------------------------------
void Coordinator::sendQueryValueMessage(unsigned fromID, unsigned regID)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   ApsimRegistration *reg = registry.find(::get, fromID, regID);

   if (reg == NULL)
      return;

   pollComponentsForGetVariable(fromID, reg->getDestinationID(), reg->getName());

   vector<ApsimRegistration *> subscriptions;
   registry.lookup(reg, subscriptions);

   previousGetValueCompID.push(fromID);
   previousGetValueRegID.push(regID);
   for (vector<ApsimRegistration *>::iterator s = subscriptions.begin();
                                              s != subscriptions.end();
                                              s++)
      {
      sendMessage(protocol::newQueryValueMessage(componentID,
                                                 (*s)->getComponentID(),
                                                 (*s)->getRegID(),
                                                 fromID));
      }
   previousGetValueCompID.pop();
   previousGetValueRegID.pop();
   }
// ------------------------------------------------------------------
// Send a returnValue message back to originating component.
// ------------------------------------------------------------------
void Coordinator::onReplyValueMessage(unsigned fromID, protocol::ReplyValueData replyValueData)
   {
   int toID = previousGetValueCompID.top();

   if (toID == parentID || components[toID]->isSystem())
      {
      sendMessage(protocol::newReplyValueMessage(componentID,
                                       toID,
                                       previousGetValueRegID.top(),
                                       replyValueData.variant));
      }
   else
      {
      sendMessage(protocol::newReturnValueMessage(componentID,
                                        toID,
                                        fromID,
                                        previousGetValueRegID.top(),
                                        replyValueData.variant));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    handle the incoming query info message.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onQueryInfoMessage(unsigned int fromID,
                                     unsigned int messageID,
                                     protocol::QueryInfoData& queryInfo)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   std::vector<ApsimRegistration *> matches;

   int queryComponentID;
   string queryName;
   registry.unCrackPath(fromID, asString(queryInfo.name), queryComponentID, queryName);

   if (queryInfo.kind == protocol::respondToGetInfo)
      {
      NativeRegistration reg(::get,
                             queryName,
                             "<undefined/>",
                             queryComponentID,
                             fromID);

      pollComponentsForGetVariable(fromID, queryComponentID, queryName);
      registry.lookup(&reg, matches);
      }
   else if (queryInfo.kind == protocol::respondToSetInfo)
      {
      NativeRegistration reg(::set,
                             queryName,
                             "<undefined/>",
                             queryComponentID,
                             fromID);
      registry.lookup(&reg, matches);
      }
   else if (queryInfo.kind == protocol::respondToEventInfo)
      {
      NativeRegistration reg(::event,
                             queryName,
                             "<undefined/>",
                             queryComponentID,
                             fromID);
      registry.lookup(&reg, matches);
      }
   else if (queryInfo.kind == protocol::componentInfo)
      {
      int childID;
      if (Is_numerical(queryName.c_str()))
         {
         childID = atoi(queryName.c_str());
         if (components.find(childID) != components.end())
            queryName = components[childID]->getName();
         if (childID == componentID)
            queryName = getName();
         }
      else
         componentNameToID(queryName.c_str(), childID);

      if (childID == INT_MAX || queryName == "") return; // XX Yuck!!

      string fqn = getName();
      fqn += ".";
      fqn += queryName;

      sendMessage(protocol::newReturnInfoMessage(
                                       componentID,
                                       fromID,
                                       messageID,
                                       childID,
                                       childID,
                                       fqn.c_str(),
                                       " ",
                                       queryInfo.kind));
      }

   for (unsigned i = 0; i != matches.size(); i++)
      {
      std::string matchName;
      std::string componentName;

      if (matches[i]->getComponentID() == componentID)
         componentName = getName();
      else if (components.find(matches[i]->getComponentID()) != components.end())
         componentName = components[matches[i]->getComponentID()]->getName();

      if (componentName == "")
         matchName = matches[i]->getName();
      else
         matchName = componentName + "." + matches[i]->getName();

      sendMessage(protocol::newReturnInfoMessage(
                                       componentID,
                                       fromID,
                                       messageID,
                                       matches[i]->getComponentID(),
                                       matches[i]->getRegID(),
                                       matchName.c_str(),
                                       matches[i]->getDDML().c_str(),
                                       queryInfo.kind));
      }
   }

// ------------------------------------------------------------------
// Handle the incoming requestSetValue message. This is sent by a component
// trying to set the value in another. We decide how it gets routed.
// ------------------------------------------------------------------
void Coordinator::onRequestSetValueMessage(unsigned int fromID,
                                           protocol::RequestSetValueData& setValueData)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   ApsimRegistration *reg = registry.find(::set, fromID, setValueData.ID);

   if (reg == NULL) throw std::runtime_error("NULL registration in Coordinator::onRequestSetValueMessage ?");

   vector<ApsimRegistration *> subs;
   registry.lookup(reg,  subs);

   bool mayNeedToPoll = (subs.size() == 0);      /* no prior registrations */

   if (mayNeedToPoll)
      {
      // apsim hack to poll modules for variables.  This is because we haven't
      // yet got all the .interface files up to date.
      string fqn;
      if (reg->getDestinationID() <= 0)
         fqn = itoa(componentID) + string(".*.") + reg->getName();
      else
         fqn = itoa(reg->getDestinationID()) + string(".") + reg->getName();

      if (variablesBeenPolledForSets.find(fqn) == /* hasn't been seen before */
          variablesBeenPolledForSets.end())
         {
         variablesBeenPolledForSets.insert(fqn);
         if (reg->getDestinationID() <= 0)
            {
            /* ask everybody whether it belongs to them */
            vector<int> candidates;
            registry.getSiblingsAndParents(fromID, candidates);
            for(unsigned i = 0; i != candidates.size(); i++)
               {
               if (candidates[i] != 1 /*masterPMID*/ &&
                   candidates[i] != componentID)
                  {
                  sendMessage(protocol::newApsimSetQueryMessage(componentID,
                                                   candidates[i],
                                                   reg->getName().c_str(),
                                                   fromID,
                                                   reg->getRegID(),
                                                   setValueData.variant));
                  }
               }
            }
         else
            {
            /* send to directed module */
            sendMessage(protocol::newApsimSetQueryMessage(componentID,
                                             reg->getDestinationID(),
                                             reg->getName().c_str(),
                                             fromID,
                                             reg->getRegID(),
                                             setValueData.variant));
            }
         // that ApsimSetQuery message will set on success - no need for any more here. Next
         // time through, it will get picked up as a subscription.
         return;
         }
      // Hmmm. We polled once for this variable and failed. Don't bother any more
      }

   if (subs.size() == 0)
      {
      string msg = "No module allows a set of the variable " +
           reg->getName();
      error(msg, true);
      }
   else if (subs.size() == 1)
      {
//      cout <<  "sending newQuerySetValueMessage, n="<<reg->getName() << ", to " << registry.componentByID(subs[0]->getComponentID()) <<"\n";
      sendMessage(newQuerySetValueMessage(fromID,
                                          subs[0]->getComponentID(),
                                          subs[0]->getRegID(),
                                          setValueData.variant));
      }
   else if (subs.size() > 1)
      {
      string msg =  "Too many modules allow a set of the variable "+
              reg->getName();
      error(msg, true);
      }
   }
// ------------------------------------------------------------------
// process the querySetValueMessage.
// ------------------------------------------------------------------
void Coordinator::onQuerySetValueMessage(unsigned fromID, protocol::QuerySetValueData& querySetData, unsigned msgID)
   {
   // nothing
//   cout <<" Coordinator::onQuerySetValueMessage\n";
   }

// ------------------------------------------------------------------
// apsim hack to poll modules for gettable variables.  This is because we haven't
// yet got all the .interface files up to date.
// ------------------------------------------------------------------
void Coordinator::pollComponentsForGetVariable(int fromID,
                                               int destID,
                                               const string& variableName)
   {
   string fqn;

   if (destID <= 0)
     {
     char buffer[100];
     sprintf(buffer, "%d", fromID);
     strcat(buffer, "*.");
     strcat(buffer, variableName.c_str());
     fqn = buffer;
     //fqn = itoa(fromID) + string("*.") + lowerName;
     }
   else
     {
     char buffer[100];
     sprintf(buffer, "%d", destID);
     strcat(buffer, ".");
     strcat(buffer, variableName.c_str());
     fqn = buffer;
     //fqn = itoa(destID) + string(".") + lowerName;
     }

   bool havePolled = (variablesBeenPolledForGets.find(fqn) !=
                        variablesBeenPolledForGets.end());
   if (!havePolled)
      {
      variablesBeenPolledForGets.insert(fqn);

      ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
      vector<int> candidates;

      if (destID <= 0)
        registry.getSiblingsAndParents(fromID, candidates);
      else
        candidates.push_back(destID);

      string lowerName = variableName;
      To_lower(lowerName);

      for(unsigned i = 0; i != candidates.size(); i++)
         {
         if (candidates[i] != componentID)
            {
            sendMessage(protocol::newApsimGetQueryMessage(componentID,
                                                          candidates[i],
                                                          lowerName.c_str()));
            }
         }
      }
   }
// ------------------------------------------------------------------
// Called by component (CANOPY) to change the order that events are sent
// to a module.
//     Modules are swapped thus:
//      e.g. if componentNames[0] = 'x',
//              componentNames[1] = 'y',
//              componentNames[2] = 'z'
//         Then y will replace x, z will replace y, and x will replace z
//         leaving: y, z, x
// ------------------------------------------------------------------
void Coordinator::onApsimChangeOrderData(unsigned int fromID, protocol::MessageData& messageData)
   {
   vector<string> componentNames;
   vector<int> componentIDs;
   messageData >> componentNames;

   for (unsigned i = 0; i != componentNames.size(); ++i)
      {
      int id = -1;
      string junk;
      ApsimRegistry::getApsimRegistry().
                       unCrackPath(fromID,
                                   componentNames[i] + ".junk",
                                   id, junk); // fixme - there should be an easier way to do this!!

      if (id <= 0)
         {
         string msg = "The CANOPY module has specified that " + componentNames[i] +
                      " be intercropped\nbut that module doesn't exist in the simulation.";
         error(msg, true);
         return;
         }
      componentIDs.push_back(id);
      }

   // Swap positions of nominated modules.
   for (unsigned i = 1; i < componentIDs.size(); i++)
     {
     vector<int>::iterator a, b;
     a = find(componentOrders.begin(), componentOrders.end(), componentIDs[i-1]);
     b = find(componentOrders.begin(), componentOrders.end(), componentIDs[i]);
     if (a != componentOrders.end() && b != componentOrders.end())
        {
        swap(*a, *b);
        }
     }
   }
// ------------------------------------------------------------------
// Handle incoming publish event message but make sure the order
// of events is the same as that specified in the componentOrder.
// ------------------------------------------------------------------
void Coordinator::reorderSubscriptions(std::vector<ApsimRegistration *>& subs)
   {
   vector<ApsimRegistration *> subsToMove = subs;
   vector<ApsimRegistration *> newSubs;
   for (unsigned o = 0; o != componentOrders.size(); o++)
      {
      bool more = true;
      while (more)
         {
         more = false;
         for (vector<ApsimRegistration *>::iterator s = subsToMove.begin();
                                                    s != subsToMove.end();
                                                    s++)
            {
            if ((*s)->getComponentID() == componentOrders[o])
               {
               newSubs.push_back(*s);
               subsToMove.erase(s);
               more = true;
               break;
               }
            }
         }
      }
   if (subsToMove.size() > 0) throw std::runtime_error("leftover subscription in Coordinator::reorderSubscriptions");
   subs = newSubs;
   }
// ------------------------------------------------------------------
// read all registrations for this Component.
// ------------------------------------------------------------------
void Coordinator::readAllRegistrations(void)
   {
   }
// ------------------------------------------------------------------
// respond to a event that has happened.
// ------------------------------------------------------------------
void Coordinator::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   protocol::PublishEventData publishEventData;
   publishEventData.ID = eventID;
   publishEventData.variant = variant;

   // need to work out if this event has come from outside this system or
   // from one of our child components.  If from outside then set the
   // fromID to point to our parent.
   unsigned foreignComponentID = fromID;
   if (components.find(fromID) == components.end())
      foreignComponentID = parentID;
   propogateEvent(foreignComponentID, publishEventData);
   }
// ------------------------------------------------------------------
// return one of our variables to caller
// ------------------------------------------------------------------
void Coordinator::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryValueData)
   {
   if ((int)queryValueData.ID == titleID)
      sendVariable(queryValueData, title);
   else if ((int)queryValueData.ID == componentsID)
      {
      std::vector<string> comps;
      for (Components::iterator c = components.begin();
                                c != components.end();
                                c++)
         {
         string dll = c->second->getExecutable();
         if (dll != "")
            comps.push_back(dll);
         }
      sendVariable(queryValueData, comps);
      }
   else
      sendQueryValueMessage(fromID, queryValueData.ID);
   }
// ------------------------------------------------------------------
// respond to a method call request
// ------------------------------------------------------------------
bool Coordinator::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   throw std::runtime_error("Coordinator::respondToSet called??");
//   if (components.find(fromID) == components.end())
//      sendQuerySetValueMessage(parentID, fromID,
//                               setValueData.ID, setValueData.ID,
//                               setValueData.variant);
//   else
//      sendQuerySetValueMessage(fromID, fromID,
//                               setValueData.ID, setValueData.ID,
//                               setValueData.variant);
//   return getSetVariableSuccess();
   }
// ------------------------------------------------------------------
// respond to a method call request
// ------------------------------------------------------------------
void Coordinator::notifyTermination(void)
   {
   for (Components::iterator componentI = components.begin();
                             componentI != components.end();
                             componentI++)
      sendMessage(protocol::newNotifyTerminationMessage(
                      componentID,
                      componentI->second->ID));
   }
// ------------------------------------------------------------------
// A parent PM has asked us to provide a registration for the
// specified variable.
// ------------------------------------------------------------------
void Coordinator::onApsimGetQuery(unsigned int fromID, protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   }

void Coordinator::onError(const std::string& fromComponentName,
                          const string& msg,
                          bool isFatal)
   {
   // ------------------------------------------------------------------
   // A child has published an error - write it to stderr.
   // ------------------------------------------------------------------
   string message = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
   if (isFatal)
      message += "                 APSIM  Fatal  Error\n";
   else
      message += "                 APSIM Warning Error\n";
   message += "                 -------------------\n";

   message += msg;
   message += "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n";
   writeStringToStream(message, cout, "");
   writeStringToStream(message, cerr, "");
   }


