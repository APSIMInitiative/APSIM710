#include <../General/pch.h>
#include <stdlib.h>
#include <string.h>
#include <General/platform.h>

#include <stdexcept>
#include <iostream>

#include <boost/regex.hpp>

#include "ApsimRegistrationType.h"
#include "ApsimRegistration.h"
#include "ApsimRegistry.h"

using namespace std;

boost::thread_specific_ptr<ApsimRegistry> ApsimRegistry::GlobalApsimRegistry;

ApsimRegistry& ApsimRegistry::getApsimRegistry(void)
// ------------------------------------------------------------------
// Return the singleton global ApsimRegistry.
// ------------------------------------------------------------------
   {
	   if (GlobalApsimRegistry.get() == 0) {
		   GlobalApsimRegistry.reset(new ApsimRegistry);
	   }
	 return *GlobalApsimRegistry;
   }

 ApsimRegistry::~ApsimRegistry()
   {
   if (components.size() > 0)
	 freeAll();
   reset();
   }

// Clean up
void ApsimRegistry::reset(void)
   {
   for (registrations_type::iterator i = registrations.begin();
		i != registrations.end();
		i++)
	  delete i->second;
   registrations.clear();
   reg_id_map.clear();
   components.clear();
   taintedComponents.clear();
   }

// Attempt to remove an ApsimRegistration from the id-keyed map
void ApsimRegistry::freeIdReg(ApsimRegistration* reg)
{
   if (reg) 
   {
      reg_id_map_type::iterator i, a, b;
      a = reg_id_map.lower_bound(reg->getRegID());
      b = reg_id_map.upper_bound(reg->getRegID());

      for (i = a; i != b; i++)
 	  {
		 if (i->second == reg)
		 {
			reg_id_map.erase(i);
			return;
		 }
	  }
   }
}

// Create a native registration.
ApsimRegistration* ApsimRegistry::createNativeRegistration
	  (EventTypeCode kind, const std::string& regName, const std::string& ddml,
	   int destinationComponentID, int componentID)
   {
   return new NativeRegistration(kind, regName, ddml, destinationComponentID, componentID);
   }

// Create a foreign registration.
ApsimRegistration* ApsimRegistry::createForeignRegistration
	  (EventTypeCode kind, const std::string& regName, const std::string& ddml,
	   int destinationComponentID, int componentID, unsigned foreignID)
   {
   return new ForeignRegistration(kind, regName, ddml, destinationComponentID, componentID, foreignID);
   }

// Add a registration if not already present
// fixme - we really need to rethink this!!
unsigned int ApsimRegistry::add(ApsimRegistration *reg)
   {
   if ( reg->getName().rfind(".") != string::npos)
	 throw std::runtime_error("trying to add a qualified registration " + reg->getName());

   if ( reg->getTypeCode() != ::respondToEvent && !isForeign(reg->getComponentID()))
	  {
	  // return old registration if present. NB. foreigns are often "re-registered"
	  //   with different IDs!!!
	  registrations_type::iterator i, j, a, b;

	  a = registrations.lower_bound(reg->getNameWithoutBrackets());
	  b = registrations.upper_bound(reg->getNameWithoutBrackets());

	  for (i = a; i != b; i++)
		 {
		 if (i->second->getTypeCode() == reg->getTypeCode() &&
			 i->second->getComponentID() == reg->getComponentID() &&
			 i->second->getDestinationID() == reg->getDestinationID() &&
			 i->second->getName() == reg->getName() )
			{
//            cout << "removing (" << i->second->getType() << "." << i->second->getComponentID() << "." <<
//               i->second->getName() << "->" << i->second->getDestinationID() << ")= " << ((unsigned int)i->second) << " called again - returning " << i->second->getRegID() << endl;
//            delete reg;
//            return ((unsigned int)i->second);
			  freeIdReg(i->second);
			  delete i->second;
			  registrations.erase(i);
			  break;
			}
		 }
	  }

   registrations.insert(registrations_type::value_type(reg->getNameWithoutBrackets(), reg));
   reg_id_map.insert(reg_id_map_type::value_type(reg->getRegID(), reg));

//   cout << "add (" << reg->getType() << ":" << componentByID(reg->getComponentID()) << "." <<
//        reg->getName();
//   if (reg->getDestinationID()>0) cout<< "->" << componentByID(reg->getDestinationID());
//   cout << ")= " << ((unsigned int)reg) << " called\n";
   return ((unsigned int)reg);
   }


// Find subscribers to an event.
void ApsimRegistry::lookup(ApsimRegistration * reg,
						   std::vector<ApsimRegistration*>&subscribers)
   {
//   string regName = reg->getName();
//   cout << "lookup:" << reg->getRegID() << ":subscribers to " <<
//        reg->getType() << ".";
//   if (reg->getDestinationID() > 0)
//      cout << reg->getDestinationID() << ".";
//   cout  << reg->getName() << "=";
   subscribers.reserve(20);
   if (reg->getDestinationID() > 0)
      {
      vector<int> destination;
      destination.push_back(reg->getDestinationID());

      lookup(reg, destination, subscribers);

      if (reg->getTypeCode() != ::event)
         pruneDuplicates(subscribers);
      }
   else if (reg->getTypeCode() == ::get || reg->getTypeCode() == ::set)
      {
      vector<int> siblingsAndParents;
	  siblingsAndParents.reserve(25);
      getSiblingsAndParents(reg->getComponentID(),
                            siblingsAndParents);

      lookup(reg, siblingsAndParents, subscribers);

      pruneDuplicates(subscribers);
      }
   else
      {
      // Push (events)
      vector<int> siblingsAndDescendants;
	  siblingsAndDescendants.reserve(25);
      getSiblingsAndDescendants(reg->getComponentID(),
                                siblingsAndDescendants);

      lookup(reg, siblingsAndDescendants, subscribers);

      // "events" can have multiple subscribers in the same component
      // so no pruning needed. But some subscribed events are "directed"
      pruneNonMatchingEvents (reg, subscribers);
      }
//   for (unsigned int i = 0; i != subscribers.size(); i++)
//     cout << componentByID(subscribers[i]->getComponentID()) << ",";
//   cout <<  "\n";
   }

// Find the "subscribers" to a registration.
//  -"candidates" is a list of component IDs that are "in scope"
//    of this registration
//  -a list of subscribers is returned
//  -duplicates may exist in the returned list
void ApsimRegistry::lookup(ApsimRegistration * reg,
                           std::vector<int> &candididates,
                           std::vector<ApsimRegistration*>&subscribers)
   {
   registrations_type::iterator i, a, b;

   string name = reg->getNameWithoutBrackets();
   a = registrations.lower_bound(name);
   b = registrations.upper_bound(name);

   for (i = a; i != b; i++)
      {
      if (reg->matchSubscriberType(i->second) &&
          std::find(candididates.begin(),
                 candididates.end(),
                 i->second->getComponentID()) != candididates.end() )
         {
            subscribers.push_back(i->second);
         }
      }
   }
void ApsimRegistry::pruneDuplicates( std::vector<ApsimRegistration*>&subscribers)
   {
   // some modules register both "respondToGet" and "respondToGetSet"
   //  - make sure the list we return is unique.
   for (vector<ApsimRegistration*>::iterator sub1 = subscribers.begin();
        sub1 != subscribers.end(); sub1++)
      {
      for (vector<ApsimRegistration*>::iterator sub2 = sub1 + 1;
           sub2 != subscribers.end(); sub2++)
         {
         if ((*sub1)->getComponentID() ==  (*sub2)->getComponentID())
            {
            subscribers.erase(sub2);
            break;
            }
         }
      }
   }

// Check whether subscribers to an event really wanted to know about them.
void ApsimRegistry::pruneNonMatchingEvents (ApsimRegistration * reg, std::vector<ApsimRegistration*>&subscribers)
   {
   bool more = 1;
   while (more)
      {
      more = 0;
      for (vector<ApsimRegistration*>::iterator sub = subscribers.begin();
           sub != subscribers.end();
           sub++)
         {
         if ((*sub)->getDestinationID() > 0)
            {
			// See if the subscriber cared about sender (eg wheat.sowing vs *.sowing)
            if ((*sub)->getDestinationID() != reg->getComponentID())
               {
               subscribers.erase(sub);
			   more = 1;
               break;
               }
            }
         }
      }
   }

void ApsimRegistry::erase(EventTypeCode type, int owner, unsigned int regID)
   {
   ApsimRegistration *reg = find(type, owner, regID);
   if (reg == NULL) return;
//   cout << "ApsimRegistry::erase name="<< reg->getName() << endl;
   // Is this correct? 
   bool found = true;
   while (found)
      {
      found = false;
      for (registrations_type::iterator i = registrations.begin();
           i != registrations.end();
           i++)
         if (reg == i->second)
            {
			freeIdReg(reg);
            delete reg;
            registrations.erase(i);
            found = true;
            break;
            }
      }
   }
// Find a registration object from a
// component & registration ID. "Native" registrations
// are easy, foreigns need special care.
ApsimRegistration *ApsimRegistry::find(EventTypeCode type,
									   int ownerID,
									   unsigned int regnID)
   {
   if (isForeign(ownerID))
	  {
      reg_id_map_type::iterator i, a, b;
      a = reg_id_map.lower_bound(regnID);
      b = reg_id_map.upper_bound(regnID);

      for (i = a; i != b; i++)
 	  {
		 if (i->second->getTypeCode() == type &&
			 i->second->getComponentID() == ownerID
			 )
			return (i->second);
	  } 
      return NULL;

	  }
   return ((ApsimRegistration *) regnID);
   }

ApsimRegistration *ApsimRegistry::find(EventTypeCode type,
									   int ownerID,
									   int destID,
									   const std::string &name)
   {
   registrations_type::iterator i, a, b;

   // Sometimes registrations are
   string regName;
   unsigned int pos = name.rfind('(');
   if (pos != std::string::npos)
	  regName = ToLower(name.substr(0, pos));
   else
	  regName = ToLower(name);

   a = registrations.lower_bound(regName);
   b = registrations.upper_bound(regName);

   for (i = a; i != b; i++)
	  {
	  if (i->second->getTypeCode() == type &&
		  i->second->getComponentID() == ownerID &&
		  i->second->getDestinationID() == destID &&
		  i->second->getNameWithoutBrackets() == regName)
		 {
		 return(i->second);
		 }
	  }
   return NULL;
   }

void ApsimRegistry::addComponent(int parentID,
								 int componentID,
								 const std::string &name,
								 const std::string &type,
								 const bool isSystem)
   {
   Component c;
   c.ID = componentID;
   c.Name = name;
   c.Type = type;
   c.IsSystem = isSystem;
   components.push_back(c);

   PTree<Component> *p = new PTree<Component>();
   p->item = c;
   p->parent = findComponent(parentID);
   p->parent->children.push_back(p);
   }

// see if a registration is in scope (ie has the same parent)
// as the calling component.
bool ApsimRegistry::inScope(int caller, ApsimRegistration *reg)
   {
   int regComponentID = reg->getComponentID();
   PTree<Component>* regNode = findComponent(regComponentID);
   if (regNode == NULL) {throw std::runtime_error("NULL node in ApsimRegistry::inScope!");}
   int regParent = regNode->parent->item.ID;

   PTree<Component>* callerNode = findComponent(caller);
   if (callerNode == NULL) {throw std::runtime_error("NULL node in ApsimRegistry::inScope!");}
   int callerParent = callerNode->parent->item.ID;
   return (regParent == callerParent);
   }

bool ApsimRegistry::inScope(ApsimRegistry::PTree<ApsimRegistry::Component>*callerNode, ApsimRegistration *reg)
   {
   int regComponentID = reg->getComponentID();
   PTree<Component>* regNode = findComponent(regComponentID);
   if (regNode == NULL) {throw std::runtime_error("NULL node in ApsimRegistry::inScope!");}
   int regParent = regNode->parent->item.ID;

   int callerParent = callerNode->parent->item.ID;
   return (regParent == callerParent);
   }


ApsimRegistry::PTree<ApsimRegistry::Component>* ApsimRegistry::findComponent(int componentID)
  {
  return findComponent(&paddocks, componentID);
  }

ApsimRegistry::PTree<ApsimRegistry::Component>* ApsimRegistry::findComponent(ApsimRegistry::PTree<ApsimRegistry::Component> *node, int componentID)
  {
  if (node == NULL || node->item.ID == componentID) return node;
  PTree<Component>* child;
  for (unsigned i = 0; i < node->children.size(); i++)
	 {
     if ((child = findComponent(node->children[i], componentID)) != NULL)
        return child;
	 }
  return NULL;
  }

void ApsimRegistry::getSiblings(int componentID, vector<int> &siblings)
   {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getSiblings!");}
   PTree<Component>* container = node->parent;

   for (unsigned i = 0; i != container->children.size(); i++)
      {
      // Need to detect the case where .MasterPM is a child of iteself.
      // We don't want that in a list of siblings.
      if (container->children[i]->item.ID != container->item.ID)
         siblings.push_back(container->children[i]->item.ID);
      }
   }
void ApsimRegistry::getChildren(int componentID, vector<int> &children)
   {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getChildren!");}
   PTree<Component>* parent = node->parent;

   for (unsigned i = 0; i != node->children.size(); i++)
      {
      // Need to detect the case where .MasterPM is a child of iteself.
      // We don't want that in a list of siblings.
      if (node->children[i]->item.ID != parent->item.ID)
         children.push_back(node->children[i]->item.ID);
      }
   }

void ApsimRegistry::getSiblingsAndDescendants(int componentID, vector<int> &siblings)
   {
// Hack * not as name suggests * parents, siblings and descendants **
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getSiblingsAndDescendants!");}
   PTree<Component>* container = node->parent;

   for (unsigned i = 0; i != container->children.size(); i++)
      {
      siblings.push_back(container->children[i]->item.ID);
      getDescendants(container->children[i], siblings);
	  }

   PTree<Component>* grandparent = container->parent;
   if (grandparent == NULL) {throw std::runtime_error("NULL node in getSiblingsAndParents2!");}
   if (grandparent != container)
      {
      for (unsigned i = 0; i != grandparent->children.size(); i++)
        {
        if (grandparent->children[i]->item.ID != container->parent->item.ID)
           siblings.push_back(grandparent->children[i]->item.ID);
        }
      }

   }

// Fixme -this should be recursive
void ApsimRegistry::getSiblingsAndParents(int componentID, vector<int> &siblings)
   {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getSiblingsAndParents!");}
   PTree<Component>* container = node->parent;

   for (unsigned i = 0; i != container->children.size(); i++)
      {
      if (container->children[i]->item.ID != componentID)
         siblings.push_back(container->children[i]->item.ID);
      }

   PTree<Component>* grandparent = container->parent;
   if (grandparent == NULL) {throw std::runtime_error("NULL node in getSiblingsAndParents2!");}
   if (grandparent != container)
      {
      for (unsigned i = 0; i != grandparent->children.size(); i++)
        {
        if (grandparent->children[i]->item.ID != container->parent->item.ID)
		   siblings.push_back(grandparent->children[i]->item.ID);
        }
      }
   // And the masterPM
   siblings.push_back(1/*masterPMID*/);
   }

void ApsimRegistry::getDescendants(ApsimRegistry::PTree<ApsimRegistry::Component>*node, vector<int> &siblings)
   {
   for (unsigned i = 0; i != node->children.size(); i++)
      {
      siblings.push_back(node->children[i]->item.ID);
	  getDescendants(node->children[i], siblings);
      }
   }

int ApsimRegistry::componentByName(const std::string &name)
   {
   for (unsigned int i = 0; i < components.size(); i++ )
      if (Str_i_Eq(components[i].Name,name))
         return components[i].ID;
   return(0);
   }
   
std::string ApsimRegistry::getComponentType(int componentID)
	 {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getComponentType!");}
   return node->item.Type; 	
	 }

bool ApsimRegistry::componentIsSystem(int componentID)
	 {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in componentIsSystem!");}
   return node->item.IsSystem; 	
	 }

std::string ApsimRegistry::componentByID(int id)
   {
   for (unsigned int i = 0; i < components.size(); i++ )
	  if (components[i].ID == id)
		 return components[i].Name;

   return "";
}
void ApsimRegistry::setForeignTaint(int id)
{
   taintedComponents.push_back(id);
}

void ApsimRegistry::clearForeignTaint(int id)
{
   vector<int>::iterator i =
		std::find(taintedComponents.begin(),
				  taintedComponents.end(),
				  id);

   if (i != taintedComponents.end())
	 taintedComponents.erase(i);
}

bool ApsimRegistry::isForeign(int id)
{
   return(std::find(taintedComponents.begin(),
                    taintedComponents.end(),
					id) != taintedComponents.end());
}


// ".paddock1.wheat.esw"
// "wheat.esw"
// "*.esw"
// "esw"
// etc
void ApsimRegistry::unCrackPath(int fromID,                 // IN: id of module asking
                                const std::string &fqName,  // IN: [module.]name string
                                std::vector<int> &ids,       // out
                                std::string &name)          // out
{
   size_t pos = fqName.rfind(".");
   if (pos != string::npos)
      {
      name = fqName.substr(pos+1);
      string componentName = fqName.substr(0,pos);
      if (strchr(componentName.c_str(), '*') == NULL && 
          strchr(componentName.c_str(), '?') == NULL) 
         {
         // It's not a RE. It's either an absolute
         // or relative path.
         if (componentName[0] == '.') 
           {
           int id = componentByName(componentName);
           if (id <= 0) {throw std::runtime_error("Unknown module name " + componentName);}
           ids.push_back(id);
           }
         else 
           {
           // prepend container name of sending component
           string container = componentByID(fromID) ;
           size_t cpos = container.rfind(".");
           if (cpos != string::npos) 
              container = container.substr(0,cpos);
         
           componentName = container + "." + componentName;
         
           int id = componentByName(componentName);
           if (id <= 0) {throw std::runtime_error("Unknown module name " + componentName);}
           ids.push_back(id);
           }
         }
      else
         {
//cout << "Testing RE: variable=\""<<name<<"\", re=\"" << componentName << "\"\n";
         // It's an RE. Test against all the component names we know about.
         //boost::regex e(componentName);
		 //
         //for (unsigned int i = 0; i < components.size(); i++)
         //  if (regex_match(components[i].Name,e)) 
         //    ids.push_back(components[i].ID);
         ids.push_back(0);
         }
      }  
   else
      {
      // No component name.
      ids.push_back(0);
      name = fqName;
      }
}

void ApsimRegistry::unCrackPath(int fromID,                 // IN: id of module asking
                                const std::string &fqName,  // IN: [module.]name string
                                int &id,                    // out
                                std::string &name)          // out
{
   vector<int> ids;
   unCrackPath(fromID, fqName, ids, name);
   if (ids.size() > 1) {
      string msg = "The component name \"";
      msg += fqName;
      msg += "\" is not unique.\n";

      throw std::runtime_error(msg);
   }
   id = ids[0];
}

// Write a description of whatever a component has registered
std::string ApsimRegistry::getDescription(int componentID)
   {
   registrations_type::iterator i;
   string returnString;

   typedef std::map<std::string, std::string> StringMap;
   StringMap properties;

   for (i = registrations.begin(); i != registrations.end(); i++)
      {
      ApsimRegistration *reg = i->second;
      if (reg->getComponentID() == componentID)
         {
         if (reg->getTypeCode() == ::respondToGet)
			   {
               string st = "   <property name=\"";
               st += reg->getName();
               st += "\" access=\"read\" init=\"F\">\n";
               st += reg->getDDML();
               st += "</property>\n";
               properties.insert(make_pair(reg->getName(), st));
               }
         else if (reg->getTypeCode() == ::respondToSet)
               {
               string st = "   <property name=\"";
               st += reg->getName();
               st += "\" access=\"write\" init=\"F\">\n";
               st += reg->getDDML();
               st += "</property>\n";
               properties.insert(make_pair(reg->getName(), st));
               }
         else if (reg->getTypeCode() == ::respondToGetSet)
               {
               string st = "   <property name=\"";
               st += reg->getName();
               st += "\" access=\"both\" init=\"F\">\n";
               st += reg->getDDML();
               st += "</property>\n";
               properties.insert(make_pair(reg->getName(), st));
               }
         else if (reg->getTypeCode() == ::respondToEvent)
            {
            XMLDocument* doc = new XMLDocument(reg->getDDML(), XMLDocument::xmlContents);
            string typeName = doc->documentElement().getAttribute("typename");
            string st = "   <event name=\"";
            st += reg->getName() + "\"";
            if (typeName != "")
               st += " typename=\"" + typeName + "\"";
            st += " kind=\"subscribed\">";
            st += doc->documentElement().innerXML();
            delete doc;
            st += "</event>\n";
            properties.insert(make_pair(reg->getName(), st));
            }
         else if (reg->getTypeCode() == ::event)
            {
            XMLDocument* doc = new XMLDocument(reg->getDDML(), XMLDocument::xmlContents);
            string typeName = doc->documentElement().getAttribute("typename");
            string st = "   <event name=\"";
            st += reg->getName() + "\"";
            if (typeName != "")
               st += " typename=\"" + typeName + "\"";
            
            st += " kind=\"published\">";
            st += doc->documentElement().innerXML();
            delete doc;
            st += "</event>\n";
            properties.insert(make_pair(reg->getName(), st));
			}
         else if (reg->getTypeCode() == ::get)
            {
            string st = "   <driver name=\"";
            st += reg->getName();
            st += "\">\n";
            st += reg->getDDML();
            st += "</driver>\n";
            properties.insert(make_pair(reg->getName(), st));
            }
         }
      }
   for (StringMap::iterator i = properties.begin();
                            i != properties.end();
                            i++)
         returnString += i->second + "\n";

   return returnString;
   }

void ApsimRegistry::dumpStats(void)
   {
   int nget=0;
   int nrespondToGet=0;
   int nset=0;
   int nrespondToSet=0;
   int nevent=0;
   int nrespondToEvent=0;
   int nrespondToGetSet=0;
   for (registrations_type::iterator i = registrations.begin();
		i != registrations.end();
        i++)
      {
      if (i->second->getTypeCode() == ::get) nget++;
      else if (i->second->getTypeCode() == ::respondToGet) nrespondToGet++;
      else if (i->second->getTypeCode() == ::set) nset++;
      else if (i->second->getTypeCode() == ::respondToSet) nrespondToSet++;
      else if (i->second->getTypeCode() == ::event) nevent++;
      else if (i->second->getTypeCode() == ::respondToEvent) nrespondToEvent++;
      else if (i->second->getTypeCode() == ::respondToGetSet) nrespondToGetSet++;
      }

   cout << "Registration types\n";
   cout << "get="<<nget<<endl;
   cout << "respondToGet="<<nrespondToGet<<endl;
   cout << "set="<<nset<<endl;
   cout << "respondToSet="<<nrespondToSet<<endl;
   cout << "event="<<nevent<<endl;
   cout << "respondToEvent="<<nrespondToEvent<<endl;
   cout << "respondToGetSet="<<nrespondToGetSet<<endl;
   }

void ApsimRegistry::freeAll(void)
{
   PTree<Component>* root = findComponent(/*masterPMID*/1);
   freeAll(root);
}

void ApsimRegistry::freeAll(PTree<Component>* node)
{
   if (node != NULL) {
	   for (unsigned i = 0; i != node->children.size(); i++)
		  {
			    freeAll(node->children[i]);
		  }
	   if (node->item.Type != "")
	   {
         delete node;
	     node = NULL;
	   }
   }
}

void ApsimRegistry::dumpAll(void)
   {
   PTree<Component>* root = findComponent(/*masterPMID*/1);
   if (root == NULL) {throw std::runtime_error("NULL node in dumpComponentTree!");}
   for (unsigned i = 0; i != root->children.size(); i++)
	  {
	  dumpAll(root->children[i]);
	  }
   }
void ApsimRegistry::dumpAll(PTree<Component>* node)
   {
   cout << getDescription(node->item.ID);
   for (unsigned i = 0; i != node->children.size(); i++)
      {
      dumpAll(node->children[i]);
      }
   }

void ApsimRegistry::dumpComponentTree(void)
   {
   PTree<Component>* root = findComponent(/*masterPMID*/1);
   if (root == NULL) {throw std::runtime_error("NULL node in dumpComponentTree!");}
   int indent = 0;
   for (unsigned i = 0; i != root->children.size(); i++)
	  {
	  dumpComponentTree(indent, root->children[i]);
      }
   }
void ApsimRegistry::dumpComponentTree(int indent, PTree<Component>* node)
   {
   for (int i=0; i < indent; i++) cout << " ";
   cout << node->item.ID << ":" << node->item.Name << endl;
   for (unsigned i = 0; i != node->children.size(); i++)
      {
      dumpComponentTree(1+indent, node->children[i]);
      }
   }

void ApsimRegistry::getComponents(vector<int> &simulationComponents)
   {
   PTree<Component>* root = findComponent(/*masterPMID*/1);
   if (root == NULL) {throw std::runtime_error("NULL node in getComponents!");}
   for (unsigned i = 0; i != root->children.size(); i++)
      {
      getComponents(root->children[i], simulationComponents);
      }
   }

void ApsimRegistry::getComponents(PTree<Component>* node, vector<int> &simulationComponents)
   {
   simulationComponents.push_back(node->item.ID);
   for (unsigned i = 0; i != node->children.size(); i++)
      {
      getComponents(node->children[i], simulationComponents);
      }
   }

bool ApsimRegistry::hasChildren(int componentID)
   {
   PTree<Component>* comp = findComponent(componentID);
   if (comp == NULL)
      return false;
   else
      return comp->children.size() > 0;
   }


// --------------------------------------------------------------------------
// Entry points for getting metadata about a simulation. Used by .NET
// --------------------------------------------------------------------------

extern "C" void EXPORT STDCALL componentByID(int ID, char* componentName)
   {
   string name = ApsimRegistry::getApsimRegistry().componentByID(ID);
   strcpy(componentName, name.c_str());
   }

extern "C" void EXPORT STDCALL getChildren(char* ComponentName, char* Data)
   {
   int componentID = ApsimRegistry::getApsimRegistry().componentByName(ComponentName);
   if (componentID > 0)
      {
      // Return a list of child components for the specified componentID
      vector<int> childIDs;
      ApsimRegistry::getApsimRegistry().getChildren(componentID, childIDs);

      strcpy(Data, "");
      for (unsigned i = 0; i != childIDs.size(); i++)
         {
         string ChildName = ApsimRegistry::getApsimRegistry().componentByID(childIDs[i]);
         if (i > 0)
            strcat(Data, ",");
         strcat(Data, "\"");
         strcat(Data, ChildName.c_str());
         strcat(Data, "\"");
		 }
	  }
   }

extern "C" bool EXPORT STDCALL findVariable(char* OwnerComponentName, char* VariableName)
   {
   int OwnerID = ApsimRegistry::getApsimRegistry().componentByName(OwnerComponentName);
   return (ApsimRegistry::getApsimRegistry().find(::respondToGet, OwnerID, 0, VariableName) != NULL ||
          ApsimRegistry::getApsimRegistry().find(::respondToGetSet, OwnerID, 0, VariableName) != NULL);
   }

extern "C" bool EXPORT STDCALL isPaddock(char* ComponentName)
   {
   int ComponentID = ApsimRegistry::getApsimRegistry().componentByName(ComponentName);
   return ApsimRegistry::getApsimRegistry().hasChildren(ComponentID);
   }

extern "C" void EXPORT STDCALL getComponentType(char* ComponentName, char* Type)
   {
   int ComponentID = ApsimRegistry::getApsimRegistry().componentByName(ComponentName);
   strcpy(Type, ApsimRegistry::getApsimRegistry().getComponentType(ComponentID).c_str());
   }
