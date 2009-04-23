#include <string>
#include <vector>
#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include "ApsimComponentData.h"
#include "ApsimSystemData.h"

#include <stdexcept>

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimSystemData::ApsimSystemData(const XMLNode& n)
   : node(n)
   {
   }

// ------------------------------------------------------------------
// Return the name of the simulation
// ------------------------------------------------------------------
string ApsimSystemData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return the executable filename
// ------------------------------------------------------------------
string ApsimSystemData::getExecutableFileName(void) const
   {
   return node.getAttribute("executable");
   }
// ------------------------------------------------------------------
// Set the name of the component.
// ------------------------------------------------------------------
void ApsimSystemData::setName(const std::string& name)
   {
   node.setAttribute("name", name);
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
void ApsimSystemData::setExecutableFileName(const std::string& executable)
   {
   node.setAttribute("executable", executable);
   }

// ------------------------------------------------------------------
// Return a list of all system names in this simulation.
// ------------------------------------------------------------------
void ApsimSystemData::getSystemNames(vector<string>& systemNames) const
   {
   GetAttribute<vector<string>, XMLNode> getNameAtt("name", systemNames);
   for_each_if(node.begin(), node.end(),
               getNameAtt,
               EqualToName<XMLNode>("system"));
   }
// ------------------------------------------------------------------
// Return a list of component names in this simulation
// ------------------------------------------------------------------
void ApsimSystemData::getComponentNames(std::vector<std::string>& componentNames) const
   {
   GetAttribute<vector<string>, XMLNode> getNameAtt("name", componentNames);
   for_each_if(node.begin(), node.end(),
               getNameAtt,
               EqualToName<XMLNode>("component"));
   }
// ------------------------------------------------------------------
// Return the component with the specified name.
// ------------------------------------------------------------------
ApsimComponentData ApsimSystemData::getComponent(const std::string& name) const
   {
   XMLNode::iterator i = find_if(node.begin(), node.end(),
                                 NodeEquals<XMLNode>("component", name));
   if (i != node.end())
      return ApsimComponentData(*i);

   throw runtime_error("Cannot find a component named: " + name
                       + " in system: " + getName());
   }
// ------------------------------------------------------------------
// Return the system with the specified name.
// ------------------------------------------------------------------
ApsimSystemData ApsimSystemData::getSystem(const std::string& name) const
   {
   XMLNode::iterator i = find_if(node.begin(), node.end(),
                                 NodeEquals<XMLNode>("system", name));
   if (i != node.end())
      return ApsimSystemData(*i);
   throw runtime_error("Cannot find a system named: " + name
                       + " in simulation: " + getName());
   }
// ------------------------------------------------------------------
// Add a component to the system
// ------------------------------------------------------------------
ApsimComponentData ApsimSystemData::addComponent(const std::string& name)
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 NodeEquals<XMLNode>("component", name));
   if (i == node.end())
      {
      ApsimComponentData component = node.appendChild("component", true);
      component.setName(name);
      return component;
      }
   else
      return ApsimComponentData(*i);
   }
// ------------------------------------------------------------------
// Add a component to the system
// ------------------------------------------------------------------
ApsimComponentData ApsimSystemData::addComponent(ApsimComponentData& component)
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 NodeEquals<XMLNode>("component", component.getName()));
   if (i == node.end())
      {
      component.node = node.appendChild(component.node, true);
      return component;
      }
   else
      return ApsimComponentData(*i);
   }
// ------------------------------------------------------------------
// Add a system to the system.
// ------------------------------------------------------------------
ApsimSystemData ApsimSystemData::addSystem(const std::string& name)
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 NodeEquals<XMLNode>("system", name));
   if (i == node.end())
      {
      ApsimSystemData system = node.appendChild("system", true);
      system.setName(name);
      return system;
      }
   else
      return ApsimSystemData(*i);
   }
// ------------------------------------------------------------------
// Return the contents of this system as an xml string.
// ------------------------------------------------------------------
std::string ApsimSystemData::getXML(void) const
   {
   return node.write();
   }
// ------------------------------------------------------------------
// Delete a component from the system.  Return true if successful.
// ------------------------------------------------------------------
bool ApsimSystemData::deleteComponent(const std::string& name)
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 NodeEquals<XMLNode>("component", name));
   if (i != node.end())
      {
      node.erase(i);
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// return this system as a component
// ------------------------------------------------------------------
ApsimComponentData ApsimSystemData::asComponent(void)
   {
   return ApsimComponentData(node);
   }

