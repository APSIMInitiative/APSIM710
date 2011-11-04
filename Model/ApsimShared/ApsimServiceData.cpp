#include <../General/pch.h>
#include <vector>
#include <string>
#include <General/stl_functions.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>

#include "ApsimServiceData.h"

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimServiceData::ApsimServiceData(const XMLNode& n)
   : node(n), xmlDoc(NULL)
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   if (initData == node.end())
      node.appendChild("initdata");
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimServiceData::ApsimServiceData(const std::string& xml)
   : node(NULL, NULL)
   {
   xmlDoc = new XMLDocument(xml, XMLDocument::xmlContents);
   node = xmlDoc->documentElement();
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ApsimServiceData::~ApsimServiceData(void)
   {
   delete xmlDoc;
   }
// ------------------------------------------------------------------
// Return name of service to caller.
// ------------------------------------------------------------------
string ApsimServiceData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return name of service to caller.
// ------------------------------------------------------------------
string ApsimServiceData::getExecutableFileName(void) const
   {
   return node.getAttribute("executable");
   }
// ------------------------------------------------------------------
// Return the value of a specific property to caller.
// ------------------------------------------------------------------
std::string ApsimServiceData::getProperty(const std::string& name) const
   {
   XMLNode initData = getInitData();
   XMLNode::iterator propertyI = find_if(initData.begin(),
                                         initData.end(),
                                         NodeEquals<XMLNode>("property", name));
   if (propertyI != initData.end())
      return propertyI->getValue();

   return "";
   }
// ------------------------------------------------------------------
// Return an iterator to the initdata node.
// ------------------------------------------------------------------
XMLNode ApsimServiceData::getInitData(void) const
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   return *initData;
   }
// ------------------------------------------------------------------
// Set the name of the component.
// ------------------------------------------------------------------
void ApsimServiceData::setName(const std::string& name)
   {
   node.setAttribute("name", name);
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
void ApsimServiceData::setExecutableFileName(const std::string& executable)
   {
   node.setAttribute("executable", executable);
   }
// ------------------------------------------------------------------
// Set the value of a specified property.
// ------------------------------------------------------------------
void ApsimServiceData::setProperty(const string& name,
                                   const string& value)
   {
   XMLNode initData = getInitData();
   XMLNode property = appendChildIfNotExist(initData, "property", name);
   property.setValue(value);
   }
// ------------------------------------------------------------------
// Return the contents of this service as an xml string.
// ------------------------------------------------------------------
std::string ApsimServiceData::getXML(void) const
   {
   return node.write();
   }

