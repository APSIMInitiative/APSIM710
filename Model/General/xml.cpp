#include <../General/pch.h>
#include <string>
#include <fstream>
#include <sstream>
#include <stdexcept>

#include <General/TreeNodeIterator.h>
#include <General/stl_functions.h>

#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/io_functions.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#ifdef __WIN32__
#include <process.h>
#endif

#include "xml.h"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& rootNode, RootName rootName)
   {
   doc = xmlNewDoc(BAD_CAST "1.0");
   xmlDocSetRootElement(doc, xmlNewNode(NULL, (xmlChar *)rootNode.c_str()));
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& fileName)
   {
   doc = xmlReadFile(fileName.c_str(), NULL, 0);
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& xml, XmlContents xmlContents)
   {
   doc = xmlReadMemory(xml.c_str(), xml.size(), "noname.xml", NULL, 0);
   dirty = true;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
XMLDocument::~XMLDocument(void)
   {
   xmlFreeDoc(doc);
//   xmlCleanupParser();
//   xmlCleanupGlobals();
   }
//---------------------------------------------------------------------------
// return the root document element
//---------------------------------------------------------------------------
XMLNode XMLDocument::documentElement(void)
   {
   return XMLNode(this, xmlDocGetRootElement(doc));
   }
//---------------------------------------------------------------------------
// write the contents of this document to the specified file.
//---------------------------------------------------------------------------
void XMLDocument::write(const std::string& fileName)
   {
   xmlSaveFormatFile(fileName.c_str(), doc, 1);
   dirty = false;
   }

bool XMLDocument::isValid(void)
   {
   return (documentElement().isValid());
   }
//---------------------------------------------------------------------------
// constructor for node.
//---------------------------------------------------------------------------
XMLNode::XMLNode(XMLDocument* doc, xmlNode* n)
   : parent(doc), node(n)
   {
   }
//---------------------------------------------------------------------------
// copy constructor for node
//---------------------------------------------------------------------------
XMLNode::XMLNode(const XMLNode& rhs)
   {
   parent = rhs.parent;
   node = rhs.node;
   }
//---------------------------------------------------------------------------
// destructor for node.
//---------------------------------------------------------------------------
XMLNode::~XMLNode(void)
   {
   }
//---------------------------------------------------------------------------
// assignment operator for a node
//---------------------------------------------------------------------------
XMLNode& XMLNode::operator= (const XMLNode& rhs)
   {
   parent = rhs.parent;
   node = rhs.node;
   return *this;
   }
//---------------------------------------------------------------------------
// return the name of the node.
//---------------------------------------------------------------------------
string XMLNode::getName(void) const
   {
   if (node != NULL && node->type != XML_CDATA_SECTION_NODE)
   {
	   string name = (char*) node->name;
	   return name;
   }
   else
      return "";
   }
// ------------------------------------------------------------------
// Return a list of attributes for node.
// ------------------------------------------------------------------
void XMLNode::getAttributes(std::vector<std::string>& names) const
   {
   if (node != NULL)
      {
      xmlAttr* attributes = node->properties;
      xmlAttr* attr = attributes;
      while (attr != NULL)
         {
         names.push_back((char*)attr->name);
         attr = attr->next;
         }
      }
   }
// ------------------------------------------------------------------
// Return an attribute of the node.
// ------------------------------------------------------------------
string XMLNode::getAttribute(const std::string& attributeName) const
   {
   string returnString;
   if (node != NULL)
      {
	xmlChar* st = xmlGetProp(node, (xmlChar *)attributeName.c_str());
      if (st != NULL)
         returnString = (char*) st;
      xmlFree(st);
      }
   return returnString;
   }
// ------------------------------------------------------------------
// Return the value of the node.
// ------------------------------------------------------------------
std::string XMLNode::getValue(void) const
   {
   string returnString;
   if (node != NULL)
      {
      xmlChar* st = xmlNodeGetContent(node);
      if (st != NULL)
         returnString = (char*) st;
      xmlFree(st);
      }
   return returnString;
   }

std::string XMLNode::childValue(const std::string& childName) const
   {
   vector<string> values = childValues(childName);
   if (values.size() == 0)
      return "";
   else if (values.size() > 1)
      throw std::runtime_error("More than 1 child values found for node: " + childName);
   return values[0];
   }

vector<std::string> XMLNode::childValues(const std::string& childName) const
   {
   vector<string> values;
   for_each_if(begin(), end(),
               GetValueFunction<vector<string>, XMLNode>(values),
               EqualToName<XMLNode>(childName));
   return values;
   }

// ------------------------------------------------------------------
// Set an attribute of this node.
// ------------------------------------------------------------------
void XMLNode::setAttribute(const string& attributeName,
                           const string& attributeValue)
   {
   if (node != NULL)
      {
      if (attributeValue != "")
         {
         xmlSetProp(node, (xmlChar *)attributeName.c_str(), (xmlChar *)attributeValue.c_str());
         parent->setDirty(true);
         }
      else
         {
         xmlUnsetProp(node, (xmlChar *)attributeName.c_str());
         }
      }
   }
// ------------------------------------------------------------------
// Set the value of this node.
// ------------------------------------------------------------------
void XMLNode::setValue(const std::string& value)
   {
   if (value.find_first_of("&<>") != string::npos)
      setValueAsCData(value);
   else
      xmlNodeSetContent(node, (xmlChar *)value.c_str());
   parent->setDirty(true);
   }
// ------------------------------------------------------------------
// Set the value of this node.
// ------------------------------------------------------------------
void XMLNode::setValueAsCData(const std::string& value)
   {
   xmlNode* cdata = xmlNewCDataBlock(node->doc, (xmlChar *)value.c_str(), value.length());
   xmlAddChild(node, cdata);
   parent->setDirty(true);
   }
// ------------------------------------------------------------------
// Add a child node to this node.  If alwaysAppend = true then
// a new node will always be appended.  If alwaysAppend = false then
// a new node will only be appended if it doesn't already exist.
// ------------------------------------------------------------------
XMLNode XMLNode::appendChild(const std::string& nodeName, bool alwaysAppend)
   {
   if (!alwaysAppend)
      {
      iterator i = find_if(begin(), end(),
                           EqualToName<XMLNode>(nodeName));
      if (i != end())
         return *i;
      }
   xmlNode* newChild = xmlNewChild(node, NULL, (xmlChar *)nodeName.c_str(), (xmlChar *)"");
   parent->setDirty(true);
   return XMLNode(parent, newChild);
   }
// ------------------------------------------------------------------
// Add a child node to this node.  If alwaysAppend = true then
// a new node will always be appended.  If alwaysAppend = false then
// a new node will only be appended if it doesn't already exist.
// ------------------------------------------------------------------
XMLNode XMLNode::appendChild(XMLNode childNode, bool alwaysAppend)
   {
   if (!alwaysAppend)
      {
      iterator i = find_if(begin(), end(),
                           EqualToName<XMLNode>(childNode.getName()));
      if (i != end())
         return *i;
      }
   parent->setDirty(true);
   xmlNode* copyOfChild = xmlCopyNode(childNode.node, 1);
   return XMLNode(parent, xmlAddChild(node, copyOfChild));
   }
// ------------------------------------------------------------------
// Delete a child node from this node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::erase(XMLNode::iterator& child)
   {
   if (child != end())
      {
      iterator next = child;
      next++;
      xmlUnlinkNode(child->node);
      xmlFreeNode(child->node);
      parent->setDirty(true);
      return next;
      }
   return child;
   }
// ------------------------------------------------------------------
// Look at the node passed in. If it is 'valid' then simply return it.
// Otherwise iterate through the siblings until a valid 1 is found.
// Returns NULL if non found.
// ------------------------------------------------------------------
xmlNode* findFirstValidNode(xmlNode* node)
   {
   while (node != NULL && (node->type == XML_TEXT_NODE || node->type == XML_CDATA_SECTION_NODE))
       node = node->next;
   return node;
   }
// ------------------------------------------------------------------
// Return the next sibling.
// ------------------------------------------------------------------
XMLNode XMLNode::getNextSibling(void) const
   {
   if (node != NULL)
      return XMLNode(parent, findFirstValidNode(node->next));
   else
      return XMLNode(parent, NULL);
   }
// ------------------------------------------------------------------
// Return an iterator to the first child node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::begin() const
   {
   if (node != NULL)
      return XMLNode::iterator(XMLNode(parent, findFirstValidNode(node->children)));
   else
      return XMLNode(parent, NULL);
   }
// ------------------------------------------------------------------
// Return an iterator to the last child node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::end() const
   {
   return XMLNode::iterator(XMLNode(parent, NULL));
   }
//---------------------------------------------------------------------------
// write the contents of this node to the specified string.
//---------------------------------------------------------------------------
string XMLNode::write() const
   {
   xmlBufferPtr buff = xmlBufferCreate();
   xmlNodeDump(buff, node->doc, node, 0, 1);
   string returnString = (const char*) xmlBufferContent(buff);
   xmlBufferFree(buff);
   return returnString;
   }

//---------------------------------------------------------------------------
// Handy function that will append a new child node IF that node doesn't
// already exist.
//---------------------------------------------------------------------------
XMLNode appendChildIfNotExist(XMLNode& node,
                              const std::string& nodeName,
                              const std::string& nameAttribute)
   {
   XMLNode::iterator childI = std::find_if(node.begin(),
                                           node.end(),
                                           NodeEquals<XMLNode>(nodeName, nameAttribute));
   if (childI != node.end())
      return XMLNode(*childI);
   else
      {
      XMLNode child = node.appendChild(nodeName, true);
      child.setAttribute("name", nameAttribute);
      return child;
      }
   }

// ------------------------------------------------------------------
// Handy function that will delete all nodes with the specified name.
// ------------------------------------------------------------------
void eraseNodes(XMLNode node, const std::string& name)
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 EqualToName<XMLNode>(name));
   while (i != node.end())
      {
      node.erase(i);
      i = find_if(node.begin(),
                  node.end(),
                  EqualToName<XMLNode>(name));
      }
   }
// ------------------------------------------------------------------
// Handy function that returns a node given a fully qualified name
// eg fqn:  root|child1|child2
// ------------------------------------------------------------------
XMLNode findNode(XMLNode node, const std::string& fqn)
   {
     return findNode(node, fqn, '|');
   }
XMLNode findNode(XMLNode node, const std::string& fqn, char delimiter)
   {
   unsigned posDelimiter = fqn.find(delimiter);
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 EqualToName<XMLNode>(fqn.substr(0, posDelimiter)));
   if (i != node.end())
      {
      if (posDelimiter == string::npos)
         return XMLNode(*i);
      else
         return findNode(*i, fqn.substr(posDelimiter+1), delimiter);
      }
   else
      return XMLNode();
   }

// ------------------------------------------------------------------
// Handy function that returns a node given a fully qualified name
// This variant uses the 'name' attribute to search.
// eg fqn:  root|child1|child2
// ------------------------------------------------------------------
XMLNode findNodeWithName(XMLNode node, const std::string& fqn) 
   {
   return findNodeWithName(node, fqn, '|');
   }
XMLNode findNodeWithName(XMLNode node, const std::string& fqn, char delimiter)
   {
   unsigned posDelimiter = fqn.find(delimiter);
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 AttributeEquals<XMLNode>("name", fqn.substr(0, posDelimiter)));
   if (i != node.end())
      {
      if (posDelimiter == string::npos)
         return XMLNode(*i);
      else
         return findNodeWithName(*i, fqn.substr(posDelimiter+1));
      }
   else
      return XMLNode();
   }

string XMLNode::innerXML()
   {
   string returnString;
   for (XMLNode::iterator i = begin(); i != end(); i++)
      returnString += i->write();
   return returnString;
   }

// ------------------------------------------------------------------
// Handy function that returns a node value given a fully qualified name
// eg fqn:  root|child1|child2
// ------------------------------------------------------------------
std::string findNodeValue(XMLNode node, const std::string& fqn)
   {
   XMLNode foundNode = findNode(node, fqn);
   if (foundNode.isValid())
      return foundNode.getValue();
   else
      return "";
   }
