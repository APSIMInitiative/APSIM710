//---------------------------------------------------------------------------
#ifndef xmlH
#define xmlH
#include <string>
#include <general/TreeNodeIterator.h>
#include <general/stl_functions.h>
#include <general/platform.h>

class XMLNode;
struct _xmlNode;
struct _xmlDoc;
//---------------------------------------------------------------------------
// This class encapsulates an XML document and provides an STL like interface.
//---------------------------------------------------------------------------
class EXPORT XMLDocument
   {
   public:
      enum RootName {rootName};
      enum XmlContents {xmlContents};
      XMLDocument(const std::string& rootNode, RootName rootName);
      XMLDocument(const std::string& fileName);
      XMLDocument(const std::string& xml, XmlContents xmlContents);

      ~XMLDocument(void);

      void write(const std::string& fileName);
      XMLNode documentElement(void);

      void setDirty(bool d) {dirty = d;}
      bool isDirty(void) const {return dirty;}

      bool isValid(void);
   private:
      _xmlDoc* doc;
      mutable bool dirty;
   };
//---------------------------------------------------------------------------
// This class encapsulates a node within an XML document
//---------------------------------------------------------------------------
class EXPORT XMLNode
   {
   public:
      typedef TreeNodeIterator<XMLNode> iterator;
      iterator begin() const;
      iterator end() const;

      XMLNode(void) : parent(NULL), node(NULL) { }
      XMLNode(XMLDocument* doc, _xmlNode* n);
       ~XMLNode(void);
      XMLNode(const XMLNode& rhs);
      XMLNode& operator= (const XMLNode& rhs);

      bool isValid(void) const {return (node != NULL);}

      std::string getName(void) const;
      void getAttributes(std::vector<std::string>& attributes) const;
      std::string getAttribute(const std::string& attributeName) const;
      std::string getValue(void) const;

      std::string childValue(const std::string& childName) const;
      std::vector<std::string> childValues(const std::string& childName) const;
      void setAttribute(const std::string& attributeName,
                        const std::string& attributeValue);
      void setValue(const std::string& value);
      void setValueAsCData(const std::string& value);
      XMLNode appendChild(const std::string& nodeName, bool alwaysAppend = false);
      XMLNode appendChild(XMLNode childNode, bool alwaysAppend = false);
      XMLNode::iterator erase(XMLNode::iterator& nodeIterator);

      std::string write() const;
      std::string innerXML();
   bool operator==(const XMLNode& rhs) const
      {
      return node == rhs.node;
      }
   bool operator!=(const XMLNode& rhs) const
      {
      return node != rhs.node;
      }
   XMLNode getNextSibling(void) const;

   private:
      XMLDocument* parent;
      _xmlNode* node;

   };

//---------------------------------------------------------------------------
// Handy predicate that can help find a node with a particular name
// and 'name' attribute.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class NodeEquals
   {
   private:
      std::string nodeName;
      std::string nameAttribute;
   public:
      NodeEquals(const std::string& nodename, const std::string& nameattribute)
         : nodeName(nodename), nameAttribute(nameattribute) {}

      bool operator () (T& arg)
         {return (Str_i_Eq(arg.getName(), nodeName) &&
                  Str_i_Eq(arg.getAttribute("name"), nameAttribute));};
   };
//---------------------------------------------------------------------------
// Handy predicate that can help find a node that has the specified name
// and attribute values.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class NodeAttributesEquals
   {
   private:
      std::string nodeName;
      std::string attribute1Name;
      std::string attribute1Value;
      std::string attribute2Name;
      std::string attribute2Value;
   public:
      NodeAttributesEquals(const std::string& nodename,
                           const std::string& attribute1name,
                           const std::string& attribute1value,
                           const std::string& attribute2name,
                           const std::string& attribute2value)
         : nodeName(nodename),
           attribute1Name(attribute1name),
           attribute1Value(attribute1value),
           attribute2Name(attribute2name),
           attribute2Value(attribute2value)
            {}

      bool operator () (T& arg)
         {return (strcmpi(arg.getName().c_str(), nodeName.c_str()) == 0 &&
                  strcmpi(arg.getAttribute(attribute1Name).c_str(), attribute1Value.c_str()) == 0 &&
                  strcmpi(arg.getAttribute(attribute2Name).c_str(), attribute2Value.c_str()) == 0);}
   };
//---------------------------------------------------------------------------
// Handy function that will append a new child node IF that node doesn't
// already exist.
//---------------------------------------------------------------------------
XMLNode EXPORT appendChildIfNotExist(XMLNode& node,
                              const std::string& nodeName,
                              const std::string& nameAttribute);

//---------------------------------------------------------------------------
// Handy predicate that can help find a node with a particular value.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class ValueEquals
   {
   private:
      std::string value;
   public:
      ValueEquals(const std::string& v) : value(v) {}

      bool operator () (T& arg)
         {return (strcmpi(arg.getValue().c_str(), value.c_str()) == 0);};
   };
//---------------------------------------------------------------------------
// Handy functor that calls T.getAttribute("name") and stores result in a container.
//---------------------------------------------------------------------------
template <class T, class CT=std::vector<std::string> >
class GetNameAttributeFunction
   {
   private:
      CT& Container;
   public:
      GetNameAttributeFunction(CT& container)
         : Container (container)
         { }

      void operator () (T &arg)
         {
         Container.push_back(arg.getAttribute("name"));
         };
   };

//---------------------------------------------------------------------------
// Handy functor that calls T.getValue() and stores result in a container.
//---------------------------------------------------------------------------
template <class CT, class T>
class GetValueFunction
   {
   private:
      CT& Container;
   public:
      GetValueFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back(arg.getValue());
         };
   };
// ------------------------------------------------------------------
// Handy function that will delete all nodes with the specified name.
// ------------------------------------------------------------------
void EXPORT eraseNodes(XMLNode node, const std::string& name);

//---------------------------------------------------------------------------
// Handy predicate that can help find a node with a particular name
// and 'name' attribute.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class AttributeEquals
   {
   private:
      std::string attributeName;
      std::string attributeValue;
   public:
      AttributeEquals(const std::string& attributename, const std::string& attributevalue)
         : attributeName(attributename), attributeValue(attributevalue) {}

      bool operator () (T& arg)
      {return (Str_i_Eq(arg.getAttribute(attributeName), attributeValue));};
   };
// ------------------------------------------------------------------
// Handy function that returns a node given a fully qualified name
// eg fqn:  root|child1|child2
// ------------------------------------------------------------------
XMLNode EXPORT findNode(XMLNode node, const std::string& fqn);
XMLNode EXPORT findNode(XMLNode node, const std::string& fqn, char delimiter);

// ------------------------------------------------------------------
// Handy function that returns a node given a fully qualified name
// This variant uses the 'name' attribute to search.
// eg fqn:  root|child1|child2
// ------------------------------------------------------------------
XMLNode EXPORT findNodeWithName(XMLNode node, const std::string& fqn);
XMLNode EXPORT findNodeWithName(XMLNode node, const std::string& fqn, char delimiter);


// ------------------------------------------------------------------
// Handy function that returns a node value given a fully qualified name
// eg fqn:  root|child1|child2
// ------------------------------------------------------------------
std::string EXPORT findNodeValue(XMLNode node, const std::string& fqn);

#endif
