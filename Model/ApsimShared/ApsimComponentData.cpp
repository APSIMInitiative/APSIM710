#include <../General/pch.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/path.h>
#include "FStringExt.h"
#include "ApsimRegistrationData.h"
#include "ApsimDataTypeData.h"
#include "ApsimDataTypesFile.h"
#include "ApsimComponentData.h"
#include "ApsimSystemData.h"

#include <General/platform.h>
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(void)
   : node(NULL, NULL), dataTypesFile(NULL)
   {
   xmlDoc = new XMLDocument("component", XMLDocument::rootName);
   node = xmlDoc->documentElement();
   node.appendChild("initdata");
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const std::string& xml)
   : node(NULL, NULL), dataTypesFile(NULL)
   {
   xmlDoc = new XMLDocument(xml, XMLDocument::xmlContents);
   node = xmlDoc->documentElement();
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const XMLNode& n)
   : node(n), xmlDoc(NULL), dataTypesFile(NULL)
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   if (initData == node.end())
      node.appendChild("initdata");
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ApsimComponentData::~ApsimComponentData()
   {
   delete xmlDoc;
   delete dataTypesFile;
   }
// ------------------------------------------------------------------
// copy constructor.
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const ApsimComponentData& rhs)
   : node(rhs.node), xmlDoc(NULL), dataTypesFile(NULL)
   {
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// Assignment operator.
// ------------------------------------------------------------------
ApsimComponentData& ApsimComponentData::operator=(const ApsimComponentData& rhs)
   {
   node = rhs.node;
   delete xmlDoc;
   xmlDoc = NULL;
   dataTypesFile = NULL;
   haveReadBaseProperties = false;
   return *this;
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return class of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getClass(void) const
   {
   return node.getAttribute("class");
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getExecutableFileName(void) const
   {
   string executable = node.getAttribute("executable");
   if (executable == "")
      {
      XMLNode::iterator execNode = find_if(node.begin(),
                                           node.end(),
                                           EqualToName<XMLNode>("executable"));
      if (execNode != node.end()) 
        {
           executable = execNode->getValue();
	       if (executable == "")
              executable = execNode->getAttribute("name");
        }
      }
#ifdef __WIN32__
   replaceAll(executable, "%dllext%", "dll");
#else
   replaceAll(executable, "%dllext%", "so");
#endif
   return executable;
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getComponentInterfaceFileName(void) const
   {
   XMLNode::iterator interfaceNode = find_if(node.begin(),
                                             node.end(),
                                             EqualToName<XMLNode>("componentinterface"));
   if (interfaceNode == node.end())
      return "";

   else
      return interfaceNode->getValue();
   }
// ------------------------------------------------------------------
// Return an iterator to the initdata node.
// ------------------------------------------------------------------
XMLNode ApsimComponentData::getInitData(void) const
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   return *initData;
   }
// ------------------------------------------------------------------
// Set the name of the component.
// ------------------------------------------------------------------
void ApsimComponentData::setName(const std::string& name)
   {
   node.setAttribute("name", name);
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
void ApsimComponentData::setExecutableFileName(const std::string& executable)
   {
   node.setAttribute("executable", executable);
   }
// ------------------------------------------------------------------
// Return the value of a specific property to caller.
// ------------------------------------------------------------------
std::string ApsimComponentData::getProperty(const std::string& sectionName,
                                            const std::string& name) const
   {
   Sections::iterator section = findSection(sectionName);
   if (section != sections.end())
      {
      Parameters::iterator parameter = section->parameters.find(ToLower(name));
      if (parameter != section->parameters.end())
         return parameter->second;
      else
         {
         // Not found - go look in any derived_from sections.
         parameter = section->parameters.find("derived_from");
         if (parameter != section->parameters.end())
            return getProperty(parameter->second, name);
         }
      }

   return "";
   }
// ------------------------------------------------------------------
// Return a matching section to caller for the specified section name
// ------------------------------------------------------------------
ApsimComponentData::Sections::iterator
ApsimComponentData::findSection(const std::string& sectionName) const
   {
   string realSectionName = sectionName;
   if (Str_i_Eq(realSectionName, "constants") || Str_i_Eq(realSectionName, "parameters"))
      realSectionName = "";
   Sections::iterator section = find_if(sections.begin(), sections.end(),
                                        EqualToName<Section>(realSectionName));
   if (section == sections.end())
      {
      XMLNode sectionNode = getInitData();
      if (realSectionName != "")
          {
          sectionNode = findNodeWithName(getInitData(), realSectionName);
          if (!sectionNode.isValid())
             sectionNode = findNode(getInitData(), realSectionName);
          }

      if (sectionNode.isValid())
         {
         section = sections.insert(sections.end(), Section());
         section->name = realSectionName;
         for (XMLNode::iterator parameter = sectionNode.begin();
                                parameter != sectionNode.end();
                                parameter++)
            section->parameters.insert(make_pair(ToLower(parameter->getName()),
                                                 parameter->getValue()));
         }
      }

   return section;
   }
// ------------------------------------------------------------------
// Return values from the specified table for the specific property
// ------------------------------------------------------------------
string ApsimComponentData::getValuesFromTable(const std::string& name, XMLNode tableNode) const
   {
   string returnString;
   for (XMLNode::iterator row = tableNode.begin();
                          row != tableNode.end();
                          row++)
      for (XMLNode::iterator property = row->begin();
                             property != row->end();
                             property++)
         {
         if (Str_i_Eq(property->getName(), name))
            {
            if (returnString != "")
               returnString += " ";
            returnString += property->getValue();
            }
         }

   return returnString;
   }

// ------------------------------------------------------------------
// Return the value of a specific property to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getProperties(const std::string& sectionName,
                                       vector<string>& names,
                                       vector<string>& values) const
   {
   names.erase(names.begin(), names.end());
   values.erase(values.begin(), values.end());

   Sections::iterator section = findSection(sectionName);
   if (section != sections.end())
      {
      for (Parameters::iterator parameter = section->parameters.begin();
                                parameter != section->parameters.end();
                                parameter++)
         {
         names.push_back(parameter->first);
         values.push_back(parameter->second);
         }
      if (names.size() == 0)
         {
         // Not found - go look in any derived_from sections.
         Parameters::iterator parameter = section->parameters.find("derived_from");
         if (parameter != section->parameters.end())
            return getProperties(parameter->second, names, values);
         }
      }
   }

std::string ApsimComponentData::findProperty(const std::string& name)
   {
   // ------------------------------------------------------------------
   // Go recursively searching for a property. Delimiter = '/'.
   // Return it's value.
   // ------------------------------------------------------------------
   XMLNode foundNode;
   size_t posLastDelimiter = name.rfind('/');
   if (posLastDelimiter != string::npos
       && Str_i_Eq(name.substr(posLastDelimiter+1), "name"))
      {
      foundNode = findNode(node, name.substr(0, posLastDelimiter), '/');
      if (foundNode.isValid())
         return foundNode.getAttribute("name");
      }
   else
      {
      foundNode = findNode(node, name, '/');
      if (foundNode.isValid())
         return foundNode.getValue();
      }
   return "";
   }
// ------------------------------------------------------------------
// return a list of variables to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getVariables(vector<string>& variables) const
   {
   variables.erase(variables.begin(), variables.end());

   Sections::iterator section = findSection("");
   pair<Parameters::iterator, Parameters::iterator>
      p = section->parameters.equal_range(ToLower("variable"));
   for (Parameters::iterator parameter = p.first; parameter != p.second; parameter++)
      variables.push_back(parameter->second);
   }
// Helper function for below
template <class T, class CT=std::vector<std::string> >
class GetRulesFunction
   {
   private:
      CT& Container;
   public:
      GetRulesFunction(CT& container)
         : Container (container)
         { }
      void operator () (T &arg)
         {
         if (Str_i_Eq(arg.getName(), "script") || Str_i_Eq(arg.getName(), "rule"))
            {
            string scriptName = arg.getAttribute("name");
            if (scriptName == "")
               scriptName = arg.childValue("event");
            Container.push_back(scriptName);
            }
         };
   };
// ------------------------------------------------------------------
// return a list of rule names to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getRuleNames(vector<string>& names) const
   {
   XMLNode initData = getInitData();
   for_each(initData.begin(), initData.end(),
            GetRulesFunction<XMLNode>(names));
   }

// ------------------------------------------------------------------
// return a rule to caller or blank if not found.
// Some are "rules", some are "scripts". 
// "rules" are found in both .con files and .apsim files.
// "scripts" are only in .apsim files
// each rule has it's own <ui> 
// ------------------------------------------------------------------
void ApsimComponentData::getRule(const std::string& name,
                                 std::string& condition,
                                 std::string& contents) const
   {
   contents = "";
   XMLNode initData = getInitData();
   XMLNode::iterator ui = find_if(initData.begin(), initData.end(),
                                  EqualToName<XMLNode>("ui"));

   for (XMLNode::iterator script = initData.begin(); script != initData.end(); script++)
      {
      string scriptName = script->getAttribute("name");
      if (scriptName == "")
         scriptName = script->childValue("event");
      if (scriptName == name)
         {
         string chunk;
         if (script->getName() == "rule")
            {
            XMLNode::iterator localui = find_if(initData.begin(), initData.end(),
                                                EqualToName<XMLNode>("ui"));
            if (localui != initData.end()) { ui = localui; } // This is not pretty. 
            // It only works by chance that 'script's under this node find this ui -
            // I'd prefer that we parsed 'rule/script's differently than scripts.
            	
            condition = script->getAttribute("condition");
            replaceManagerMacros(condition, *ui);

            chunk = script->getValue();
            }
         else
            {
            condition = findNodeValue(*script, "event");
            replaceManagerMacros(condition, *ui);

            chunk = findNodeValue(*script, "text");
            }
         replaceManagerMacros(chunk, *ui);
         contents += chunk;
         }
      }
   Replace_all(contents, "[cr]", "\n");
   }
// ------------------------------------------------------------------
// Replace all manager macros found in the specified contents
// by using the nodes under <ui>
// ------------------------------------------------------------------
void ApsimComponentData::replaceManagerMacros(std::string& contents, XMLNode ui) const
   {
   if (ui.isValid())
      {
      replaceAll(contents, "[name]", getName());
      for (XMLNode::iterator child = ui.begin(); child != ui.end(); child++)
         {
         if (child->getName() != "category")
            {
            replaceAll(contents, "[" + child->getName() + "]", child->getValue());

            }
         }
      }
   }
// ------------------------------------------------------------------
// Return the contents of this service as an xml string.
// ------------------------------------------------------------------
std::string ApsimComponentData::getXML(void) const
   {
   return node.write();
   }
// ------------------------------------------------------------------
// Return an iterator to the first registration
// ------------------------------------------------------------------
ApsimComponentData::RegIterator ApsimComponentData::regBegin(void) const
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 EqualToName<XMLNode>("registrations"));
   if (i != node.end())
      return i->begin();
   else
      return regEnd();
   }
// ------------------------------------------------------------------
// Return an iterator to the last registration
// ------------------------------------------------------------------
ApsimComponentData::RegIterator ApsimComponentData::regEnd(void) const
   {
   return RegIterator(node.end());
   }

//---------------------------------------------------------------------------
// Return a specific data type to caller.  Will throw if that type doesn't
// exist.
//---------------------------------------------------------------------------
ApsimDataTypeData ApsimComponentData::getDataType
   (const string& name) const
   {
   XMLNode::iterator types = find_if(node.begin(),
                                     node.end(),
                                     EqualToName<XMLNode>("types"));
   if (types != node.end())
      {
      XMLNode::iterator i = find_if(types->begin(),
                                    types->end(),
                                    NodeEquals<XMLNode>("type", name));
      if (i != types->end())
         return ApsimDataTypeData(*i);
      }
   if (dataTypesFile == NULL)
      dataTypesFile = new ApsimDataTypesFile;
   return dataTypesFile->getDataType(name);
   }

// ------------------------------------------------------------------
// return the name of the interface file for this component
// ------------------------------------------------------------------
std::string ApsimComponentData::getInterfaceFileName(void) const
   {
   string dll = getExecutableFileName();
   if (dll != "")
      {
      Path interfaceFilePath(dll);
      interfaceFilePath.Back_up_directory();
      interfaceFilePath.Set_extension(".interface");
      if (interfaceFilePath.Exists())
         return interfaceFilePath.Get_path();
      }
   return "";
   }


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
extern "C" uintptr_t EXPORT STDCALL newApsimComponentData
   (const char* xml, unsigned xmlLength)
   {
   string xmlString(xml, xmlLength);
   return (uintptr_t) new ApsimComponentData(xmlString);
   }
extern "C" void EXPORT STDCALL deleteApsimComponentData
   (ApsimComponentData* componentData)
   {
   delete componentData;
   }
extern "C" bool EXPORT STDCALL ApsimComponentData_getProperty
   (ApsimComponentData* componentData,
    const FString& propertyType,
    const FString& name,
    FString& value)
   {
   string lowerValue = componentData->getProperty(asString(propertyType), asString(name));
   To_lower(lowerValue);
   value = lowerValue.c_str();
   return (value.length() > 0);
   }
extern "C" void EXPORT STDCALL ApsimComponentData_getRuleNames
   (ApsimComponentData** componentData,
    char* names,
    unsigned* maxNumNames,
    unsigned* numNames,
    unsigned namesLength)
   {
   vector<string> ruleNames;
   (*componentData)->getRuleNames(ruleNames);
   FStrings(names, namesLength, *maxNumNames, 0) = ruleNames;
   *numNames = ruleNames.size();
   }

typedef vector<string> *stringVector;

// DANGER - to allow for 64-bit pointers, we use a 64-bit "handle".
// The calling program needs to be careful to call this with a
// corresponding 64 bit argument
extern "C" void EXPORT STDCALL ApsimComponentData_allocateRules(uint64_t* handle)
{
	*handle = uint64_t(new vector<string>);
}

extern "C" void EXPORT STDCALL ApsimComponentData_deallocateRules(uint64_t* handle)
{
	delete (stringVector)*handle;
}

extern "C" void EXPORT STDCALL ApsimComponentData_loadRule
   (uint64_t* handle,
    ApsimComponentData** componentData,
    const char* name,
	char* condition,
    unsigned nameLength,
	unsigned conditionLength)
   {
   string contents;
   string ruleCondition;
   (*componentData)->getRule(asString(FString(name, nameLength, FORString)),
                             ruleCondition,
                             contents);
   FString(condition, conditionLength, EmptyString) = ruleCondition.c_str();
   std::replace( contents.begin(), contents.end(), '\t', ' '); // replace all tabs with spaces
   Split_string(contents, "\n", *(stringVector(*handle)));
   }
extern "C" unsigned EXPORT STDCALL ApsimComponentData_getNumRuleLines
   (uint64_t* handle)
   {
   return stringVector(*handle)->size();
   }
extern "C" void EXPORT STDCALL ApsimComponentData_getRuleLine
   (uint64_t* handle,
    unsigned* lineNumber,
    char* line,
    unsigned lineLength)
   {
   FString(line, lineLength, EmptyString) = (*stringVector(*handle))[*lineNumber].c_str();
   }

