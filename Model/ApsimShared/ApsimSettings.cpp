#include <stdexcept>

#include <string>
#include <vector>
#include <sstream>

#include <boost/lexical_cast.hpp>
#include <General/IniFile.h>
#include <General/path.h>
#include <General/platform.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>

#include "ApsimSettings.h"
#include "ApsimVersion.h"
#include "ApsimDirectories.h"

#ifdef __WIN32__
   #include <windows.h>
   #include <shlobj.h>
#endif

using namespace std;
using namespace boost;

// ------------------------------------------------------------------
//	constructor
// ------------------------------------------------------------------
ApsimSettings::ApsimSettings(void)
	{
  string originalPath;
  // 1. find our version number from installation dir
  originalPath  =  getApsimDirectory() + "/Apsim.xml";
  if (!fileExists(originalPath.c_str())) {throw std::runtime_error("Couldn't find ApsimSettings file " + originalPath);}
  original = new XMLDocument(originalPath);

  // 2. look for local customisations
  string localPath;
  string versionNumber;
  this->read("version|apsim", versionNumber);
#ifdef __WIN32__
  char szPath[MAX_PATH];
  if (SHGetFolderPath( NULL, CSIDL_APPDATA, NULL, 0, szPath ) != S_OK)
      return;

  localPath  = string(szPath) + "/Apsim/" + versionNumber + "/Apsim.xml";
#else 
  char *home = getenv("HOME");
  if (home == NULL) 
      return;

  localPath  = string(home) + "/.Apsim/" + versionNumber + "/Apsim.xml";
#endif

  if (fileExists(localPath))  
     {
     delete original;
     original = new XMLDocument(localPath);
     }
  }
// ------------------------------------------------------------------
//	destructor
// ------------------------------------------------------------------
ApsimSettings::~ApsimSettings(void)
   {
   delete original;
   }
// ------------------------------------------------------------------
// refresh.
// ------------------------------------------------------------------
void ApsimSettings::refresh(void)
   {
   }
// ------------------------------------------------------------------
// return the section name from the specified key.
// ------------------------------------------------------------------
string ApsimSettings::getSection(const std::string& key) const
   {
   int posBar = key.find('|');
   if (posBar != string::npos)
      return key.substr(0, posBar);
   else
      return "";
   }
// ------------------------------------------------------------------
// return the key name from the specified key.
// ------------------------------------------------------------------
string ApsimSettings::getKey(const std::string& key) const
   {
   int posBar = key.find('|');
   if (posBar != string::npos)
      return key.substr(posBar+1);
   else
      return "";
   }

// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const std::string& key, std::string& value,
                         bool replaceMacros) const
   {
   XMLNode Node = findNode(original->documentElement(), key);
   if (Node.isValid())
      value = Node.getValue();
   else
      value = "";
   if (replaceMacros)
   {
      replaceAll(value, "%apsim%", getApsimDirectory());
      replaceAll(value, "%ausfarm%", getAusFarmDirectory());
   }
   }

// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, int& value) const
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<int> (stringValue);
   }
// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, bool& value) const
   {
   string stringValue;
   read(key, stringValue);
   if (stringValue == "")
     throw runtime_error("Cannot find value for key: " + key);
   value = Str_i_Eq(stringValue, "yes");
   }
// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, double& value) const
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<double> (stringValue);
   }
// ------------------------------------------------------------------
// Read and return a list of values for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, vector<string>& values,
                         bool replaceMacros) const
   {
   values.clear();
   XMLNode Node = findNode(original->documentElement(), getSection(key));
   if (Node.isValid())
      {
      for_each_if(original->documentElement().begin(), original->documentElement().end(),
                  GetValueFunction<vector<string>, XMLNode>(values),
                  EqualToName<XMLNode>(getKey(key)));
      if (replaceMacros)
         {
         for (unsigned v = 0; v != values.size(); v++)
		 {
            replaceAll(values[v], "%apsim%", getApsimDirectory());
            replaceAll(values[v], "%ausfarm%", getAusFarmDirectory());
		 }
         }
      }
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, const string& value)
   {
   XMLNode Node = findNode(original->documentElement(), getSection(key));
   if (Node.isValid())
      {
      Node = Node.appendChild(getKey(key), false);
      Node.setValue(value);
      }
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, int value)
   {
   write(key, lexical_cast<string>(value));
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, bool value)
   {
   if (value)
      write(key, "yes");
   else
      write(key, "no");
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, double value)
   {
   write(key, lexical_cast<string>(value));
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, const vector<string>& values)
   {
   XMLNode Node = findNode(original->documentElement(), getSection(key));
   if (Node.isValid())
      {
      eraseNodes(Node, getKey(key));
      for (unsigned i = 0; i != values.size(); i++)
         {
         Node = Node.appendChild(getKey(key), true);
         Node.setValue(values[i]);
         }
      }
   }
// ------------------------------------------------------------------
// Return a complete list of all keys under the specified key.
// ------------------------------------------------------------------
void ApsimSettings::getKeysUnder(const string& key, vector<string>& keys)
	{
	for_each(original->documentElement().begin(), original->documentElement().end(),
	         GetName<XMLNode>(keys));
   }

// ------------------------------------------------------------------
// Add in a %apsim% macro to the specified string if possible
// ------------------------------------------------------------------
void ApsimSettings::addMacro(std::string& st)
   {
   string StLower = st;
   To_lower(StLower);
   string ApsimDir = getApsimDirectory();
   To_lower(ApsimDir);
   unsigned Pos = StLower.find(ApsimDir);
   if (Pos != string::npos)
      st.replace(Pos, getApsimDirectory().length(), "%apsim%");
   }

// ------------------------------------------------------------------
// Erase the specified key
// ------------------------------------------------------------------
void ApsimSettings::deleteKey(const std::string& key)
   {
   XMLNode Node = findNode(original->documentElement(), getSection(key));
   if (Node.isValid())
      eraseNodes(Node, getKey(key));
   }

// ------------------------------------------------------------------
// Convert a control file version string e.g. 6.1(2) into major and
// minor numbers.
// ------------------------------------------------------------------
void versionStringToMajorMinor(const std::string& versionString,
                               int& major, int& minor)
   {
   string versionMajor = versionString;
   string versionMinor = splitOffBracketedValue(versionMajor, '(', ')');
   double majorfloat = atof(versionMajor.c_str()) * 10;
   major = int(majorfloat + 0.01);  // we need the 0.01 as there is sometimes round off error.

   if (versionMinor == "")
      minor = 1;
   else
      minor = int(atof(versionMinor.c_str()));
   }

// ------------------------------------------------------------------
// Return a list of conversion nodes to get the specified version number
// up to the most recent APSIM version.
// ------------------------------------------------------------------

void ApsimSettings::getConversionNodes(const std::string& version,
                                       std::vector<XMLNode>& conversionNodes,
                                       std::string& toVersionString)
   {
   toVersionString = "";
   int versionMajor, versionMinor;
   versionStringToMajorMinor(version, versionMajor, versionMinor);

   conversionNodes.clear();
   for (XMLNode::iterator node = original->documentElement().begin();
                          node != original->documentElement().end();
                          node++)
      {
      XMLNode ToNode = findNode(*node, "to");
      if (ToNode.isValid())
         {
         int toMajor, toMinor;
         toVersionString = ToNode.getValue();

         versionStringToMajorMinor(toVersionString, toMajor, toMinor);

         if (toMajor > versionMajor ||
             (toMajor == versionMajor && toMinor > versionMinor) )
             {
            conversionNodes.push_back(*node);
            }
         }
      }
   }

// ------------------------------------------------------------------
// return a component ordering.
// ------------------------------------------------------------------
void ApsimSettings::getComponentOrder(std::vector<std::string>& order)
   {
   XMLNode ComponentOrderNode = findNode(original->documentElement(), "ComponentOrder");
   if (ComponentOrderNode.isValid())
      {
      for (XMLNode::iterator component = ComponentOrderNode.begin();
                             component != ComponentOrderNode.end();
                             component++)
         order.push_back(component->getValue());
      }
   else
      throw runtime_error("Cannot find component order for ConToSim");
   }

