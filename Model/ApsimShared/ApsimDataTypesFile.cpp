#include <../General/pch.h>
#include <string>
#include <vector>
#include <algorithm>

#include <stdexcept>

#include <General/TreeNodeIterator.h>
#include <General/xml.h>

#include "ApsimDataTypeData.h"
#include "ApsimDataTypesFile.h"
#include "ApsimDirectories.h"

using namespace std;

//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
ApsimDataTypesFile::ApsimDataTypesFile(void)
   : fileName(getApsimDirectory() + "/apsim/infra/datatypes.interface"), xmlDoc(fileName)
   {
   if (!xmlDoc.isValid())
      throw std::runtime_error("Cannot find file: " + fileName);
   }
//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
ApsimDataTypesFile::ApsimDataTypesFile(const string& ddml)
   : xmlDoc(ddml, XMLDocument::xmlContents)
   {
   }
//---------------------------------------------------------------------------
// Return a specific data type to caller.  Will throw if that type doesn't
// exist.
//---------------------------------------------------------------------------
ApsimDataTypeData ApsimDataTypesFile::getDataType(const string& name)
   {
   XMLNode::iterator i = find_if(xmlDoc.documentElement().begin(),
                                 xmlDoc.documentElement().end(),
                                 NodeEquals<XMLNode>("type", name));
   if (i == xmlDoc.documentElement().end())
      i = find_if(xmlDoc.documentElement().begin(),
                  xmlDoc.documentElement().end(),
                  NodeEquals<XMLNode>("builtin", name));

   if (i != xmlDoc.documentElement().end())
      return ApsimDataTypeData(*i);
   else
      return ApsimDataTypeData(*find_if(xmlDoc.documentElement().begin(),
                                       xmlDoc.documentElement().end(),
                                       NodeEquals<XMLNode>("builtin", "string")));
   }
//---------------------------------------------------------------------------
// return iterator to first data type.
//---------------------------------------------------------------------------
ApsimDataTypesFile::iterator ApsimDataTypesFile::begin() 
   {
   return xmlDoc.documentElement().begin();
   }
//---------------------------------------------------------------------------
// return iterator to last data type.
//---------------------------------------------------------------------------
ApsimDataTypesFile::iterator ApsimDataTypesFile::end() 
   {
   return xmlDoc.documentElement().end();
   }

