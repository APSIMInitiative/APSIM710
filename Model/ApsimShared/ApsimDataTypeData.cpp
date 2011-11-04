#include <../General/pch.h>
#include <string.h>
#include <vector>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/stristr.h>
#include <General/path.h>
#include <General/xml.h>
#include "FStringExt.h"
#include "ApsimRegistrationData.h"
#include "ApsimDataTypeData.h"


// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
unsigned ApsimDataTypeData::getNumFields(void) const
   {
   unsigned count = 0;
   iterator i = begin();
   while (i != end())
      {
      ++count;
      i++;
      }
   return count;
   }
// ------------------------------------------------------------------
// Return true if data type is a structure.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isStructure(void) const
   {
   return begin() != end();
   }
// ------------------------------------------------------------------
// Return true if data type is an array.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isArray(void) const
   {
     return (Str_i_Eq(node.getAttribute("array"), "T"));
   }
// ------------------------------------------------------------------
// Return true if data type is a built in type
// ------------------------------------------------------------------
bool ApsimDataTypeData::isBuiltIn(void) const
   {
     return (Str_i_Eq(node.getName(), "builtin"));
   }
// ------------------------------------------------------------------
// Return true if data type is a message
// ------------------------------------------------------------------
bool ApsimDataTypeData::isMessage(void) const
   {
   return (Str_i_Eq(node.getAttribute("message"), "T"));
   }
// ------------------------------------------------------------------
// Return true if data type is an array element
// ------------------------------------------------------------------
bool ApsimDataTypeData::isArrayElement(void) const
   {
   return (Str_i_Eq(node.getName(), "element"));
   }
// ------------------------------------------------------------------
// Return true if data type is an array element
// ------------------------------------------------------------------
bool ApsimDataTypeData::isEvent(void) const
   {
   return (Str_i_Eq(node.getName(), "event"));
   }

// ------------------------------------------------------------------
// Return data type name
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return data type kind
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getKind(void) const
   {
   return node.getAttribute("kind");
   }
// ------------------------------------------------------------------
// Return data type description
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getDescription(void) const
   {
   return node.getAttribute("description");
   }
// ------------------------------------------------------------------
// Return data type units
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getUnit(void) const
   {
   return node.getAttribute("unit");
   }
// ------------------------------------------------------------------
// Return data type lower bound
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getLowerBound(void) const
   {
   return node.getAttribute("lower_bound");
   }
// ------------------------------------------------------------------
// Return data type upper bound.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getUpperBound(void) const
   {
   return node.getAttribute("upper_bound");
   }
// ------------------------------------------------------------------
// Return full type string.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getTypeString(void) const
   {
   return node.write();
   }
// ------------------------------------------------------------------
// Return full type string.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getType(void) const
   {
   return node.getAttribute("type");
   }

ApsimDataTypeData::iterator ApsimDataTypeData::begin(void) const {return iterator(TreeNodeIterator<XMLNode>(node.begin()));};
ApsimDataTypeData::iterator ApsimDataTypeData::end(void) const  {return iterator(TreeNodeIterator<XMLNode>(node.end()));};
