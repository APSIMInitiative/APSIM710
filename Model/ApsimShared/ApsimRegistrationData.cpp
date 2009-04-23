//---------------------------------------------------------------------------
#include <string>
#include <stdexcept>
#include <vector>

#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/string_functions.h>
#include "ApsimRegistrationData.h"

// ------------------------------------------------------------------
// return true if this registration is an auto-register.
// ------------------------------------------------------------------
bool ApsimRegistrationData::doAutoRegister(void) const
   {
   return !Str_i_Eq(node.getAttribute("autoregister"), "F");
   }
// ------------------------------------------------------------------
// return the name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// return the name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getUnits(void) const
   {
   return node.getAttribute("unit");
   }
// ------------------------------------------------------------------
// return the type of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getType(void) const
   {
   return node.getName();
   }
// ------------------------------------------------------------------
// return data type name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getDataTypeName(void) const
   {
   return node.getAttribute("type");
   }
// ------------------------------------------------------------------
// return data type name of the registration.
// ------------------------------------------------------------------
bool ApsimRegistrationData::isOfType(const std::string& type) const
   {
   if (Str_i_Eq(type, "getVariable"))
      return Str_i_Eq(node.getName(), "getVariableReg");
   else if (Str_i_Eq(type, "setVariable"))
      return Str_i_Eq(node.getName(), "setVariableReg");
   else if (Str_i_Eq(type, "methodCall"))
      return Str_i_Eq(node.getName(), "methodCallReg");
   else if (Str_i_Eq(type, "event"))
      return Str_i_Eq(node.getName(), "eventReg");
   else if (Str_i_Eq(type, "respondToGet"))
      return Str_i_Eq(node.getName(), "respondToGetReg") ||
             Str_i_Eq(node.getName(), "respondToGetSetReg");
   else if (Str_i_Eq(type, "respondToSet"))
      return Str_i_Eq(node.getName(), "respondToSetReg") ||
             Str_i_Eq(node.getName(), "respondToGetSetReg");
   else if (Str_i_Eq(type, "respondToMethodCall"))
      return Str_i_Eq(node.getName(), "respondToMethodCallReg");
   else if (Str_i_Eq(type, "respondToEvent"))
      return Str_i_Eq(node.getName(), "respondToEventReg");
   else if (Str_i_Eq(type, "read"))
      return Str_i_Eq(node.getName(), "read");
   else
      {
      std::string msg = "Bad type of registration.  Type = ";
      msg += type;
      throw msg;
      }
   }
// ------------------------------------------------------------------
// return the internal name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getInternalName(void) const
   {
   return node.getAttribute("internalName");
   }

