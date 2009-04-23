#include "RegistrationType.h"
#include <General/string_functions.h>

#include <stdexcept>
using namespace std;

//---------------------------------------------------------------------------
// constructor taking a string.
//---------------------------------------------------------------------------
RegistrationType::RegistrationType(const std::string& st)
   {
   if (Str_i_Eq(st, "get"))
      regType = get;
   else if (Str_i_Eq(st, "set"))
      regType = set;
   else if (Str_i_Eq(st, "event"))
      regType = event;
   else if (Str_i_Eq(st, "respondToGet"))
      regType = respondToGet;
   else if (Str_i_Eq(st, "respondToSet"))
      regType = respondToSet;
   else if (Str_i_Eq(st, "respondToEvent"))
      regType = respondToEvent;
   else
      throw runtime_error("Invalid registration type: " + st);
   }
//---------------------------------------------------------------------------
// Return a string representation of the specified registration type.
//---------------------------------------------------------------------------
string RegistrationType::asString()
   {
    switch (regType)
      {
      case get     : return "get";
      case set     : return "set";
      case event   : return "event";

      case respondToGet     : return "respondToGet";
      case respondToSet     : return "respondToSet";
      case respondToEvent   : return "respondToEvent";

      case respondToGetSet  : return "respondToGetSet";
      };
   throw runtime_error("Invalid registration type.");
   }
//---------------------------------------------------------------------------
// Return the opposite of the specified type.
// e.g. if getVariableReg passed in, returns respondToGetReg
//---------------------------------------------------------------------------
RegistrationType RegistrationType::opposite()
   {
    switch (regType)
      {
      case get      : return respondToGet;
      case set      : return respondToSet;
      case event    : return respondToEvent;

      case respondToGet     : return get;
      case respondToSet     : return set;
      case respondToEvent   : return event;
      };
   throw runtime_error("Invalid registration type.");
   }
//---------------------------------------------------------------------------
// Return true if this type is an active one.
//---------------------------------------------------------------------------
bool RegistrationType::isPassive()
   {
   return (regType == respondToGet || regType == respondToSet ||
           regType == respondToEvent);
   }

