#include <general/string_functions.h>

#include "ApsimRegistrationType.h"

#include <stdexcept>
using namespace std;

//---------------------------------------------------------------------------
EventTypeCode stringToTypeCode(const std::string& st)
   {
   if (Str_i_Eq(st, "get"))
      return get;
   else if (Str_i_Eq(st, "set"))
      return set;
   else if (Str_i_Eq(st, "event"))
      return  event;
   else if (Str_i_Eq(st, "respondToGet"))
      return  respondToGet;
   else if (Str_i_Eq(st, "respondToSet"))
      return  respondToSet;
   else if (Str_i_Eq(st, "respondToEvent"))
      return  respondToEvent;

   throw runtime_error("Invalid registration type: " + st);
   }
//---------------------------------------------------------------------------
// Return a string representation of the specified registration type.
//---------------------------------------------------------------------------
std::string typeCodeToString (EventTypeCode type) 
   {
   switch (type)
      {
      case get     : return "get";
      case set     : return "set";
      case event   : return "event";

      case respondToGet     : return "respondToGet";
      case respondToSet     : return "respondToSet";
      case respondToEvent   : return "respondToEvent";

      case respondToGetSet  : return "respondToGetSet";
      };
   throw runtime_error(string("Invalid registration type: ") + itoa(type));
   }
//---------------------------------------------------------------------------
// Return the opposite of the specified type.
// e.g. if getVariableReg passed in, returns respondToGetReg
//---------------------------------------------------------------------------
EventTypeCode opposite(EventTypeCode type)
   {
    switch (type)
      {
      case get      : return respondToGet;
      case set      : return respondToSet;
      case event    : return respondToEvent;

      case respondToGet     : return get;
      case respondToSet     : return set;
      case respondToEvent   : return event;
      };
   throw runtime_error(string("Invalid opposite registration type: ") + itoa(type));
   }
