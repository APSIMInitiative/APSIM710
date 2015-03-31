#include <../General/pch.h>
#include <General/string_functions.h>

#include "ApsimRegistrationType.h"

#include <stdexcept>

// When using the pre-compiled headers from General, MS C++ thinks "get" is ambiguous
// We can avoid that problem by qualifying "get" with its enumeration, but that's a
// nonstandard extension. Hence we only do this for Windows
//#ifdef __WIN32__
#define get EventTypeCode::get
//#endif

//---------------------------------------------------------------------------
EventTypeCode stringToTypeCode(const std::string& st)
   {
   if (Str_i_Eq(st, "get"))
      return get;
   else if (Str_i_Eq(st, "set"))
      return set;
   else if (Str_i_Eq(st, "event"))
      return event;
   else if (Str_i_Eq(st, "respondToGet"))
      return respondToGet;
   else if (Str_i_Eq(st, "respondToSet"))
      return respondToSet;
   else if (Str_i_Eq(st, "respondToEvent"))
      return respondToEvent;

   throw std::runtime_error("Invalid registration type: " + st);
   }
//---------------------------------------------------------------------------
// Return a string representation of the specified registration type.
//---------------------------------------------------------------------------
const std::string typeCodeToString (EventTypeCode type)
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
   throw std::runtime_error(std::string("Invalid registration type: ") + itoa(type));
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
   throw std::runtime_error(std::string("Invalid opposite registration type: ") + itoa(type));
   }
