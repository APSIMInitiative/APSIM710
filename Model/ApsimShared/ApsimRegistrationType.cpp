#include <../General/pch.h>
#include <General/string_functions.h>

#include "ApsimRegistrationType.h"

#include <stdexcept>

//---------------------------------------------------------------------------
EventTypeCode stringToTypeCode(const std::string& st)
   {
   if (Str_i_Eq(st, "get"))
      return EventTypeCode::get;
   else if (Str_i_Eq(st, "set"))
      return EventTypeCode::set;
   else if (Str_i_Eq(st, "event"))
      return EventTypeCode::event;
   else if (Str_i_Eq(st, "respondToGet"))
      return EventTypeCode::respondToGet;
   else if (Str_i_Eq(st, "respondToSet"))
      return EventTypeCode::respondToSet;
   else if (Str_i_Eq(st, "respondToEvent"))
      return EventTypeCode::respondToEvent;

   throw std::runtime_error("Invalid registration type: " + st);
   }
//---------------------------------------------------------------------------
// Return a string representation of the specified registration type.
//---------------------------------------------------------------------------
const std::string typeCodeToString (EventTypeCode type)
   {
   switch (type)
      {
      case EventTypeCode::get     : return "get";
      case EventTypeCode::set     : return "set";
      case EventTypeCode::event   : return "event";

      case EventTypeCode::respondToGet     : return "respondToGet";
      case EventTypeCode::respondToSet     : return "respondToSet";
      case EventTypeCode::respondToEvent   : return "respondToEvent";

      case EventTypeCode::respondToGetSet  : return "respondToGetSet";
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
      case EventTypeCode::get      : return EventTypeCode::respondToGet;
      case EventTypeCode::set      : return EventTypeCode::respondToSet;
      case EventTypeCode::event    : return EventTypeCode::respondToEvent;

      case EventTypeCode::respondToGet     : return EventTypeCode::get;
      case EventTypeCode::respondToSet     : return EventTypeCode::set;
      case EventTypeCode::respondToEvent   : return EventTypeCode::event;
      };
   throw std::runtime_error(std::string("Invalid opposite registration type: ") + itoa(type));
   }
