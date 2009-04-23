//---------------------------------------------------------------------------

#include <stdexcept>
#include "ApsimRegistration.h"

bool ApsimRegistration::matchSubscriberType(const ApsimRegistration *rhs) 
  {
  if (type == ::get) 
     {
     return (rhs->type == ::respondToGet || 
             rhs->type == ::respondToGetSet);
     }
  else if (type == ::set) 
     {
     return (rhs->type == ::respondToSet || 
             rhs->type == ::respondToGetSet);
     }
  else if (type == ::respondToGet) 
     {
     return (rhs->type == ::get);
     }
  else if (type == ::respondToSet) 
     {
     return (rhs->type == ::set);
     }
  else if (type == ::event) 
     {
     return (rhs->type == ::respondToEvent);
     }
  else if (type == ::respondToEvent) 
     {
     return (rhs->type == ::event);
     }
  /* notreached */
  throw std::runtime_error("Unknown event type code");
  }

