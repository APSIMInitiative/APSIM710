//---------------------------------------------------------------------------
#include <stdexcept>
#include "Registrations.h"

using namespace protocol;

static const unsigned int MAX_NUM_REGISTRATIONS = 2000;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Registrations::Registrations(Component* p)
   : registrations(MAX_NUM_REGISTRATIONS), parent(p)
   {
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Registrations::~Registrations(void)
   {
   for (unsigned i = 0; i != registrations.size(); i++)
      delete registrations[i];
   }
//---------------------------------------------------------------------------
// Find a registration item and return a pointer to it.
// Returns NULL if not found.
//---------------------------------------------------------------------------
RegistrationItem* Registrations::find(RegistrationType kind,
                                      const FString& name)
   {
   return(find(kind, name, FString("")));
   }

RegistrationItem* Registrations::find(RegistrationType kind,
                                      const FString& name,
                                      const FString& componentNameOrID)
   {
   for (unsigned i = 0; i != registrations.size(); ++i)
      {
      if (registrations[i]->isMatch(kind, name, componentNameOrID))
         return registrations[i];
      }
   return NULL;
   }
//---------------------------------------------------------------------------
// Return the number of matching registrations
//---------------------------------------------------------------------------
unsigned Registrations::count(RegistrationType kind,
                              const FString& name,
                              const FString& componentNameOrID)
   {
   unsigned c = 0;
   for (unsigned i = 0; i != registrations.size(); i++)
      {
      if (registrations[i]->isMatch(kind, name, componentNameOrID))
         c++;
      }
   return c;
   }

//---------------------------------------------------------------------------
// Add a registration and return a pointer to it if registration was added.  If
// the registration already exists, then return NULL.
//---------------------------------------------------------------------------
RegistrationItem* Registrations::add(RegistrationType kind,
                                     const FString& name,
                                     const Type& type)
   {
   return(add(kind, name, type, FString("")));
   }

RegistrationItem* Registrations::add(RegistrationType kind,
                                     const FString& name,
                                     const Type& type,
                                     const FString& componentNameOrID)
   {
   RegistrationItem* item = new RegistrationItem(parent, kind, name, type, componentNameOrID);
   registrations.push_back(item);
   return item;
   }

