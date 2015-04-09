//---------------------------------------------------------------------------
#include <General/pch.h>
#pragma hdrstop

#include <stdexcept>
#include "Registrations.h"
#include <General/stl_functions.h>
#include <General/string_functions.h>

#pragma package(smart_init)
using namespace std;

using std::placeholders::_1;
using std::placeholders::_2;
using std::placeholders::_3;

//---------------------------------------------------------------------------
// internal registration class.
//---------------------------------------------------------------------------
struct RegistrationInternal
   {
   RegistrationInternal(Registration reg, int destId)
      : details(reg), destinationId(destId), resolved(false) { }

   Registration details;
   unsigned destinationId;
   vector<Registration> subscriptions;
   bool resolved;

   bool operator< (const RegistrationInternal& rhs) const
      {
      return (details.componentId < rhs.details.componentId ||
              (details.componentId == rhs.details.componentId && details.id < rhs.details.id));
      }
   bool operator== (const RegistrationInternal& rhs) const
      {
      return (details.componentId == rhs.details.componentId &&
              (details.id == rhs.details.id));
      }

   };
typedef std::vector<RegistrationInternal> Regs;


//---------------------------------------------------------------------------
// Return true if the specified registration matches the componentId
// and name. componentId can be 0 so all components
// are matched. name can be blank so that all registrations are matched.
//---------------------------------------------------------------------------
bool isAMatch(const RegistrationInternal& regInternal,
              unsigned componentIdToMatch, const string& name, unsigned ourComponentId)
   {
   bool doesMatch = true;
   if (componentIdToMatch > 0)
      doesMatch = (componentIdToMatch == regInternal.details.componentId);
   if (doesMatch && regInternal.destinationId > 0)
      doesMatch = (regInternal.destinationId == ourComponentId);
   if (doesMatch && name.length() > 0)
      doesMatch = Str_i_Eq(name, regInternal.details.name);
   return doesMatch;
   }
//---------------------------------------------------------------------------
// Return true if the specified registration matches the componentId
// and name.
//---------------------------------------------------------------------------
bool matchOnName(const RegistrationInternal& regInternal,
                 unsigned componentId, const string& name)
   {
   return (componentId == regInternal.details.componentId &&
           Str_i_Eq(name, regInternal.details.name));
   }
//---------------------------------------------------------------------------
// Return true if the specified registration matches the componentId
// and registrationId.
//---------------------------------------------------------------------------
bool matchOnId(const RegistrationInternal& regInternal,
               unsigned componentId, unsigned regId)
   {
   return (componentId == regInternal.details.componentId &&
           regId == regInternal.details.id);
   }
//---------------------------------------------------------------------------
// Function to do a push_back on a Regs container. Useful in a for_each context.
//---------------------------------------------------------------------------
void addRegistration(vector<Registration>* registrations, RegistrationInternal& regInternal)
   {
   registrations->push_back(regInternal.details);
   }



//---------------------------------------------------------------------------
// Internal registrations class.
//---------------------------------------------------------------------------
class RegistrationsInternal
   {
   private:
      Regs gets;
      Regs sets;
      Regs events;
      Regs respondToGets;
      Regs respondToSets;
      Regs respondToEvents;
      static bool beginResolving;

      //---------------------------------------------------------------------------
      // Get a reference to a container of registrations of type 'type'
      //---------------------------------------------------------------------------
      Regs& getRegsForType(RegistrationType type)
         {
         if (type == RegistrationType::get)
            return gets;
         if (type == RegistrationType::set)
            return sets;
         if (type == RegistrationType::event)
            return events;
         if (type == RegistrationType::respondToGet)
            return respondToGets;
         if (type == RegistrationType::respondToSet)
            return respondToSets;
         if (type == RegistrationType::respondToEvent)
            return respondToEvents;
         throw runtime_error("Bad registration type in begin");
         }
      //---------------------------------------------------------------------------
      // Resolve the specified registration.
      //---------------------------------------------------------------------------
      void resolve(RegistrationInternal& regInternal)
         {
         regInternal.subscriptions.clear();
         Registration& reg = regInternal.details;

         if (reg.type.isPassive())
            {
            // loop through all active registrations and resolve each one.
            Regs& activeRegistrations = getRegsForType(reg.type.opposite());
            for_each_if(activeRegistrations.begin(), activeRegistrations.end(),
                        std::bind(&RegistrationsInternal::resolve, this, _1),
                        std::bind(&isAMatch, _1, regInternal.destinationId, reg.name, regInternal.details.componentId));
            }
         else
            findMatching(regInternal.destinationId, reg.name, reg.type.opposite(), regInternal.subscriptions,
                         regInternal.details.componentId);
         regInternal.resolved = true;
         }

   public:
      //---------------------------------------------------------------------------
      // Add a registration. destinationId if specified, specifically directs
      // this registration to another component.
      //---------------------------------------------------------------------------
      void add(Registration& registration, unsigned destinationId)
         {
         Regs& registrations = getRegsForType(registration.type);
         if (find_if(registrations.begin(),
                     registrations.end(),
                     std::bind(&matchOnId, _1, registration.componentId, registration.id))
             != registrations.end())
            throw runtime_error("Registration already exists. Cannot re-add. Registration name = " + registration.name);

         registrations.push_back(RegistrationInternal(registration, destinationId));
         RegistrationInternal newReg = registrations[registrations.size()-1];

         // resolve if necessary.
         if (beginResolving && !newReg.resolved)
            resolve(newReg);
         }
      //---------------------------------------------------------------------------
      // Return a registration to caller.
      //---------------------------------------------------------------------------
      Registration get(unsigned componentId, unsigned regId, RegistrationType type)
         {
         Regs& registrations = getRegsForType(type);
         Regs::iterator reg = find_if(registrations.begin(),
                                      registrations.end(),
                                      std::bind(&matchOnId, _1, componentId, regId));
         if (reg == registrations.end())
            throw runtime_error("Cannot get registration " + std::to_string((_ULonglong)regId));

         return reg->details;
         }
      //---------------------------------------------------------------------------
      // Return a registration to caller given a name
      //---------------------------------------------------------------------------
      Registration get(unsigned componentId, const string& regName, RegistrationType type)
         {
         Regs& registrations = getRegsForType(type);
         Regs::iterator reg = find_if(registrations.begin(),
                                      registrations.end(),
                                      std::bind(&isAMatch, _1, componentId, regName, 0));
         if (reg == registrations.end())
            throw runtime_error("Cannot get registration " + regName);

         return reg->details;
         }

      //---------------------------------------------------------------------------
      // Delete a registration
      //---------------------------------------------------------------------------
      void erase(unsigned componentId, unsigned regId, RegistrationType type)
         {
         Regs& registrations = getRegsForType(type);
         Regs::iterator reg = find_if(registrations.begin(),
                                      registrations.end(),
                                      std::bind(&matchOnId, _1, componentId, regId));
         if (reg == registrations.end())
            throw runtime_error("Cannot erase registration " + std::to_string((_ULonglong)regId));

         registrations.erase(reg);
         }
      //---------------------------------------------------------------------------
      // Return a list of subscribed registrations.
      //---------------------------------------------------------------------------
      void getSubscriptions(unsigned componentId, unsigned regId,
                            RegistrationType type,
                            vector<Registration>& subscriptions)
         {
         Regs& registrations = getRegsForType(type);
         Regs::iterator reg = find_if(registrations.begin(),
                                      registrations.end(),
                                      std::bind(&matchOnId, _1, componentId, regId));
         if (reg == registrations.end())
            throw runtime_error("Cannot get subscriptions for registration " + std::to_string((_ULonglong)regId));
         if (reg->details.type.isPassive())
            throw runtime_error("Cannot get subscriptions for a passive registration. RegId = " + std::to_string((_ULonglong)regId));

         beginResolving = true;
         if (!reg->resolved)
            resolve(*reg);

         subscriptions = reg->subscriptions;
         }

      //---------------------------------------------------------------------------
      // Find matching registrations. componentId can be 0 so all components
      // are matched. name can be blank so that all registrations are matched.
      //---------------------------------------------------------------------------
      void findMatching(unsigned componentId, const std::string& name, RegistrationType type,
                        std::vector<Registration>& matches, unsigned ourComponentId)
         {
         Regs& registrations = getRegsForType(type);
         for_each_if(registrations.begin(), registrations.end(),
                     std::bind(&addRegistration, &matches, _1),
                     std::bind(&isAMatch, _1, componentId, name, ourComponentId));
         }
      //---------------------------------------------------------------------------
      // Return true if registration has been resolved.
      //---------------------------------------------------------------------------
      bool isResolved(unsigned componentId, unsigned regId, RegistrationType type)
         {
         Regs& registrations = getRegsForType(type);
         Regs::iterator reg = find_if(registrations.begin(),
                                      registrations.end(),
                                      std::bind(&matchOnId, _1, componentId, regId));
         if (reg == registrations.end())
            throw runtime_error("Cannot get registration " + std::to_string((_ULonglong)regId));
         return reg->resolved;
         }
      //---------------------------------------------------------------------------
      // Get the destination id of registration.
      //---------------------------------------------------------------------------
      unsigned getDestId(unsigned componentId, unsigned regId, RegistrationType type)
         {
         Regs& registrations = getRegsForType(type);
         Regs::iterator reg = find_if(registrations.begin(),
                                      registrations.end(),
                                      std::bind(&matchOnId, _1, componentId, regId));
         if (reg == registrations.end())
            throw runtime_error("Cannot find registration " + std::to_string((_ULonglong)regId));

         return reg->destinationId;
         }

   };
bool RegistrationsInternal::beginResolving = false;







//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Registrations::Registrations(void)
   {
   registrations = new RegistrationsInternal;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Registrations::~Registrations(void)
   {
   delete registrations;
   }
//---------------------------------------------------------------------------
// Add a registration. destinationId if specified, specifically directs
// this registration to another component.
//---------------------------------------------------------------------------
void Registrations::add(Registration registration, unsigned destinationId)
   {
   if (registration.type == RegistrationType::respondToGetSet)
      {
      registration.type = RegistrationType::respondToGet;
      registrations->add(registration, destinationId);
      registration.type = RegistrationType::respondToSet;
      registrations->add(registration, destinationId);
      }

   else
      registrations->add(registration, destinationId);
   }
//---------------------------------------------------------------------------
// Return a registration name to caller.
//---------------------------------------------------------------------------
Registration Registrations::get(unsigned componentId, unsigned regId, RegistrationType type)
   {
   return registrations->get(componentId, regId, type);
   }
//---------------------------------------------------------------------------
// Return the name of a registration.
//---------------------------------------------------------------------------
std::string Registrations::getName(unsigned componentId, unsigned regId, RegistrationType type)
   {
   Registration reg = registrations->get(componentId, regId, type);
   return reg.name;
   }
//---------------------------------------------------------------------------
// Get the destination id of registration.
//---------------------------------------------------------------------------
unsigned Registrations::getDestId(unsigned componentId, unsigned regId, RegistrationType type)
   {
   return registrations->getDestId(componentId, regId, type);
   }

//---------------------------------------------------------------------------
// Delete a registration
//---------------------------------------------------------------------------
void Registrations::erase(unsigned componentId, unsigned regId, RegistrationType type)
   {
   registrations->erase(componentId, regId, type);
   }
//---------------------------------------------------------------------------
// Return a list of subscribed registrations.
//---------------------------------------------------------------------------
void Registrations::getSubscriptions(unsigned componentId, unsigned regId,
                                     RegistrationType type,
                                     Subscriptions& subscriptions)
   {
   registrations->getSubscriptions(componentId, regId, type, subscriptions);
   }

//---------------------------------------------------------------------------
// Find matching registrations. componentId can be 0 so all components
// are matched. name can be blank so that all registrations are matched.
//---------------------------------------------------------------------------
void Registrations::findMatching(unsigned componentId, const std::string& name, RegistrationType type,
                                 std::vector<Registration>& matches)
   {
   registrations->findMatching(componentId, name, type, matches, 0);
   }
//---------------------------------------------------------------------------
// Return true if registration has been resolved.
//---------------------------------------------------------------------------
bool Registrations::isResolved(unsigned componentId, unsigned regId, RegistrationType type)
   {
   return registrations->isResolved(componentId, regId, type);
   }

