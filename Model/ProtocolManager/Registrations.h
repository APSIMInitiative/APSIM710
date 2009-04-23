//---------------------------------------------------------------------------
#ifndef PROTOCOLMANAGERRegistrationsH
#define PROTOCOLMANAGERRegistrationsH

#include <set>
#include <vector>
#include <ComponentInterface/RegistrationType.h>

class RegistrationsInternal;

//---------------------------------------------------------------------------
// Simple structure to encapsulate a registration.
//---------------------------------------------------------------------------
struct Registration
   {
   Registration(unsigned compId, unsigned regId,
                const std::string& regName, const std::string& ddmL, RegistrationType regType)
      : componentId(compId), id(regId), name(regName), ddml(ddmL), type(regType)
      { }


   unsigned componentId;
   unsigned id;
   std::string name;
   std::string ddml;
   RegistrationType type;
   };

//---------------------------------------------------------------------------
// This class encapsulates a collection of registrations.
// A componentId, regId and type uniquely identify a registration.
//---------------------------------------------------------------------------
class Registrations
   {
   public:
      //---------------------------------------------------------------------------
      // constructor
      //---------------------------------------------------------------------------
      Registrations(void);

      //---------------------------------------------------------------------------
      // destructor
      //---------------------------------------------------------------------------
      ~Registrations(void);

      //---------------------------------------------------------------------------
      // Add a registration. destinationId if specified, specifically directs
      // this registration to another component.
      //---------------------------------------------------------------------------
      void add(Registration registration, unsigned destinationId = 0);

      //---------------------------------------------------------------------------
      // Return a registration name to caller.
      //---------------------------------------------------------------------------
      Registration get(unsigned componentId, unsigned regId, RegistrationType type);

      //---------------------------------------------------------------------------
      // Return the name of a registration.
      //---------------------------------------------------------------------------
      std::string getName(unsigned componentId, unsigned regId, RegistrationType type);

      //---------------------------------------------------------------------------
      // Return the id of a registration given a name.
      //---------------------------------------------------------------------------
      unsigned find(unsigned componentId, const std::string& regName, RegistrationType type);

      //---------------------------------------------------------------------------
      // Get the destination id of registration.
      //---------------------------------------------------------------------------
      unsigned getDestId(unsigned componentId, unsigned regId, RegistrationType type);

      //---------------------------------------------------------------------------
      // Delete a registration
      //---------------------------------------------------------------------------
      void erase(unsigned componentId, unsigned regId, RegistrationType type);

      //---------------------------------------------------------------------------
      // Find matching registrations. componentId can be 0 so all components
      // are matched. name can be blank so that all registrations are matched.
      //---------------------------------------------------------------------------
      void findMatching(unsigned componentId, const std::string& name, RegistrationType type,
                        std::vector<Registration>& matches);


      //---------------------------------------------------------------------------
      // Return a list of subscribed registrations.
      //---------------------------------------------------------------------------
      typedef std::vector<Registration> Subscriptions;
      void getSubscriptions(unsigned componentId, unsigned regId,
                            RegistrationType type,
                            Subscriptions& subscriptions);

      //---------------------------------------------------------------------------
      // Return true if registration has been resolved.
      //---------------------------------------------------------------------------
      bool isResolved(unsigned componentId, unsigned regId, RegistrationType type);

   private:
      RegistrationsInternal* registrations;

   };
#endif
