#ifndef PlantComponentH
#define PlantComponentH

#include <ComponentInterface/Component.h>
// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Plant model and an APSIM simulation.
// ------------------------------------------------------------------
class IPlant;
class PlantComponent : public protocol::Component
   {
   private:
      IPlant     *plant;                     // The plant model
      XMLDocument* doc;

   public:
      PlantComponent(void);
      ~PlantComponent(void);
      void doInit1(const protocol::Init1Data&);
      void doInit2(void);

      void writeString (const char *msg);
      void warningError (const char *msg);
   };
#endif
