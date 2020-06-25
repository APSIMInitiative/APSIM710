//---------------------------------------------------------------------------

#ifndef BiomassCMH
#define BiomassCMH

#include "PlantComponents.h"
#include "Biomass.h"
namespace Maize {
   //------------------------------------------------------------------------------------------------

   class BiomassCM : public Biomass
      {
      private:

         double minPGR;

         void  doRegistrations(void);
         void  initialize(void);

         void calcBiomassRetranslocation(void);

         // public Methods -------------------------------------------------------
      public:
         // plant
         BiomassCM(ScienceAPI2 &, Plant *p);
         ~BiomassCM();

         virtual void  readParams (void);
         virtual void process(void);

         virtual void  calcPartitioning(void);
         virtual void  calcRetranslocation(void);

      };
   }
#endif
