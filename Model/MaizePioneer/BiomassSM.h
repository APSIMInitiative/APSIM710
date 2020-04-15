//---------------------------------------------------------------------------

#ifndef BiomassSMH
#define BiomassSMH

#include "PlantComponents.h"
#include "biomass.h"
namespace Maize {
   //------------------------------------------------------------------------------------------------

   class BiomassSM : public Biomass
      {
      private:

         double minPGR;

         void  doRegistrations(void);
         void  initialize(void);

         void calcBiomassRetranslocation(void);

         // public Methods -------------------------------------------------------
      public:
         // plant
         BiomassSM(ScienceAPI2 &, Plant *p);
         ~BiomassSM();

         virtual void  readParams (void);
         virtual void process(void);

         virtual void  calcPartitioning(void);
         virtual void  calcRetranslocation(void);

      };
   }
#endif
