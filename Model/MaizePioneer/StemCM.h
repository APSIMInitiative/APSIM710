//---------------------------------------------------------------------------
//# define PI 3.14159265359

#ifndef StemCMH
#define StemCMH

#include "Stem.h"
namespace Maize {
   //------------------------------------------------------------------------------------------------

   class StemCM : public Stem
      {

   
      // Private Methods -------------------------------------------------------
      void  doRegistrations(void);
      void  initialize(void);

      // Pioneer Cohort
      double dmLabile;            // dynamic pool of remobilisable carbon
      double dltDmLabile;

      double stemRSCfrac;


      // public Methods -------------------------------------------------------
      public:
         StemCM(ScienceAPI2 &, Plant *p);

         // plant
         virtual void  readParams (void);
         virtual void  updateVars(void);
   
         // biomass
         virtual double dmRetransAvailable(void);
         
         // phenology
         virtual void  phenologyEvent(int);

            // Pioneer Cohort
         double partitionLabileDM(double carbo);
      };
   }
#endif
