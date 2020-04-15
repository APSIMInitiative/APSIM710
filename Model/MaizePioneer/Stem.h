//---------------------------------------------------------------------------
//# define PI 3.14159265359

#ifndef StemH
#define StemH

#include "PlantComponents.h"
#include "Utilities.h"
namespace Maize {
   //------------------------------------------------------------------------------------------------

   class Stem : public PlantPart
      {
      protected:
         // Parameters ----------------------------------------------------------

         double initialDM;
         TableFn heightFn;
         double translocFrac;
         // nitrogen
         double initialNConc;
         TableFn targetNFn;
         TableFn structNFn;
         double dilnNSlope;
         double dilnNInt;
         double retransRate;           // rate that stem biomass can be retranslocated to grain

			double stemWtAvail;		// available
         //  Variables  -----------------------------------------------------

         double density;
         double canopyHeight;
         double dltCanopyHeight;

         // biomass
         double dmGreenStem;           // stem dry weight / plant
         
         // nitrogen
         double dltNConc;

         // Private Methods -------------------------------------------------------
         void  doRegistrations(void);
         void  initialize(void);



         // public Methods -------------------------------------------------------
      public:
         Stem(ScienceAPI2 &, Plant *p);

         // plant
         void  calcCanopyHeight(void);
         virtual void  readParams (void);
         virtual void  updateVars(void);
         void  process(void);
         double getCanopyHeight(void)const{return canopyHeight;}

         // nitrogen
         double calcNDemand(void);
         double calcStructNDemand(void);
         double provideN(double requiredN);

         // phosphorus
         double calcPDemand(void);

         // biomass
         void  partitionDM(double dltDM){dltDmGreen = dltDM;}
         virtual double dmRetransAvailable(void);
         void  dmRetrans(double dltDm){dmRetranslocate = dltDm;}

         // phenology
         void  phenologyEvent(int);

      };
   }
#endif
