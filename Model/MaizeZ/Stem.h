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

// Parameters ----------------------------------------------------------

   float initialDM;
   TableFn heightFn;
   float translocFrac;
   // nitrogen
   float initialNConc;
   TableFn targetNFn;
   TableFn structNFn;
   float dilnNSlope;
   float dilnNInt;
   float retransRate;           // rate that stem biomass can be retranslocated to grain

//  Variables  -----------------------------------------------------

   float density;
   float canopyHeight;
   float dltCanopyHeight;

   // biomass
   float dmGreenStem;           // stem dry weight / plant

   // nitrogen
   float dltNConc;

// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);


// public Methods -------------------------------------------------------
   public:
   Stem(ScienceAPI2 &, Plant *p);

   // plant
   void  calcCanopyHeight(void);
   void  readParams (void);
   void  updateVars(void);
   void  process(void);
   float getCanopyHeight(void)const{return canopyHeight;}

   // nitrogen
   float calcNDemand(void);
   float calcStructNDemand(void);
   float provideN(float requiredN);

   // phosphorus
   float calcPDemand(void);

   // biomass
   void  partitionDM(float dltDM){dltDmGreen = dltDM;}
   float dmRetransAvailable(void);
   void  dmRetrans(float dltDm){dmRetranslocate = dltDm;}

   // phenology
   void  phenologyEvent(int);
   };

//---------------------------------------------------------------------------
//-------------------------------------------------------------------------
}
#endif
