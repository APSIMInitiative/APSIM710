//---------------------------------------------------------------------------

#ifndef RachisH
#define RachisH

#include "PlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Rachis : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------

   // nitrogen
   float initialNConc;
   float targetNConc;
   float structRachisNConc;
   float dilnNSlope;
   float dilnNInt;

//  Variables  ----------------------------------------------------------


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);

// public Methods -------------------------------------------------------
   public:
   Rachis(ScienceAPI &, Plant *p);
   ~Rachis();
   // plant
   void  readParams (void);
   void  updateVars(void);

   // biomass
   float partitionDM(float dltDM);

   // nitrogen
   float calcNDemand(void);
   float calcStructNDemand(void);
   float provideN(float requiredN);

   // phosphorus
   float calcPDemand(void);

   // phenology
   void  phenologyEvent(int);
  };


//---------------------------------------------------------------------------
#endif
