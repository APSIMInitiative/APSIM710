//---------------------------------------------------------------------------

#ifndef GrainH
#define GrainH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Maize {
//------------------------------------------------------------------------------------------------

class Grain : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------
   float dmPerSeed;
   float waterContent;

   // nitrogen
   float grainFillRate;
   float targetNConc;

// Variables ----------------------------------------------------------

   float totDMGreenFI;          // total plant dm at FI
   float grainNo;
   float finalGrainNo;
   float grainSize;
   float yield;
   float potGFRate;             // potential grain filling rate in g/grain/oCd
   float maxGFRate;             // maximum grain filling rate in g/grain/oCd


   float dltDMStressMax;
   float dltDMGrainDemand;
   float addGrainWeight;        // used in source sink demand

   // parameters for grain number / ASI response to drought - Karine Chenu et al. 1/2007    KC2007
   int UseGN;
   float PGRt0, PGRt1;              // period before and after anthesis to calculate PGR
   int GNmax;                       // maximum grain number
   float PGRbase;                   // base PGR
   float GNk;                       // k in the eqn Andrade et al. 1999
   // variables for grain number / ASI response to drought - Karine Chenu et al. 1/2007    KC2007
   float plantDMt0, plantDMt1;      // plant dm at PGRt1 and t2
   int nDays;
   float grainPGR;
   double pKGR;                     // potential kernel growth rate in mg/grain/oC
   float potKernelWt;               // potential kernel weight in mg/grain


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   float calcGrainNumber(void);
   float yieldPartDemandStress(void);
   float calcDMGrainSourceSink(void);
   void  calcDemandStress(void);
   void  calcBiomassDemand(void);
   float calcDMGrainDemand(void);

// public Methods -------------------------------------------------------
   public:
    Grain(ScienceAPI2 &, Plant *p);
   ~Grain();

   // plant
   void  readParams (void);
   void  updateVars(void);
   void  process(void);

   // nitrogen
   float calcNDemand(void);
   void  RetranslocateN(float N);

   // biomass
   float partitionDM(float dltDM);
   float grainDMDifferential(void);
   void  dmRetrans(float dltDm){dmRetranslocate = dltDm;}
   void  Harvest(void);

   // nitrogen
   float getNConc(void)const{return nConc;}

   // phosphorus
   float calcPDemand(void);
   float calcPRetransDemand(void);
   float getPConc(void)const{return pConc;}

   // phenology
   void  phenologyEvent(int);

   void  Summary(void);
   };


//---------------------------------------------------------------------------
}
#endif
