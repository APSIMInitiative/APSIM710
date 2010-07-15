//---------------------------------------------------------------------------

#ifndef GrainH
#define GrainH

#include "PlantComponents.h"
#include "Utilities.h"

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

   // heat effects on grain number
   vector<float> grainTempWindow;
   vector<float> grainTempOrdinals;
   TableFn grainTempTable;
   float tempFactor;


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

// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   float calcGrainNumber(void);
   float yieldPartDemandStress(void);
   float calcDMGrainSourceSink(void);
   void  calcDemandStress(void);
   void  calcBiomassDemand(void);
   float  calcTempFactor(void);              // high temp stress on grain number

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
#endif
