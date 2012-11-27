//---------------------------------------------------------------------------

#ifndef GrainH
#define GrainH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Sorghum {
//------------------------------------------------------------------------------------------------
class Grain : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------
   double dmPerSeed;
   double maxGFRate;             // maximum grain filling rate in g/grain/oCd
   double waterContent;

   // nitrogen
   double grainFillRate;
   double targetNConc;

   // heat effects on grain number
   vector<double> grainTempWindow;
   vector<double> grainTempOrdinals;
   TableFn grainTempTable;


// Variables ----------------------------------------------------------

   double totDMGreenFI;          // total plant dm at FI
   double grainNo;
   double finalGrainNo;
   double grainSize;
   double yield;
   double potGFRate;             // potential grain filling rate in g/grain/oCd

   double dltDMStressMax;
   double dltDMGrainDemand;

	double tempFactor;

// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   double calcGrainNumber(void);
   double yieldPartDemandStress(void);
   double calcDMGrainSourceSink(void);
   void  calcDemandStress(void);
   void  calcBiomassDemand(void);
   double  calcTempFactor(void);              // high temp stress on grain number

// public Methods -------------------------------------------------------
   public:
    Grain(ScienceAPI2 &, Plant *p);
   ~Grain();

   // plant
   void  readParams (void);
   void  updateVars(void);
   void  process(void);

   // nitrogen
   double calcNDemand(void);
   void  RetranslocateN(double N);

   // biomass
   double partitionDM(double dltDM);
   double grainDMDifferential(void);
   void  dmRetrans(double dltDm){dmRetranslocate = dltDm;}
   void  Harvest(void);

   // nitrogen
   double getNConc(void)const{return nConc;}

   // phosphorus
   double calcPDemand(void);
   double calcPRetransDemand(void);
   double getPConc(void)const{return pConc;}

   // phenology
   void  phenologyEvent(int);

   void  Summary(void);
   };

//---------------------------------------------------------------------------
}
#endif
