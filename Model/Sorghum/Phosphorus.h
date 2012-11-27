//------------------------------------------------------------------------------------------------

#ifndef PhosphorusH
#define PhosphorusH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Sorghum {
//------------------------------------------------------------------------------------------------
class Phosphorus : public PlantProcess
   {
   private:
   // Active - has the SoilP module been included?
   bool active;

   double stage;
   vector<PlantPart *> StressParts;


// Parameters ----------------------------------------------------------

   double phenoSlope;
   double photoSlope;
   double expansionSlope;
   double grainSlope;

   //  Variables  ---------------------------------------------------------
   //double profileDepth; //not used

   double phenoStress;
   double expansionStress;
   double photoStress;
   double grainStress;

   double pBiomass;
   double pStover;
   double pGreenBiomass;
   double pUptakeTotal;
   double pPlant;

   // supply

   vector<double> pGreen;
   vector<double> dltPGreen;
   vector<double> dltPRetrans;
   vector<double> pSenesced;
//   vector<double> pDead;
//   vector<double> dltPDead;
   vector<double> dltPDetached;
//   vector<double> dltPDetachedDead;
   vector<double> pDemand;

   //double plantPDemand;    // plant demand - grain demand

   // demand
   double totalDemand;
//   double supplyDemandRatio;
//   double pSupply;


//   int currentLayer;                   // number of the layer that the roots are in now (starts at 0)
//   double rootDepth;


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);

   void  setOtherVariables (void);

   void  calcStress(void);
   double pStress(void);


   // plant
   void  getOtherVariables (void);
   void  supply(void);
   void  demand(void);
   void  uptake(void);
   void  partition(void);
   void  senescence(void);
   void  detachment(void);
   void  updateP(void);
   void  retranslocate(void);

// public Methods -------------------------------------------------------
   public:
   Phosphorus(ScienceAPI2 &, Plant *p);
   ~Phosphorus();

    // plant
   void  readParams (void);
   void  updateVars(void);


   void  prepare(void);
   void  process(void);
   void  onNewProfile(NewProfileType &p);

   // Leaf
   double getExpansionStress(void){return expansionStress;}
   double getPhotoStress(void){return photoStress;}

   // phenology
   double getPhenoStress(void){return phenoStress;}
   void  detachment(vector<double> senDetachFrac);

   void  getPGreen(float &);
   void  getDltPGreen(vector<float> &);
   void  getDltPRetrans(vector<float> &);
   void  getPSenesced(float &);
   void  getPDead(float &);
   void  getDltPDetached(vector<float> &);
   void  getDltPDead(vector<float> &);
   void  getDltPDeadDetached(vector<float> &);
   void  getPDemand(float &);

   bool  Active(void){return active;}
   void  Summary(void);
   double getPStover(void){return pStover;}

   // phenology
   void  phenologyEvent(int){};
   void Update(void){updateVars();};
   };  // Phosphorus
//------------------------------------------------------------------------------------------------
}
#endif

