//------------------------------------------------------------------------------------------------

#ifndef NitrogenH
#define NitrogenH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Maize {
//------------------------------------------------------------------------------------------------

class Nitrogen : public PlantProcess
   {
   private:

// Parameters ----------------------------------------------------------

   float diffnConstant;
   int   nLayers;
   vector<float> dLayer;

//  Variables  ---------------------------------------------------------
   float profileDepth;

   // stress
   float phenoStress;
   float expansionStress;
   float photoStress;
   vector<float> photoStressTotal;

   float nBiomass;
   float nStover;
   float nGreenBiomass;
   float nUptakeTotal;
   float nPlant;

   // uptake
   float maxUptakeRate;
   float nUptakeCease;        // tt after flowering when uptake ceases
   float nSupplyFrac;

   // supply
   vector<float> massFlowSupply;
   vector<float> diffusionSupply;
   vector<float> fixationSupply;

   vector<float> nGreen;
   vector<float> dltNGreen;
   vector<float> dltNRetrans;
   vector<float> nSenesced;
   vector<float> dltNDetached;

   float sumDiffSupply;            // debug


   float actualMassFlow;
   float actualDiffusion;
   float actualTotal;
   float plantNDemand;    // plant demand - grain demand

   // demand
   float totalDemand;
   float supplyDemandRatio;
   float nSupply;

   vector<float> no3;
   vector<float> no3Min;
   vector<float> dltNo3;

   int   currentLayer;                   // number of the layer that the roots are in now (starts at 0)
   float rootDepth;


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   void  calcMassFlow(void);
   void  calcDiffusion(void);
   void  calcFixation(void);

   void  setOtherVariables (void);
   float layerProportion(void);

   void  getOtherVariables (void);
   // plant
   void  supply(void);
   void  demand(void);
   void  uptake(void);
   void  partition(void);

// public Methods -------------------------------------------------------
   public:
   Nitrogen(ScienceAPI2 &, Plant *p);
   ~Nitrogen();

   // plant
   void  readParams (void);
   void  updateVars(void);
   void  process(void);

   void  onNewProfile(NewProfileType &);

   // Leaf
   float getExpansionStress(void){return expansionStress;}
   float getPhotoStress(void){return photoStress;}

   float sumPhotoStressTotal(int from, int to);

   // phenology
   float getPhenoStress(void){return phenoStress;}
   void  detachment(vector<float> senDetachFrac);

   void  getNGreen(float &);
   void  getDltNGreen(vector<float> &);
   void  getDltNRetrans(vector<float> &);
   void  getNSenesced(float &);

   float getNStover(void){return nStover;}
   void  Summary(void);

   // phenology
   void  phenologyEvent(int);
   void Update(void){updateVars();};

   };  // Nitrogen

//------------------------------------------------------------------------------------------------
}
#endif
