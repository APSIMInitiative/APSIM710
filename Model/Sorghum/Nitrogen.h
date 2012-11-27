//------------------------------------------------------------------------------------------------

#ifndef NitrogenH
#define NitrogenH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Sorghum {
//------------------------------------------------------------------------------------------------
class Nitrogen : public PlantProcess
   {
   private:

// Parameters ----------------------------------------------------------

   double diffnConstant;
   int   nLayers;
   vector<double> dLayer;

	// uptake
   double maxUptakeRate;
   double nUptakeCease;        // tt after flowering when uptake ceases
   double nSupplyFrac;

//  Variables  ---------------------------------------------------------
   double profileDepth;

   // stress
   double phenoStress;
   double expansionStress;
   double photoStress;
   vector<double> photoStressTotal;

   double nBiomass;
   double nStover;
   double nGreenBiomass;
   double nUptakeTotal;
   double nPlant;

   // supply
   vector<double> massFlowSupply;
   vector<double> diffusionSupply;
   vector<double> fixationSupply;

   vector<double> nGreen;
   vector<double> dltNGreen;
   vector<double> dltNRetrans;
   vector<double> nSenesced;
   vector<double> dltNDetached;

   double sumDiffSupply;            // debug


   double actualMassFlow;
   double actualDiffusion;
   double actualTotal;
   double plantNDemand;    // plant demand - grain demand

   // demand
   double totalDemand;
   double supplyDemandRatio;
   double nSupply;

   vector<double> no3;
   vector<double> no3Min;
   vector<double> dltNo3;

   int   currentLayer;                   // number of the layer that the roots are in now (starts at 0)
   double rootDepth;


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   void  calcMassFlow(void);
   void  calcDiffusion(void);
   void  calcFixation(void);

   void  setOtherVariables (void);
   double layerProportion(void);

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
   double getExpansionStress(void){return expansionStress;}
   double getPhotoStress(void){return photoStress;}

   double sumPhotoStressTotal(int from, int to);

   // phenology
   double getPhenoStress(void){return phenoStress;}
   void  detachment(vector<double> senDetachFrac);

   void  getNGreen(float &);
   void  getDltNGreen(vector<float> &);
   void  getDltNRetrans(vector<float> &);
   void  getNSenesced(float &);

   double getNStover(void){return nStover;}
   void  Summary(void);

   // phenology
   void  phenologyEvent(int);
   void Update(void){updateVars();};

   };  // Nitrogen
//------------------------------------------------------------------------------------------------
}
#endif
