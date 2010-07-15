//------------------------------------------------------------------------------------------------

#ifndef PhosphorusH
#define PhosphorusH

#include "PlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Phosphorus : public PlantProcess
   {
   private:
   // Active - has the SoilP module been included?
   bool active;

   float stage;
   vector<PlantPart *> StressParts;


// Parameters ----------------------------------------------------------

//   float diffnConstant;
   int nLayers;
   vector<float> dLayer;

   float phenoSlope;
   float photoSlope;
   float expansionSlope;
   float grainSlope;

   //  Variables  ---------------------------------------------------------
   float profileDepth;

   float phenoStress;
   float expansionStress;
   float photoStress;
   float grainStress;

   float pBiomass;
   float pStover;
   float pGreenBiomass;
   float pUptakeTotal;
   float pPlant;

   // supply

   vector<float> pGreen;
   vector<float> dltPGreen;
   vector<float> dltPRetrans;
   vector<float> pSenesced;
   vector<float> pDead;
   vector<float> dltPDead;
   vector<float> dltPDetached;
   vector<float> dltPDetachedDead;
   vector<float> pDemand;

   float plantPDemand;    // plant demand - grain demand

   // demand
   float totalDemand;
   float supplyDemandRatio;
   float pSupply;


   int currentLayer;                   // number of the layer that the roots are in now (starts at 0)
   float rootDepth;


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);

   void  setOtherVariables (void);
   float layerProportion(void);

   void  calcStress(void);
   float pStress(void);


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
   float getExpansionStress(void){return expansionStress;}
   float getPhotoStress(void){return photoStress;}

   // phenology
   float getPhenoStress(void){return phenoStress;}
   void  detachment(vector<float> senDetachFrac);

   void  getPGreen(float &);
   void  getDltPGreen(vector<float> &);
   void  getDltPRetrans(vector<float> &);
   void  getPSenesced(float &);
   void  getPDead(float &);
   void  getDltPDetached(vector<float> &);
   void  getDltPDead(vector<float> &);
   void  getDltPDeadDetached(vector<float> &);
   void  getPDemand(vector<float> &);

   bool  Active(void){return active;}
   void  Summary(void);
   float getPStover(void){return pStover;}

   // phenology
   void  phenologyEvent(int){};
   void Update(void){updateVars();};
   };  // Phosphorus

//------------------------------------------------------------------------------------------------
#endif

