//------------------------------------------------------------------------------------------------

#ifndef WaterH
#define WaterH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Sorghum {
//------------------------------------------------------------------------------------------------
class Water : public PlantProcess
   {
   private:

// Parameters ----------------------------------------------------------

   TableFn swPhenoTable;
   TableFn swExpansionTable;

   vector<float> dLayer;
   vector<float> dulDep;
   vector<float> satDep;
   vector<float> bd;
   vector<float> eswCap;           // extractable soil water capacity
   vector<float> ll;
   vector<float> ll15Dep;
   vector<float> kl;
   vector<float> xf;
   vector<float> llDep;

   float profileDepth;
   int    nLayers;

//  Variables  -----------------------------------------------------

   int   currentLayer;             // number of the layer that the roots are in now (starts at 0)
   float lastLayerPropn;           // proportion of the currentLayer occupied
   float totalAvail;
   float totalAvailPot;
   float totalSupply;
   float AccTotalSupply;
   float dltUptake;
   float totalUptake;
   float eswTot;

   vector<float> available;
   vector<float> availablePot;
   vector<float> supply;
   vector<float> swUptake;
   vector<float> swDep;
   vector<float> esw;              // extractable soil water  (swDep[i] - llDep[i])
   vector<float> swDef;
   float ep;
   // uptake
   vector<float> dltSwDep;


   float swDemand;
   float sdRatio;
   // stresses
   float photoStress;
   float phenoStress;
   float expansionStress;

   float rootDepth;

   vector<float> photoStressTotal;
   vector<float> phenoStressTotal;
   vector<float> expanStressTotal;

   
// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   void  calcAvailable(void);
   float layerProportion(void);
   void  calcAvailablePot(void);
   void  calcSupply(void);
   float calcSwDefPheno(void);
   float calcSwDefPhoto(void);
   float calcSwDefExpansion(void);

   void   setOtherVariables (void);

// public Methods -------------------------------------------------------
   public:
   // plant
   Water(ScienceAPI2 &, Plant *p);
   ~Water();

   // plant
   void  calcDailySupply(void);
   void  calcStresses(void);
   void  calcUptake(void);
   void  getOtherVariables (void);
   void  readParams (void);
   void  readLL(void);
   void  updateVars(void);
   void  process(void);
   float calcDemand(void);

   void  onNewProfile(NewProfileType &);

   // Roots
   float swAvailRatio(int currentLayer);

   // Phenology
   float calcPeswSeed(void);
   float phenologyStress(void){return phenoStress;}


   float photosynthesisStress(void){return photoStress;}
   float getExpansionStress(void){return expansionStress;}
   float getTotalSupply(void)const{return totalSupply;}
   float getSdRatio(void)const{return sdRatio;}
   float swAvailFracLayer(int layer);
   float swDepLayer(int layer){return swDep[layer];}
   float dltSwDepLayer(int layer){return dltSwDep[layer];}

   //Registration functions
   void  getEswLayers(vector<float> &);
   void  getSwDefLayers(vector<float> &);
   void  getSwUptakeLayers(vector<float> &);
   void  getEpLayers(vector<float> &);
   void  getllDep(vector<float> &);

   float getESW(void){return sumVector(esw);}
   float getESWAvail(void)const{return totalAvail;}
   float sumPhotoStressTotal(int from, int to);
   float sumExpanStressTotal(int from, int to);

   // phenology
   void  phenologyEvent(int){};
   };

//------------------------------------------------------------------------------------------------
}
#endif
