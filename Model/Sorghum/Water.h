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

   vector<double> dLayer;
   vector<double> dulDep;
   vector<double> satDep;
   vector<double> bd;
   vector<double> eswCap;           // extractable soil water capacity
   vector<double> ll;
   vector<double> ll15Dep;
   vector<double> kl;
   vector<double> xf;
   vector<double> llDep;

   double profileDepth;
   int    nLayers;

//  Variables  -----------------------------------------------------

   int   currentLayer;             // number of the layer that the roots are in now (starts at 0)
   double lastLayerPropn;           // proportion of the currentLayer occupied
   double totalAvail;
   double totalAvailPot;
   double totalSupply;
   double AccTotalSupply;
   double dltUptake;
   double totalUptake;
   double eswTot;

   vector<double> available;
   vector<double> availablePot;
   vector<double> supply;
   vector<double> swUptake;
   vector<double> swDep;
   vector<double> esw;              // extractable soil water  (swDep[i] - llDep[i])
   vector<double> swDef;
   double ep;
   // uptake
   vector<double> dltSwDep;


   double swDemand;
   double sdRatio;
   // stresses
   double photoStress;
   double phenoStress;
   double expansionStress;

   double rootDepth;

   vector<double> photoStressTotal;
   vector<double> phenoStressTotal;
   vector<double> expanStressTotal;

   
// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   void  calcAvailable(void);
   double layerProportion(void);
   void  calcAvailablePot(void);
   void  calcSupply(void);
   double calcSwDefPheno(void);
   double calcSwDefPhoto(void);
   double calcSwDefExpansion(void);

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
   double calcDemand(void);

   void  onNewProfile(NewProfileType &);

   // Roots
   double swAvailRatio(int currentLayer);

   // Phenology
   double calcPeswSeed(void);
   double phenologyStress(void){return phenoStress;}


   double photosynthesisStress(void){return photoStress;}
   double getExpansionStress(void){return expansionStress;}
   double getTotalSupply(void)const{return totalSupply;}
   double getSdRatio(void)const{return sdRatio;}
   double swAvailFracLayer(int layer);
   double swDepLayer(int layer){return swDep[layer];}
   double dltSwDepLayer(int layer){return dltSwDep[layer];}

   //Registration functions
   void  getEswLayers(vector<float> &);
   void  getSwDefLayers(vector<float> &);
   void  getSwUptakeLayers(vector<float> &);
   void  getEpLayers(vector<float> &);
   void  getllDep(vector<float> &);

   double getESW(void){return sumVector(esw);}
   double getESWAvail(void)const{return totalAvail;}
   double sumPhotoStressTotal(int from, int to);
   double sumExpanStressTotal(int from, int to);

   // phenology
   void  phenologyEvent(int){};
   };

//------------------------------------------------------------------------------------------------
}
#endif
