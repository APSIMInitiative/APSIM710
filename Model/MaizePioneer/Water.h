//------------------------------------------------------------------------------------------------

#ifndef WaterH
#define WaterH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Maize {
   //------------------------------------------------------------------------------------------------
   class Water : public PlantProcess
      {
      private:

         // Parameters ----------------------------------------------------------

         TableFn swPhenoTable;
         TableFn swExpansionTable;
         TableFn swSilkExpansionTable;
         TableFn swEarExpansionTable;

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
         double SwAvailRatio;              // FTSW = totalAvail /  totalAvailPot
         double profileSwAvailRatio;
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
         void  calcAvailablePot(void);
         void  calcSupply(double swSupply);
         void  setOtherVariables (void);
         double calcSwDefPheno(void);
         double calcSwDefPhoto(void);
         double calcSwDefExpansion(void);
         double layerProportion(void);

			// process
         void  calcDailySupply(double swSupply);
         void  calcStresses(void);
         void  calcUptake(void);
         void  getOtherVariables (void);
         double earExpansionStress; // silk
         double calcSwDefEarExpansion(void);
         void calcProfileSwAvailRatio(void);                    // total ftsw over profile



         // public Methods -------------------------------------------------------
      public:
         // plant
         Water(ScienceAPI2 &, Plant *p);
         ~Water();

         // plant
         void  onNewProfile(NewProfileType &);
         void  readParams (void);
         void  readLL(void);
         void  updateVars(void);
         void  process(void);
         double calcDemand(double &swSupply); //NOT USED

         // Roots
         double swAvailRatio(int currentLayer);

         // Phenology
         double calcPeswSeed(void);
         double phenologyStress(void){return phenoStress;}
         double photosynthesisStress(void){return photoStress;}
         double getExpansionStress(void){return expansionStress;}
         double getEarExpansionStress(void){return earExpansionStress;} // silk
         double getTotalSupply(void)const{return totalSupply;}
         double getSdRatio(void)const{return sdRatio;}
         double swAvailFracLayer(int layer);
         double swDepLayer(int layer){return swDep[layer];}
         double dltSwDepLayer(int layer){return dltSwDep[layer];}

         double calcSilkSwDefExpansion(void);

         double getSwAvailRatio(void){return SwAvailRatio;}
         double getProfileSwAvailRatio(void){return profileSwAvailRatio;} // total ftsw over profile

         //Registration functions
         void  getEswLayers(vector<float>&);
         void  getSwDefLayers(vector<float>&);
         void  getSwUptakeLayers(vector<float>&);
         void  getEpLayers(vector<float>&);
         void  getllDep(vector<float>&);
         double getESW(void){return sumVector(esw);}
         double getESWAvail(void)const{return totalAvail;}
         double sumPhotoStressTotal(int, int);
         double sumExpanStressTotal(int, int);

         // phenology
         void  phenologyEvent(int){};
         double getDemand(void)const{return swDemand;}
      };
   //------------------------------------------------------------------------------------------------
   }
#endif
