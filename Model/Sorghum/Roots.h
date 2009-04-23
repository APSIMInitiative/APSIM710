//---------------------------------------------------------------------------

#ifndef RootsH
#define RootsH

#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Roots : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------

   float initialRootDepth;
   float initialDM;

   vector<float> dLayer;
   vector<float> xf;
   float profileDepth;
   int    nLayers;

   TableFn swRoot;
   TableFn rldFn;

   vector<float> rootDepthRate;    // mm/day for each stage
   float specificRootLength;

   // senescence
   float dmRootSenFrac;

   // nitrogen
   float initialNConc;
   float targetNConc;


//  Variables  -----------------------------------------------------
   float rootDepth;
   float rootFront;
   float leftDist;                 // left distance of 2d soil square
   float rightDist;                // right distance of 2d soil square
   float dltRootDepth;             // daily increment
   float dltRootFront;             // daily increment
   int   currentLayer;             // number of the layer that the roots are in now (starts at 0)
   float lastLayerPropn;           // proportion of the currentLayer occupied

   // length
   vector<float> rootLength;
   vector<float> dltRootLength;
   vector<float> dltScenescedRootLength;
   vector<float> rlvFactor;


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  doLayerRegistrations(void);
   void  initialize(void);
   void  calcInitialLength(void);
   float calcDltRootDepth(float stage);
   float calcDltRootFront(float stage);
   float swAvailFactor(int layer);
   float layerProportion(void);
   float getRootArea(float top, float bottom, float rootLength, float dist);

   float totalBiomass(void)const{return dmGreen + dmSenesced;}
   float totalN(void)const{return nGreen + nSenesced;}
   float totalP(void)const{return pGreen + pSenesced;}


// public Methods -------------------------------------------------------
   public:
   // plant
   void   calcSenLength(void);
   void   calcRootDistribution(void);
   Roots(ScienceAPI &, Plant *p);
   ~Roots();

   void  readParams (void);
   void  updateVars(void);
   void  process(void);
   void  onNewProfile(NewProfileType &);

   // biomass
   void  calcSenescence(void);
   void  partitionDM(float dltDM);

   float getRootDepth(void)const{return rootDepth;}
   float getRootFront(void)const{return rootFront;}
   float RootProportionInLayer(int layer);

   float calcNDemand(void);
   float calcPDemand(void);

   void incorporateResidue(void);

   //Registration functions
   void getRP(vector<float>&);
   void getRootLength(vector<float>&);
   void getRLV(vector<float>&);

   // phenology
   void phenologyEvent(int);
   };  // Roots

//------------------------------------------------------------------------------------------------
#endif
