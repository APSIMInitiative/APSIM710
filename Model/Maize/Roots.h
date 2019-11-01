//---------------------------------------------------------------------------

#ifndef RootsH
#define RootsH

#include "Utilities.h"
namespace Maize {

   //------------------------------------------------------------------------------------------------

   class Roots : public PlantPart
      {
      private:

         // Parameters ----------------------------------------------------------

         double initialRootDepth;
         double initialDM;

         vector<double> dLayer;
         vector<double> xf;
         double profileDepth;
         int    nLayers;

         TableFn swRoot;
         TableFn rldFn;
		 TableFn swAFPS;

         vector<double> rootDepthRate;    // mm/day for each stage
         double specificRootLength;

         // senescence
         double dmRootSenFrac;

         // nitrogen
         double initialNConc;
         double targetNConc;

		 // Root Angle
		 int useRootAngle;
		 double rootAngle;
		 double rootSpread;

         //  Variables  -----------------------------------------------------
         double rootDepth;
         double rootFront;
         double leftDist;                 // left distance of 2d soil square
         double rightDist;                // right distance of 2d soil square
         double dltRootDepth;             // daily increment
         double dltRootFront;             // daily increment
         int   currentLayer;              // number of the layer that the roots are in now (starts at 0)
         double lastLayerPropn;           // proportion of the currentLayer occupied

         // length
         vector<double> rootLength;
         vector<double> dltRootLength;
         vector<double> dltScenescedRootLength;
         vector<double> rlvFactor;
		 vector<double> rootProportion;

         // Private Methods -------------------------------------------------------
         void  doRegistrations(void);
         void  initialize(void);
         void  calcInitialLength(void);
         double calcDltRootDepth(double stage);
         double calcDltRootFront(double stage);
         double swAvailFactor(int layer);
		 double swAFPSFactor(int layer);
         double layerProportion(void);
         double getRootArea(double top, double bottom, double rootLength, double dist);

         double totalBiomass(void)const{return dmGreen + dmSenesced;}
         double totalN(void)const{return nGreen + nSenesced;}
         double totalP(void)const{return pGreen + pSenesced;}

		 void calcRootProportions(void);

         // public Methods -------------------------------------------------------
      public:
         // plant
         Roots(ScienceAPI2 &, Plant *p);
         ~Roots();

         void  readParams (void);
         void  updateVars(void);
		 void  calcSenLength(void);
		 void  calcRootDistribution(void);
		 void  onNewProfile(NewProfileType &);

         // biomass
         void  calcSenescence(void);
         void  partitionDM(double dltDM);

         double getRootDepth(void)const{return rootDepth;}
         double getRootFront(void)const{return rootFront;}
         double RootProportionInLayer(int layer);

         double calcNDemand(void);
         double calcPDemand(void);

         void incorporateResidue(void);

         //Registration functions
         void getRP(vector<float>&);
         void getRootLength(vector<float>&);
         void getRLV(vector<float>&);

         // phenology
         void phenologyEvent(int);
      };  // Roots
   }
#endif
