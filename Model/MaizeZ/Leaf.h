//------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------

#ifndef LeafH
#define LeafH

#include <vector>
#include <string>
#include "Utilities.h"
//#include "CanopyPhotosynthesis.h"
using namespace std;
namespace Maize {

class CanopyPhotosynthesis;

//------------------------------------------------------------------------------------------------
class Leaf : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------
   string crop;

   float noSeed;
   float noEmergence;
   float initRate;
   float initialTPLA;
   float tplaInflectionRatio,tplaProductionCoef;
   // leaf appearance
   float appearanceRate1;
   float appearanceRate2;
   float appearanceRate3;
   float noRateChange1;
   float noRateChange2;
   float minLeafNo,maxLeafNo;

   // leaf dry mass
   float initialDM;
   float translocFrac;
   float leafPartitionRate;
   float dmTotal;

   // leaf area   TPLA
   float tillerCoef;
   float mainStemCoef;

   // leaf area individual leaf
   // these variables are opted out, 2007
   /*
   float x0Const;                //largest leaf no intercept
   float x0Slope;                //largest leaf no slope
   float y0Const;                //largest leaf area intercept
   float y0Slope;                //largest leaf area slope
   float aConst;                 //leaf area breadth intercept
   float aSlope1;                //leaf area breadth slope1
   float aSlope2;                //leaf area breadth slope2
   float bConst;                 //leaf area skewness intercept
   float bSlope1;                //leaf area skewness slope1
   float bSlope2;                //leaf area skewness slope2
   */

   float leafNoCorrection;       //corrects for other growing leaves
   //Birch, Hammer bell shaped curve parameters
   double aX0;                    // Eqn 14 calc x0 - position of largest leaf
   double aMaxA;                  // Eqn 13 calc aMax - area of largest leaf
   // sla
   TableFn slaMin;
   TableFn slaMax;

   // sencescence
   float splaIntercept;
   float splaSlope;
   float splaProdCoef;
   float senRadnCrit;        // radiation level for onset of light senescence
   float senLightTimeConst;  // delay factor for light senescence
   float senWaterTimeConst;  // delay factor for water senescence
   float senThreshold;       // supply:demand ratio for onset of water senescence
   float frostKill;          // temperature threshold for leaf death

   // nitrogen
   float initialSLN;
   float targetSLN;
   float newLeafSLN;
   float senescedLeafSLN;
   float nTotal;
   float dilnNSlope;
   float dilnNInt;

   // plant
   float density;

   // leaf angle model
   int useLeafAngles;

//  Variables  -----------------------------------------------------
   // Leaf Number
   vector<float> leafNo;              // leaf number per stage
   vector<float> nodeNo;              // node number per stage
   vector<float> leafSize;            // size of individual laeves

   float finalLeafNo;
   float dltLeafNo;
   float nLeaves;
   float nDeadLeaves;
   float dltDeadLeaves;

   // LAI
   float lai;
   float dltPotentialLAI;
   float dltStressedLAI;
   float dltLAI;
   float maxLai;

   float extinctionCoef;
   float coverGreen;
   float coverSen;
   float coverTot;
   // TPLA
   float tplaMax;                     // maximum possible total plant leaf area
   float tplaPot;
   float tpla;
   float spla;

   // senescence
   float sLai;
   float dltSlai;
   float maxLaiPossible;
   float waterStressLaiLoss;
   float dltSlaiAge;
   float dltSlaiLight;
   float dltSlaiWater;
   float dltSlaiFrost;
//   float dltDmSenesced;
   vector<float> laiEquilibLight;
   vector<float> laiEquilibWater;
   vector<float> avSD;
   float dltSlaiN;

   // dead
   float deadLai;
   // sLai
   float deadLeafConst;
   float deadLeafSlope;
   

   // SLN
   float SLN;
   float SLN0;

   // Private Methods -------------------------------------------------------
   void   doRegistrations(void);
   void   initialize(void);
   void   initLeafNo(void);
   void   calcFinalLeafNo(void);
   void   calcLeafAppearance(void);
   void   calcTplaMax(void);
   float calcDltPotentialTPLA(void);
   float calcStressedLeafArea(void);
   float calcMaxLaiPossible(void);
   float calcLaiSenescenceAge(void);
   float calcDltDeadLeaves(void);

   float calcLaiSenescenceLight(void);
   float calcLaiSenescenceWater(void);
   float calcLaiSenescenceFrost(void);
   float calcSLN(void);
   float calcLAI(void);
   void  calcCover(void);

   // leaf area individual leaf
   void leafAreaPotBellShapeCurve(void);
   void calcLeafSize(void);
   double calcIndividualLeafSize(double leafNo);


   //Leaf Angle
   CanopyPhotosynthesis *canPhoto;

// public Methods -------------------------------------------------------
   public:
   Leaf(ScienceAPI2 &, Plant *p);
   ~Leaf();

   ///////////////////////////
   float dltNSenesced;
   void areaActual(void);           // plant
   void senesceArea(void);          // plant
   //////////////////////////
   void readParams (void);
   void updateVars(void);

   void process(void);              // plant
   void calcLeafNo(void);           // plant
   void calcPotentialArea(void);    // plant

   float calcEmergFlagTT(void);                           // phenology

   // nitrogen
   float calcNDemand(void);                         // nitrogen
   float calcNewLeafNDemand(void);                  // nitrogen
   float provideN(float requiredN);
   float getSLN(void)const{return SLN;}             // nitrogen
   float laiToday(void)const;                       // nitrogen

   float getDltLAI(void)const{return dltLAI;}       // nitrogen
   float getLAI(void)const{return lai;}             // nitrogen
   float getDltSlai(void)const{return dltSlai;}     // nitrogen
   void  addDltSlai(float add);                    // nitrogen


   float getCoverGreen(void)const{return coverGreen;}
   float getCoverTot(void)const{return coverTot;}

   // phosphorus
   float calcPDemand(void);

//   float getDmGreen(void)const{return dmGreen;}            // biomass
//   float getDmSenesced(void)const{return dmSenesced;}      // biomass
   void  calcSenescence(void);                             // biomass
   float partitionDM(float dltDM);                        // biomass
   float dmRetransAvailable(void);                         // biomass
   void  dmRetrans(float dltDm){dmRetranslocate = dltDm;} // biomass
   float getLeafNo(void){return nLeaves;}
   void  laiDetachment(vector<float> senDetachFrac);

   void Summary(void);

   // phenology
   void phenologyEvent(int);

   //Leaf Angles
   void calcLayerVars(float*, float*);

   // reporting
   //void getLeafArea(protocol::Component *system, protocol::QueryValueData &qd);


   };  // Leaf

//------------------------------------------------------------------------------------------------
}
#endif
