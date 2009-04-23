//------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------

#ifndef LeafH
#define LeafH

#include <vector>
#include <string>
#include "Utilities.h"
using namespace std;

//------------------------------------------------------------------------------------------------

class Leaf : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------

   float noSeed;
   float noEmergence;
   float initRate;
   float initialTPLA;
   float tplaInflectionRatio,tplaProductionCoef;
   // leaf appearance
   float appearanceRate1;
   float appearanceRate2;
   float noRateChange;
   float minLeafNo,maxLeafNo;

   // leaf dry mass
   float initialDM;
   float translocFrac;
   float leafPartitionRate;
   float dmTotal;

   // leaf area
   float tillerCoef;
   float mainStemCoef;

   float slaMin;
   float slaMax;

   // sencescence
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
   float dilnNSlope;
   float dilnNInt;

   // plant
   float density;

//  Variables  -----------------------------------------------------
   // Leaf Number
   vector<float> leafNo;              // leaf number per stage
   vector<float> nodeNo;              // node number per stage

   float finalLeafNo;
   float dltLeafNo;
   float nLeaves;

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
   float gpla;

   // senescence
   float sLai;
   float dltSlai;
   float maxLaiPossible;
   float waterStressLaiLoss;
   float dltSlaiLight;
   float dltSlaiWater;
   float dltSlaiFrost;
//   float dltDmSenesced;
   vector<float> laiEquilibLight;
   vector<float> laiEquilibWater;
   vector<float> avSD;
   float dltSlaiN;

   // SLN
   float SLN;
   float SLN0;


// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);
   void  initLeafNo(void);
   void  calcFinalLeafNo(void);
   void  calcLeafAppearance(void);
   void  calcTplaMax(void);
   float calcDltPotentialTPLA(void);
   float calcStressedLeafArea(void);
   float calcMaxLaiPossible(void);
   float calcLaiSenescenceLight(void);
   float calcLaiSenescenceWater(void);
   float calcLaiSenescenceFrost(void);
   float calcSLN(void);
   float calcLAI(void);
   void  calcCover(void);


// public Methods -------------------------------------------------------
   public:
   Leaf(ScienceAPI &, Plant *p);
   ~Leaf();

   // plant
   float dltNSenesced;
   void  areaActual(void);
   void  senesceArea(void);

   void  readParams (void);
   void  updateVars(void);

   void  process(void);
   void  calcLeafNo(void);
   void  calcPotentialArea(void);

    // phenology
   float calcEmergFlagTT(void);

   // nitrogen
   float calcNDemand(void);
   float calcNewLeafNDemand(void);
   float provideN(float requiredN);
   float getSLN(void)const{return SLN;}
   float laiToday(void)const;

   float getDltLAI(void)const{return dltLAI;}
   float getLAI(void)const{return lai;}
   float getDltSlai(void)const{return dltSlai;}
   void  addDltSlai(float add);


   float getCoverGreen(void)const{return coverGreen;}
   float getCoverTot(void)const{return coverTot;}

   // phosphorus
   float calcPDemand(void);

   // biomass
   void  calcSenescence(void);
   float partitionDM(float dltDM);
   float dmRetransAvailable(void);
   void  dmRetrans(float dltDm){dmRetranslocate = dltDm;}
   float getLeafNo(void){return nLeaves;}
   void  laiDetachment(vector<float> senDetachFrac);


   void  Summary(void);

   // phenology
   void  phenologyEvent(int);

   };  // Leaf

//------------------------------------------------------------------------------------------------
#endif
