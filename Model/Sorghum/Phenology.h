//------------------------------------------------------------------------------------------------

#ifndef PhenologyH
#define PhenologyH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Sorghum {
typedef enum  {noCrop, sowing, germination, emergence, endJuv, fi, flag, flowering,
                  startGrainFill, endGrainFill, maturity, harvest, endCrop} stages;

#define nStages 13

//------------------------------------------------------------------------------------------------

class Phenology : public PlantProcess
   {
   protected:

// Parameters ----------------------------------------------------------

   vector<string> stageNames;
   vector<float> ttTarget;       // phenology targets for each stage

   // first cut at phenology -    sowing is sowing to germination

   float ttEndJuvInit;
   float ttFlowerMaturity;
   float peswGerm;

   TableFn photoParams;
   TableFn ttParams;
   TableFn ttFmParams;

   float latitude;
   float twilight;
   float stageCode;

//  Variables  ---------------------------------------------------------
   string stageName;

   vector<float> ttTotal;
   vector<float> ttTotalFM;
   vector<float> daysTotal;

   float dltPhase;
   float dltStage;
   float stage;
   float dltTT;
   float dltTTFM;
   float ttCurrStage;

   int   floweringDAS;
   int   floweringDOY;
   int   maturityDAS;
   int   maturityDOY;

// Private Methods -------------------------------------------------------
   void  doRegistrations(void);
   void  initialize(void);

   void  checkTargets(void);
   void  calcThermalTimes(Today *today);
   void  calcPhaseDevelopment(void);
   void  calcDevelopment(void);

   float calcStressesTT(void);
   float germinationPhase (void);
   float phaseFraction(float stage, vector<float> ttTotal,float dltTT,vector<float> stageTT);
   float calcDailyTT(Today *today);
   float calcDailyTTFM(Today *today);
   float temp3Hr (float tMax, float tMin, float period);

// public Methods -------------------------------------------------------
   public:
      // called from plant
   Phenology(ScienceAPI2 &api, Plant *p);
   ~Phenology();
   void  readParams (void);
   void  updateVars(void);
   virtual void   development(void);

   void  setStage(float stageNow);
   float currentStage(void) {return stage;}

   float sumTTtarget (int from, int to);
   float sumTTtotal  (int from, int to);
   float sumTTtotalFM(int from, int to);
   float sumDaysTotal(int from, int to);

   float getDltTTFM(void)const{return dltTTFM;}
   float getDltTT(void)const{return dltTT;}
   float getDltStage(void)const{return dltStage;}

   void  getTTTot(vector<float>&);
   void  getPhaseTT(vector<float>&);

   void Summary(void);
   string returnStageName(void)const{ return stageName;}

   // phenology
   void  phenologyEvent(int){};
   };
}
#endif
