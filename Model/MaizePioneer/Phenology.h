//------------------------------------------------------------------------------------------------

#ifndef PhenologyH
#define PhenologyH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Maize {
   typedef enum  {noCrop, sowing, germination, emergence, endJuv, fi, flag, flowering,
      startGrainFill, endGrainFill, maturity, harvest, endCrop} stages;

#define nStages 13

   //------------------------------------------------------------------------------------------------

   class Phenology : public PlantProcess
      {
      protected:

         // Parameters ----------------------------------------------------------

         vector<string> stageNames;
         vector<double> ttTarget;       // phenology targets for each stage

         // first cut at phenology -    sowing is sowing to germination

         double ttEndJuvInit;
         double ttFlowerMaturity;
         double peswGerm;

         TableFn photoParams;
         TableFn ttParams;

         double latitude;
         double twilight;
         double stageCode;
         double photoPeriod;

         //  Variables  ---------------------------------------------------------
         string stageName;

         vector<double> ttTotal;
         vector<double> ttTotalFM;
         vector<double> daysTotal;

         double dltPhase;
         double dltStage;
         double stage;
         double dltTT;
         double ttCurrStage;

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

         double calcStressesTT(void);
         double germinationPhase (void);
         double phaseFraction(double stage, vector<double> ttTotal,double dltTT,vector<double> stageTT);
         double calcDailyTT(Today *today);
         double temp3Hr (double tMax, double tMin, double period);
         //------------------------------------------------------------------------------------------------
         


         // public Methods -------------------------------------------------------
      public:
         // called from plant
         Phenology(ScienceAPI2 &api, Plant *p);
         ~Phenology();
         void  readParams (void);
         void  updateVars(void);
         virtual void   development(void);

         void  setStage(double stageNow);
         double currentStage(void) {return stage;}
         double getLatitude(void)const {return latitude;}

         double sumTTtarget (int from, int to);
         double sumTTtotal  (int from, int to);
         double sumTTtotalFM(int from, int to);
         double sumDaysTotal(int from, int to);

         double getTTtarget(int stage)const{return ttTarget[stage];} 

         double getDltTT(void)const{return dltTT;}
         double getDltStage(void)const{return dltStage;}

         void  getTTTot(vector<float>&);
         void  getPhaseTT(vector<float>&);

         void Summary(void);
         string returnStageName(void)const{ return stageName;}

         // phenology
         void  phenologyEvent(int){};
         double calcStageTarget(int baseStage, double deltaTT);
      };
   }
#endif
