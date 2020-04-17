//---------------------------------------------------------------------------

#ifndef GrainCMH
#define GrainCMH
#include "PlantComponents.h"
#include "Utilities.h"
#include "Grain.h"

namespace Maize {

   enum CohortStatus { empty, growing, mature, aborted };
   class GrainCohort
      {
      private:
         int   kernelNumber;
         double weight;
         int   position;
         double dltDm;
         double maxKernelWt;
         double cDemand;
         int   age;
         double ageTT;
         double k;
         CohortStatus status;

      public:
         GrainCohort(void)
            {
            age = 0;
            kernelNumber = 0; //kernelsPerRing;
            weight = 0.0;
            status = empty;
            };

         CohortStatus getStatus(void){return status;}
         void  setStatus(CohortStatus _status){status =  _status;}
         void  setKernelNumber(int num){ kernelNumber = num;}
         void  setAge(int myAge){ age = myAge;}
         void  setAgeTT(double myAgeTT){ ageTT = myAgeTT;}
         void  setk(double myk){ k = myk;}
         void  setPosition(int num){ position = num;}

         void  setMaxKernelWt(double potKernelWt,double beta0, double beta1)      // Eqn[15]
            {
            maxKernelWt = Min(potKernelWt, beta0 + beta1 * divide(weight * 1000.0,kernelNumber));
            }
         void  setMaxKernelWt(double potKernelWt){maxKernelWt = potKernelWt;}

         int   getKernelNumber(void){ return kernelNumber;}
         double getKernelWt(void){ return divide(weight,kernelNumber) * 1000.0;}
         int   getAge(void){ return age;}
         double getAgeTT(void){ return ageTT;}
         double getk(void){ return k;}
         double carbonDemand(double tt, double potKGR)
            {
            double demand = tt * potKGR * k * kernelNumber / 1000.0;    // in g/cohort      Eqn[5ii]
            // calculate how much to fill grains
            double required =  kernelNumber * maxKernelWt / 1000.0 - weight; // g/cohort
            cDemand = Min(demand,required);
            return cDemand;
            }

         double getCarbonDemand(void){return cDemand;}

         // add carbon to cohort
         void  init(void){dltDm = 0.0;}
         void  supplyC(double supply)
            {
            dltDm += supply;
            }

         void  update(double minViable, int accDays)
            {
            // if no carbon was added and weight is < minViable, abort
            if (dltDm <= 0.00001 && weight < (minViable * kernelNumber  / 1000.0) && accDays >= age)
               {
               status = aborted;                                               // Eqn[13]
               }
            weight += dltDm;                                                   // Eqn[12]
            if(fabs(weight - (maxKernelWt * kernelNumber / 1000.0)) < 0.001)
               status = mature;
            }


         double getDltDm(void)const{return dltDm;}

      };

   class GrainCM : public Grain
      {
      private:

         //TableFn grainTempTable;
         // Parameters ----------------------------------------------------------
         double dmPerSeed;
         
         // Variables ----------------------------------------------------------

         double totDMGreenFI;          // total plant dm at FI

         double dltDMStressMax;
         double addGrainWeight;        // used in source sink demand

         // Cohort Silking Parameters
         double minPGR;
         double egrSlope;
         double maxSilk;
         double minEarBio;
         double extrRate;

         int applyStress2Silking; // 1 = apply stress procedures to silking dynamics, 0 - no
         double minBioStress;
         double minExtrStress;
         double ttGrnNo;
         double ttEarInit;
         double silkDays;

         int   lagPhase;
         double kernelCobRatio;
         double beta0;
         double beta1;
         double abortKernelFrac;
         double minKGRfrac;
         int   maxCohorts;
         int   kernelsPerRing;
         double signalStrength;


         // Cohort variables
         double silkNumber;
         double grFract;
         double gfTemp;
         int   nDays;      // number of days from -ttGrnNo (270 oC.d in default) - commented by Zhanshan, Jan 7, 2009
         double ttFromEmerg;
         double accSwStress;// accumulated water stress - commented by Zhanshan, Jan 8, 2009
         int   silkDay;
         int   daysFromEmerg;
         double silkStage;
         double silkPlus3;
         int   silkPlus3Day;
         double accEarBio;
         int   nCohorts;
         double potKernelWtFrac;
         double potKGR;

         double KGR;
         double KGRfrac;    // applied to lagPhase to calculate when KGR = potKGR
         double relMaxKGR;  // applied to the expolinear equation. it is rm in the reference paper. added by Zhanshan.

         double grainDemand;
         double potCDemand;
         bool  updated;
         double grainWeight;
         double cobWeight;
         double grain_Weight;
         double cob_Weight;
         double leftoverDM; // leftover DM if kernel can not use all

         double PGR;

         // Private Methods -------------------------------------------------------

         void  initialize(void);
         double calcGrainNumber(void);
         double yieldPartDemandStress(void);
         double calcDMGrainSourceSink(void);

         // Pioneer Cohort
         void  initializeCohortModel();
         //void  doCohortRegistrations(void);
         //void  readCohortParameters(string);
         vector<GrainCohort> cohorts;
         void  initCohorts(double silkNumber, double earBio);
         void  partition2Cohorts(double dm);
         double calcSilkGrainNumber(double averageStress);
         void updateCohortVars(void);

         void UpdatePotKernelWt(void);
         double CalcAvStress(void);
         double calcSilkNumber(double averageStress);
         void calcSilkingDay(void);
		 virtual double calcCarbonDemand();

         // public Methods -------------------------------------------------------
      public:
         GrainCM(ScienceAPI2 &api, Plant *p);
         ~GrainCM();
         void  readParams (void);
         virtual void  updateVars(void);
         virtual void  process(void);
         virtual void  Harvest(void);

         // biomass
         void  calcDemandStress(void);
         virtual void  calcBiomassDemand(void);
         virtual double  dmRetrans(double dltDm);
         void  doRegistrations(void);

         // phenology
         virtual void  phenologyEvent(int);
         virtual double partitionDM(double dltDM);


         // nitrogen
         double getNConc(void)const{return nConc;}
         
         // phosphorus
         double calcPRetransDemand(void);
         double getPConc(void)const{return pConc;}

         int   getNCohorts(void)const{return nCohorts;}
		 virtual double calcDltCDemand(double dailyBiom);
         
         double getFinalGrainNo(void)const{return finalGrainNo;}
         virtual double getGRFract(void){return grFract;}
         double calcEGR(double PGR);                           // from biomass
         double getLeftoverDM(void) { return leftoverDM;} // return to labile pool

         double getSilkTT(void)const{return silkStage;}
         double calcGRFract(void); // for whole ear
         double calcCohortGRFract(double age); // for individual cohort

         void  getKernels(vector<float> &result);
         void  getKernelWt(vector<float> &result);
         void  getStatus(vector<float> &result);
      };
   }
//---------------------------------------------------------------------------
#endif
