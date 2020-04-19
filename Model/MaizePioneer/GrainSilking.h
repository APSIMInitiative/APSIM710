//---------------------------------------------------------------------------

#ifndef GrainSilkingH
#define GrainSilkingH
#include "PlantComponents.h"
#include "Utilities.h"
#include "Grain.h"

namespace Maize 
   {
   // Enumeration for Grain Status
   enum GrainStatus { Proto, Initiated, Emerged, Pollinated, Growing, Aborted, Mature, Dead };

   // Represents a Kernel in a Cohort
   class Kernel
      {
      public:
         Kernel()
            {
            status = Proto;
            receptivity = 1.0;
            k = 0.0;
            age = 0.0;
            pKGR = 0.0;
            GWRatio = 0.0;
            potentialWeight =0.0;
            biomass =0.0;
            };
         GrainStatus status;
         double receptivity;
         double k;
         double age;
         double pKGR;
         double GWRatio;     //Ratio of actual kernel weight to potential at TTB
         double potentialWeight;
         double biomass;
         double CalcK(double , double , double , double, double, int, double);
      };

   // Represents a Cohort
   class Cohort
      {
      public:
         //variables
         int nGrains;
         GrainStatus status;
         double silkLength;  // length of silk in mm
         vector<Kernel> kernels;
         double age;
         double receptivity;
         double grFract;
         double k;
         double biomass;
         int    strikes;


         //functions
         Cohort(int _nGrains);
         void calcReceptivity(double todayTT);
         double calcGRFract();
         void setStatus(GrainStatus _status);
      };

   // Represents Grain
   class GrainSilking : public Grain
      {
      protected:

         double kernelGR;
         int asi;

      public:
         //------------------------------------------
         //-Parameters-------------------------------
         // Cohort Params
         int      kernelsPerCohort;
         int      potKernels;
         int      nCohorts;

         int useGWRatio;

         // Silking Initiation Window
         double   ttSilkingBeforeShedding;
         double   ttSilkingDuration;

         // Silk Parameters
         double   emergeSilkLength;
         double   beta;  // in sim file: tempDependSilkElongRate
         double   alpha; // in sim file: silkWaterStressFactor

         // Pollen Parameters
         double   sheddingPollenCount;

         double abortFrac;
         //Cohort Parameters
         //------------------------------------------
         double   signalStrength;

         //------------------------------------------
         //-Local Variables--------------------------
         // cohorts
         vector<Cohort> cohorts;
         vector<double> cohortGrainSize;
         vector<int>    cohortGrains;
         vector<int>    cohortNumber;
         GrainStatus status;


         double earWt;
         double cobWt;
         double grainWt;
         double abortedGrainWt;
         double earWtPlant;
         double cobWtPlant;
         double grainWtPlant;
         double abortedGrainWtPlant;
         // reporting and overhead
         double   carryOverDd, sheddingDd, startSilkingDd, endSilkingDd;   // dd after sowing that shedding will occur;
         int      sheddingDas, maturityDas, silkingDas;
         double startSilkWindow, endSilkWindow; // window to look at pgr for actual silk number calculations

         // daily updated values
         int      das;
         double   dltTT , yesterdayAccumTT, todayAccumTT, waterSD, maxT;

         // pollen viability
         double   PV;               // pollen viability factor f(maxT)
         double   pollenStressStartTemp;// curve of temp factor
         double   pollenStressHighTemp; // curve of temp factor
         double   pollenFactor;
         double   minDailyPollen;

         double daysMaxPollen;          
         double pollenDistribution;     
         double ttPollenViabilityWindow;

         int maxNumStrikes;
         double ringSize;
         TableFn actSilksTable;
         vector<double> silkNoWindow;    // window to measure pgr for actual silking
         double silkNoPGR;
         vector<double> sigmaDltTT; 
         vector<double> sigmaDltdm; 

         int cohortsEmerged;


         // pollenation
         int      pollenToday;
         int      kernelNumber;     // kernels pollinated today
         int      deadKernels;      // kernels dead today
         int      pollinated;       // total kernels pollinated

         // test for pollination window
         int pWCount;    // to test pollen wondow
         int  pWDay1;int pWDay2;


         // SER = α(T-Tb) * (1-β(1-SD))		Silk elongation  changes daily f(tt,sd)
         double   silkGrowth;       // silk growth today (mm)

         //------------------------------------------
         //-Methods----------------------------------
         GrainSilking(ScienceAPI2 &api, Plant *p);
         ~GrainSilking();
         // kernel growth
         double   grFract;
         double   egr;              // todays ear growth rate
         double   earBio;           // total ear biomass to now
         bool     prePollination;
         vector<double> kS;
         double   KGRfrac;
         double   lagPhase;
         double   relMaxKGR;
         double   potKGR;
         double   cobK;
         double   deltaCobWeight;
         double   cobDemand;
         double   cobWeight;
         double   deltaRetrans;
         double   deltaSupply;
         double   deltaDemand;
         double   kernelBio;
         double   kernelCobRatio;
         int      grainsPerPlant;
         double   cobGRFract;

         // overrides
         virtual void readParams ();
         virtual void initialize();
         virtual void doRegistrations();
         //virtual void   updateVars();
         virtual void process();
         //------------------------------------------
         virtual void updateVars();

         // silking model specific
         void     setSilkEnlongation();
         void     calcNeverToBePollinated();
         void     updateKernels();
         void     calcInitiated();
         void     calcEmerged();
         void     calcPollenFactors();
         void     calcPollination();
         void     calcGRFract();
         void     calcEarGrowth();
         void     calcCobWeight();
         bool     allPollinated();
         bool     allDead();
         void     allocateToCohorts();
         double   kernelGrowthRate();
         int      calcGrainNo();

         void calcActualSilks();

         virtual double getGRFract(void){return grFract;}
         virtual double calcDltCDemand(double dailyBiom);
         virtual double dmRetrans(double dltDM);
         virtual double partitionDM(double dltDM);//{return Max(dltDmGreen,0.0);};
         virtual double getDmGreen(void){return dmGreen + cobWeight; };
         // accesors
         double   silkLength(int cohortNo);
         //------------------------------------------
      };
   }
//---------------------------------------------------------------------------
#endif