//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include <numeric>

#include "GrainSilking.h"
#include "Plant.h"

using namespace Maize;

//---------------------------------------------------------------------------
//------ Cohort Constructor
//------------------------------------------------------------------------------------------------
Cohort::Cohort(int _nGrains)
   {
   age = 0.0;
   receptivity = 1.0;
   grFract = 0.0;
   nGrains = _nGrains;
   status = Proto;
   silkLength = 0.0;
   biomass = 0.0;
   k = 0.0;
   strikes = 0;
   kernels.clear();
   for (int i = 0; i < nGrains; i++)
      {
      Kernel kernel = Kernel();
      kernels.push_back(kernel);
      }
   }
//---------------------------------------------------------------------------
//------ Grain Constructor
//------------------------------------------------------------------------------------------------
GrainSilking::GrainSilking(ScienceAPI2 &api, Plant *p) : Grain(api, p)
   {
   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Grain Destructor
//------------------------------------------------------------------------------------------------
GrainSilking::~GrainSilking()
   {
   }
//--------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void GrainSilking::readParams (void)
   {
   Grain::readParams();

   // Cohort Params
   scienceAPI.read("kernelsPerCohort"			   , "", 0, kernelsPerCohort);				//Documents (pg.3.2)

   // Silking Initiation Window
   scienceAPI.read("ttSilkingBeforeShedding"	   , "", 0, ttSilkingBeforeShedding);		//Figure 2 (pg.2) Documents (pg.3.2)
   scienceAPI.read("ttSilkingDuration"			   , "", 0, ttSilkingDuration);			   //Figure 2 (pg.2) Documents (pg.3.2)

   // Silk Parameters
   scienceAPI.read("emergeSilkLength"			   , "", 0, emergeSilkLength);				//Figure 2 in the Document and Notes
   scienceAPI.read("tempDependSilkElongRate"    , "", 0, alpha);	                     //Alpha in Eq. 1 
   scienceAPI.read("silkWaterStressFactor"	   , "", 0, beta);			               //Beta in Eq. 2

   // Pollen Parameters
   scienceAPI.read("sheddingPollenCount"		   , "", 0, sheddingPollenCount);		   //Figure 2 (pg.2) Documents (pg.3.1)
   scienceAPI.read("daysMaxPollen"	            , "", 0, daysMaxPollen);               //The days afters shedding that the maximum number of pollen is available
   scienceAPI.read("pollenDistribution"         , "", 0, pollenDistribution);          //The shape of the normal distribution curve.
   scienceAPI.read("ttPollenViabilityWindow"    , "", 0, ttPollenViabilityWindow);     //The thermal time window before shedding that we calculate the viablitiy of the pollen
   scienceAPI.read("pollenStressStartTemp"      , "", 0, pollenStressStartTemp);       //The Lowestest Temperature where pollen begin losing viability (also called t0)
   scienceAPI.read("pollenStressHighTemp"       , "", 0, pollenStressHighTemp);        //The Temperature after which NO pollens are viable (also called t1)
   scienceAPI.read("minDailyPollen"             , "", 0, minDailyPollen);              //If pollen is less than this number, calculate pollen factor (eq. 3)

   // Grain Filling and Partitioning
   scienceAPI.read("KGRfrac"                    , "", 0, KGRfrac);
   scienceAPI.read("lagPhase"                   , "", 0, lagPhase);
   scienceAPI.read("relMaxKGR"                  , "", 0, relMaxKGR);
   scienceAPI.read("cobK"                       , "", 0, cobK);    //cobK = 0.027;
   scienceAPI.read("potKernelWt"                , "", 0, potKernelWt);
   scienceAPI.read("kernelCobRatio"             , "", 0, kernelCobRatio);
   scienceAPI.read("signalStrength"             , "", 0, signalStrength);
   scienceAPI.read("maxNumStrikes"             , "", 0, maxNumStrikes);
   scienceAPI.read("ringSize"             , "", 0, ringSize);
   actSilksTable.read(scienceAPI, "pltGrowthRate","ActSilks");
   scienceAPI.read("silkNoWindow",      "", 0, silkNoWindow);

   scienceAPI.read("abortFrac"             , "", 0, abortFrac);

   useGWRatio = 0;
   scienceAPI.read("useGWRatio"             , "", 1, useGWRatio);
   
   scienceAPI.read("pWCount"             , "", 1, pWCount);
   scienceAPI.read("pWDay1"             , "", 1, pWDay1);
   scienceAPI.read("pWDay2"             , "", 1, pWDay2);

   //Need Phenology Params just for calculating potkgr
   double ttFlowerMaturity = 0.0;
   scienceAPI.read("tt_flower_to_maturity", "", false, ttFlowerMaturity);

   //Calculated Parameters
   nCohorts = (int)ceil(GNmaxCoef / (potKernelWt*kernelsPerCohort) * 1000);   
   potKGR = potKernelWt / (ttFlowerMaturity - lagPhase);

   cohorts.clear();
   cohortGrainSize.clear();
   cohortNumber.clear();

   cohortGrainSize.assign(nCohorts,0.0);
   cohortGrains.assign(nCohorts,0);

   for(int i =0; i < nCohorts; i++)
      {
      cohorts.push_back(Cohort(kernelsPerCohort));
      cohortNumber.push_back(1);
      }

   cohortGrainSize.assign(nCohorts,0.0);
   potKernels  = nCohorts * kernelsPerCohort;

   }
//------------------------------------------------------------------------------------------------
void GrainSilking::initialize()
   {
   Grain::initialize();

   // reporting and overhead
   carryOverDd = 0.0; sheddingDd = 0.0; startSilkingDd = 0.0; endSilkingDd = 0.0;
   sheddingDas = 0; maturityDas = 0; silkingDas = 0;

   // pollen viability
   PV = 1.0; 
   pollenFactor = 1.0;

   // pollenation
   pollenToday = 0; kernelNumber = 0; deadKernels = 0; pollinated = 0;

   // Silk elongation  changes daily f(tt,sd)
   silkGrowth = 0.0;
   startSilkWindow = 0.0; endSilkWindow = 0.0; 
   silkNoPGR = 0.0;

   // kernel growth
   grFract = 0.0; egr = 0.0; earBio = 0.0;
   prePollination = true;

   status = Proto;


   deltaCobWeight = 0.0;
   cobWeight = 0.0;
   cobDemand = 0.0;
   kernelBio = 0;
   kernelNumber = 0;
   kernelNumber=0;

   deltaSupply = 0;
   deltaDemand = 0;
   kernelGR    = 0;
   asi         = 0;

   yesterdayAccumTT = 0.0;
   todayAccumTT = 0.0;
   dltTT = 0.0;

   ringSize = 0.0;

   earWt = 0.0;
   cobWt = 0.0;
   grainWt = 0.0;
   earWtPlant = 0.0;
   cobWtPlant = 0.0;
   grainWtPlant = 0.0;
   abortedGrainWt = 0.0;
   abortedGrainWtPlant = 0.0;

   pWCount = 0;    // to test pollen wondow
   pWDay1 = 0;
   pWDay2 = 0;

   cohortsEmerged = 0;
      cobGRFract = 0.0;


   }
//------------------------------------------------------------------------------------------------
void GrainSilking::doRegistrations()
   {
   Grain::doRegistrations();
   /*scienceAPI.expose("silksInitiated",""         ,       "Total silks initiated"              ,    false, dmGreen);*/
   scienceAPI.expose("nCohorts",""                 ,       "nCohorts"                           ,    false, nCohorts);
   scienceAPI.expose("emergeSilkLength",""         ,       "emergeSilkLength"                   ,    false, emergeSilkLength); 
   scienceAPI.expose("silksPollinated","()"        ,       "Total silks pollenated"             ,    false, pollinated);
   scienceAPI.expose("pollenNumber","()"           ,       "Todays effectpollen count"          ,    false, pollenToday);
   scienceAPI.expose("pollenViability","()"        ,       "Pollen Viability"                   ,    false, PV);
   scienceAPI.expose("pollenFactor","()"           ,       "Pollen Factor"                      ,    false, pollenFactor);
   scienceAPI.expose("ASI","()"                    ,       "ASI"                                ,    false, asi);
   scienceAPI.expose("DMsupply","()"               ,       "DM Supply to the ear"               ,    false, deltaSupply);
   scienceAPI.expose("dltEarDMDemand","()"         ,       "Todays ear DM demand"               ,    false, deltaDemand);
   scienceAPI.expose("dltKernelDMDemand","()"      ,       "Todays kernel DM demand"            ,    false, kernelGR);
   scienceAPI.expose("dltCobDMDemand","()"         ,       "Todays cob DM demand"               ,    false, cobDemand);
   scienceAPI.expose("dltDMCob","()"               ,       "DM Allocation to the cob"           ,    false, deltaCobWeight);
   scienceAPI.expose("dltDMKernels","()"           ,       "DM Allocation to the kernels"       ,    false, kernelBio);
   scienceAPI.expose("dltSilkElong","()"           ,       "Todays silk elongation"             ,    false, silkGrowth);
   scienceAPI.expose("earWeight","()"              ,       "Ear Weight"                         ,    false, earWt);
   scienceAPI.expose("cobWeight","()"              ,       "Cob Weight"                         ,    false, cobWt);
   scienceAPI.expose("grainWeight","()"            ,       "Kernel Weight"                      ,    false, grainWt);
   scienceAPI.expose("AbortedGrainWeight","()"     ,       "Aborted Kernel Weight"              ,    false, abortedGrainWt);
   scienceAPI.expose("earWeightPlant","()"         ,       "Ear Biomass per Plant"              ,    false, earWtPlant);
   scienceAPI.expose("cobWeightPlant","()"         ,       "Cob Biomass per Plant"              ,    false, cobWtPlant);
   scienceAPI.expose("grainWeightPlant","()"       ,       "Kernel Weight per Plant"            ,    false, grainWtPlant);
   scienceAPI.expose("AbortedGrainWeightPlant","()",       "Aborted Kernel Weight per Plant"    ,    false, abortedGrainWtPlant);
   scienceAPI.expose("cohortGrainSize","()"        ,       "Average Cohort GrainSize"           ,    false, cohortGrainSize);
   scienceAPI.expose("cohortGrains","()"           ,       "Number of Grains Per Cohort"        ,    false, cohortGrains);
   scienceAPI.expose("cohortNumber","()"           ,       "Cohort Number for Reporting"        ,    false, cohortNumber);
   scienceAPI.expose("KernelsPerRing","()"           ,       "KernelsPerRing  for Reporting"        ,    false, kernelsPerCohort);
   
   scienceAPI.expose("silkingDas","()"           ,       "silkingDas"        ,    false, silkingDas);
   scienceAPI.expose("sheddingDas", "()", "sheddingDas", false, sheddingDas);
   
   scienceAPI.expose("kernelNumber","()"           ,       "kernelNumber"        ,    false, kernelNumber);
   scienceAPI.expose("cohortsEmerged","()"           ,       "cohortsEmerged"        ,    false, cohortsEmerged);
   scienceAPI.expose("cobGRFract","()"           ,       "cobGRFract"        ,    false, cobGRFract);
   scienceAPI.expose("grainGRFract", "()", "grainGRFract", false, grFract);

   
   

   //scienceAPI.expose("yesterdayAccumTT","()"              ,       "Total Ear Bio"              scienceAPI.expose("dltTT","()"              ,       "Total Ear Bio"                      ,    false, dltTT);
   //scienceAPI.expose("todayAccumTT","()"              ,       "Total Ear Bio"                      ,    false, todayAccumTT);
   //        ,    false, yesterdayAccumTT);

   }
//------------------------------------------------------------------------------------------------
void GrainSilking::updateVars()
   {

   //Map some of the variable to the base class
   //dltDmGreen = kernelBio;
   grainNo  = calcGrainNo() * plant->getPlantDensity();

   //This is now done in calcGrainNo()
   //for (unsigned int i = 0; i < nCohorts; i++)
   //   {
   //   cohortGrainSize[i] = (cohorts[i].biomass / kernelsPerCohort) * 1000.0;
   //   }


   if (asi > -999 && silkingDas > 0.0 && sheddingDas > 0.0)
      asi = (int)(silkingDas - sheddingDas);

   //This next part is from Grain::updateVars;
   //Put in here because I think it was adding the retrans incorrectly - see commented line
   // initialise P - must be better way
   if(dmGreen < 1e-5 && dltDmGreen > 0)
      pGreen = initialPConc * dltDmGreen;

   dmGreen += dltDmGreen;
   //dmGreen += dmRetranslocate;
   nGreen += dltNGreen  + dltNRetranslocate;
   nConc = divide(nGreen,dmGreen,0) * 100.0;

   stage = plant->phenology->currentStage();

   earWt = earBio * plant->getPlantDensity();
   cobWt = cobWeight * plant->getPlantDensity();
   grainWt = grainWtPlant * plant->getPlantDensity();
   abortedGrainWt = abortedGrainWtPlant * plant->getPlantDensity();

   earWtPlant = earBio;
   cobWtPlant = cobWeight;
   yield = grainWt * 10.0;												// in kg/ha

   grainSize = divide (grainWt, grainNo, 0.0) * 1000.0;		// 100 grain wt in grams


   }
//------------------------------------------------------------------------------------------------
//------ Daily Routines
//------------------------------------------------------------------------------------------------
void GrainSilking::updateKernels()
   {
   // if a silk was pollinated the previous day, change to growing
   // if a kernel is growing, update its age
   for ( int i = 0; i < nCohorts; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status == Pollinated)
            {
            cohorts[i].kernels[k].status = Growing;
            cohorts[i].status = Growing;
            }
         if (cohorts[i].kernels[k].status == Growing)
            cohorts[i].kernels[k].age += dltTT;
         }
      }
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::process()
   {
   if (plant->phenology->currentStage() <= fi)return;

   // Should Pry do somewhere else
   if (sheddingDd < 1)
      {
      sheddingDd = plant->phenology->sumTTtarget(sowing, flowering);
      startSilkingDd = sheddingDd - ttSilkingBeforeShedding;
      endSilkingDd = sheddingDd - (ttSilkingBeforeShedding - ttSilkingDuration);
      startSilkWindow = sheddingDd - silkNoWindow[0];
      endSilkWindow= sheddingDd - silkNoWindow[1];
      }

   double s = stage;
   //DailyValues
   dltTT = plant->phenology->getDltTT();
   todayAccumTT = plant->phenology->sumTTtotal(sowing, maturity);
   yesterdayAccumTT = todayAccumTT - dltTT;
   waterSD = plant->water->getSdRatio();
   maxT = plant->today.maxT;
   das = plant->das;

   if (sheddingDas < 1 && todayAccumTT > sheddingDd)
      sheddingDas = das;

   // Update the Kernel status 
   updateKernels();

   // calculate actual silks
   calcActualSilks();

   // calculate the initiated silks
   calcInitiated();

   // calculate the length of the silks by growing them each day
   setSilkEnlongation();

   // now grow any silks grow thw silks to see if any emergence
   calcEmerged();

   // now calculate pollination factors and calculate the amount of pollen today
   calcPollenFactors();

   // now calculate the number in each cohort that will be pollinated and the number that will be lost
   // and the rest will be yet to be pollinated
   calcPollination();
   calcNeverToBePollinated();

   // kernel growth
   calcGRFract();
   calcCobWeight();

   kernelGR = kernelGrowthRate();
   deltaDemand = kernelGR + cobDemand;
   dltDMGrainDemand = deltaDemand * plant->getPlantDensity();

   //calcEarGrowth();
   //allocateToCohorts();

   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcActualSilks()
   {
   // if before the silk window or after return
   if (todayAccumTT < startSilkWindow || todayAccumTT > endSilkWindow)return;

   // if in the silk calculation window, calculate the pgr / oCd

   sigmaDltTT.push_back(dltTT); 
   sigmaDltdm.push_back(plant->biomass->getDltDM());

   double sigmaDm =std::accumulate(sigmaDltdm.begin(),sigmaDltdm.end(),0.0);//#include <numeric>
   double sigmaTT =std::accumulate(sigmaDltTT.begin(),sigmaDltTT.end(),0.0);//#include <numeric>
   double pgr = (sigmaDm / plant->getPlantDensity()) / sigmaTT;

   double d = actSilksTable.value(pgr);
//   nCohorts = (int)Min(actSilksTable.value(pgr), potKernels) / kernelsPerCohort ;

   emergeSilkLength = nCohorts * ringSize;


   }

void GrainSilking::calcInitiated()
   {
   // firstly check to see if any silks should be initiated
   // see if we are in the silkInit window. need to check when das spans the window edge ( too complex?)
   if ((todayAccumTT > startSilkingDd && yesterdayAccumTT < endSilkingDd) || carryOverDd > 0.0)
      {
      // must be in the window. now check how many dd today. can be part of the day at the edges
      double initDd = 0.0;
      if (yesterdayAccumTT <= startSilkingDd)
         initDd = todayAccumTT - startSilkingDd;
      else if (todayAccumTT >= endSilkingDd)
         {
         if (yesterdayAccumTT < endSilkingDd)
            initDd = endSilkingDd - yesterdayAccumTT;
         }
      else
         initDd = dltTT;
      // calculate the number of cohorts initiated
      // over 60dd 40 cohorts are initiated so 2/3 cohorts per dd
      // carry over
      initDd += carryOverDd; carryOverDd = 0.0;
      int nInit = (int)ceil((nCohorts / ttSilkingDuration) * initDd);
      carryOverDd = initDd - nInit * (ttSilkingDuration / nCohorts);
      // update the cohort status
      for (int i = 0; i < nCohorts && nInit > 0; i++)
         {
         if (cohorts[i].status == Proto)
            {
            cohorts[i].setStatus(Initiated);
            nInit--;
            }
         }
      // change status to initiated
      if (status == Proto)
         status = Initiated;
      }


   }
//------------------------------------------------------------------------------------------------
void GrainSilking::setSilkEnlongation()
   {
   // SER = α(T-Tb) * (1-β(1-SD))
   silkGrowth = alpha * dltTT * plant->water->calcSilkSwDefExpansion();        //[ EQN 1]  amended
   //     silkGrowth = alpha * dltTT * (1 - beta * (1 - waterSD));        //[ EQN 1]

   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcEmerged()
   {
   for (int i = 0; i < nCohorts; i++)
      {
      if (cohorts[i].status == Initiated)
         {
         cohorts[i].silkLength += silkGrowth;
         cohorts[i].calcReceptivity(dltTT);
         }
      if (cohorts[i].silkLength >= emergeSilkLength) // && status < Pollinated
         {
         if (cohorts[i].status == Initiated)
            {
            cohorts[i].setStatus(Emerged);
            cohortsEmerged++;
            }
         if (silkingDas == 0)
            silkingDas = das;
         cohorts[i].calcReceptivity(dltTT);
         }
      }
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcPollenFactors()
   {
   // calculate PV (pollen viability factor). 
   // as sorghum in the period from shedding -150 to shedding
   if (yesterdayAccumTT > sheddingDd - ttPollenViabilityWindow && yesterdayAccumTT < sheddingDd)
      {
      if (maxT > pollenStressStartTemp)
         {
         double todaysTempFactor = Min((maxT - pollenStressStartTemp) / (pollenStressHighTemp - pollenStressStartTemp), 1.0);
         double todaysContribution = dltTT / ttPollenViabilityWindow;
         PV -= todaysTempFactor * todaysContribution;
         }
      }

   // calculate pollen factor from PV
   //double theta = 5;   // standard deviation 
   //double mu = 3;    // position of mean  

   pollenToday = (int)(sheddingPollenCount * PV * exp(-0.5 * pow((todayAccumTT - sheddingDd - daysMaxPollen) / sqrt(pollenDistribution), 2.0)));
   if(pWCount > 100)    // to test pollen wondow
      {
      pollenToday = 0;
      if(das == pWDay1 || das == pWDay2)
         pollenToday = pWCount;
      }


   if (pollenToday < minDailyPollen)
      pollenFactor = pollenToday / minDailyPollen;
   else
      pollenFactor = 1;
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcPollination()
   {
   kernelNumber = 0;     // kernels pollinated today
   deadKernels = 0;      // kernels dead today
   // no need to do this if there is no pollen
   if (pollenToday < 1.0) return;


   double silkNumber = 0;
   double silkReceptivity = 0;

   // find total kernels where status = emerged
   // sum receptivity where status = emerged
   for ( int i = 0; i < nCohorts; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status == Emerged)
            {
            silkNumber++;
            silkReceptivity += cohorts[i].kernels[k].receptivity;
            }
         }
      }
   // overall values (average silk receptivity)
   if (silkNumber <= 0.00001)
      return;
   silkReceptivity /= silkNumber;
   kernelNumber = (int)(silkNumber * silkReceptivity * pollenFactor);
   deadKernels = (int)(silkNumber * (1 - silkReceptivity));

   // change status to pollinated;
   status = Pollinated;
   // now allocate the kernel number to the cohorts
   // change the status of the kernels from emerged to pollinated
   int nPollinated = kernelNumber;
   pollinated += nPollinated;
   for ( int i = 0; i < nCohorts; i++)
      {
      if (nPollinated == 0)
         break;
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (nPollinated == 0)
            break;
         if (cohorts[i].kernels[k].status == Emerged)
            {
            cohorts[i].kernels[k].status = Pollinated;
            if (cohorts[i].status == Emerged)
               {
               cohorts[i].status = Pollinated;
               }
            nPollinated--;
            }
         }
      }

   // now allocate the deadKernels to the cohorts
   // change the status of the kernels from emerged to dead
   // implementation note: place nDead check inside for loop as an "or clause"
   int nDead = deadKernels;
   for ( int i = 0; i < nCohorts && nDead > 0; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size() && nDead > 0; k++)
         {
         if (cohorts[i].kernels[k].status == Emerged)
            {
            cohorts[i].kernels[k].status = Dead;
            nDead--;
            }
         }
      }
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcNeverToBePollinated()
   {
   // TODO: should only run this once
   // TODO: where does the 5 come from
   if (sheddingDas >0 && pollenToday < 1.0 && das > sheddingDas + 5)
      // anything not polinated and no more pollen can be called dead
      for ( int i = 0; i < nCohorts; i++)
         {
         for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
            {
            if (cohorts[i].kernels[k].status == Emerged)
               cohorts[i].kernels[k].status = Dead;
            }
         }
   }
//------------------------------------------------------------------------------------------------
int GrainSilking::calcGrainNo()
   {
   int yesterday = grainsPerPlant;
   grainsPerPlant = 0;
   grainWtPlant = 0.0;
   abortedGrainWtPlant = 0.0;
   for ( int i = 0; i < nCohorts; i++)
      {
      cohortGrains[i] = 0;
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status == Pollinated 
            || cohorts[i].kernels[k].status == Growing
            || cohorts[i].kernels[k].status == Mature)
            {
            grainsPerPlant++;
            cohortGrains[i]++;
            }
         }
      if (cohortGrains[i] > 0)
         cohortGrainSize[i] = (cohorts[i].biomass / cohortGrains[i]) * 1000.0;
      else
         cohortGrainSize[i] = 0.0;

      if (cohorts[i].status == Pollinated 
         || cohorts[i].status == Growing
         || cohorts[i].status == Mature)
         grainWtPlant += cohorts[i].biomass;
      else
         abortedGrainWtPlant += cohorts[i].biomass;
      }
   return grainsPerPlant;
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcGRFract()
   {
   // for each kernel that is growing, calculate the k
   // get a weighted average of these
   grFract = 0.0;
   double gf = 0.0;
   double gfcount = 0.0;
   double TTb = KGRfrac * lagPhase;
   for ( int i = 0; i < nCohorts; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status == Growing || cohorts[i].kernels[k].status == Pollinated)
            {
            gf += cohorts[i].kernels[k].CalcK(TTb, dltTT, relMaxKGR, potKGR, cohorts[i].biomass / cohorts[i].nGrains, useGWRatio, potKernelWt);       // EQN [5]
            gfcount++;
            }
         }
      }

   if (gfcount > 0)
      {
      gf = gf / gfcount;
      grFract = pow(gf, signalStrength);   // EQN[6]
      }
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::calcCobWeight()
   {
   // ear growth (egr) prior to pollination is 1/150g per oCd - 1g at shedding
   // stop this at first pollination
   double TTb = KGRfrac * lagPhase;
   if (todayAccumTT > startSilkingDd)
      {
      double maxSilk = nCohorts * kernelsPerCohort;
      
      int pollinatedOvules = 0;
      for (int i = 0; i < nCohorts; i++)
         {
         for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
            {
            if (cohorts[i].kernels[k].status == Growing
               || cohorts[i].kernels[k].status == Pollinated
               || cohorts[i].kernels[k].status == Aborted
               || cohorts[i].kernels[k].status == Mature)
               pollinatedOvules++;
            }
         }

      // Set the potential A
      double potA = maxSilk * potKernelWt * kernelCobRatio / 1000; // g
      // the realized A is the the fraction of polinated silks
      double A = potA * (pollinatedOvules / maxSilk); // g
      // Calculate where we are on the sigmoid curve
      double tTT = todayAccumTT - sheddingDd;
      double todayCobWt = A / (1 + exp(-cobK * (tTT - TTb / 2)));

      // Max Rate is the slope of the sigmoid at the midpoint
      double maxRate = A * cobK / 4;      // demand at max per oCd
      cobDemand = 0.0;
      // If Pre Pollination grow at 1/150
      if (pollinatedOvules < 1.0)
         cobDemand = (1.0 / 150.0)*dltTT;
      // If Post Pollination Start Growing on Sigmoid
      else if (!allPollinated())
         cobDemand = Max(todayCobWt - cobWeight, (1.0 / 150.0)*dltTT);
      // If Kernels are not allDead Continue to grow cob
      else if(!allDead())
         cobDemand = Max(todayCobWt - cobWeight, 0.0);
      // Calculate the sink strength ratio for the cob 
      // --Calculated as the ratio of the current slope of growth and the maxium slope
      if (maxRate > 0 && dltTT > 0.0)
         cobGRFract = (cobDemand / dltTT) / maxRate;  // ratio of today's demand to max demand per oCd
      else 
         cobGRFract = 0;

      }
   // ear growth is now minimum of supply and demand
   }
//------------------------------------------------------------------------------------------------
bool GrainSilking::allPollinated()
   {
   for ( int i = 0; i < nCohorts; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status < Pollinated)
            {
            return false;
            }
         }
      }
   return true;
   }
//------------------------------------------------------------------------------------------------
bool GrainSilking::allDead()
   {
   for (int i = 0; i < nCohorts; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status <= Growing || cohorts[i].kernels[k].status == Mature)
            {
            return false;
            }
         }
      }
   return true;
   }
//------------------------------------------------------------------------------------------------
//void GrainSilking::calcEarGrowth()
//   {
//   if (status == Initiated)
//      {
//      // calculate carbon supply and ear demand
//      // Equation [7]
//      deltaRetrans = plant->stem->dmRetransAvailable();
//      deltaSupply = PGR + deltaRetrans;// *grFract;// *(1 / (1 - p.kernelCobRatio));
//      double kernelGR = kernelGrowthRate();
//      deltaDemand = kernelGR + deltaCobWeight;
//
//      egr = Min(deltaSupply, deltaDemand);
//      //void  dmRetrans(double dltDm){dmRetranslocate = dltDm;}
//
//      // retrans to stem
//      double dltStemRestrans = 0.0;
//      if (PGR < egr)      // need some from stem
//         {
//         double amtNeeded = egr - PGR;
//         //plant->stem->dmRetrans(-1.0 * Min(deltaRetrans, amtNeeded));
//         }
//      if (PGR > egr)     // rest to stem
//         {
//         double excess = PGR - egr;
//         //plant->stem->dmRetrans(excess);
//         }
//      earBio += egr;
//      kernelBio = Max( egr - deltaCobWeight, 0.0); // to allocate to the cohorts
//      }
//   }
//------------------------------------------------------------------------------------------------
double GrainSilking::kernelGrowthRate()
   {
   // sum all the potential growth rates for growing kernels     EQN[7]
   double sigmaPKGR = 0.0;
   for ( int i = 0; i < nCohorts; i++)
      {
      for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
         {
         if (cohorts[i].kernels[k].status == Growing || cohorts[i].kernels[k].status == Pollinated)
            {
            sigmaPKGR += cohorts[i].kernels[k].pKGR;
            }
         }
      }
   return sigmaPKGR;
   }
//------------------------------------------------------------------------------------------------
void GrainSilking::allocateToCohorts()
   {
   // allocate todays kernelBio to the growing cohorts
   // for each cohort, calculate the PKGR by summing the PKGR for each kernel
   double availableKernelBio = kernelBio;


   for ( int i = 0; i < nCohorts; i++)
      {
      if (cohorts[i].status == Growing || cohorts[i].status == Pollinated)
         {

         double cohortPGR = 0.0;
         int nGrowing = 0;

         for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
            {
            if (cohorts[i].kernels[k].status == Pollinated || cohorts[i].kernels[k].status == Growing || cohorts[i].kernels[k].status == Mature)
               {
               cohortPGR += cohorts[i].kernels[k].pKGR;
               nGrowing++;
               }
            }

         double maxCohortWt = potKernelWt * nGrowing / 1000;

         double KGR = Min(availableKernelBio, cohortPGR);
         KGR = Min(maxCohortWt - cohorts[i].biomass, KGR);
         cohorts[i].biomass += KGR;
         // Need to assign biomass to every kernel proportional to their demand
         for (unsigned int k = 0; k < cohorts[i].kernels.size(); k++)
            {
            if (cohorts[i].kernels[k].status == Pollinated || cohorts[i].kernels[k].status == Growing || cohorts[i].kernels[k].status == Mature)
               {
               cohorts[i].kernels[k].biomass += KGR * (cohorts[i].kernels[k].pKGR / cohortPGR);
               }
            }
         availableKernelBio -= KGR;
         // test for abortion
        if (KGR < 0.00001 && cohorts[i].biomass < abortFrac * maxCohortWt)     // EQN [9]
            {
            cohorts[i].strikes++;
            if (cohorts[i].strikes == maxNumStrikes)
               cohorts[i].setStatus(Aborted);
            }
         else
            {
            cohorts[i].strikes = 0;
            }
         // test for maturity
         if (cohorts[i].biomass >= maxCohortWt)
            cohorts[i].setStatus(Mature);
         }
      }
   }

//------------------------------------------------------------------------------------------------
double GrainSilking::calcDltCDemand(double dailyBiom)
   {
   if (status > Initiated)
      {
      // calculate carbon supply and ear demand
      // Equation [7]
      kernelGR = kernelGrowthRate();
      deltaDemand = kernelGR + cobDemand;
      return deltaDemand * plant->getPlantDensity();
      }
   return 0;
   }



////------------------------------------------------------------------------------------------------
////------- calc Grain demand  Pioneer Cohort
////------------------------------------------------------------------------------------------------
//double GrainCM::calcCarbonDemand()
//   {
//   // calculate demand for carbon using cohort approach
//   // uses a potential kernel growth rate (mg/oCd/kernel) limited by
//   // potential kernel weight
//   double dltTT = plant->phenology->getDltTT();
//   double totalCDemand = 0;
//   for(int i=0;i < maxCohorts;i++)
//      {
//      if(cohorts[i].getStatus() == growing)
//         {
//         totalCDemand += cohorts[i].carbonDemand(dltTT,potKGR);
//         }
//      }
//
//   // carbon demand by the grain and cob
//   return totalCDemand * plant->getPlantDensity() * (1/(1-kernelCobRatio));
//   }

//------------------------------------------------------------------------------------------------
double GrainSilking::dmRetrans(double dltDM)
   {
   //Calculate The strength of rebmobilization for cob and grains
   double totalFract = Max(0.0, grFract + cobGRFract);

   // Get the minimum of what is required and the ammount that 
   // can be aquired and total Fract is maximum is 1.0
   dmRetranslocate = Min(dltDMGrainDemand - dltDmGreen, dltDM * Min(totalFract, 1.0));
   dltDmGreen += dmRetranslocate;

   double dltDMPlant = dltDmGreen / plant->getPlantDensity();

   deltaSupply = dltDMPlant;
   earBio += dltDMPlant;

   
   deltaCobWeight = Min(dltDMPlant, cobDemand);
   cobWeight += deltaCobWeight;

   kernelBio = Max( dltDMPlant - deltaCobWeight, 0.0); // to allocate to the cohorts

   double s = stage;
   allocateToCohorts();

   return -1.0 * dmRetranslocate;


   }
double GrainSilking::partitionDM(double dltDM)
   {
   dltDmGreen = Min(dltDMGrainDemand, dltDM);
   return Max(dltDmGreen,0.0);
   }
//---------------------------------------------------------------------------
//------ Cohort Set Grain Status
//------------------------------------------------------------------------------------------------
void Cohort::setStatus(GrainStatus _status)
   {
   status = _status;
   for (unsigned int i = 0; i < kernels.size(); i++)
      if (_status == Mature)
         {
         if (kernels[i].status == Growing || kernels[i].status == Pollinated)
            {
            kernels[i].status = _status;
            }
         }
      else
         {
         kernels[i].status = _status;
         }
   }
//---------------------------------------------------------------------------
//------ Cohort Calculate Silk Receptivity
//------------------------------------------------------------------------------------------------
void Cohort::calcReceptivity(double todayTT)
   {
   age += todayTT;
   if (age <= 250) receptivity = 1.0;
   else
      receptivity = Max(0, 1 - (age - 250) / (350 - 250));
   for (unsigned int i = 0; i < kernels.size(); i++)
      kernels[i].receptivity = receptivity;
   }
//---------------------------------------------------------------------------
//------ Calclate K on Kernel
//------------------------------------------------------------------------------------------------
double Kernel::CalcK(double TTb, double dltTT, double relMaxKGR, double potKGR, double kernelWeight, int GWRatioSwitch, double potKerneWeight)
   {
   double value = exp(relMaxKGR * (age - TTb));
   k = (value / (1 + value));                             //  EQN [5]
   k = Max(Min(k, 1.0), 0);                     //  Eqn[5i]
   pKGR = k * potKGR;                            //  EQN [5]  in mg/oCd
   pKGR = pKGR * dltTT / 1000;    // in g/day

   //Piece to change kernel weight by ratio that occurs on TTb
   //Implentation of Reduction in Potential Grain Size based on
   //Early grain growth a la Borras [DO NOT USE ME] [DELETE ASAP]
   if(GWRatioSwitch == 1)
      {
      if(age >= TTb)
         {
         if(GWRatio == 0.0)
            {
            GWRatio = biomass / potentialWeight;
            }
         pKGR *= GWRatio;
         k *= GWRatio;
         if(kernelWeight >= (potKerneWeight * GWRatio) / 1000)  
            {
            pKGR = 0;
            k = 0;
            }
         }
      }

   potentialWeight += pKGR;

   return k;
   }