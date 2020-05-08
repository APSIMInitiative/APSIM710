//------------------------------------------------------------------------------------------------
#include <stdio.h>

#include "GrainCM.h"
#include "Plant.h"

using namespace Maize;

//---------------------------------------------------------------------------
//------ Grain Constructor
//------------------------------------------------------------------------------------------------
GrainCM::GrainCM(ScienceAPI2 &api, Plant *p) : Grain(api, p)
   {
   initialize();

   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Grain Destructor
//------------------------------------------------------------------------------------------------
GrainCM::~GrainCM()
   {
   cohorts.clear();
   }
//--------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void GrainCM::readParams (void)
   {
   Grain::readParams();

   // Cohort Silking Parameters
   //   maxEGR = readVar(plantInterface,sections,"maxEGR");
   /* TODO: switch between linear and non-linear EGR - PGR relationship  */
   scienceAPI.read("dm_per_seed","", 0, dmPerSeed);
   scienceAPI.read("egrSlope", "", 0, egrSlope);

   scienceAPI.read("minPGR", "", 0, minPGR);
   scienceAPI.read("maxSilk", "", 0, maxSilk);
   scienceAPI.read("minEarBio", "", 0, minEarBio);
   scienceAPI.read("extrRate", "", 0, extrRate);

   scienceAPI.read("applyStress2Silking", "", 0, applyStress2Silking); 
   scienceAPI.read("minBioStress", "", 0, minBioStress);
   scienceAPI.read("minExtrStress", "", 0, minExtrStress);
   scienceAPI.read("ttGrnNo", "", 0, ttGrnNo);
   scienceAPI.read("ttEarInit", "", 0, ttEarInit);
   //
   // This parameter is the days after silking. We use it to
   // estimate potential kernel number
   scienceAPI.read("silkDays", "", 0, silkDays);

   scienceAPI.read("lagPhase", "", 0, lagPhase);
   scienceAPI.read("potKGR", "", 0, potKGR);
   scienceAPI.read("potKernelWt", "", 0, potKernelWt);
   scienceAPI.read("kernelCobRatio", "", 0, kernelCobRatio);
   scienceAPI.read("beta0", "", 0, beta0);
   scienceAPI.read("beta1", "", 0, beta1);
   scienceAPI.read("abortKernelFrac", "", 0, abortKernelFrac);
   scienceAPI.read("minKGRfrac", "", 0, minKGRfrac);
   scienceAPI.read("KGRfrac", "", 0, KGRfrac);
   scienceAPI.read("relMaxKGR", "", 0, relMaxKGR);

   scienceAPI.read("maxCohorts", "", 0, maxCohorts);
   scienceAPI.read("KernelsPerRing", "", 0, kernelsPerRing);
   scienceAPI.read("signalStrength", "", 0, signalStrength);
   scienceAPI.read("potKernelWtFrac", "", 0, potKernelWtFrac);

   if(cohorts.size() == 0)
      {
      for(int i = 0; i < maxCohorts; i++)
         {
         GrainCohort newCohort;
         cohorts.push_back(newCohort);
         }
      }
   }
//------------------------------------------------------------------------------------------------
void GrainCM::initialize(void)
   {
   Grain::initialize();

   grainWeight = 0.0;      // added by zhanshan, April 11, 2007
   addGrainWeight = -1;

   // Pioneer Silking
   silkNumber = 0.0;
   silkDay = 0;
   nDays = 0;
   accSwStress = 0.0;
   silkStage = 0.0;
   //===========================================================
   // Commented by Zhanshan, April 11, 2007
   // From season to season, this variable must be set to zero
   // Otherwise, the simulation results for the second season and thereafter are
   // incorrect
   // This bug is fixed by calling this method after crop harvesting
   //=================================================================
   accEarBio = 0.0;

   cohorts.clear();

   // Pioneer Cohort
   nCohorts = 0;
   gfTemp = 0.0;
   potKernelWtFrac = 1.0;

   PGR = 0;
   }
//------------------------------------------------------------------------------------------------
//------ read Grain parameters
//------------------------------------------------------------------------------------------------
void GrainCM::doRegistrations(void)
   {
   Grain::doRegistrations();
   // Pioneer Cohort
   scienceAPI.expose("silkNumber", "silks/plant", "Silk Number", false, silkNumber);
   scienceAPI.expose("nCohorts", "", "Number of cohorts", false,nCohorts);

   scienceAPI.exposeFunction("kernels", "", "Kernels per cohort", FloatArrayFunction(&GrainCM::getKernels));
   scienceAPI.exposeFunction("kernelWt", "mg", "Kernel weight", FloatArrayFunction(&GrainCM::getKernelWt));
   scienceAPI.exposeFunction("CohortStatus", "", "Cohort Status", FloatArrayFunction(&GrainCM::getStatus));

   //========================================================================
   // Commented by Zhanshan, March 27, 2007
   // I double checked the code. The following two lines are misleading.
   // The actual unit of the two variable is g/m2, sam as "grain_wt"
   //   setupGetVar("grainWeight", grainWeight, "g", "GrainWeight per Plant"); //misunderstanding
   //   setupGetVar("cobWeight", cobWeight, "g", "CobWeight per Plant"); // misunderstanding
   //=======================================================================
   scienceAPI.expose("grainWeight", "kg/ha", "GrainWeight (kg/ha)", false, grain_Weight);
   scienceAPI.expose("cobWeight", "kg/ha", "CobWeight (kg/m2)", false, cob_Weight);

   scienceAPI.expose("grFract", "()", "Grain factor", false, grFract);
   scienceAPI.expose("gfTemp", "()", "Grain factor", false, gfTemp);
   scienceAPI.expose("SilkDay", "()", "Silking days after planting", false, silkDay);

   }
//------------------------------------------------------------------------------------------------
double GrainCM::partitionDM(double dltDM)
   {
   dltDmGreen = Min(dltDMGrainDemand, dltDM);
   return Max(dltDmGreen,0.0);
   }
//------------------------------------------------------------------------------------------------
//------ update variables
//------------------------------------------------------------------------------------------------
void GrainCM::updateVars(void)
   {
   // initialise P - must be better way
   if(dmGreen < 1e-5 && dltDmGreen > 0)
      pGreen = initialPConc * dltDmGreen;

   // Pioneer Cohort
   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;

   nGreen += dltNGreen  + dltNRetranslocate;
   nConc = divide(nGreen,dmGreen,0) * 100.0;

   // the calculated grainSize includes cob weight
   // Zhanshan, Oct 29, 2007
   // Replaced in the updateCohortVars funciton
   //grainSize = divide (dmGreen + dmDead, grainNo, 0.0);

   grainSize = divide (dmGreen, grainNo, 0.0) * 1000.0;		// 100 grain wt in grams
   stage = plant->phenology->currentStage();
   //yield = dmGreen * 10.0;												// in kg/ha

   // Ramp grain number from 0 at StartGrainFill to finalGrainNo at SGF + 100dd
   double gfTTNow = plant->phenology->sumTTtotalFM(startGrainFill,maturity);
   grainNo = Min((gfTTNow/100.0 *  finalGrainNo),finalGrainNo);

   dltDMGrainDemand = 0.0;
   updateCohortVars();

   yield = grainWeight;

   }
//------------------------------------------------------------------------------------------------
void GrainCM::updateCohortVars(void)
   {
   // Pioneer Cohort
   // calculate number of cohorts
   nCohorts = 0;
   grainNo = 0;
   grainWeight = 0.0;
   for(int i=0;i < maxCohorts;i++)
      {
      if ((cohorts[i].getStatus() == growing)||(cohorts[i].getStatus() == mature))nCohorts++;

      if(cohorts[i].getStatus() != aborted)
         grainNo += cohorts[i].getKernelNumber();                               // Eqn[14]
      grainWeight += (cohorts[i].getKernelWt() * cohorts[i].getKernelNumber());

      }
   grainNo *= plant->getPlantDensity();
   grainWeight *= plant->getPlantDensity() / 1000;            // grain weight in g

   cobWeight = dmGreen - grainWeight;
   grain_Weight = grainWeight*10;
   cob_Weight = cobWeight*10;
   grainSize = divide(grainWeight, grainNo, 0.0);
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void GrainCM::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
   case emergence :
      break;
   case fi :
      totDMGreenFI = plant->biomass->getTotalBiomass();                  // for grain number
      updated = false;
      break;
   case startGrainFill :
      //pKGR = divide(potKernelWt,plant->phenology->sumTTtarget(startGrainFill, endGrainFill));

      finalGrainNo = calcGrainNumber();

      break;
      }
   }
//------------------------------------------------------------------------------------------------
void GrainCM::process(void)
   {

   stage = plant->phenology->currentStage();

   daysFromEmerg = (int)plant->phenology->sumDaysTotal(emergence,startGrainFill);
   ttFromEmerg = plant->phenology->sumTTtotal(emergence, maturity);

   // daily grain biomass demand (EGR) This can be before flowering
   calcBiomassDemand();

   // update the potential kernel weight halfway between silking and maturity
   UpdatePotKernelWt();

   // calculate average stress between flowering - ttGrnNo and Flowering
   // This line was in the following if statement. It could not do the job it suppose to do
   // Zhanshan moved it here. Jan 7, 2009
   double averageStress = CalcAvStress();

   if(stage >= flag && stage < startGrainFill)
      {
      // calculate Silking  if not calculated and <= 5 days after flowering
      int daysAfterFlowering = (int)plant->phenology->sumDaysTotal(flowering,maturity);
      if ((silkNumber < 1) && (daysAfterFlowering <= 5))
         {
         // see if today is silking day
         if(silkDay == 0)
            calcSilkingDay();

         //if (daysFromEmerg - silkDay == silkDays - 1)      // ie 'silkDays' (3) after silking
         // =================================================
         // Commented by Zhanshan, April 11, 2007
         // Three day after plants silk or 5 days after sheeding and plant has some silks
         // the silk number should be calculated and converted to potential grain number
         //====================================================
         if ((daysFromEmerg - silkDay == silkDays - 1)      // ie 'silkDays' (3) after silking
            || ((daysAfterFlowering==5) && (silkDay != 0) && (silkNumber<1)))
            // last chance to be pollinated, calculated silk number anyway
            {
            // calculate average stress between flowering - ttGrnNo and Flowering
            // double averageStress = CalcAvStress(); // commented out by Zhanshan, move to before the if-statement. Jan 7, 2009
            silkNumber = calcSilkNumber(averageStress);
            }
         }
      if ((silkNumber > 1) && (daysFromEmerg - silkDay == silkDays - 1))
         {
         silkPlus3 = ttFromEmerg;
         silkPlus3Day = daysFromEmerg;
         initCohorts(silkNumber,accEarBio);
         finalGrainNo = silkNumber * plant->getPlantDensity();
         }
      //finalGrainNo = silkNumber * plant->getPlantDensity();
      }

   // calculate ageTT for each cohort.
   // this is TT relative to silkPlus3 when grains can start filling depending on their age group
   if(finalGrainNo > 1 && daysFromEmerg <= silkPlus3Day + 2)
      for (int i=0; i<maxCohorts; i++)
         {
         if (cohorts[i].getStatus() == growing)
            {
            switch (cohorts[i].getAge())
               {
            case 0:
               cohorts[i].setAgeTT(0.0);
               break;
            case 1:
               if(daysFromEmerg == silkPlus3Day + 1)
                  cohorts[i].setAgeTT(ttFromEmerg - silkPlus3);
               break;
            case 2 :
               if (daysFromEmerg == silkPlus3Day + 2)
                  cohorts[i].setAgeTT(ttFromEmerg - silkPlus3);
               break;
               }
            }

         //calcBiomassDemand();
         }

      // calculate k based on ageTT
      if(finalGrainNo > 1)
         {
         for (int i=0; i<maxCohorts; i++)
            cohorts[i].setk(calcCohortGRFract(cohorts[i].getAgeTT()));
         // then the k value of whole ear is calculated.
         grFract = calcGRFract();
         }
   }
//------------------------------------------------------------------------------------------------
void GrainCM::UpdatePotKernelWt(void)
   {
   //--------------------------------------------------------------------------------------------
   // Duing the testing, we found the the kernel weight is much less than we expected
   // Becuase of the small maximum kernel weight, crop matures quite earlier than the actual crop
   // Try to discard the rule for updating maximum kernel weight to let crop
   // grow large kernel and mature later
   return; // zhanshan added this line to avoid use the following rule to update MKW, Mar 6, 2007
   //---------------------------------------------------------------------------------------------
   // Greg added following line to make it flexible
   // The equation in this rule is coming from data set that describe the relationship
   // between kernel weight and TT with base temperature = 8oC
   // TT calculated in this model has a base temperature of 0oC
   //
   // potKernelWtTT -  set the proportion of TT between flowering and maturity to
   // elapse when MKW is calculated. potKernelWtFrac =  0 - 1
   /*
   double potKernelWtTT = plant->phenology->sumTTtarget(flowering, maturity)  * potKernelWtFrac;
   double ttFromFlowering = plant->phenology->sumTTtotalFM(flowering, maturity);

   if(ttFromFlowering > potKernelWtTT && !updated)
   {
   updated = true;
   for(int i=0;i < maxCohorts;i++)
   if(cohorts[i].getStatus() == growing)
   cohorts[i].setMaxKernelWt(potKernelWt, beta0, beta1);             // Eqn[15]
   }
   */
   }
//------------------------------------------------------------------------------------------------
double GrainCM::CalcAvStress(void)
   {
   // calculate average stress
   double stressStage = plant->phenology->calcStageTarget(flowering, -ttGrnNo);
   double avgStress = 0.0;
   if (stage  > stressStage)
      {

      // The following three lines commented out by Zhanshan, Jan 7, 2009
      //	  nDays++;
      //        accSwStress += Min(1.0,plant->water->getSdRatio());
      //	  avgStress = accSwStress / nDays;
      // Zhanshan added the following lines to calculate average expansion stress (baseed on leaf expansion)
      // Then the result will be used to adjust slik extrusion rate or maximum silk number

      nDays++;
      accSwStress += Min(1.0,plant->water->getEarExpansionStress());
      avgStress = accSwStress / nDays;
      }
   return Max(0.0,avgStress);
   }
//------------------------------------------------------------------------------------------------
void GrainCM::calcBiomassDemand(void)
   {
   // calculate the stage when ear initiation occurs ( flowering - ttEarInit)
   //   PGR = Plant Growth Rate; EGR = Ear Growth Rate
   //======================================================
   // Comments by Zhanshan, April 11 ,2007
   // Ear initiation time is march earlier in reality
   // It happens around V-stage = 7-8
   // To do the simulation in this way, make it impossible to simulate ASI and ear biomass
   // correctly
   //========================================================
   double earInitStage = plant->phenology->calcStageTarget(flowering, -ttEarInit);
   if (stage  >= earInitStage)
      {
      PGR = divide(plant->biomass->getDltDM(),plant->getPlantDensity());
      double EGR = 0;
/* TODO: switch between linear and non-linear EGR - PGR relationship  */
      if(PGR > minPGR)
//         EGR = maxEGR * (1 - exp(-1 /maxEGR * (PGR - minPGR)));               // Eqn[3]
         EGR = egrSlope * (PGR - minPGR);
      accEarBio += EGR;
      dltDMGrainDemand = EGR *plant->getPlantDensity();
      }
   }
//------------------------------------------------------------------------------------------------
double GrainCM::calcEGR(double PGR)                           // from biomass
   {
   // This is the exponential equation of PGR-EGR relationship
   /* TODO: switch between linear and non-linear EGR - PGR relationship  */
   //   double EGR =  maxEGR * (1 - exp(-1/maxEGR * (PGR/plant->getPlantDensity())));
   double EGR =  egrSlope * PGR/plant->getPlantDensity();

   return EGR * plant->getPlantDensity();                    // Eqn[9iii] part 2 Push demand
   }
//------------------------------------------------------------------------------------------------
double GrainCM::calcDltCDemand(double dailyBiom)
   {
   // DEMAND
   double pullDemand = calcCarbonDemand() * grFract;                  // Eqn[9iii part 1]  Pull

   // demand from PGR-EGR (push)    This is the maximum that can go to the grain
   double pushDemand = calcEGR(dailyBiom) * (1 - grFract);                  // Eqn[9iii part 2]  Push

   dltDMGrainDemand = pushDemand + pullDemand;                       // Eqn[9iii]
   return dltDMGrainDemand;
   }
//------------------------------------------------------------------------------------------------
void GrainCM::calcDemandStress(void)
   {
   if(stage >= startGrainFill && stage <= endGrainFill)
      {
      dltDMStressMax = yieldPartDemandStress();
      }
   }
//------------------------------------------------------------------------------------------------
//------- calc Grain demand  Pioneer Cohort
//------------------------------------------------------------------------------------------------
double GrainCM::calcCarbonDemand()
   {
   // calculate demand for carbon using cohort approach
   // uses a potential kernel growth rate (mg/oCd/kernel) limited by
   // potential kernel weight
   double dltTT = plant->phenology->getDltTT();
   double totalCDemand = 0;
   for(int i=0;i < maxCohorts;i++)
      {
      if(cohorts[i].getStatus() == growing)
         {
         totalCDemand += cohorts[i].carbonDemand(dltTT,potKGR);
         }
      }

   // carbon demand by the grain and cob
   return totalCDemand * plant->getPlantDensity() * (1/(1-kernelCobRatio));
   }
//------------------------------------------------------------------------------------------------
double GrainCM::calcGRFract(void) // this is for whole ear
   {
   // average over all cohorts to get a sink strength signal
   double kernelNumber = 0.0;
   double weightedk = 0.0;
   for (int i=0; i<maxCohorts; i++)
      {
      if (cohorts[i].getStatus() == growing)
         {
         weightedk += cohorts[i].getKernelNumber() * cohorts[i].getk();
         kernelNumber += cohorts[i].getKernelNumber();
         }
      }

   double gf = divide(weightedk, kernelNumber);                // Eqn[6]
   gfTemp = gf;
   gf = exp(signalStrength * log(gf));                        // Eqn[6]
   return gf;
   }
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
double GrainCM::calcCohortGRFract(double ageTT)
   {
   // calculate how far we are through the startup phase of grain growth before the linear phase
   // this is for an individual cohort

   // Zhanshan Dong Dec 2006
   // For expolinear equation, please refer to the following paper
   //
   // J. Lee, J. Goudriaan, and H. Challa. 2003. Using the expolinear equation for
   //   modelling crop growth in year-round cut chrysanthemum. Annuals of Botany, 92:697-708
   //

   double k = 0.0;
   double fillingTT = ttFromEmerg - (silkPlus3+ageTT); // TT from start growth to now
   // ageTT may be negative. It means not initialized.
   if ((ageTT >= 0.0 )&& (fillingTT >= 0.0)) // cohorts are old enough to suck carbon
      {
      if (KGRfrac > 1.0)
         {
         double TTb = KGRfrac * lagPhase;
         double value = exp(relMaxKGR * (fillingTT - TTb));
         k = bound ((value / (1 + value)),0.0,1.0);                     //  Eqn[5i]
         }
      }

   return k;
   }
//------------------------------------------------------------------------------------------------

void GrainCM::partition2Cohorts(double dm)
   {
   // partition dm to cohorts in sequence.
   // dm/ grain is limited by growth rate potKGR and maxKernelWt.
   // if a cohort has reached maxKernelWt, it is mature (change status)
   // if there is no carbon for a cohort, decide on abortion
   dm /= plant->getPlantDensity();

   // first fill demand, then allocate any excess dm equally. put this in dltDm

   for(int i=0;i < maxCohorts;i++)
      {
      if(cohorts[i].getStatus() == growing)
         {
         double supply = Min(cohorts[i].getCarbonDemand(), dm);
         // supply carbon to cohort
         cohorts[i].init();
         cohorts[i].supplyC(supply);
         dm -= supply;                                  // Eqn[8]
         }
      }

   // test for abortion, update weight and check for cohort maturity
   for(int i=0;i < maxCohorts;i++)
      {
      if(cohorts[i].getStatus() == growing)
         cohorts[i].update(abortKernelFrac * potKernelWt,(daysFromEmerg - silkPlus3Day));
      }

   // check for ear maturity
   bool earMature = true;
   double lastKernel = 0.0;
   for(int i=0;i < maxCohorts;i++)
      {
      if(cohorts[i].getStatus() == growing)
         {
         earMature = false;
         lastKernel = divide(cohorts[i].getDltDm(), cohorts[i].getKernelNumber());
         }
      }

   // if the growth rate of the last cohort is reduced to
   //    minKGRfrac * potKGR (0.46)   ear is mature
   double dltTT = plant->phenology->getDltTT();
   double GR = lastKernel / dltTT * 1000.0;    // in g/cohort
   if((GR < minKGRfrac * potKGR) && (plant->stem->dmRetransAvailable()<= 0.01))
      earMature = true;
   if(earMature)
      plant->phenology->setStage(maturity);

   // whatever left will return back to the labile pool
   leftoverDM = dm;
   }

//------------------------------------------------------------------------------------------------
//------- calc Grain number Pioneer
//------------------------------------------------------------------------------------------------
void GrainCM::calcSilkingDay(void)
   {
   // see if today is silking day
   if (accEarBio > minEarBio)
      {
      silkDay = daysFromEmerg;
      silkStage = stage;
      }
   }
//------------------------------------------------------------------------------------------------
double GrainCM::calcSilkNumber(double averageStress)
   {
   // calculate silk no
   //
   double silkNo = 0.0;
   //   double minBioS = minEarBio + (1.0 - averageStress) * minBioStress;
   //   double extrRateS = extrRate - (1.0 - averageStress) * minExtrStress;
   double minBioS = minEarBio;
   double extrRateS = extrRate;
   double maxSilkS = maxSilk;
   if (applyStress2Silking == 1) {
      minBioS = minEarBio * (2.0 - averageStress);
      extrRateS = extrRate * averageStress;
      maxSilkS = maxSilk * averageStress;
      }

   if (accEarBio > minEarBio)
      {
      silkNo = maxSilkS  * (1 - exp(-extrRateS * (accEarBio - minBioS)));        // Eqn[2]
      }
   return silkNo;
   }
//------------------------------------------------------------------------------------------------
// Pioneer Silking
void GrainCM::initCohorts(double silkNumber, double earBio)
   {
   // calculate the number of cohorts needed and initialise them
   // xs = minBio90, x0 = minEarBio, x = earBio, eta = extrRate (Mesinna et al)
   //

   nCohorts = (int) (silkNumber / kernelsPerRing) + 1;                     // Eqn[1]
   // the following if statement is added by Zhanshan, Dec 4, 2008
   // When we do the sensitivity analysis, we use some large values (>1000) for mxSilk
   // That will generate more than 1000 silknumber and more than 50 (maxCohorts)
   // Because of the nature of an fixed array, the model crashed
   // USe the following if statement to prevent that happened
   // We need to find better to handle this
   if (nCohorts > maxCohorts ) {
      nCohorts = maxCohorts;
      silkNumber = nCohorts * kernelsPerRing;
      }

   double minBio90 = minEarBio -  log(1-0.9) / extrRate;                    // Eqn[4i]
   double threshold = divide(earBio - minEarBio, minBio90 - minEarBio);     // Eqn[4ii]
   double ageGroup[3];
   double ageGroups[3];
   ageGroup[0] = Min(1.0/3.0 * threshold, 1.0);                            // Eqn[4iii]
   ageGroup[2] = Max((2.0-threshold)/3.0, 0.0);                            // Eqn[4v]
   ageGroup[1] = Max((3.0-threshold)/3.0 - ageGroup[2],0.0);               // Eqn[4iv]

   ageGroups[0] = ageGroup[0];
   ageGroups[1] = ageGroup[0] + ageGroup[1];
   ageGroups[2] = ageGroup[0] + ageGroup[1] + ageGroup[2];

   for(int i=0;i < nCohorts;i++)
      {
      cohorts[i].setStatus(growing);
      cohorts[i].setKernelNumber(kernelsPerRing);
      if (i==nCohorts - 1) // the kernel number of last cohorts is not exactly equal to kernels per ring
         {
         cohorts[i].setKernelNumber(int(silkNumber - kernelsPerRing *(nCohorts-1)));
         }
      cohorts[i].setMaxKernelWt(potKernelWt);
      cohorts[i].setPosition(i);
      // set cohort ages
      cohorts[i].setAge (0);
      // set cohort age related TT
      cohorts[i].setAgeTT (-1.0);
      // adjust age according to age groups
      if ((i >= (int)(nCohorts * ageGroups[0])) && (i< (int)(nCohorts * ageGroups[1])))
         {
         cohorts[i].setAge(1);
         }
      else if (i>= (int)(nCohorts * ageGroups[1]))
         {
         cohorts[i].setAge(2);
         }
      }
   }

//------------------------------------------------------------------------------------------------
double GrainCM::calcGrainNumber(void)
   {
   // increase in plant biomass between fi and start grain fill
   double dltDMPlant = plant->biomass->getTotalBiomass()  - totDMGreenFI;

   // growth rate per day
   double nDays1 = plant->phenology->sumDaysTotal(fi,startGrainFill); // change nDays to nDays1 to avoid potential conflicts
   double growthRate = divide(dltDMPlant,nDays1);
   return divide(growthRate, dmPerSeed);
   }
//------------------------------------------------------------------------------------------------
// Calculate the stress factor for diminishing potential harvest index
double GrainCM::yieldPartDemandStress(void)
   {
   double rueReduction = Min(plant->getTempStress(), Min(plant->nitrogen->getPhotoStress(), plant->phosphorus->getPhotoStress()));
   return plant->water->photosynthesisStress() * rueReduction;
   }
//------------------------------------------------------------------------------------------------
// calculate daily grain dm demand using source / sink approach
double GrainCM::calcDMGrainSourceSink(void)
   {
   // proportion of grain filling stage
   double fracGF = stage - startGrainFill;
   double ttFlowerMaturity = plant->phenology->sumTTtarget(flowering,maturity);
   double lag = divide((140 - plant->phenology->getTTtarget(flowering)),
      (ttFlowerMaturity - plant->phenology->getTTtarget(flowering)));

   if(fracGF <= lag)return plant->biomass->getDltDM() * 0.25;

   // fraction 0.78 used because end_grain_fill is 95% of total flowering to maturity
   // Ronnie started levelling off at 515 GDD which is 0.78 of 95% of 695
   if(fracGF < 0.78)
      {
      double totDMCaryopsis = divide(plant->biomass->getDltDM() , grainNo);
      totDMCaryopsis = divide(totDMCaryopsis, plant->phenology->getDltTT());
      return (0.0000319 + 0.4026 * totDMCaryopsis) * plant->phenology->getDltTT() * grainNo * 2;
      }                                                              /* TODO : ILL EAGLE */

   // ony occurs first time fracGF >= 0.78
   if(addGrainWeight <= 0.0)
      {
      double dmDead = 0; //Should always be 0 - we don't carry this var in base class anymore
      double grainWt = divide((dmGreen + dmDead),grainNo);
      double grainWtMax = divide(grainWt,(0.85 + 0.6* (fracGF-0.75)));
      addGrainWeight = divide((grainWtMax - grainWt),
         (ttFlowerMaturity - (ttFlowerMaturity * fracGF)));
      }
   return addGrainWeight * plant->phenology->getDltTT() * grainNo;
   }

//------------------------------------------------------------------------------------------------
double GrainCM::dmRetrans(double dltDM)
   {
   dmRetranslocate =  dltDM;
   if (dmRetranslocate >= 0)
      partition2Cohorts(dmRetranslocate * (1 - kernelCobRatio) );
   return 0;
   }
//------------------------------------------------------------------------------------------------
void  GrainCM::Harvest(void)
   {
   Grain::Harvest();

   initialize();
   }
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
void GrainCM::getKernels(vector<float> &result)
   {
   vector<float> kernels;
   kernels.assign(7,0.0);

   for(unsigned i=0;i < cohorts.size();i++)
      if (cohorts[i].getStatus() == aborted) kernels[i] = 0.0f;
      else kernels[i] = (float)(cohorts[i].getKernelNumber());

      result = kernels;
   }
//------------------------------------------------------------------------------------------------
void GrainCM::getKernelWt(vector<float> &result)
   {
   vector<float> kernelWt;
   kernelWt.assign(7,0.0);

   for(unsigned i=0;i < cohorts.size();i++)
      kernelWt[i] = (float)(cohorts[i].getKernelWt());

   result = kernelWt;
   }
//------------------------------------------------------------------------------------------------
void GrainCM::getStatus(vector<float> &result)
   {
   vector<float> status;
   status.assign(7,0);

   for(unsigned i=0;i < cohorts.size();i++)
      status[i] = (float)(cohorts[i].getStatus());

   result = status;
   }

//------------------------------------------------------------------------------------------------




