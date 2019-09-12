//------------------------------------------------------------------------------------------------
#include <stdio.h>

#include "Plant.h"
#include "Nitrogen.h"

using namespace Sorghum;
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------ Nitrogen Constructor
//------------------------------------------------------------------------------------------------
Nitrogen::Nitrogen(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
   {
   plant = p;
	//soil is updated once, initialize can be called more often
	nLayers = 0;
	profileDepth = 0.0;

   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Nitrogen Destructor
//------------------------------------------------------------------------------------------------
Nitrogen::~Nitrogen()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Nitrogen::doRegistrations(void)
   {

   scienceAPI.expose("nfact_pheno",          "()",    "Nitrogen stress factor for phenology",            false, phenoStress);
   scienceAPI.expose("nfact_expan",          "()",    "Nitrogen stress factor for leaf expansion",       false, expansionStress);
   scienceAPI.expose("nfact_photo",          "()",    "Nitrogen stress factor for photosynthesis",       false, photoStress);
   scienceAPI.expose("n_sd_ratio",           "()",    "Nitrogen supply/demand ratio",                    false, supplyDemandRatio);
   scienceAPI.expose("n_supply_soil",        "g/m^2", "Today's total N supply from soil profile",        false, nSupply);
   scienceAPI.expose("n_massflow_uptake",    "g/m^2", "Today's N uptake by massflow from soil profile",  false, actualMassFlow);
   scienceAPI.expose("n_diffusion_uptake",   "g/m^2", "Today's N uptake by diffusion from soil profile", false, actualDiffusion);
   scienceAPI.expose("n_total_uptake",       "g/m^2", "Today's N uptake by mass flow and diffusion",     false, actualTotal);
   scienceAPI.expose("no3_demand",           "g/m^2", "Today's total crop N demand",                     false, plantNDemand);
   test = &plantNDemand;
   scienceAPI.expose("diffusion_supply_tot", "g/m^2", "Accumulative total of crop N supply by diffusion",false, sumDiffSupply);
   scienceAPI.expose("biomass_n",            "g/m^2", "N above ground biomass including grain",          false, nBiomass);
   scienceAPI.expose("stover_n",             "g/m^2", "N above ground biomass excluding grain",          false, nStover);
   scienceAPI.expose("green_biomass_n",      "g/m^2", "N in live above ground biomass including grain",  false, nGreenBiomass);
   scienceAPI.expose("n_cum_uptake",         "g/m^2", "Cumulative N Uptake",                             false, nUptakeTotal);
   scienceAPI.expose("n_Plant",              "g/m^2", "Total Nitrogen in the plant including roots",     false, nPlant);


   scienceAPI.exposeFunction("GreenN", "g/m^2", "N content of live plant parts",
                    FloatFunction(&Nitrogen::getNGreen));
   scienceAPI.exposeFunction("DeltaGreenN", "g/m^2", "Daily N increase in live plant parts",
                    FloatArrayFunction(&Nitrogen::getDltNGreen));
   scienceAPI.exposeFunction("dlt_n_retrans", "g/m^2", "N retranslocated from plant parts to grain",
                    FloatArrayFunction(&Nitrogen::getDltNRetrans));
   scienceAPI.exposeFunction("SenescedN", "g/m^2", "N content of senesced plant parts",
                    FloatFunction(&Nitrogen::getNSenesced));

   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Nitrogen::initialize(void)
   {
   phenoStress =     1.0;
   expansionStress = 1.0;
   photoStress =     1.0;

   nBiomass = 0.0;
   nStover =  0.0;
   nPlant =   0.0;
   nSupply =  0.0;
   actualTotal =   0.0;
   nUptakeTotal =  0.0;
   nGreenBiomass = 0.0;
   plantNDemand =  0.0;
   actualMassFlow =  0.0;
   actualDiffusion = 0.0;

   currentLayer = 0;

   //Set up reporting vectors
   int nParts = plant->PlantParts.size();
   nGreen.assign       (nParts,0.0);
   dltNGreen.assign    (nParts,0.0);
   dltNRetrans.assign  (nParts,0.0);
   nSenesced.assign    (nParts,0.0);
   dltNDetached.assign (nParts,0.0);
   photoStressTotal.assign(nStages+1,0.0);
	
   if(nLayers > 0  && nLayers < 100)
   {
     no3.assign(nLayers, 0.0);
   }

   massFlowSupply.clear();
   diffusionSupply.clear();
   fixationSupply.clear();
   dltNo3.clear();

   rootDepth = 0.0;
	sumDiffSupply = 0.0;
	totalDemand = 0.0;
	supplyDemandRatio = 0.0;
   }
//------------------------------------------------------------------------------------------------
//----------- read Nitrogen parameters
//------------------------------------------------------------------------------------------------
void Nitrogen::readParams (void)
   {
   scienceAPI.read("NO3_diffn_const", "", false, diffnConstant);
   scienceAPI.read("maxUptakeRate",   "", false, maxUptakeRate);
   scienceAPI.read("nUptakeCease",    "", false, nUptakeCease );
   scienceAPI.read("nSupplyFrac",     "", false, nSupplyFrac );
   }
//------------------------------------------------------------------------------------------------
//-------- Get Nitrogen variables from other modules
//------------------------------------------------------------------------------------------------
void Nitrogen::getOtherVariables (void)
   {
   std::vector<double> values;

   if (!scienceAPI.get("no3", "kg/ha", true, values, 0.0, 10000.0))
      {
      // we have no N supply - make non-limiting.
      for (int i = 0; i < nLayers; i++)
         {
         values.push_back(10000.0);
         }
      }
 //  convertVector(values,no3);
   fillVector(values, no3);

   values.clear();

   scienceAPI.get("no3_min", "kg/ha", true, values, 0.0, 10000.0);

   fillVector(values, no3Min);

   // convert to g/m2
   for (int i = 0; i < nLayers; i++)
      {
      no3[i]    *= (kg2gm / ha2sm);
      no3Min[i] *= (kg2gm / ha2sm);
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Set Nitrogen variables in other modules
//------------------------------------------------------------------------------------------------
void Nitrogen::setOtherVariables (void)
   {
   std::vector<double> dltNo3Values;
   for(int i=0;i < nLayers;i++)dltNo3Values.push_back(0.0);

   for(unsigned i=0;i < dltNo3.size();i++)
      {
      dltNo3Values[i] = dltNo3[i] * gm2kg /sm2ha;
      }
   scienceAPI.set("dlt_no3", "kg/ha", dltNo3Values);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Nitrogen::onNewProfile(NewProfileType &v /* message */)
   {
   FVecToDVec(&dLayer, v.dlayer);

   // dlayer may be changed from its last setting due to erosion
   profileDepth = sumVector(dLayer);      // depth of soil profile (mm)
   nLayers = dLayer.size();

   /* TODO : Insert new root profile and llDep code for change in profile due to erosion */
   /* TODO : Check validity of ll,dul etc as in crop_check_sw */
   /* TODO : why does this not include no3 */
   }
//------------------------------------------------------------------------------------------------
//----------- perform daily nitrogen dynamics  ---------------------------------------------------
//------------------------------------------------------------------------------------------------
void Nitrogen::process(void)
   {
   getOtherVariables ();
   supply();          // potential N in g/m2 from mass flow and diffusion
   demand();
   uptake();
   partition();
   }
//------------------------------------------------------------------------------------------------
//----------- update nitrogen state variables at the end of the day
//------------------------------------------------------------------------------------------------
void Nitrogen::updateVars(void)
   {
   // calc stress factors
   double SLN = plant->leaf->getSLN();

   phenoStress = (1.0/0.7) * SLN * 1.25 - (3.0/7.0);
   phenoStress = bound(phenoStress,0.0,1.0);

   photoStress = (2.0/(1.0 + exp(-6.05*(SLN-0.41)))-1.0);
   photoStress = Max(photoStress,0.0);
   accumulate(photoStress, photoStressTotal, plant->phenology->currentStage(), plant->phenology->getDltStage());

   //Hammer, G.L. and Muchow, R.C. (1994).  Assessing climatic risk to sorghum production
   // in water-limited subtropical environments. I. Development and testing of a simulation model.
   // Field Crops Research, 36:221-234.
   // Muchow and Sinclair, 1994. Crop Sci. 34: 721-727.


   sumDiffSupply = sumVector(diffusionSupply);
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      nGreen[i] = plant->PlantParts[i]->getNGreen();
      dltNGreen[i] = plant->PlantParts[i]->getDltNGreen();
      dltNRetrans[i] = plant->PlantParts[i]->getDltNRetranslocate();
      nSenesced[i] = plant->PlantParts[i]->getNSenesced();
      dltNDetached[i] = plant->PlantParts[i]->getDltDetNSenesced();
      }

   rootDepth = plant->roots->getRootDepth();
   currentLayer = findIndex(rootDepth, dLayer);

   actualTotal = actualMassFlow + actualDiffusion;

   setOtherVariables ();

   nPlant = sumVector(nGreen) + sumVector(nSenesced);
   nGreenBiomass = sumVector(nGreen) - plant->roots->getNGreen();
   nBiomass = nGreenBiomass + sumVector(nSenesced) - plant->roots->getNSenesced();
   nStover = nBiomass - plant->grain->getNGreen() - plant->grain->getNSenesced();
   nUptakeTotal += actualTotal;

   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Nitrogen::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
      case emergence :
         break;
      case fi :
         break;
      case startGrainFill :
         break;
      }
   }
//------------------------------------------------------------------------------------------------
//------- calculate nitrogen supply potential from mass flow diffusion and fixation
//------------------------------------------------------------------------------------------------
void Nitrogen::supply(void)
   {
   calcMassFlow();   // N g/m2 from Mass Flow
   calcDiffusion();  // potential N g/m2 from Diffusion
   calcFixation();
   }
//------------------------------------------------------------------------------------------------
//------- Mass Flow Supply
//------------------------------------------------------------------------------------------------
//-----  Return potential nitrogen uptake (supply) by mass flow (water uptake) (g/m^2)
void Nitrogen::calcMassFlow(void)
   {
   massFlowSupply.clear();
   for(int layer = 0;layer <= currentLayer;layer++)
      {
	   double swdep = plant->water->swDepLayer(layer);
	   double dltSwdep = -plant->water->dltSwDepLayer(layer);

	   double no3ConcLayer = divide(no3[layer],plant->water->swDepLayer(layer));
      double no3MassFlow = no3ConcLayer * (-plant->water->dltSwDepLayer(layer));
      massFlowSupply.push_back(Min(no3MassFlow,no3[layer] - no3Min[layer]));
      }
   }
//------------------------------------------------------------------------------------------------
//------- Diffusion Supply
//------------------------------------------------------------------------------------------------
void Nitrogen::calcDiffusion(void)
   {
   diffusionSupply.clear();
   for(int layer = 0;layer <= currentLayer;layer++)
      {
      // restricts NO3 available for diffusion to NO3 in plant available water range
      double swAvailFrac = plant->water->swAvailFracLayer(layer);
      double no3Diffn = bound(swAvailFrac,0,1.0) * no3[layer];
      diffusionSupply.push_back(Min(no3Diffn,no3[layer] - no3Min[layer]));
      }
   double prop = layerProportion();
   diffusionSupply[currentLayer] *= layerProportion();
   }
//------------------------------------------------------------------------------------------------
//------- Fixation Supply
//------------------------------------------------------------------------------------------------
void Nitrogen::calcFixation(void)
   {
   //  fixationSupply.clear();

   /* TODO : Have to do this sometime for non-Sorghum crops */

   }
//------------------------------------------------------------------------------------------------
//------- calculate nitrogen demand in each plant part
//------------------------------------------------------------------------------------------------
void Nitrogen::demand(void)
   {
   totalDemand = 0;
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
	   double dem = plant->PlantParts[i]->calcNDemand();
	   totalDemand += dem;
      }
   }
//------------------------------------------------------------------------------------------------
//------- calculate nitrogen uptake
//------------------------------------------------------------------------------------------------
//     Return actual plant nitrogen uptake from each soil layer.
void Nitrogen::uptake(void)
   {
   // no3 (g/m2) available from diffusion
   vector<double> diffnAvailable;
   for(int layer = 0;layer <= currentLayer;layer++)
      diffnAvailable.push_back(Max(diffusionSupply[layer] - massFlowSupply[layer],0.0));
   double totalMassFlowSupply = sumVector(massFlowSupply);
   double totalDiffusionSupply = sumVector(diffnAvailable);
   double potentialSupply = totalMassFlowSupply + totalDiffusionSupply;

   // get actual total nitrogen uptake for diffusion and mass flow.
   // If demand is not satisfied by mass flow, then use diffusion.

   double lfDemand = plant->leaf->getNDemand();
   double sDemand = plant->stem->getNDemand();

   plantNDemand = totalDemand - plant->grain->getNDemand();

   // nUptakeCease oCd after anthesis, stop diffusion uptake
   double ttElapsed =   plant->phenology->sumTTtotalFM(flowering, maturity);

   if(ttElapsed > nUptakeCease)totalMassFlowSupply = 0.0;
   actualMassFlow = totalMassFlowSupply;
   actualDiffusion = 0.0;

   if(totalMassFlowSupply < plantNDemand && ttElapsed < nUptakeCease)
      {
      // need diffusion
      /* TODO : Put in fixation here - one day */
      actualDiffusion = bound(plantNDemand - totalMassFlowSupply,0.0,totalDiffusionSupply);
      // limit the amount of diffusion that can happen in one day
      actualDiffusion = divide(actualDiffusion,diffnConstant);
      // implement a maximum transport rate on diffusion
      // total uptake cannot exceed 0.029 g N /m2 /dd
      // reduce maxUptakeRate by total / 100kgha-1

//      nSupplyFrac (5) to limit n uptake
      double maxUptakeRateFrac = Min(1.0,potentialSupply / nSupplyFrac) * maxUptakeRate;
	  double growingTT = plant->phenology->getDltTT();
	  double maxUptake = maxUptakeRateFrac * plant->phenology->getDltTT() - actualMassFlow;
      actualDiffusion = Min(actualDiffusion, maxUptake);
	  int tmp = 0;
      }

   vector<double> mff,df;
   //get actual change in N contents
   dltNo3.clear();
   double duptake = 0.0;
   for(int layer = 0;layer <= currentLayer;layer++)
      {
      double massFlowFraction = divide(massFlowSupply[layer],totalMassFlowSupply);
      mff.push_back(massFlowFraction);
      double diffusionFraction = divide(diffnAvailable[layer],totalDiffusionSupply);
      df.push_back(diffusionFraction);
      double layerUptake = actualMassFlow * massFlowFraction +
                             actualDiffusion  * diffusionFraction;
      dltNo3.push_back(-layerUptake);
	  duptake += layerUptake;
      }

   supplyDemandRatio = 0.0;
//   double totalUptake = sumVector(dltNo3);
   nSupply = actualMassFlow + actualDiffusion;

   if(plantNDemand > 0.0)
      supplyDemandRatio = Min(nSupply / plantNDemand,1.0);
   }
//------------------------------------------------------------------------------------------------
//------- partition Nitrogen
//------------------------------------------------------------------------------------------------
//     allocate N to each plant part
void Nitrogen::partition(void)
   {

	double forStem = 0.0;
	double forRachis = 0.0;
	double forLeafFromStem = 0.0;
	double forLeaf = 0.0;

	double stemStructuralDemand = 0.0;
	double rachisStructuralDemand = 0.0;
	double leafStructuralDemand = 0.0;
	const bool FORLEAF = true;

   double nAvailable = nSupply;
   // 1. allocate to roots in proportion to demand
   double rootDemand = plant->roots->calcNDemand();
   double nRequired = supplyDemandRatio * rootDemand;
   plant->roots->partitionN(nRequired);
   nAvailable -= nRequired;

   // 2. allocate structural N to stem and rachis
   // If not enough N available, senesce leaf
   // stem first
   nRequired = plant->stem->calcStructNDemand();
   stemStructuralDemand = nRequired;
   if(nRequired > 0)
      {
      if(nRequired <= nAvailable)
         {
         plant->stem->partitionN(nRequired);
         nAvailable -= nRequired;
         }
      else
         {
         // get from leaf to provide structN deficit
		  forStem = plant->leaf->provideN(nRequired - nAvailable, !FORLEAF);
         plant->stem->partitionN(nAvailable + forStem);
         nAvailable =0.0;
         }
      }
   // now rachis
   nRequired = plant->rachis->calcStructNDemand();
   rachisStructuralDemand = nRequired;
   if(nRequired > 0)
      {
      if(nRequired <= nAvailable)
         {
         plant->rachis->partitionN(nRequired);
         nAvailable -= nRequired;
         }
      else
         {
         // get from leaf to provide structN deficit
		  forRachis = plant->leaf->provideN(nRequired - nAvailable, !FORLEAF);
         plant->rachis->partitionN(nAvailable + forRachis);
         nAvailable =0.0;
         }
      }

   // 3. Now allocate N to new leaf with SLN 1.0
   // If not enough N available, take from stem and canopy
   nRequired = plant->leaf->calcNewLeafNDemand();
   leafStructuralDemand = nRequired;
   if(nRequired > 0)
      {
      if(nRequired <= nAvailable)
         {
         plant->leaf->partitionN(nRequired);
         nAvailable -= nRequired;
         }
      else
         {
         plant->leaf->partitionN(nAvailable);
         nRequired -= nAvailable;
         nAvailable =0.0;
         /* get from stem and canopy to provide new LAI deficit */
         double transN = plant->stem->provideN(nRequired);
		 forLeafFromStem = transN;
         plant->leaf->partitionN(transN);
         nRequired -= transN;
         if(nRequired > 0)
            {
            transN = plant->leaf->provideN(nRequired, FORLEAF);
			forLeaf = transN;
            plant->leaf->partitionN(transN);
            }
         }
      }

   // 4. Allocate to leaf and stem_rachis in proportion to demand
   double leafDemand = plant->leaf->calcNDemand();
   double stemDemand = plant->stem->calcNDemand();
   double rachisDemand = plant->rachis->calcNDemand();
   double totalDemand = leafDemand + stemDemand + rachisDemand;

   double toLeaf = Min(1.0,divide(leafDemand,totalDemand) * nAvailable);
   plant->leaf->partitionN(toLeaf);

   double toRachis = Min(1.0,divide(rachisDemand,totalDemand) * nAvailable);
   plant->rachis->partitionN(toRachis);

   // rest to stem
   double toStem = nAvailable - toLeaf - toRachis;
   plant->stem->partitionN(toStem );


   // get the grain N demand
   // translocate from Stem, rachis and leaf to meet demand
   nRequired = plant->grain->calcNDemand();
   double nRachis,nStem,nLeaf;
   if(nRequired > 0)
      {
      nRachis = plant->rachis->provideN(nRequired);
      nRequired -= nRachis;
      plant->grain->RetranslocateN(nRachis);
      if(nRequired > 0)
         {
         nStem = plant->stem->provideN(nRequired);
         nRequired -= nStem;
         plant->grain->RetranslocateN(nStem);
         }
      if(nRequired > 0)
         {
         nLeaf = plant->leaf->provideN(nRequired, !FORLEAF);
         plant->grain->RetranslocateN(nLeaf);
         }
      }
   }
//------------------------------------------------------------------------------------------------
//------- Calculate plant Nitrogen detachment from senesced and dead pools
//------------------------------------------------------------------------------------------------
void Nitrogen::detachment(vector<double> senDetachFrac)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      plant->PlantParts[i]->NDetachment(senDetachFrac);
      }
   }
//------------------------------------------------------------------------------------------------
double Nitrogen::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   double layerTop    = sumVector(dLayer, currentLayer);
   double layerBottom = sumVector(dLayer, currentLayer+1);

   return Min(divide(rootDepth - layerTop,layerBottom - layerTop),1.0);
   }
//------------------------------------------------------------------------------------------------
void Nitrogen::getNGreen(float &result)
   {
   result = (float)sumVector(nGreen);
   }
//------------------------------------------------------------------------------------------------
void Nitrogen::getDltNGreen(vector<float> &result)
   {
   DVecToFVec(result, dltNGreen);
   }
//------------------------------------------------------------------------------------------------
void Nitrogen::getDltNRetrans(vector<float> &result)
   {
   DVecToFVec(result, dltNRetrans);
   }
//------------------------------------------------------------------------------------------------
void Nitrogen::getNSenesced(float &result)
   {
   result = (float)sumVector(nSenesced);
   }
//------------------------------------------------------------------------------------------------
void Nitrogen::Summary(void)
   {
   char msg[120];
   sprintf(msg,"Grain N percent    (%%)     =  %8.2f \t Grain N            (kg/ha) = %8.2f\n",
            plant->grain->getNConc(),plant->grain->getNGreen() * 10.0); scienceAPI.write(msg);
   sprintf(msg,"Total N content    (kg/ha) =  %8.2f \t Senesced N content (kg/ha) = %8.2f\n",
            nBiomass * 10.0,sumVector(nSenesced) * 10.0); scienceAPI.write(msg);
   sprintf(msg,"Green N content    (kg/ha) =  %8.2f\n",
            sumVector(nGreen) * 10.0 - plant->grain->getNGreen() * 10.0); scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
double Nitrogen::sumPhotoStressTotal(int from, int to)
   {
   return sumVector(photoStressTotal,from,to);
   }
//------------------------------------------------------------------------------------------------

