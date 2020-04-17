//------------------------------------------------------------------------------------------------
#include <stdio.h>

#include "Plant.h"
#include "Phosphorus.h"

using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ Phosphorus Constructor
//------------------------------------------------------------------------------------------------
Phosphorus::Phosphorus(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
   {
   plant = p;

   StressParts.push_back(plant->leaf);
   StressParts.push_back(plant->stem);
   StressParts.push_back(plant->grain);

   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Phosphorus Destructor
//------------------------------------------------------------------------------------------------
Phosphorus::~Phosphorus()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Phosphorus::doRegistrations(void)
   {
   double p_dead = 0.0;
   scienceAPI.expose("pfact_pheno",     "",     "Phosphorus stress factor for phenology",      false, phenoStress);
   scienceAPI.expose("pfact_expansion", "",     "Phosphorus stress factor for leaf expansion", false, expansionStress);
   scienceAPI.expose("pfact_photo",     "",     "Phosphorus stress factor for photosynthesis", false, photoStress);
   scienceAPI.expose("pfact_grain",     "",     "Phosphorus stress factor for grain",          false, grainStress);
   scienceAPI.expose("p_total_uptake",  "g/m2", "Today's P uptake",                            false, pUptakeTotal);
   scienceAPI.expose("p_dead",          "g/m2", "p_dead",                                      false, p_dead);


   scienceAPI.expose("BiomassP",        "g/m2", "BiomassP",                                    false, pBiomass);

   scienceAPI.exposeFunction("GreenP", "g/m2", "P content of live plant parts",
      FloatFunction(&Phosphorus::getPGreen));
   scienceAPI.exposeFunction("SenescedP","g/m2", "P content of senesced plant parts",
      FloatFunction(&Phosphorus::getPSenesced));
   scienceAPI.exposeFunction("p_demand", "kg/ha", "P demand of plant parts",
      FloatArrayFunction(&Phosphorus::getPDemand));
   scienceAPI.exposeFunction("dlt_p_green", "g/m2", "Daily P increase in live plant parts",
      FloatArrayFunction(&Phosphorus::getDltPGreen));
   scienceAPI.exposeFunction("dlt_p_retrans", "g/m2", "P retranslocated from plant parts to grain",
      FloatArrayFunction(&Phosphorus::getDltPRetrans));
   scienceAPI.exposeFunction("dlt_p_detached", "g/m2", "Actual P loss with detached plant",
      FloatArrayFunction(&Phosphorus::getDltPDetached));

   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Phosphorus::initialize(void)
   {
   active = false;

   phenoStress     = 1.0;
   expansionStress = 1.0;
   photoStress     = 1.0;
   grainStress     = 1.0;

   totalDemand = 0.0;

   pPlant =   0.0;
   pStover =  0.0;
   pBiomass = 0.0;
   pUptakeTotal =  0.0;
   pGreenBiomass = 0.0;

   currentLayer =  0;

   //Set up reporting vectors
   int nParts = plant->PlantParts.size();
   pGreen.assign      (nParts,0.0);
   dltPGreen.assign   (nParts,0.0);
   pGreen.assign      (nParts,0.0);
   dltPRetrans.assign (nParts,0.0);
   pSenesced.assign   (nParts,0.0);
   dltPRetrans.assign (nParts,0.0);
   dltPDetached.assign(nParts,0.0);
   pDemand.assign     (nParts,0.0);
   }
//------------------------------------------------------------------------------------------------
//----------- read Phosphorus parameters
//------------------------------------------------------------------------------------------------
void Phosphorus::readParams (void)
   {
   std::vector<double> values;
   if (scienceAPI.get("labile_p", "", 1, values, 0.0, 10000.0))
      {
      active = true;
      }
   else return;

   scienceAPI.read("pfact_pheno_slope"    ,"", 0, phenoSlope);
   scienceAPI.read("pfact_photo_slope"    ,"", 0, photoSlope);
   scienceAPI.read("pfact_expansion_slope","", 0, expansionSlope);
   scienceAPI.read("pfact_grain_slope"    ,"", 0, grainSlope);
   }
//------------------------------------------------------------------------------------------------
//-------- Get Phosphorus variables from other modules
//------------------------------------------------------------------------------------------------
void Phosphorus::getOtherVariables (void)
   {
   stage = plant->phenology->currentStage();
   }
//------------------------------------------------------------------------------------------------
//-------- Set Phosphorus variables in other modules
//------------------------------------------------------------------------------------------------
void Phosphorus::setOtherVariables (void)
   {
   }
//------------------------------------------------------------------------------------------------
//----------- perform daily phosphorus dynamics  ---------------------------------------------------
void Phosphorus::prepare(void)
   {
   getOtherVariables ();
   demand();
   calcStress();
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::process(void)
   {
	if(!Active())return;
   //   supply();
   uptake();
   partition();
   senescence();
   detachment();
   retranslocate();
   updateP();
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::calcStress(void)
   {
   double pfact = pStress();          // generic p stress factor

   phenoStress = bound(pfact * phenoSlope,0.0,1.0);
   photoStress = bound(pfact * photoSlope,0.0,1.0);
   expansionStress = bound(pfact * expansionSlope,0.0,1.0);
   grainStress = bound(pfact * grainSlope,0.0,1.0);
   }
//------------------------------------------------------------------------------------------------
double Phosphorus::pStress(void)
   {
   // calculate a generic p stress factor by getting the max and min concentrations
   // and calculating where the actual concentration is
   double wt = 0.0, max = 0.0, min = 0.0, act = 0.0;
   double stress;
   for(unsigned i=0;i < StressParts.size();i++)
      {
      double partWt = StressParts[i]->getDmGreen();
      wt += partWt;
      max += StressParts[i]->pConcMax() * partWt;
      min += StressParts[i]->pConcMin() * partWt;
      act += StressParts[i]->getPGreen();
      }

   double actConc = divide(act, wt, 1.0);
   double maxConc = divide(max, wt, 1.0);
   double minConc = divide(min, wt, 1.0);

   if ((wt < 1.0e-5) || (act < 1.0e-5)) stress = 1.0;
   else stress = divide(actConc - minConc, maxConc - minConc, 1.0);

   return bound(stress,0.0,1.0);
   }
//------------------------------------------------------------------------------------------------
//----------- update phosphorus state variables at the end of the day
//------------------------------------------------------------------------------------------------
void Phosphorus::updateVars(void)
   {
   if(!active)return;
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      pGreen[i] =      plant->PlantParts[i]->getPGreen();
      pDemand[i] =     plant->PlantParts[i]->getPDemand() * 10;
      pSenesced[i] =   plant->PlantParts[i]->getPSenesced();
      dltPGreen[i] =   plant->PlantParts[i]->getDltPGreen();
      dltPRetrans[i] = plant->PlantParts[i]->getDltPRetrans();

      }


   pPlant = sumVector(pGreen) + sumVector(pSenesced);
   pGreenBiomass = sumVector(pGreen) - plant->roots->getPGreen();
   pBiomass = pGreenBiomass + sumVector(pSenesced) - plant->roots->getPSenesced();
   pStover =  pBiomass - plant->grain->getPGreen() - plant->grain->getPSenesced();

   }
//------------------------------------------------------------------------------------------------
//------- calculate phosphorus supply potential from mass flow diffusion and fixation
//------------------------------------------------------------------------------------------------
void Phosphorus::supply(void)
   {
   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------- calculate phosphorus demand in each plant part
//------------------------------------------------------------------------------------------------
void Phosphorus::demand(void)
   {
   totalDemand = 0;
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      pDemand[i] = plant->PlantParts[i]->calcPDemand() * 10;
      }
   totalDemand = sumVector(pDemand);
   }
//------------------------------------------------------------------------------------------------
//------- calculate phosphorus uptake
//------------------------------------------------------------------------------------------------
//     Get total P uptake
void Phosphorus::uptake(void)
   {
   vector<double> layeredUptake;

   if (!scienceAPI.get("uptake_p_sorghum", "", 1, layeredUptake, 0.0f, 10000.0f))
      {
      // we have no P uptake - set to demand
      pUptakeTotal = totalDemand * kg2gm/ha2sm;
      }
   else
      pUptakeTotal = sumVector(layeredUptake);
   }
//------------------------------------------------------------------------------------------------
//------- partition Phosphorus
//------------------------------------------------------------------------------------------------
//     allocate P to each plant part
void Phosphorus::partition(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->partitionP(pUptakeTotal * divide(plant->PlantParts[i]->getPDemand(),
         totalDemand,0.0));
      }
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::senescence(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->calcDltPSenesced();
      }
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::detachment(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->calcDltPDetached();
      }
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::updateP(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->updateP();
      }
   }
//------------------------------------------------------------------------------------------------
//------- partition Phosphorus
//------------------------------------------------------------------------------------------------
void Phosphorus::retranslocate(void)
   {
   // dummy code to get this done - will change
   string plantParts = "ssssd";
   vector<double> supply;
   vector<double> demand;

   for( unsigned i=0;i < plantParts.size();i++)
      {
      if(plantParts[i] == 's')
         {
         double minP = plant->PlantParts[i]->pConcMin() * plant->PlantParts[i]->getDmGreen();
         supply.push_back(Max(plant->PlantParts[i]->getPGreen() - minP,0.0));
         demand.push_back(0.0);
         }
      else
         {
         supply.push_back(0.0);
         double maxP = plant->PlantParts[i]->pConcMax() * plant->PlantParts[i]->getDmGreen();
         demand.push_back(Max(maxP - plant->PlantParts[i]->getPGreen(),0.0));
         }
      }
   // retranslocate

   for(unsigned i=0;i < plant->PlantParts.size();i++)
      if(plantParts[i] == 's')
         {
         double fraction = bound(divide(sumVector(demand),sumVector(supply),0.0),0.0,1.0);
         plant->PlantParts[i]->setPRetrans(-supply[i]*fraction);
         }
      else
         {
         double fraction = bound(divide(sumVector(supply),sumVector(demand),0.0),0.0,1.0);
         plant->PlantParts[i]->setPRetrans(demand[i]*fraction);
         }
   }

//------------------------------------------------------------------------------------------------
//------- Calculate plant Phosphorus detachment from senesced pool
//------------------------------------------------------------------------------------------------
void Phosphorus::detachment(vector<double> senDetachFrac)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      //      plant->PlantParts[i]->NDetachment(senDetachFrac);
      }
   }
//------------------------------------------------------------------------------------------------
double Phosphorus::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   double layerTop    = sumVector(dLayer, currentLayer);
   double layerBottom = sumVector(dLayer, currentLayer+1);

   return Min(divide(rootDepth - layerTop,layerBottom - layerTop),1.0);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPGreen(float &result)
   {
   result = (float)sumVector(pGreen);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPSenesced(float &result)
   {
   result = (float)sumVector(pSenesced);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPDemand(vector<float> &result)
   {
   DVecToFVec(result, pDemand);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPGreen(vector<float> &result)
   {
   DVecToFVec(result, dltPGreen);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPRetrans(vector<float> &result)
   {
   DVecToFVec(result, dltPRetrans);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDetached(vector<float> &result)
   {
   DVecToFVec(result, dltPDetached);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDead(vector<float> &result)
   {
   DVecToFVec(result, dltPDead);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDeadDetached(vector<float> &result)
   {
   DVecToFVec(result, dltPDetachedDead);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPDead(float &result)
   {
   result = (float)sumVector(pDead);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::Summary(void)
   {
   char msg[120];
   sprintf(msg, "Grain P percent    (%%)     =  %8.2f \t Grain P uptake     (kg/ha) = %8.2f\n",
      plant->grain->getPConc() * 100,plant->grain->getPGreen() * 10.0); scienceAPI.write(msg);
   sprintf(msg, "Total P content    (kg/ha) =  %8.2f \t Senesced P content (kg/ha) = %8.2f\n",
      pBiomass * 10.0,sumVector(pSenesced) * 10.0); scienceAPI.write(msg);
   sprintf(msg, "Green P content    (kg/ha) =  %8.2f \n",
      sumVector(pGreen) * 10.0 - plant->grain->getPGreen() * 10.0); scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Phosphorus::onNewProfile(NewProfileType &p /* message */)
   {
   /* TODO : Insert new root profile for change in profile due to erosion */
   }
//------------------------------------------------------------------------------------------------

