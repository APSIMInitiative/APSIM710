//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include <vector>
#include <General/string_functions.h>

#include "Plant.h"
#include "Water.h"
using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ Water Constructor
//------------------------------------------------------------------------------------------------
Water::Water(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
   {
   plant = p;
   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Water Destructor
//------------------------------------------------------------------------------------------------
Water::~Water()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Water::doRegistrations(void)
   {
   scienceAPI.expose("WaterSD",           "()", "Water Supply/Demand ratio",                       false, sdRatio);
   scienceAPI.expose("sw_supply_sum",     "mm", "Accumulative soil water supply over the profile", false, AccTotalSupply);
   scienceAPI.expose("sw_supply",         "mm", "Daily soil water supply over the profile",        false, totalSupply);
   scienceAPI.expose("sw_demand",         "mm", "Total crop demand for water",                     false, swDemand);
   scienceAPI.expose("transpiration",     "mm", "Daily water uptake from all rooted soil layers",  false, dltUptake);
   scienceAPI.expose("transpiration_tot", "mm", "Accumulative water uptake from the whole profile",false, totalUptake);
   scienceAPI.expose("cep",               "mm", "Accumulative water uptake from the whole profile",false, totalUptake);
   scienceAPI.expose("swdef_photo",       "",   "Water stress factor for photosynthesis",          false, photoStress);
   scienceAPI.expose("swdef_pheno",       "",   "Water stress factor for phenology",               false, phenoStress);
   scienceAPI.expose("swdef_expan",       "",   "Water stress factor for leaf expansion growth",   false, expansionStress);
   scienceAPI.expose("esw_profile",       "",   "Plant extractable water over the whole profile",  false, eswTot);
   scienceAPI.expose("ep",                "",   "Water uptake from the whole profile",             false, ep);
   scienceAPI.expose("ll",                "",   "Crop lower limit",                                false, ll);

   scienceAPI.exposeFunction("esw_layer", "mm", "Plant extractable soil water in each layer",
      FloatArrayFunction(&Water::getEswLayers));
   scienceAPI.exposeFunction("sw_deficit", "mm", "Soil water deficit below dul (dul - sw)",
      FloatArrayFunction(&Water::getSwDefLayers));
   scienceAPI.exposeFunction("sw_uptake", "mm", "Daily water uptake in each different rooted layers",
      FloatArrayFunction(&Water::getSwUptakeLayers));
   scienceAPI.exposeFunction("ll_dep", "mm", "Crop lower limit",
      FloatArrayFunction(&Water::getllDep));

   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Water::initialize(void)
   {
   photoStress = 1;phenoStress = 1;expansionStress = 1;
   currentLayer = 0;
   lastLayerPropn = 0;
   sdRatio = 1;
   rootDepth = 0;
   totalUptake = 0.0;
   AccTotalSupply = 0.0;
   totalSupply = 0.0;
   ep = 0.0;
   swDemand = 0.0;

   //Init Accumulation Vars
   phenoStressTotal.assign(nStages,0.0);
   photoStressTotal.assign(nStages,0.0);
   expanStressTotal.assign(nStages,0.0);

   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
double Water::calcSwDefEarExpansion(void)
  {
  double sdRatio = divide(totalSupply,swDemand,10.0);
  return swEarExpansionTable.value(sdRatio);
  }
//------------------------------------------------------------------------------------------------
//------ read Water parameters
//------------------------------------------------------------------------------------------------
void Water::readParams (void)
   {
   swPhenoTable.read(    scienceAPI,"x_sw_avail_ratio", "y_swdef_pheno");
   swExpansionTable.read(scienceAPI,"x_sw_demand_ratio","y_swdef_leaf");
   swSilkExpansionTable.read(scienceAPI,"x_silk_sw_demand_ratio","y_swdef_leaf");

   scienceAPI.read("kl", "", 0, kl);
   scienceAPI.read("xf", "", 0, xf);
   scienceAPI.read("ll","mm/mm", 0, ll);
   swEarExpansionTable.read(scienceAPI,"x_sw_ear_demand_ratio","y_swdef_ear");

   if (ll.size() != (unsigned int)nLayers)
      {
      string msg = "Number of soil layers (";
      msg += itoa(nLayers) ;
      msg += ") doesn't match ll parameter (";
      msg += itoa(ll.size());
      msg += ").";
      throw std::runtime_error(msg);
      }

   llDep.clear();
   eswCap.clear();
   for(int layer = 0; layer < nLayers; layer++)
      {
      llDep.push_back(ll[layer]*dLayer[layer]);
      eswCap.push_back(dulDep[layer] - llDep[layer]);
      }

   // report
   char msg[100];
   sprintf(msg,"\n");   scienceAPI.write(msg);
   sprintf(msg,"\n");   scienceAPI.write(msg);
   sprintf(msg,"                       Root Profile\n");                     scienceAPI.write(msg);
   sprintf(msg,"    ---------------------------------------------------\n"); scienceAPI.write(msg);
   sprintf(msg,"         Layer       Kl           Lower    Exploration\n");  scienceAPI.write(msg);
   sprintf(msg,"         Depth     Factor         Limit      Factor\n");     scienceAPI.write(msg);
   sprintf(msg,"         (mm)         ()        (mm/mm)       (0-1)\n");     scienceAPI.write(msg);
   sprintf(msg,"    ---------------------------------------------------\n"); scienceAPI.write(msg);

   for (int layer = 0; layer < nLayers; layer++)
      {
      sprintf (msg, "    %9.1f%10.3f%15.3f%12.3f\n", dLayer[layer],kl[layer],
         ll[layer],xf[layer]);
      scienceAPI.write(msg);
      }
   scienceAPI.write("    ---------------------------------------------------\n");
   scienceAPI.write("\n");

   }
//------------------------------------------------------------------------------------------------
//------ update Water parameters
//------------------------------------------------------------------------------------------------
void Water::updateVars(void)
   {
   if(swDemand < 0.001)sdRatio = 1.0;
   else sdRatio = Min(divide(totalSupply, swDemand),1.0f);
   rootDepth = plant->roots->getRootDepth();
   currentLayer = findIndex(rootDepth, dLayer);
   setOtherVariables ();

   ep = -sumVector(dltSwDep);
   for(int i = 0; i < nLayers; i++)
      {
      swUptake[i] = -dltSwDep[i];
      swDef[i] = dulDep[i] - swDep[i];
      dltSwDep[i] = 0.0;
      }
   eswTot = sumVector(esw);

   }
//------------------------------------------------------------------------------------------------
//-------- Get Water variables from other modules
//------------------------------------------------------------------------------------------------
void Water::getOtherVariables (void)
   {
   // get sw from Soilwat2
   scienceAPI.get("sw_dep", "mm", false, swDep, 0.0, 1000.0);

   for (int i = 0; i < nLayers; i++)
      {
      esw[i] = Max(0.0,swDep[i] - llDep[i]);
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Set Water variables in other modules
//------------------------------------------------------------------------------------------------
void Water::setOtherVariables (void)
   {
   scienceAPI.set("dlt_sw_dep", "mm", dltSwDep);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Water::onNewProfile(NewProfileType &v /* message */)
   {
   FVecToDVec(&dLayer,v.dlayer);
   FVecToDVec(&ll15Dep,v.ll15_dep);
   FVecToDVec(&dulDep,v.dul_dep);
   FVecToDVec(&satDep,v.sat_dep);
   FVecToDVec(&swDep,v.sw_dep);
   FVecToDVec(&bd,v.bd);

   // dlayer may be changed from its last setting due to erosion
   profileDepth = (double)(sumVector(dLayer));      // depth of soil profile (mm)
   nLayers = dLayer.size();

   //Reset the working vectors
   available.clear();
   availablePot.clear();
   supply.clear();
   swUptake.clear();
   swDep.clear();
   esw.clear();
   swDef.clear();
   dltSwDep.clear();
   for(int i = 0; i < nLayers; i++)
      {
      available.push_back(0.0);
      availablePot.push_back(0.0);
      supply.push_back(0.0);
      swUptake.push_back(0.0);
      swDep.push_back(0.0);
      esw.push_back(0.0);
      swDef.push_back(0.0);
      dltSwDep.push_back(0.0);
      }

   /* TODO : Insert new root profile and llDep code for change in profile due to erosion */
   /* TODO : Check validity of ll,dul etc as in crop_check_sw */
   /* TODO : why does this not include no3 */
   }
//------------------------------------------------------------------------------------------------
void Water::process(void)
   {
   double maxSupply = -1.0;    //flux limited suppy -- Slow Wilting
   plant->leaf->calcSWSupplyDemand(swDemand,maxSupply);
   getOtherVariables ();
   calcDailySupply(maxSupply);
   if (swDemand > totalSupply)
      {
      plant->leaf->balanceWater(swDemand,totalSupply);
      }
   calcStresses();
   calcUptake();
   }
//------------------------------------------------------------------------------------------------
double Water::swAvailRatio(int currentLayer)
   {
   return  divide (esw[currentLayer],eswCap[currentLayer], 10.0);
   }
//------------------------------------------------------------------------------------------------
//--------------- Plant transpiration and soil water extraction
//-----------     Calculate daily water demand - called from plant->prepare
//------------------------------------------------------------------------------------------------
//NOT USED
double Water::calcDemand(double &swSupply)
   {
   //swDemand = divide (plant->biomass->getDltDMPotRUE(),plant->getTranspEff());
   //return swDemand;
   return 1;
   }
//------------------------------------------------------------------------------------------------
//-----------     Calculate daily water supply - called from plant->process
//------------------------------------------------------------------------------------------------
void Water::calcDailySupply(double maxSupply)
   {
   calcAvailable();
   calcAvailablePot();
   calcSupply(maxSupply);
   calcProfileSwAvailRatio();
   }
//------------------------------------------------------------------------------------------------
//-----------  calculate daily water stresses - called from plant->process
//------------------------------------------------------------------------------------------------
void Water::calcStresses(void)
   {
   photoStress = calcSwDefPhoto();
   phenoStress = calcSwDefPheno();
   expansionStress = calcSwDefExpansion();
   earExpansionStress = calcSwDefEarExpansion();
   accumulate(photoStress, photoStressTotal, plant->phenology->currentStage(), plant->phenology->getDltStage());
   accumulate(phenoStress, phenoStressTotal, plant->phenology->currentStage(), plant->phenology->getDltStage());
   accumulate(expansionStress, expanStressTotal, plant->phenology->currentStage(), plant->phenology->getDltStage());
   }
//------------------------------------------------------------------------------------------------
void Water::calcAvailable(void)
   {
   for(int layer = 0;layer <= currentLayer;layer++)
      {
      available[layer] = Max(swDep[layer] - llDep[layer],0.0);
      }
   available[currentLayer] *= layerProportion();
   totalAvail = sumVector(available);
   }
//------------------------------------------------------------------------------------------------
double Water::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   double layerTop    = sumVector(dLayer, currentLayer);
   double layerBottom = sumVector(dLayer, currentLayer+1);

   return Min(divide(rootDepth - layerTop,layerBottom - layerTop),1.0);
   }
//------------------------------------------------------------------------------------------------
void Water::calcAvailablePot(void)
   {
   for(int layer = 0;layer <= currentLayer;layer++)
      {
      availablePot[layer] = Max(dulDep[layer] - llDep[layer],0.0);
      }
   availablePot[currentLayer] *= layerProportion();
   totalAvailPot = sumVector(availablePot);
   }
//------------------------------------------------------------------------------------------------
void Water::calcSupply(double maxSupply)
   {
   /*Return potential water uptake from each layer of the soil profile
   by the crop (mm water). Row Spacing and configuration (skip) are used
   to calculate semicircular root front to give proportion of the
   layer occupied by the roots. This fraction is applied to the supply */

   for(int layer = 0;layer <= currentLayer;layer++)
      {
      double prop = plant->roots->RootProportionInLayer(layer);
      supply[layer] = Max(available[layer] * kl[layer] *  prop,0.0);
      }
   double totalWaterSupply = sumVector(supply);
   if (maxSupply >= 0) // Limited by flux
      {
      if(totalWaterSupply > maxSupply)
         {
         double factor = maxSupply / totalWaterSupply;
         for(int layer = 0;layer <= currentLayer;layer++)
            {
            supply[layer] *= factor;
            }
         }
      }
   totalSupply = Max(0.0,sumVector(supply));
   AccTotalSupply += totalSupply;
   }
//------------------------------------------------------------------------------------------------
//-------- Calculate the daily crop water stresses
//------------------------------------------------------------------------------------------------
double Water::calcSwDefPhoto(void)
   {
   return bound(divide(totalSupply,swDemand,1.0),0.0,1.0);
   }
//------------------------------------------------------------------------------------------------
double Water::calcSwDefPheno(void)
   {
   SwAvailRatio = divide(totalAvail,totalAvailPot,1.0);
   SwAvailRatio = bound(SwAvailRatio,0.0,1.0);
   return swPhenoTable.value(SwAvailRatio);
   }
//------------------------------------------------------------------------------------------------
double Water::calcSwDefExpansion(void)
   {
   double sdRatio = divide(totalSupply,swDemand,10.0);
   return swExpansionTable.value(sdRatio);
   }
//------------------------------------------------------------------------------------------------
double Water::calcSilkSwDefExpansion(void)
   {
   double sdRatio = divide(totalSupply,swDemand,10.0);
   return swSilkExpansionTable.value(sdRatio);
   }
//------------------------------------------------------------------------------------------------
//-------- Calculate the daily uptake
//------------------------------------------------------------------------------------------------
void Water::calcUptake(void)
   {
   //we have no uptake if there is no demand or potential
   if(totalSupply <= 0.0 || swDemand <= 0.0)
      {
      return;
      }
   // if demand is less than roots could take up. water is non-limiting.
   // distribute demand proportionately in all layers.
   if(swDemand < totalSupply)
      {
      for(int layer = 0; layer <= currentLayer; layer++)
         {
         dltSwDep[layer] = -1 * divide (supply[layer],totalSupply, 0.0) * swDemand;
         }
      }
   // water is limiting - not enough to meet demand so take what is available
   else
      {
      for(int layer = 0; layer <= currentLayer; layer++)
         {
         dltSwDep[layer] = -1 * supply[layer];
         }
      }
   totalUptake += sumVector(dltSwDep) * -1;
   }
//------------------------------------------------------------------------------------------------
double Water::calcPeswSeed(void)
   {
   return divide(esw[currentLayer],dLayer[currentLayer],0.0);
   }
//------------------------------------------------------------------------------------------------
double Water::swAvailFracLayer(int layer)
   {
   return divide(available[layer],availablePot[layer]);
   }
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
void Water::getEswLayers(vector<float> &result)
   {
   DVecToFVec(result,esw);
   }
//------------------------------------------------------------------------------------------------
void Water::getSwDefLayers(vector<float> &result)
   {
   DVecToFVec(result,swDef);
   }
//------------------------------------------------------------------------------------------------
void Water::getSwUptakeLayers(vector<float> &result)
   {
   DVecToFVec(result, swUptake);
   }
//------------------------------------------------------------------------------------------------
void Water::getllDep(vector<float> &result)
   {
   DVecToFVec(result,llDep);
   }
//------------------------------------------------------------------------------------------------
double Water::sumPhotoStressTotal(int from, int to)
   {
   return sumVector(photoStressTotal,from,to);
   }
//------------------------------------------------------------------------------------------------
double Water::sumExpanStressTotal(int from, int to)
   {
   return sumVector(expanStressTotal,from,to);
   }
//------------------------------------------------------------------------------------------------
void Water::calcProfileSwAvailRatio(void)                    // total ftsw over profile
   {
   vector<double> avail;
   vector<double> maximum;
   for(unsigned layer = 0;layer < swDep.size();layer++)
      {
      avail.push_back(Max(swDep[layer] - llDep[layer],0.0));
      maximum.push_back(Max(dulDep[layer] - llDep[layer],0.0));
      }
   profileSwAvailRatio = divide(sumVector(avail),sumVector(maximum));
   }
//------------------------------------------------------------------------------------------------