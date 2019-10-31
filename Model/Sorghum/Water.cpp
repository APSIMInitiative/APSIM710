//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include <vector>
#include <General/string_functions.h>

#include "Plant.h"
#include "Water.h"
using namespace Sorghum;
//------------------------------------------------------------------------------------------------
//------ Water Constructor
//------------------------------------------------------------------------------------------------
Water::Water(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
   {
   nLayers = 0;
   totalAvail = 0.0;
   totalAvailPot = 0.0;
   totalSupply = 0.0;
   dltUptake = 0.0;
   eswTot = 0.0;

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
   scienceAPI.expose("ll",                "",   "Crop lower limit",                                true,  ll);
   scienceAPI.expose("xf",                "",   "Exploration factor",                              true,  xf);
   scienceAPI.expose("kl",                "",   "kl factor",                                       true,  kl);
   scienceAPI.expose("avail",             "",	"Available SW by layer",						   false, available);
   scienceAPI.expose("totalAvail",        "",	"Total available SW",							   false, totalAvail);
   scienceAPI.expose("availablePot",      "",	"Potential available SW by layer",				   false, availablePot);
   scienceAPI.expose("totalAvailablePot", "",	"Total potential available SW",					   false, totalAvailPot);
   scienceAPI.expose("sorgh_esw",		  "",	"ESW as used in sorghum root growth calcs",		   false, sorghEsw);
   scienceAPI.expose("sorgh_esw_cap",	  "",	"esw_cap as used in sorghum root growth calcs",	   false, sorghEswCap);
   scienceAPI.expose("sorgh_sw_dep",	  "",	"sw_dep as used in sorghum root growth calcs",	   false, swDep);

   scienceAPI.exposeFunction("esw_layer", "mm", "Plant extractable soil water in each layer",
                    FloatArrayFunction(&Water::getEswLayers));
   scienceAPI.exposeFunction("sw_deficit", "mm", "Soil water deficit below dul (dul - sw)",
                    FloatArrayFunction(&Water::getSwDefLayers));
   scienceAPI.exposeFunction("sw_uptake", "mm", "Daily water uptake in each different rooted layers",
                    FloatArrayFunction(&Water::getSwUptakeLayers));
   scienceAPI.exposeFunction("ll_dep", "mm", "Crop lower limit",
                    FloatArrayFunction(&Water::getllDep));

   scienceAPI.expose("StressTrace",                "",   "Water Stress values",                                false, avSD);
   scienceAPI.expose("EnvType",                "",   "Environment type",                                false, EnvType);
	
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
   sorghEsw = 0;
   sorghEswCap = 0;
   AccTotalSupply = 0.0;
   ep = 0.0;
   swDemand = 0.0;

   //Init Accumulation Vars
   phenoStressTotal.assign(nStages,0.0);
   photoStressTotal.assign(nStages,0.0);
   expanStressTotal.assign(nStages,0.0);

	// init WaterSD variables
	dailyWaterSD.clear();
	dailyTT.clear();
	avSD.assign(9,0.0);
	EnvType = 1;

   }
//------------------------------------------------------------------------------------------------
//------ read Water parameters
//------------------------------------------------------------------------------------------------
void Water::readParams (void)
   {
   swPhenoTable.read(    scienceAPI,"x_sw_avail_ratio", "y_swdef_pheno");
   swExpansionTable.read(scienceAPI,"x_sw_demand_ratio","y_swdef_leaf");

   vector<float> ll15, dul;
   
   scienceAPI.read("kl", "", 0, kl);
   scienceAPI.read("xf", "", 0, xf);
   scienceAPI.read("ll","mm/mm", 0, ll);
   scienceAPI.get("ll15","mm/mm", 0, ll15);
   scienceAPI.get("dul","mm/mm", 0, dul);

   if (ll.size() != (unsigned int)nLayers)
      {
      string msg = "Number of soil layers (";
      msg += itoa(nLayers) ;
      msg += ") doesn't match ll parameter (";
      msg += itoa(ll.size());
      msg += ").";
      throw std::runtime_error(msg);
      }
      
   // Bound check LL
   for (unsigned int layer = 0; layer != ll.size(); layer++)
      checkRange(scienceAPI, ll[layer], ll15[layer], dul[layer], "CLL");

   llDep.clear();
   eswCap.clear();
   for(int layer = 0; layer < nLayers; layer++)
      {
      llDep.push_back(ll[layer]*dLayer[layer]);
      eswCap.push_back(dulDep[layer] - llDep[layer]);
      }
   rootDepth = plant->roots->getRootDepth();
   currentLayer = findIndex(rootDepth, dLayer);
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
	if (swDemand < 0.001) {
		sdRatio = 1.0;
	}
	else {
		sdRatio = Min(divide(totalSupply, swDemand), 1.0f);
	}
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

	calcStressTrace();

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
   FVecToDVec(&dLayer, v.dlayer);
   FVecToDVec(&ll15Dep, v.ll15_dep);
   FVecToDVec(&dulDep, v.dul_dep);
   FVecToDVec(&satDep, v.sat_dep);
   FVecToDVec(&swDep, v.sw_dep);
   FVecToDVec(&bd, v.bd);

   // dlayer may be changed from its last setting due to erosion
   profileDepth = sumVector(dLayer);      // depth of soil profile (mm)
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
   getOtherVariables ();
   calcDailySupply();
   calcStresses();
   calcUptake();
   }
//------------------------------------------------------------------------------------------------
double Water::swAvailRatio(int currentLayer)
   {
	sorghEsw = esw[currentLayer];
	sorghEswCap = eswCap[currentLayer];
	// dh - in new apsim, uptakes are immediately taken out of the soil.
   return  divide (esw[currentLayer] + dltSwDep[currentLayer],eswCap[currentLayer], 10.0);
   }
//------------------------------------------------------------------------------------------------
//--------------- Plant transpiration and soil water extraction
//-----------     Calculate daily water demand - called from plant->prepare
//------------------------------------------------------------------------------------------------
double Water::calcDemand(void)
   {
   swDemand = divide (plant->biomass->getDltDMPotRUE(),plant->getTranspEff());
   return swDemand;
   }
//------------------------------------------------------------------------------------------------
//-----------     Calculate daily water supply - called from plant->process
//------------------------------------------------------------------------------------------------
void Water::calcDailySupply(void)
   {
   calcAvailable();
   calcAvailablePot();
   calcSupply();
   }
//------------------------------------------------------------------------------------------------
//-----------  calculate daily water stresses - called from plant->process
//------------------------------------------------------------------------------------------------
void Water::calcStresses(void)
   {
   photoStress = calcSwDefPhoto();
   phenoStress = calcSwDefPheno();
   expansionStress = calcSwDefExpansion();
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
   double currentLayerProportion = layerProportion();
   available[currentLayer] *= currentLayerProportion;
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
void Water::calcSupply(void)
   {
   /*       Return potential water uptake from each layer of the soil profile
           by the crop (mm water). Row Spacing and configuration (skip) are used
           to calculate semicircular root front to give proportion of the
           layer occupied by the roots. This fraction is applied to the supply */

   for(int layer = 0;layer <= currentLayer;layer++)
      {
      double prop = plant->roots->RootProportionInLayer(layer);
      supply[layer] = Max(available[layer] * kl[layer] *  prop,0.0);
      }
   totalSupply = sumVector(supply);
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
   double swAvailRatio = divide(totalAvail,totalAvailPot,1.0);
   swAvailRatio = bound(swAvailRatio,0.0,1.0);
   return swPhenoTable.value(swAvailRatio);
   }
//------------------------------------------------------------------------------------------------
double Water::calcSwDefExpansion(void)
  {
  double sdRatio = divide(totalSupply,swDemand,10.0);
  return swExpansionTable.value(sdRatio);
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
  double tmpUptake = sumVector(dltSwDep) * -1;
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
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
void Water::getEswLayers(vector<float> &result)
   {
   DVecToFVec(result, esw);
   }
//------------------------------------------------------------------------------------------------
void Water::getSwDefLayers(vector<float> &result)
   {
   DVecToFVec(result, swDef);
   }
//------------------------------------------------------------------------------------------------
void Water::getSwUptakeLayers(vector<float> &result)
   {
   DVecToFVec(result, swUptake);
   }
//------------------------------------------------------------------------------------------------
void Water::getllDep(vector<float> &result)
   {
   DVecToFVec(result, llDep);
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
void Water::calcStressTrace(void)
	{
	// store daily for stress trace
	dailyWaterSD.push_back(sdRatio);
	dailyTT.push_back(plant->phenology->getDltTT());

	// if the stage is >= flowering, calculate the 9 average stresses up to the latest stress
	double currentStage = plant->phenology->currentStage();
	if(currentStage < flowering)return;

	static int daysToFlower = 0;

	if(currentStage == flowering)
		{
		daysToFlower = plant->das;
		// calculate the stresses up to flowering going back in 100oCdd
		double accTT = 0;
		double accStress = 0;
		int period = 4;int days = 0;
		for(int das = daysToFlower;das;das--)
			{
			accTT += dailyTT[das];
			accStress += dailyWaterSD[das];
			days++;
			if(accTT >= 100.0)	//end of one window - calculate average daily stress
				{
				avSD[period] = accStress / days;
				days = 0;accStress = 0;accTT = 0;
				period--;
				}
			if(period < 0)break;
			}
		}
	else
		{		// post anthesis
		double accTT = 0;
		double accStress = 0;
		int period = 5;int days = 0;
		for(unsigned int das = daysToFlower + 1;das < dailyTT.size();das++)
			{
			accTT += dailyTT[das];
			accStress += dailyWaterSD[das];
			days++;
			if(accTT >= 100.0)	//end of one window - calculate average daily stress forward in 100oCdd
				{
				avSD[period] = accStress / days;
				days = 0;accStress = 0;accTT = 0;
				period++;
				}
			if(period > 8)break;
			}
		}
	// if the 5 standard stress traces are available, select the closest stress trace
	EnvType = classify(avSD);
	
	}

int Water::classify(vector<double> stress)
   {
   // see which class is closest to this line bu adding up the squares of the distances
   // at each point
	   // Australian Sorghum. Used as default if centres.csv cannot be found
   double sdIndex[][9] = {
		{0.9994660,0.9877131,0.9627857,0.9479593,0.9497890,0.9526913,0.9543730,0.9702138,0.9717316},
		{0.9986755,0.9777828,0.9336676,0.9052058,0.9042635,0.9007016,0.8298966,0.6586733,0.5532561},
		{0.9926408,0.8824799,0.6839383,0.5238767,0.4114531,0.4135201,0.5787614,0.8622198,0.8936729},
		{0.9944262,0.9038232,0.6964455,0.4601622,0.3360969,0.3294746,0.3231550,0.2993245,0.4153366},
		{0.9981921,0.9688293,0.9035107,0.8408004,0.7362116,0.5745088,0.4196704,0.3369962,0.3818236}};


   vector<double> distances;
   for(int i=0;i < 5;i++)
      {
      double dist =0.0;
      for(int j=0;j < 9;j++)
         {
         dist += pow(sdIndex[i][j] - stress[j],2.0);
         }
      distances.push_back(dist);
      }

	
   return minIndx(distances) + 1;


   }

int Water::minIndx(vector<double> distances)
   {
   // return the index (0 - n) of the lowest value in the vector
   double minValue = 10e10;
   int indx = 0;
   for(unsigned i=0;i < distances.size(); i++)
      {
      if(distances[i] < minValue)
         {
         minValue = distances[i];
         indx = i;
         }
      }
   return indx;
   }