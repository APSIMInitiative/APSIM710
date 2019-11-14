//------------------------------------------------------------------------------------------------
#include "Plant.h"
#include "Stem.h"
using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ Stem Constructor
//------------------------------------------------------------------------------------------------
Stem::Stem(ScienceAPI2 &api, Plant *p) : PlantPart(api) 
   {
   plant = p;
   name = "Stem";
   partNo = 2;

   initialize();
   doRegistrations();
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Stem::doRegistrations(void)
   {
   scienceAPI.expose("StemGreenWt",    "g/m^2", "Stem dry weight",           false, dmGreen);
   scienceAPI.expose("StemGreenN",     "g/m^2", "N in stem",                 false, nGreen);
   scienceAPI.expose("StemGreenNConc", "%",     "Live stem N concentration", false, nConc);
   scienceAPI.expose("DeltaStemGreenN","g/m^2", "Today's N increase in stem",false, dltNGreen);
   scienceAPI.expose("StemGreenNConc", "%",     "Live stem N concentration", false, nConc);
   scienceAPI.expose("StemGreenP",     "g/m^2" ,"P in live Stem",            false, pGreen);
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Stem::initialize(void)
   {
   canopyHeight    = 0.0;
   dmGreenStem     = 0.0;
   dltCanopyHeight = 0.0;
	dmPlantMax = 9999;

   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Stem parameters
//------------------------------------------------------------------------------------------------
void Stem::readParams (void)
   {
   heightFn.read(scienceAPI,"x_stem_wt","y_height");
   scienceAPI.read("dm_stem_init",   "", 0, initialDM);
   scienceAPI.read("stem_trans_frac","", 0, translocFrac);
   scienceAPI.read("retransRate",    "", 0, retransRate);
   // nitrogen
   scienceAPI.read("initialStemNConc", "", 0, initialNConc);
   targetNFn.read (scienceAPI,"x_stem_n","targetStemNConc");
   structNFn.read (scienceAPI,"x_stem_n","structStemNConc");

   scienceAPI.read("stemDilnNSlope","", 0, dilnNSlope);
   scienceAPI.read("stemDilnNInt",  "", 0, dilnNInt);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_stem");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_stem");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_stem");
   scienceAPI.read("p_conc_init_stem", "", 0, initialPConc);

   density = plant->getPlantDensity();

	dmPlantMax = 9999;
	scienceAPI.read("PGRt1", "", 0, dmPlantMaxTT);

   }

//------------------------------------------------------------------------------------------------
void Stem::process(void)
   {
   calcCanopyHeight();
   }
//------------------------------------------------------------------------------------------------
//------ update Stem variables
//------------------------------------------------------------------------------------------------
void Stem::updateVars(void)
   {
   double dayNConc = divide(nGreen,dmGreen,0);
   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;

   dmGreenStem = dmGreen / density;

   canopyHeight += dltCanopyHeight;
   nGreen += (dltNGreen +  dltNRetranslocate);
   nConc = divide(nGreen,dmGreen,0) * 100;
   dltNConc = dayNConc - nConc;

   stage = plant->phenology->currentStage();
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Stem::phenologyEvent(int iStage)
   {
   ExternalMassFlowType EMF;
	double dmPlantStem;
   switch (iStage)
      {
   case emergence :
      dmGreen = initialDM * density;
      nGreen = initialNConc * dmGreen;
      pGreen = initialPConc * dmGreen;
      EMF.PoolClass = "crop";
      EMF.FlowType = "gain";
      EMF.DM = 0.0;
      EMF.N  = (float)(nGreen * gm2kg/sm2ha);
      EMF.P  = (float)(pGreen * gm2kg/sm2ha);
      EMF.C = 0.0; // ?????
      EMF.SW = 0.0;
      scienceAPI.publish("ExternalMassFlow", EMF);
      break;
   case flowering :
      //set the minimum weight of stem; used for translocation to grain and stem
      dmPlantStem = divide (dmGreen, density);
      dmPlantMin = dmPlantStem * (1.0 - translocFrac);
		
      break;
      }
   }
//------------------------------------------------------------------------------------------------
void Stem::calcCanopyHeight(void)
   {
   double newHeight = heightFn.value(dmGreenStem);
   dltCanopyHeight = Max(0.0,newHeight - canopyHeight);
   }
//------------------------------------------------------------------------------------------------
double Stem::calcNDemand(void)
   {
   nDemand = 0.0;
   // STEM demand (g/m2) to keep stem [N] at levels from  targetStemNConc
   double nRequired = (dmGreen + dltDmGreen) * targetNFn.value(stage);
   nDemand = Max(nRequired - nGreen,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
double Stem::calcStructNDemand(void)
   {
   // calculate N required to maintain structural [N]
   double structNDemand = 0.0;
   if(stage >= startGrainFill)return structNDemand;

   // STEM demand to keep stem [N] at levels of structStemNConc
   double nRequired = (dmGreen + dltDmGreen) * structNFn.value(stage);
   structNDemand = Max(nRequired - nGreen,0.0);
   return structNDemand;
   }
//------------------------------------------------------------------------------------------------

double Stem::provideN(double requiredN)
   {
   // calculate the N available for translocation to other plant parts
   // N could be required for structural Stem/Rachis N, new leaf N or grain N
   // Stem N is availavle at a rate which is a function of stem [N]
   // dltStemNconc per dd  = 0.0062 * stemNconcPct - 0.001
   // cannot take below Structural stem [N]% 0.5

   double nProvided;

   double stemNConcPct = divide((nGreen), (dmGreen + dltDmGreen)) * 100;
   if (stemNConcPct < structNFn.value(stage) * 100)
	   return 0;

   if(dltNGreen > requiredN)
      {
      nProvided = requiredN;
      dltNRetranslocate -= nProvided;
      return nProvided;
      }
   else
      {
      nProvided = dltNGreen;
      requiredN -= nProvided;
      }

   double dltStemNconc = (dilnNSlope * (stemNConcPct) + dilnNInt)
      * plant->phenology->getDltTT();

   double availableN = (dltStemNconc) / 100 * (dmGreen + dltDmGreen);
   // cannot take below structural N
   double structN =  (dmGreen + dltDmGreen) * structNFn.value(stage);
   availableN = Min(availableN,(nGreen) - structN);

   availableN = Max(availableN,0.0);

   nProvided += Min(availableN,requiredN);
   dltNRetranslocate -= nProvided;
   return nProvided;
   }
//------------------------------------------------------------------------------------------------
double Stem::dmRetransAvailable(void)
   {
   // calculate dry matter available for translocation to grain
   double stemWt = dmGreen + dltDmGreen;
   double stemWtAvail = (stemWt - (dmPlantMin * density)) * retransRate;
   return Max(stemWtAvail,0.0);
   }
//------------------------------------------------------------------------------------------------
double  Stem::partitionDM(double dltDM)
	{
	// calculate maximum stem size at flowering + dmPlantMaxTT
	double ttNow = plant->phenology->sumTTtotal(sowing,maturity);

	if(dmPlantMax >  9990 && ttNow > dmPlantMaxTT + plant->phenology->sumTTtotal  (sowing,flowering))		// not yet calculated - do once
		dmPlantMax = dmGreen;		// maximum stem size /m2

	double dmAvailable =  Min(dmPlantMax - dmGreen , dltDM);
	dltDmGreen = Max(dmAvailable,0.0);
	return dltDmGreen;
	}
//------------------------------------------------------------------------------------------------
double Stem::calcPDemand(void)
   {
   // Leaf P demand
   double rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
      plant->biomass->getAboveGroundBiomass(),0.0);

   double deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------

