//------------------------------------------------------------------------------------------------
#include "Rachis.h"
#include "Plant.h"

//------------------------------------------------------------------------------------------------
//------ Rachis Constructor
//------------------------------------------------------------------------------------------------
Rachis::Rachis(ScienceAPI2 &api, Plant *p) : PlantPart(api)
   {
   plant = p;
   name = "Rachis";
   partNo = 3;

   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Rachis Destructor
//------------------------------------------------------------------------------------------------
Rachis::~Rachis()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Rachis::doRegistrations(void)
   {
   scienceAPI.expose("RachisGreenWt",    "g/m^2", "Live rachis dry weight", false, dmGreen);
   scienceAPI.expose("RachisGreenN",     "g/m^2", "N in rachis",            false, nGreen);
   scienceAPI.expose("RachisGreenNConc", "%",     "Flower N concentration", false, nConc);
   scienceAPI.expose("RachisGreenP",     "g/m^2" ,"P in live rachis",       false, pGreen);
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Rachis::initialize(void)
   {
   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Rachis parameters
//------------------------------------------------------------------------------------------------
void Rachis::readParams (void)
   {
   // nitrogen
   scienceAPI.read("initialRachisNConc","", 0, initialNConc);
   scienceAPI.read("targetRachisNConc" ,"", 0, targetNConc);
   scienceAPI.read("structRachisNConc" ,"", 0, structRachisNConc );
   scienceAPI.read("rachisDilnNSlope","", 0, dilnNSlope);
   scienceAPI.read("rachisDilnNInt","",   0, dilnNInt);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_flower");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_flower");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_flower");
   scienceAPI.read("p_conc_init_flower", "", 0, initialPConc);
   }

//------------------------------------------------------------------------------------------------
//------ read Rachis parameters
//------------------------------------------------------------------------------------------------
void Rachis::updateVars(void)
   {
   dmGreen += dltDmGreen;
   nGreen  += (dltNGreen + dltNRetranslocate);
   nConc    = divide(nGreen,dmGreen,0);
   stage    = plant->phenology->currentStage();
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Rachis::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
      case emergence :
         dmGreen = 0.0;
         nGreen = initialNConc * dmGreen;
         pGreen = initialPConc * dmGreen;
         break;
      }
   }
//------------------------------------------------------------------------------------------------
//------- nitrogen
//------------------------------------------------------------------------------------------------
float Rachis::calcNDemand(void)
   {
   nDemand = 0.0;
   if(stage >= startGrainFill)return nDemand;

   // RACHIS N demand (g/m2) to keep rachis [N] at targetRachisNConc
   float nRequired = (dmGreen + dltDmGreen) * targetNConc;
   nDemand = Max(nRequired - nGreen,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
float Rachis::calcStructNDemand(void)
   {
   // calculate N required to maintain structural [N]
   float structNDemand = 0.0;
   if(stage >= startGrainFill)return structNDemand;

   // RACHIS demand to keep rachis [N] at levels of structRachisNConc
   float nRequired = (dmGreen + dltDmGreen) * structRachisNConc;
   structNDemand = Max(nRequired - nGreen,0.0);
   return structNDemand;
   }
//------------------------------------------------------------------------------------------------
float Rachis::provideN(float requiredN)
   {
   // calculate the N available for translocation to other plant parts
   // N could be required for structural Stem/Rachis N, new leaf N or grain N
   // Rachis N is available at a rate which is a function of rachis [N]
   // dltRachisNconc per day (17dd) = 0.076*rachisNconcPct - 0.0199

   
   float rachisNConcPct = divide((nGreen + dltNGreen),(dmGreen + dltDmGreen)) * 100;
   if(rachisNConcPct < structRachisNConc * 100)return 0;
   float NConcPctAvailable = (dilnNSlope * rachisNConcPct + dilnNInt)
                           * plant->phenology->getDltTT();
   float availableN = NConcPctAvailable / 100 * (dmGreen + dltDmGreen);
   availableN = Max(availableN,0.0);
   
   float nProvided = Min(availableN,requiredN);
   dltNRetranslocate -= nProvided;
   return nProvided;
   }
//------------------------------------------------------------------------------------------------
float Rachis::partitionDM(float dltDM)
   {
   dltDmGreen = dltDM;
   return dltDmGreen;
   }
//------------------------------------------------------------------------------------------------
float Rachis::calcPDemand(void)
   {
   // Leaf P demand

   float rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
         plant->biomass->getAboveGroundBiomass(),0.0);

   float deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------

