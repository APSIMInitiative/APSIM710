#include "../StdPlant.h"
#include "WholePlantSeasonalArbitrator.h"
#include "../CompositePart.h"
#include "Environment.h"
#include "../Phenology/Phenology.h"


// Purpose: This arbitrator uses the ratio of soil to air temperature (Tsoil/Tair)
//          as a driver of fractional partitioning
//          of biomass to roots. Note that it must be used with values of
//          radiation use efficiency for "total biomass" in parameter file.
//          Plant and Food Research (PFR) implementation.

WholePlantSeasonalArbitrator::WholePlantSeasonalArbitrator(ScienceAPI& scienceAPI, plantInterface& p)
   : Arbitrator(scienceAPI, p)
   {

   ratioRootPlantTable.read(scienceAPI
         ,"x_tsoil_tair_partition", "()", 0.0, 5.0
         ,"y_tsoil_tair_root","()",0.0,1.0);

   for (unsigned i = 0; i != PartitionRules.size(); i++)
      {
      vector <float> empty;

      if (PartitionRules[i] == "frac")
         {
         vector <float> frac;
         scienceAPI.read("frac_"+PartitionParts[i], frac, 0.0, 1.0);
         Fracs.push_back(frac);
         }
      else
         Fracs.push_back(empty);
      }
   }

float WholePlantSeasonalArbitrator::TotalPotentialGrowthRate(void)
   {
   // whole of plant
   return plant.All().dltDmPotRue();
   }

float WholePlantSeasonalArbitrator::ratioRootPlant(void)
   {
   float TsoilTairRatio = divide (plant.environment().rootActivityTemperature(), plant.environment().meant(),0.0);
   return ratioRootPlantTable.value(TsoilTairRatio);
   }

float WholePlantSeasonalArbitrator::fracDMRemainingInPart(int partNumber)
   {
   return plant.phenology().doLookup(Fracs[partNumber]);
   }

