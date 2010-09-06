#include "../StdPlant.h"
#include "WholePlantSeasonalPpArbitrator.h"
#include "../CompositePart.h"
#include "../Environment.h"
#include "../Phenology/Phenology.h"

WholePlantSeasonalPpArbitrator::WholePlantSeasonalPpArbitrator(ScienceAPI& scienceAPI, plantInterface& p)
   : Arbitrator(scienceAPI, p), plant(p)
   {
    ratioRootPlantTable.read(scienceAPI                                     //Plant and Food Research implementation
                        , "x_ratio_root_plant_pp_", "()", 5.0, 17.0                      // Photoperiod
                        , "y_ratio_root_plant", "()", 0.0, 1.0);       // partitioning ratio of root to total plant (fractional)

    scienceAPI.read("ratio_root_plant_pp_scaling_factor", ratioRootPlantScalingFactor, 0.0f, 1.0f);
       
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

float WholePlantSeasonalPpArbitrator::TotalPotentialGrowthRate(void)
   {
   // whole of plant
   return plant.All().dltDmPotRue();
   }

float WholePlantSeasonalPpArbitrator::ratioRootPlant(void)
   {
   float photoperiod = plant.environment().dayLength();
   float ratioRootPlantSeasonalAdjustment =  plant.environment().deltaDayLength() * ratioRootPlantScalingFactor;
   return ratioRootPlantTable.value(photoperiod) - ratioRootPlantSeasonalAdjustment;
   }

float WholePlantSeasonalPpArbitrator::fracDMRemainingInPart(int partNumber)
   {
   return plant.phenology().doLookup(Fracs[partNumber]);
   }

