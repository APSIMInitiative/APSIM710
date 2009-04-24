#include "../StdPlant.h"

#include "WholePlantGenericArbitratorXY.h"
#include "../Phenology/Phenology.h"
#include "../CompositePart.h"

WholePlantGenericArbitratorXY::WholePlantGenericArbitratorXY(ScienceAPI& scienceAPI, plantInterface& p)
   : Arbitrator(scienceAPI, p)
   {

   for (unsigned i = 0; i != PartitionRules.size(); i++)
      {
      interpolationFunction empty;
      if (PartitionRules[i] == "frac")
         {
         interpolationFunction frac;
         frac.read(scienceAPI
                  ,"x_stage_no_partition", "()", 0.0, 100.0
                  ,"y_frac_"+PartitionParts[i],"()",0.0,1.0);
         Fracs.push_back(frac);
         }
      else
         Fracs.push_back(empty);
      }
   }


float WholePlantGenericArbitratorXY::fracDMRemainingInPart(int partNumber)
   {
   return plant.phenology().doInterpolation(Fracs[partNumber]);
   }

float WholePlantGenericArbitratorXY::TotalPotentialGrowthRate(void)
   {
   // whole of plant
   return plant.All().dltDmPotRue();
   }
