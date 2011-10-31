#include "StdPlant.h"

#include "GenericArbitratorXY.h"
#include "../Phenology/Phenology.h"

GenericArbitratorXY::GenericArbitratorXY(ScienceAPI& scienceAPI, plantInterface& p)
   : Arbitrator(scienceAPI, p)
   {
   ratio_root_shoot.read (scienceAPI
                         ,"x_stage_no_partition", "()", 0.0, 100.0
                         ,"y_ratio_root_shoot","()",0.0,100.0);

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

float GenericArbitratorXY::ratioRootShoot()
   {
   return plant.phenology().doInterpolation(ratio_root_shoot);
   }

float GenericArbitratorXY::fracDMRemainingInPart(int partNumber)
   {
   return plant.phenology().doInterpolation(Fracs[partNumber]);
   }

