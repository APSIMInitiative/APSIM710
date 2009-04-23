#include "StdPlant.h"

#include "GenericArbitrator.h"
#include "Phenology/Phenology.h"

GenericArbitrator::GenericArbitrator(ScienceAPI& scienceAPI, plantInterface& p)
   : Arbitrator(scienceAPI, p)
   {
   scienceAPI.read("ratio_root_shoot", ratio_root_shoot, 0.0f, 1000.0f);

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

float GenericArbitrator::ratioRootShoot()
   {
   return plant.phenology().doLookup(ratio_root_shoot);
   }

float GenericArbitrator::fracDMRemainingInPart(int partNumber)
   {
   return plant.phenology().doLookup(Fracs[partNumber]);
   }

