#include "StdPlant.h"

#include "Fixation.h"
#include "Phenology/Phenology.h"
Fixation::Fixation(ScienceAPI& api, plantInterface& p, const std::string& name)
//===========================================================================
   : plantThing(api, name), plant(p)
   {
   scienceAPI.read("n_fix_rate", n_fix_rate, 0.0f, 1.0f);
   scienceAPI.expose("dlt_n_fixed_pot", "g/m^2", "potential N fixation", n_fix_pot);
   }

float Fixation::Potential(float biomass, float swdef_fixation)
//===========================================================================
   {
   n_fix_pot = plant.phenology().doLookup(n_fix_rate) * biomass * swdef_fixation;
   return n_fix_pot;
   }
