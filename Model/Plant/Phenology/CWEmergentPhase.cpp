#include "../StdPlant.h"

#include "CWEmergentPhase.h"

void CWEmergentPhase::read()
//=======================================================================================
   {
   CWVernalPhase::read();

   rel_emerg_rate.read(scienceAPI,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);

   }
void CWEmergentPhase::OnSow(float sowing_depth)
   {
   float shoot_lag, shoot_rate;
   scienceAPI.read("shoot_lag", shoot_lag, 0.0f, 100.0f);
   scienceAPI.read("shoot_rate", shoot_rate, 0.0f, 100.0f);
   target = shoot_lag + sowing_depth * shoot_rate;
   }
float CWEmergentPhase::stress()
   {
   return min(vern_eff, photop_eff) * rel_emerg_rate[plant.getFaswSeed()];
   }

