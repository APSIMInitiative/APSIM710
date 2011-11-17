#include "StdPlant.h"

#include "CWFixedPhase.h"
#include "../Root/RootBase.h"

void CWFixedPhase::read()
//=======================================================================================
   {
   CWVernalPhase::read();
   string key = "tt_"+name();
   scienceAPI.read(key, target, 0.0f, 1000000.0f);

   stressFunction.readOptional(scienceAPI, "x_sw_avail_ratio_" + name(), "()", 0.0, 1.0,
                                   "y_swdef_pheno_" + name(), "()", 0.0, 5.0);
   }

string CWFixedPhase::description()
//=======================================================================================
   {
   unsigned pad = max(0, 23 - (int)name().size());
   return "         tt_"+name()+string(pad, ' ') + " = "+ftoa(target, "7.0")+ " (dd)\n";
   }

void CWFixedPhase::calcPhaseDevelopment(int /*das*/,
                                      float& dlt_tt_phenol, float& phase_devel)
   {
   dlt_tt_phenol = TT() * stress();

   phase_devel = (float)divide(getTT() + dlt_tt_phenol, getTTTarget(), 1.0);
   }

float CWFixedPhase::stress()
//=========================================================================
// Get the soil water availability ratio in the root zone
// and calculate the 0 - 1 stress factor for phenology.
// 1 is no stress, 0 is full stress.
   {
   if (plant.root().swAvailablePotential() > 0.0 && stressFunction.isInitialised())
      {
      float sw_avail_ratio = (float)divide (plant.root().swAvailable(), plant.root().swAvailablePotential(), 1.0);
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0);
      return stressFunction.value(sw_avail_ratio);
      }
   else
      return 1.0;
   }


