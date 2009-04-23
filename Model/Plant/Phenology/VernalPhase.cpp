#include "StdPlant.h"

#include "Environment.h"
#include "Phase.h"
#include "VernalPhase.h"
#include "Utility/Output.h"
#include "Utility/OutputVariable.h"
#include "Phenology.h"

void VernalPhase::reset()
//=======================================================================================
   {
   Phase::reset();
   cumvd = 0.0;
   dlt_cumvd = 0.0;
   scienceAPI.expose("cumvd", "", "Cumulative Vernal Days", cumvd);
   }

void VernalPhase::read()
//=======================================================================================
   {
   Phase::read();

   string key1 = "cumvd_"+name();
   string key2 = "tt_"+name();

   vernal_tt.read(scienceAPI,
                  key1.c_str(), "vd", 0.0, 100.0,
                  key2.c_str(), "dd", 0.0, 1e6);
   vernal_days.read(scienceAPI,
                      "x_vernal_temp", "(oc)", -10., 60.0,
                      "y_vernal_days", "(days)", 0.0, 1.0);
   }


void VernalPhase::updateTTTargets()
//=======================================================================================
   {
   if (plant.phenology().inPhase("vernalisation"))
      {
      dlt_cumvd = linint_3hrly_temp (plant.environment().maxt(), plant.environment().mint(), &vernal_days);
      cumvd = cumvd + dlt_cumvd;
      target = vernal_tt.value(cumvd);
      }
    }
string VernalPhase::description()
//=======================================================================================
   {
   return vernal_tt.description();
   }


