#include "StdPlant.h"

#include "InductivePhase.h"
#include "../Environment.h"

float InductivePhase::stress()
   {
   return min(plant.getSwDefPheno(), min(plant.getNFactPheno(), plant.getPFactPheno()));
   }


void InductivePhase::updateTTTargets()
//=======================================================================================
   {
   dlt_cumvd = vernal_days.value(plant.environment().meant());
   cumvd = cumvd + dlt_cumvd;
   target = vernal_tt.value(cumvd);
   }

