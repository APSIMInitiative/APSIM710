#include "StdPlant.h"

#include "CWInductivePhase.h"


void CWInductivePhase::read()
//=======================================================================================
   {
   CWVernalPhase::read();
   string key = "tt_"+name();
   scienceAPI.read(key, target, 0.0f, 1000000.0f);
   }


float CWInductivePhase::stress()
   {
   return min(plant.getSwDefPheno(), min(plant.getNFactPheno(), plant.getPFactPheno()))
          * min(vern_eff, photop_eff);
   }



