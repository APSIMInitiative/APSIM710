#include "../StdPlant.h"

#include "WaitingPhase.h"

WaitingPhase::WaitingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
   : Phase (scienceAPI, p, stage_name)
   {
   target = 10000000;
   }
