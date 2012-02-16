#include "StdPlant.h"

#include "WaitingPhase.h"

WaitingPhase::WaitingPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
   : Phase (scienceAPI, p, StartStageName, EndStageName)
   {
   target = 10000000;
   }
