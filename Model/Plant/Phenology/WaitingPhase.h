#ifndef WaitingPhaseH
#define WaitingPhaseH

#include "Phase.h"
class WaitingPhase : public Phase
   {
   public:
      WaitingPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName);

   };


#endif

