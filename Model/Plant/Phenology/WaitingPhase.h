#ifndef WaitingPhaseH
#define WaitingPhaseH

#include "Phase.h"
class WaitingPhase : public Phase
   {
   public:
      WaitingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name);

   };


#endif

