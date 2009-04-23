#ifndef SowingPhaseH
#define SowingPhaseH

#include "Phase.h"
class SowingPhase : public Phase
   {
   public:
      SowingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : Phase (scienceAPI, p, stage_name){};

      void calcPhaseDevelopment(int das,
                                float& dlt_tt_phenol, float& phase_devel);
   private:
      bool germinating();

   };


#endif

