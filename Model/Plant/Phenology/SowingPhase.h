#ifndef SowingPhaseH
#define SowingPhaseH

#include "Phase.h"
class SowingPhase : public Phase
   {
   public:
      SowingPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : Phase (scienceAPI, p, StartStageName, EndStageName){};

      void calcPhaseDevelopment(int das,
                                float& dlt_tt_phenol, float& phase_devel);
   private:
      bool germinating();

   };


#endif

