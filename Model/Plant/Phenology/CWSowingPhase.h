#ifndef CWSowingPhaseH
#define CWSowingPhaseH

#include "CWVernalPhase.h"
class CWSowingPhase : public CWVernalPhase
   {
   public:
      CWSowingPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : CWVernalPhase (scienceAPI, p, StartStageName, EndStageName){};

      void calcPhaseDevelopment(int das,
                                float& dlt_tt_phenol, float& phase_devel);
   private:
      bool germinating();

   };


#endif

