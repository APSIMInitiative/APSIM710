#ifndef CWSowingPhaseH
#define CWSowingPhaseH

#include "CWVernalPhase.h"
class CWSowingPhase : public CWVernalPhase
   {
   public:
      CWSowingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : CWVernalPhase (scienceAPI, p, stage_name){};

      void calcPhaseDevelopment(int das,
                                float& dlt_tt_phenol, float& phase_devel);
   private:
      bool germinating();

   };


#endif

