#ifndef InductivePhaseH
#define InductivePhaseH

#include "VernalPhase.h"
class InductivePhase : public VernalPhase
   {
   protected:
      virtual float stress();

      virtual void updateTTTargets();
   public:
      InductivePhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : VernalPhase (scienceAPI, p, StartStageName, EndStageName){};

   };

#endif
