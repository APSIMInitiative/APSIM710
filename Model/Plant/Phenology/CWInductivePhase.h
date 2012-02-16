#ifndef CWInductivePhaseH
#define CWInductivePhaseH

#include "CWVernalPhase.h"
class CWInductivePhase : public CWVernalPhase
   {
   protected:
      virtual float stress();

   public:
      CWInductivePhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : CWVernalPhase (scienceAPI, p, StartStageName, EndStageName){};

      virtual void read();

   };

#endif
