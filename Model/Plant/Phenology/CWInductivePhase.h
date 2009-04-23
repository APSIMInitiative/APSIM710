#ifndef CWInductivePhaseH
#define CWInductivePhaseH

#include "CWVernalPhase.h"
class CWInductivePhase : public CWVernalPhase
   {
   protected:
      virtual float stress();

   public:
      CWInductivePhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : CWVernalPhase (scienceAPI, p, stage_name){};

      virtual void read();

   };

#endif
