#ifndef CWEmergentPhaseH
#define CWEmergentPhaseH

#include "CWVernalPhase.h"
class CWEmergentPhase : public CWVernalPhase
   {
   protected:
      interpolationFunction rel_emerg_rate;

      virtual float stress();

   public:
      CWEmergentPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : CWVernalPhase (scienceAPI, p, stage_name){};

      virtual void read();
      virtual void OnSow(float sowing_depth);

   };


#endif

