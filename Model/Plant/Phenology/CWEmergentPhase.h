#ifndef CWEmergentPhaseH
#define CWEmergentPhaseH

#include "CWVernalPhase.h"
class CWEmergentPhase : public CWVernalPhase
   {
   protected:
      interpolationFunction rel_emerg_rate;

      virtual float stress();

   public:
      CWEmergentPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : CWVernalPhase (scienceAPI, p, StartStageName, EndStageName){};

      virtual void read();
      virtual void OnSow(float sowing_depth);

   };


#endif

