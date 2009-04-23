#ifndef EmergentPhaseH
#define EmergentPhaseH

class Environment;

#include "Phase.h"
class EmergentPhase : public Phase
   {
   protected:
      float emergent_tt;       // Growing degree days to complete phase as a function of cum vd
      float shoot_lag;
      float shoot_rate;
      float sowing_depth;
      interpolationFunction rel_emerg_rate;
      virtual void OnSow(float sowing_depth);

   public:
      void read();
      EmergentPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : Phase (scienceAPI, p, stage_name){};
      void setupTTTarget(void);
      virtual string description();
      float stress();
   };


#endif

