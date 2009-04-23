#ifndef FixedPhaseH
#define FixedPhaseH

#include "Phase.h"
// A fixed duration phenological phase.
class FixedPhase : public Phase
   {
   protected:
      interpolationFunction stressFunction;

      virtual float stress();
   public:
      void read();
      FixedPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : Phase (scienceAPI, p, stage_name){};
      virtual string description();
   };


#endif

