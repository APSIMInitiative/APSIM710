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
      FixedPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : Phase (scienceAPI, p, StartStageName, EndStageName){};
      virtual string description();
   };


#endif

