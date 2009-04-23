#ifndef VernalPhaseH
#define VernalPhaseH

class Environment;
class Output;

#include "Phase.h"
class VernalPhase : public Phase
   // A phenological phase.
   {
   protected:
      interpolationFunction vernal_days;     // relate temperature to vernalisation
      interpolationFunction vernal_tt;       // Growing degree days to complete phase as a function of cum vd
      float dlt_cumvd;
      float cumvd;

   public:
      void read();
      VernalPhase(ScienceAPI& scienceAPI, plantInterface& plant, const string& stage_name)
         : Phase (scienceAPI, plant, stage_name){};
      void updateTTTargets();
      virtual void reset();
      virtual string description();
   };


#endif

