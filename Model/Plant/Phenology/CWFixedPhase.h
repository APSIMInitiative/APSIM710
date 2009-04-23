#ifndef CWFixedPhaseH
#define CWFixedPhaseH

#include "CWVernalPhase.h"
class CWFixedPhase : public CWVernalPhase
   {
   protected:
      interpolationFunction stressFunction;

      float stress();

   public:
      CWFixedPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : CWVernalPhase (scienceAPI, p, stage_name){};

      virtual string description();
      virtual void read();
      virtual void calcPhaseDevelopment(int das, 
                                        float& dlt_tt_phenol, float& phase_devel);

   };

#endif
