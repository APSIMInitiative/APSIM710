#ifndef CWFixedPhaseH
#define CWFixedPhaseH

#include "CWVernalPhase.h"
class CWFixedPhase : public CWVernalPhase
   {
   protected:
      interpolationFunction stressFunction;

      float stress();

   public:
      CWFixedPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : CWVernalPhase (scienceAPI, p, StartStageName, EndStageName){};

      virtual string description();
      virtual void read();
      virtual void calcPhaseDevelopment(int das, 
                                        float& dlt_tt_phenol, float& phase_devel);

   };

#endif
