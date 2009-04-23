#ifndef FixationH
#define FixationH
#include <vector>

class Fixation : public plantThing
   {
   public:
      Fixation(ScienceAPI& scienceAPI, plantInterface& p, const std::string& name);
      float Potential(float biomass, float swdef_fixation);
      float NFixPot(){return n_fix_pot;};
   private:
      plantInterface& plant;
      std::vector<float> n_fix_rate;     // potential rate of N fixation (g N fixed
      float n_fix_pot;
    };

#endif
