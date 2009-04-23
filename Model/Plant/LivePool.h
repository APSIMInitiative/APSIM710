#ifndef LivePoolH
#define LivePoolH
#include <string>
#include "Pool.h"
class ScienceAPI;

class LivePool : public Pool
   {
   public:
      LivePool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);
      interpolationFunction n_conc_min;
      interpolationFunction n_conc_crit;
      interpolationFunction n_conc_max;
      float Ncrit(float StageCode){return n_conc_crit.value(StageCode);};
      float Nmin(float StageCode){return n_conc_min.value(StageCode);};
      float Nmax(float StageCode){return n_conc_max.value(StageCode);};
   private:

   };

#endif
