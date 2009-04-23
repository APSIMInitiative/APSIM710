#ifndef GenericArbitratorH
#define GenericArbitratorH

#include "Arbitrators/arbitrator.h"
class GenericArbitrator : public Arbitrator
   {
   private:
      std::vector<float> ratio_root_shoot;              // root:shoot ratio of new dm ()
      vector <vector <float> > Fracs;
   protected:
      virtual float ratioRootShoot();
      virtual float fracDMRemainingInPart(int partNumber);
   public:
      GenericArbitrator(ScienceAPI& scienceAPI, plantInterface& p);
   };

#endif
