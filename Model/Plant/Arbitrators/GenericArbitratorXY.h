#ifndef GenericArbitratorXYH
#define GenericArbitratorXYH

#include "Arbitrator.h"
class GenericArbitratorXY : public Arbitrator
   {
   private:
      interpolationFunction ratio_root_shoot;
      vector <interpolationFunction> Fracs;
   protected:
      virtual float ratioRootShoot();
      virtual float fracDMRemainingInPart(int partNumber);
   public:
      GenericArbitratorXY(ScienceAPI& scienceAPI, plantInterface& p);
   };

#endif
