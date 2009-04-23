#ifndef WholePlantGenericArbitratorXYH
#define WholePlantGenericArbitratorXYH

#include "arbitrator.h"
class WholePlantGenericArbitratorXY : public Arbitrator
   {
   private:
      vector <interpolationFunction> Fracs;
   protected:
      virtual float fracDMRemainingInPart(int partNumber);
      virtual float TotalPotentialGrowthRate(void);
   public:
      WholePlantGenericArbitratorXY(ScienceAPI& scienceAPI, plantInterface& p);
   };

#endif
