#ifndef WholePlantSeasonalPpArbitratorH
#define WholePlantSeasonalPpArbitratorH

#include "Arbitrator.h"
class WholePlantSeasonalPpArbitrator : public Arbitrator
   {
   private:
      vector <vector <float> > Fracs;
      interpolationFunction ratioRootPlantTable;              // root:shoot ratio of new dm ()
      float ratioRootPlantScalingFactor;
      plantInterface& plant;

   protected:
      virtual float ratioRootPlant(void);
      virtual float fracDMRemainingInPart(int partNumber);
      virtual float TotalPotentialGrowthRate(void);
   public:
      WholePlantSeasonalPpArbitrator(ScienceAPI& scienceAPI, plantInterface& p);
   };

#endif