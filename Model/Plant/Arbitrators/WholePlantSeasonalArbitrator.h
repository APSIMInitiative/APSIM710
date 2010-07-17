#ifndef WholePlantSeasonalArbitratorH
#define WholePlantSeasonalArbitratorH

#include "Arbitrator.h"
class WholePlantSeasonalArbitrator : public Arbitrator
   {
   private:
      vector <vector <float> > Fracs;
      interpolationFunction ratioRootPlantTable;              // root:shoot ratio of new dm ()
   protected:
      virtual float ratioRootPlant(void);
      virtual float fracDMRemainingInPart(int partNumber);
      virtual float TotalPotentialGrowthRate(void);
   public:
      WholePlantSeasonalArbitrator(ScienceAPI& scienceAPI, plantInterface& p);
   };

#endif