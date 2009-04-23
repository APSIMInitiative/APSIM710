#ifndef RootGrowthOption2H
#define RootGrowthOption2H
#include "RootPart.h"

class rootGrowthOption2 : public RootPart
//=======================================================================================
//
   {
 private:
   float rootDistributionPattern;
 public:
   rootGrowthOption2(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
      : RootPart(scienceAPI, p, name) {};
   void read();
   void root_length_growth (void);
   };

#endif /* RootGrowthOption2 */
