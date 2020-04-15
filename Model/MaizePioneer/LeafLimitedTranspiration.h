//---------------------------------------------------------------------------

#ifndef LeafLimitedTranspirationH
#define LeafLimitedTranspirationH
#include "PlantComponents.h"
#include "Utilities.h"
#include "LeafSlowWilting.h"

namespace Maize {
   class LeafLimitedTranspiration : public LeafSlowWilting
      {
	  private:
	     double maxTrans;
		 double maxTransEffect;
		 vector <double> adjustedHBio;
		 
		 virtual void doRegistrations();
		 virtual void initialize();
		 virtual void readParams();
	  public:
         LeafLimitedTranspiration(ScienceAPI2 &, Plant *p);
         ~LeafLimitedTranspiration();
		 virtual void calcSWSupplyDemand(double &actDemand,double &maxSupply);
       virtual void balanceWater(double &swDemand,double &totalSupply);
       virtual double calcDltDMPotTE(void);
      };
}
#endif