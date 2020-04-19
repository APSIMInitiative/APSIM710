//---------------------------------------------------------------------------

#ifndef LeafSlowWiltingH
#define LeafSlowWiltingH
#include "PlantComponents.h"
#include "Utilities.h"
#include "Leaf.h"

namespace Maize {
   class LeafSlowWilting : public Leaf
      {
	  protected:
         double latitude;
         double maxFlux;
         double maxLag;
         double nightCoef;
         double minLag;

         vector <double> hRadn;
         vector <double> TAirParam;
         vector <double> TAir;
         vector <double> SVP;
         vector <double> RH;
         vector <double> VPDair;
         vector <double> transDemand;
         vector <double> transDemandFluxLimited;
		 vector <double> hBio;
		 
		 virtual void doRegistrations();
		 virtual void initialize();
		 virtual void readParams();
		 virtual void readTParams();
	  public:
         LeafSlowWilting(ScienceAPI2 &, Plant *p);
         ~LeafSlowWilting();
		 virtual void calcSWSupplyDemand(double &actDemand,double &maxSupply);
		 virtual void calcHourlyVars();
      };
}
#endif