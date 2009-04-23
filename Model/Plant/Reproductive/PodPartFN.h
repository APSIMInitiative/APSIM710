#ifndef PodPartFN
#define PodPartFN
#include "PodPart.h"

class FruitCohortFN;

class fruitPodPartFN : public fruitPodPart {
  public:
   fruitPodPartFN(ScienceAPI& scienceAPI, plantInterface *p, const string &name) ;
   ~fruitPodPartFN() {};
   void onInit1(protocol::Component *);
   void prepare(void);
   void update(void);
   void onFlowering(void);
   void onStartGrainFill(void);
   void doDmMin(void);
   void doDmDemand(float  dlt_dm_supply);
   void doProcessBioDemand(void);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmRetranslocateSupply(float DemandDifferential) ;

   void zeroAllGlobals(void);
   void zeroDeltas(void);

   void doTECO2();

   private:

};

#endif /* PodPartFN */
