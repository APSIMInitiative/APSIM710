#ifndef OilPartH
#define OilPartH
#include "SimplePart.h"


class fruitOilPart : public SimplePart {
  public:
   fruitOilPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~fruitOilPart() {};
   void onInit1(protocol::Component *);
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);


   void onFlowering(void);
   void onStartGrainFill(void);
   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   float grainEnergy(void);
   float energyAdjustHI(float harvestIndex) ;
   float addEnergy(float DM) ;
   float removeEnergy(float DM) ;
   void doDMDemand(float dlt_dm_grain_demand) ;       //remove
   void doDMDemandGrain(float dlt_dm_grain_demand);
   float giveDmGreen(float delta);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmGreen(void) ;
   float dltDmGreenRetransUptake(void);
   float dmDemandDifferential(void) ;

   void doBioGrainOil (void);
   float nCapacity(void){return 0.0;};
   float availableRetranslocateN(void){return 0.0;};
   virtual bool isYieldPart(void)  {return true;};

  private:


   float cGrain_oil_conc;                            // fractional oil content of grain (0-1)
   float dmOil_conv_retranslocate;
   float cCarbo_oil_conv_ratio;
   float gGrain_energy;                 // multiplier of grain weight to account for seed energy content

};

#endif /* OilPartH */

