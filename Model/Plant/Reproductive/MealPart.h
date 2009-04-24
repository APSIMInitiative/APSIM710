#ifndef MealPartH
#define MealPartH
#include "../ReproductivePart.h"
#include "../SimplePart.h"


class fruitMealPart : public ReproductivePart {
  public:
   fruitMealPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~fruitMealPart() {};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onFlowering(void);
   void onStartGrainFill(void);

   void doDMDemand(float dm_demand);         //remove
   void doNDemand(float dm_demand);         //remove
   void doDMDemandGrain(float dm_demand);
   void doNRetranslocate( float dltN, float grain_n_demand);
   float nDemandGrain2(void);
   void doNConcGrainLimits(float n_min_grain, float n_crit_grain, float n_max_grain);
   float nCapacity2(void);
   float N_conc_pot(float);
   float nCapacity(void){return 0.0;};
   float availableRetranslocateN(void){return 0.0;};
   virtual bool isYieldPart(void)  {return true;};

  private:
};

#endif /* MealPartH */
