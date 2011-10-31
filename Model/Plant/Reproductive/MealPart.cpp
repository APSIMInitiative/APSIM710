#include "StdPlant.h"

#include "MealPart.h"
using namespace std;

fruitMealPart::fruitMealPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : ReproductivePart(scienceAPI, p, name)
   {
   Vegetative.ClearPools();
   VegetativeTotal.ClearPools();
   Grain.AddPool(Green);
   GrainTotal.AddPool(Green);
   GrainTotal.AddPool(Senesced);
   }

void fruitMealPart::doDMDemand (float dm_demand)                    //remove
//     ===========================================================  //remove
{                                                                   //remove
    doDMDemandGrain(dm_demand);                                      //remove
}                                                                   //remove

void fruitMealPart::doNDemand (float n_demand)                      //remove
//     ===========================================================  //remove
{                                                                   //remove
    NDemand = n_demand;                                             //remove
}                                                                   //remove

void fruitMealPart::doDMDemandGrain (float dm_demand)
//     ===========================================================
{
    DMGreenDemand = dm_demand;
}

float fruitMealPart::nDemandGrain2(void)
   //===========================================================================
{
   return l_bound(NDemand - dlt.n_senesced_retrans - Growth.N(), 0.0);
}

void fruitMealPart::doNRetranslocate(float dltN, float /*grain_n_demand*/)
//     ===========================================================
{
    Retranslocation.SetN(dltN);
}

void fruitMealPart::onHarvest(float /* cutting_height */, float /*remove_fr*/,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// biomass is removed, nothing is sent to surface residues..
{
     dm_type.push_back (myName);
     fraction_to_residue.push_back (0.0);
     dlt_crop_dm.push_back ((Green.DM()+Senesced.DM()) * gm2kg/sm2ha);
     dlt_dm_n.push_back    ((Green.N()+Senesced.N())  * gm2kg/sm2ha);
     dlt_dm_p.push_back    ((Green.P()+Senesced.P())  * gm2kg/sm2ha);

     Senesced.Clear();
     Green.Clear();
}


void fruitMealPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitMealPart::onStartGrainFill(void)
{  // do nothing
}

void fruitMealPart::doNConcGrainLimits(float n_min_grain, float n_crit_grain, float n_max_grain)
{
   float dm_meal;                                // meal mass (g/m2)
   dm_meal = (Green.DM() + Growth.DM() + Retranslocation.DM());
   g.n_conc_crit = divide (n_crit_grain, dm_meal, 0.0);
   g.n_conc_max = divide (n_max_grain, dm_meal, 0.0);
   g.n_conc_min = divide (n_min_grain, dm_meal, 0.0);

}

float fruitMealPart::nCapacity2(void)
{
   float n_potential  = (Green.DM() + Growth.DM() + Retranslocation.DM()) * g.n_conc_max;
   return (n_potential - Green.N());
}

float fruitMealPart::N_conc_pot(float nfact_grain_conc)
{
   return (g.n_conc_min + (g.n_conc_crit - g.n_conc_min) * nfact_grain_conc);
}
