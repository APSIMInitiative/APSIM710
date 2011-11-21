#include "StdPlant.h"

#include "RootPart.h"
#include "RootGrowthOption1.h"
#include "../Utility/PlantUtility.h"
#include "../Population.h"

using namespace std;

void rootGrowthOption1::root_length_growth (void)
//============================================================================
//  (was cproc_root_length_growth1)
//  Calculate the increase in root length density in each rooted
//  layer based upon soil hospitality, moisture and fraction of
//  layer explored by roots.
   {
   float dlt_length_tot;      // total root length increase (mm/m^2)
   float rlv_factor_tot;      // total rooting factors across profile
   float branching_factor;    //
   float plant_rld;
   float rld;

   setTo(dltRootLength, (float)0.0);

   float depth_today = root_depth + dltRootDepth;
   int deepest_layer = (*soil[0]).find_layer_no (depth_today);

   vector<float> rlv_factor((*soil[0]).num_layers);  // relative rooting factor for all layers

   rlv_factor_tot = 0.0;
   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      rld = (float)divide (root_length[layer], (*soil[0]).dlayer[layer], 0.0);

      plant_rld = (float)divide (rld, plant->population().Density(), 0.0);

      branching_factor = rel_root_rate.value(plant_rld);

      rlv_factor[layer] = (float)(sw_fac_root.value(sw_avail_ratio(layer)) *
                 branching_factor *                                      // branching factor
                   (*soil[0]).xf [layer]  *                                       // growth factor
                     divide((*soil[0]).dlayer[layer],      // space weighting
                            root_depth, 0.0));                            //       factor

      rlv_factor[layer] = l_bound(rlv_factor[layer], 1e-6f);
      rlv_factor_tot += rlv_factor[layer];
      }

   dlt_length_tot = Growth.DM()/sm2smm * specificRootLength;

   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      dltRootLength[layer] = (float)(dlt_length_tot *
                              divide (rlv_factor[layer], rlv_factor_tot, 0.0));
      }

}
