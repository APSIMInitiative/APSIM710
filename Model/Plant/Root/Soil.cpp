#include "StdPlant.h"
#include "Soil.h"
#include <numeric>
#include "../Phenology/Phenology.h"
using namespace std;


Soil::Soil(ScienceAPI& s, plantInterface& p)
   : scienceAPI(s), plant(p)
//=======================================================================================
// Constructor
   {
   //scienceAPI = s;
   zero();
   }

void Soil::onInit1(protocol::Component *)
//=======================================================================================
// Perform all component initialisation.
   {

   scienceAPI.subscribe("new_profile", NewProfileFunction(&Soil::onNewProfile));

   scienceAPI.expose("xf", "", "Root exploration factor", xf);
   scienceAPI.expose("kl", "", "Root water uptake parameter", kl);
   scienceAPI.expose("ll_dep", "mm", "Plant Lower Limit (mm/mm)", ll_dep);

   }

void Soil::Read(void)
//=======================================================================================
// Read parameters
   {
   scienceAPI.read("sw_ub", sw_ub, 0.0f, 1.0f);
   scienceAPI.read("sw_lb", sw_lb, 0.0f, 1.0f);
   scienceAPI.read("sw_dep_ub", sw_dep_ub, 0.0f, 10000.0f);
   scienceAPI.read("sw_dep_lb", sw_dep_lb, 0.0f, 10000.0f);
   scienceAPI.read("kl_ub",kl_ub, 0.0f, 1.0f);
   scienceAPI.read("no3_ub", no3_ub, 0.0f, 100000.0f);
   scienceAPI.read("no3_lb", no3_lb, 0.0f, 100000.0f);
   scienceAPI.read("nh4_ub", nh4_ub, 0.0f, 100000.0f);
   scienceAPI.read("nh4_lb", nh4_lb, 0.0f, 100000.0f);
   scienceAPI.read("n_uptake_option", n_uptake_option, 1, 3);
   scienceAPI.read("n_supply_preference", n_supply_preference);

   if (n_uptake_option==1)
      scienceAPI.read("no3_diffn_const", no3_diffn_const, 0.0f, 100.0f);
   else if (n_uptake_option==2)
      {
      scienceAPI.read("no3_uptake_max", no3_uptake_max, 0.0f, 1.0f);
      scienceAPI.read("no3_conc_half_max", no3_conc_half_max, 0.0f, 100.0f);
      scienceAPI.read("total_n_uptake_max", total_n_uptake_max, 0.0f, 100.0f);
      }
   else if (n_uptake_option==3)
      {
      scienceAPI.read("kno3", kno3, 0.0f, 1.0f);
      scienceAPI.read("no3ppm_min", no3ppm_min, 0.0f, 10.0f);
      scienceAPI.read("knh4", knh4, 0.0f, 1.0f);
      scienceAPI.read("nh4ppm_min", nh4ppm_min, 0.0f, 10.0f);
      scienceAPI.read("total_n_uptake_max", total_n_uptake_max, 0.0f, 100.0f);
      }


   // Read Rooting parameters
    vector<float> ll ;   // lower limit of plant-extractable
                         // soil water for soil layer l
                         // (mm water/mm soil)

    if (scienceAPI.readOptional("ll", ll, 0.0, sw_ub))
       {
       ll_dep.clear();
       for (unsigned int layer = 0; layer != ll.size(); layer++)
          ll_dep.push_back (ll[layer]*dlayer[layer]);

       if ((int)ll.size() != num_layers)
          throw std::runtime_error ("Size of LL array doesn't match soil profile.");
       }
    else
       {
       scienceAPI.getOptional("ll15", "", ll, 0.0, sw_ub);
       if (ll.size() == 0)
          throw std::runtime_error("No Crop Lower Limit found");

       ll_dep.clear();
       for (unsigned int i=0; i< ll.size(); i++) ll_dep.push_back(ll[i]*dlayer[i]);
       cout << "        Using externally supplied Lower Limit (ll15)\n";
       }

   scienceAPI.read("kl", kl, 0.0f, kl_ub);
   if (kl.size() != (unsigned) num_layers)
      throw std::runtime_error  ("Size of KL array doesn't match soil profile.");

   scienceAPI.read("xf", xf, 0.0f, 1.0f);
   if (xf.size() != (unsigned) num_layers)
       throw std::runtime_error ("Size of XF array doesn't match soil profile.");


   }

void Soil::onNewProfile(protocol::NewProfileType &v)
//=======================================================================================
// Handler for OnNewProfile event
   {
    vector<float> previousLayers;
    for (unsigned layer = 0; layer != max_layer; layer++)
       previousLayers.push_back(dlayer[layer]);

    vector<float> scratch = v.dlayer;
    num_layers = scratch.size();

    for (unsigned i = 0; i < scratch.size(); i++)
      dlayer[i] = scratch[i];

    scratch = v.ll15_dep;
    for (unsigned i = 0; i < scratch.size(); i++)
      ll15_dep[i] = scratch[i];

    scratch = v.dul_dep;
    for (unsigned i = 0; i < scratch.size(); i++)
      dul_dep[i] = scratch[i];

    scratch = v.sat_dep;
    for (unsigned i = 0; i < scratch.size(); i++)
      sat_dep[i] = scratch[i];

//    scratch = v.sw_dep;
//    for (unsigned i = 0; i < scratch.size(); i++)
//      sw_dep[i] = scratch[i];

    scratch = v.bd;
    for (unsigned i = 0; i < scratch.size(); i++)
      bd[i] = scratch[i];

//    if (xf.size()==0)
//       for (int layer = 0; layer != num_layers; layer++)
//          xf.push_back(0.0);


    // dlayer may be changed from its last setting due to erosion
//    profile_depth = sum_real_array(dlayer,max_layer);

//    if (root_depth > profile_depth)
//        {
//        vector<float> vdlayer;
//        for (unsigned layer = 0; layer != max_layer; layer++)
//          vdlayer.push_back(dlayer[layer]);
//        vector<float> vprevdlayer;
//        for (unsigned layer = 0; layer != previousLayers.size(); layer++)
//          vprevdlayer.push_back(previousLayers[layer]);
//
//        redistribute( vprevdlayer,  vdlayer, profile_depth);
//
//        }
//     for (unsigned layer = 0; layer < (unsigned) num_layers; layer++)
//        {
//        ll_dep[layer] = divide (ll_dep[layer], previousLayers[layer], 0.0)
//                              * dlayer[layer];
//        }


   }

void Soil::zero(void)
//=======================================================================================
// Zero everything
   {
      sw_lb = 0.0;
      sw_ub = 0.0;
      sw_dep_ub = 0.0;
      sw_dep_lb = 0.0;

      num_layers = 0;
      fill_real_array (dlayer , 0.0, max_layer);
      fill_real_array (ll15_dep , 0.0, max_layer);
      fill_real_array (dul_dep , 0.0, max_layer);
      fill_real_array (sat_dep , 0.0, max_layer);
      fill_real_array (bd , 0.0, max_layer);
      fill_real_array (no3gsm , 0.0, max_layer);
      fill_real_array (nh4gsm , 0.0, max_layer);

      fill_real_array (sw_dep , 0.0, max_layer);
      ll_dep.clear();
      kl.clear();
      kl_ub = 0.0;

      xf.clear();

      no3_diffn_const = 0.0;
      no3_uptake_max = 0.0;
      no3_conc_half_max = 0.0;


      ZeroDeltas();
   }

void Soil::ZeroDeltas(void)
//=======================================================================================
// Zero daily data
   {
   fill_real_array (dlt_sw_dep , 0.0, max_layer);

   fill_real_array (sw_avail , 0.0, max_layer);
   fill_real_array (sw_avail_pot , 0.0, max_layer);
   fill_real_array (sw_supply , 0.0, max_layer);

   fill_real_array (dlt_no3gsm , 0.0, max_layer);
   fill_real_array (dlt_nh4gsm , 0.0, max_layer);
   fill_real_array (no3gsm_uptake_pot, 0.0, max_layer);
   fill_real_array (nh4gsm_uptake_pot, 0.0, max_layer);

   }

int Soil::find_layer_no(float depth)
//=======================================================================================
// Return the index of the layer corresponding to the given depth
   {
   unsigned int indx;
   float progressive_sum = 0.0;

   for(indx = 0; indx < (unsigned) num_layers; indx++)
      {
      progressive_sum = progressive_sum + dlayer[indx];
      if(progressive_sum >= depth)
         break;
      }
   if (indx != 0 && indx==(unsigned)num_layers) return (indx - 1); // last element in array
   return indx;                                            // index of
   }

int Soil::find_layer_no(float depth,      // depth in profile
                  float *dlayr,     // layer depth array
                  int num_layers)   // lowest layer
//===========================================================================

/*Purpose
 *   returns layer number of depth in profile dlayr
 *Definition
 *   Each of the "num_layers" elements of "dlayr" holds the
 *   height of the corresponding soil layer.  The height of the
 *   top layer is held in "dlayr"(0), and the rest follow in
 *   sequence down into the soil profile.  This function
 *   returns the index of the first element of "dlayr" which
 *   has its lower surface deeper than or equal to "depth".  If
 *   "depth" is deeper than the lower surface of the layer
 *   corresponding to "dlayr"("num_layers"), then "num_layers"
 *   is returned.
 */

   {
   return get_cumulative_index_real(depth, dlayr, num_layers);
   }

int Soil::find_layer_no(float depth, const vector<float> &dlayer )
//===========================================================================
   {
   float progressive_sum = 0.0; //cumulative sum_of
   unsigned int indx;                    //index count_of_real_vals

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum +=  dlayer[indx];
      if(progressive_sum >= depth)
         {
         break;
         }
      }
   if (indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                // index of
   }

float Soil::pesw(int depth)
//=======================================================================================
// Calculate plant extractable soil water at the given depth.
   {
   int layerNo = find_layer_no((float)depth);
   return (float)divide (sw_dep[layerNo] - ll_dep[layerNo], dlayer[layerNo], 0.0);
   }
float Soil::fasw(int depth)
//=======================================================================================
// calculate the fraction of available soil water at the given depth (mm)
   {
   int layerNo = find_layer_no((float)depth);
   float fasw = (float)divide (sw_dep[layerNo] - ll_dep[layerNo],
                        dul_dep[layerNo] - ll_dep[layerNo], 0.0);
   return bound (fasw, 0.0, 1.0);
   }

float Soil::layer_fasw(int layerNo)
//=======================================================================================
// calculate the fraction of available soil water at the given depth (mm)
   {
   float fasw = (float)divide (sw_dep[layerNo] - ll_dep[layerNo],
                        dul_dep[layerNo] - ll_dep[layerNo], 0.0);
   return bound (fasw, 0.0, 1.0);
   }

float Soil::swAvailable(float root_depth)
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   return sum_real_array (sw_avail, deepest_layer+1);
   }


void Soil::doSWAvailable(float root_depth)
//===========================================================================
// Return actual water available for extraction from each layer in the
// soil profile by the crop (mm water)
   {
   fill_real_array (sw_avail, 0.0, max_layer);

   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      sw_avail[layer] = sw_dep[layer] - ll_dep[layer];
      sw_avail[layer] = l_bound (sw_avail[layer], 0.0);
      }
   // correct bottom layer for actual root penetration
   sw_avail[deepest_layer] = sw_avail[deepest_layer] * root_proportion(deepest_layer, root_depth);
   }

void Soil::doSWSupply(float root_depth)
//=========================================================================
// Return potential water uptake from each layer of the soil profile
// by the crop (mm water). This represents the maximum amount in each
// layer regardless of lateral root distribution but takes account of
// root depth in bottom layer.
   {
   fill_real_array (sw_supply, 0.0, max_layer);

   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   float sw_avail;
   for(int i = 0; i <= deepest_layer; i++)
      {
      sw_avail = (sw_dep[i] - ll_dep[i]);
      sw_supply[i] = sw_avail * kl[i];
      sw_supply[i] = l_bound (sw_supply[i], 0.0);
      }
   //now adjust bottom layer for depth of root
   sw_supply[deepest_layer] = sw_supply[deepest_layer] * root_proportion(deepest_layer,root_depth);
   }
float Soil::root_proportion (int layer, float root_depth)
//===========================================================================
//  Returns the proportion of layer that has roots in it (0-1).
//  Definition
//     Each element of "dlayr" holds the height of  the
//     corresponding soil layer.  The height of the top layer is
//     held in "dlayr"(1), and the rest follow in sequence down
//     into the soil profile.  Given a root depth of "root_depth",
//     this function will return the proportion of "dlayr"("layer")
//     which has roots in it  (a value in the range 0..1).
//
   {
   // Local Variables
   float depth_to_layer_bottom;   // depth to bottom of layer (mm)
   float depth_to_layer_top;      // depth to top of layer (mm)
   float depth_to_root;           // depth to root in layer (mm)
   float depth_of_root_in_layer;  // depth of root within layer (mm)
   // Implementation Section ----------------------------------
   depth_to_layer_bottom = sum_real_array(dlayer, layer+1);
   depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
   depth_to_root  = min(depth_to_layer_bottom, root_depth);
   depth_of_root_in_layer = (float)max(0.0, depth_to_root-depth_to_layer_top);

   return (divide (depth_of_root_in_layer, dlayer[layer], 0.0));
   }

void Soil::doPotentialExtractableSW(float root_depth)
//===========================================================================
// Return potential available soil water from each layer in the root zone.
   {
   fill_real_array (sw_avail_pot, 0.0, max_layer);

   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   for (int layer = 0; layer <= deepest_layer; layer++)
      sw_avail_pot[layer] = dul_dep[layer] - ll_dep[layer];

   // correct bottom layer for actual root penetration
   sw_avail_pot[deepest_layer] = sw_avail_pot[deepest_layer] * root_proportion(deepest_layer,root_depth);
   }
void Soil::doWaterSupply (float root_depth)
//=======================================================================================
// Calculate today's daily water supply from this root system
// based on the KL approach
   {
   crop_check_sw(sw_lb, dlayer, dul_dep, sw_dep);

   // potential extractable sw
   doPotentialExtractableSW(root_depth);

   // actual extractable sw (sw-ll)
   doSWAvailable(root_depth);

   doSWSupply(root_depth);
   }

//=========================================================================
void Soil::crop_check_sw(
                   float minsw,    // (INPUT)  lowest acceptable value for ll
                   float *dlayer,   // (INPUT)  thickness of soil layer I (mm)
                   float *dul_dep,  // (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                   float *sw_dep)   // (INPUT)  soil water content of layer L (mm)

//=========================================================================
/*  Purpose
*       Check validity of soil water parameters for all soil profile layers.
*
*  Mission Statement
*       Check validity of soil water parameters for all soil profile layers.
*
*  Notes
*           Reports an error if
*           - ll_dep and dul_dep are not in ascending order
*           - ll is below c_minsw
*           - sw < c_minsw
*
*  Changes
*     21/5/2003 ad converted to BC++
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks
*/
   {
   //  Local Variables
   float dul;              // drained upper limit water content of layer (mm water/mm soil)
   char err_msg[80];          // error message
   int layer;              // layer number
   float ll;               // lower limit water content of layer (mm water/mm soil)
   float sw;               // soil water content of layer l (mm water/mm soil)
   int num_layers;
   // Implementation Section ----------------------------------

   num_layers = count_of_real_vals (dlayer, max_layer);  //XX index_of_real_vals
   for(layer = 0; layer <= num_layers; layer++)
      {
      sw = (float)divide (sw_dep[layer], dlayer[layer], 0.0);
      dul = (float)divide (dul_dep[layer], dlayer[layer], 0.0);
      ll = (float)divide (ll_dep[layer], dlayer[layer], 0.0);

      if (ll < minsw)
         {
         sprintf(err_msg,
            " lower limit of %8.2f in layer %d\n         is below acceptable value of %8.2f",
            ll, layer, minsw);
         scienceAPI.warning(err_msg);
         }
      else
         {
         }

      if (dul < ll)
         {

         sprintf(err_msg,
            " Drained upper limit of %8.2f in layer %d\n         is at or below lower limit of %8.2f",
            dul,layer, ll);
         scienceAPI.warning(err_msg);
         }
      else
         {
         }
      if (sw < minsw)
         {
         sprintf(err_msg,
            " Soil water of %8.2f in layer %d\n         is below acceptable value of %8.2f",
            sw, layer, minsw);
         scienceAPI.warning(err_msg);
         }
      else
         {
         }
      }

   }

void Soil::doWaterUptakeExternal (string uptake_source, string crop_type)
//=======================================================================================
// Gets todays daily water uptake by this root system
   {
    int   layer;                                  // layer number of profile ()
    float ext_sw_supply[max_layer];
    
    // If uptake source is swim3 then use standard get from apsim
    // otherwise use value of uptake source as it is.
    string source = uptake_source;
    if (uptake_source =="swim3") source = "apsim";
    	
     plant_get_ext_uptakes(source.c_str()
                          ,crop_type.c_str()
                          ,"water"
                          ,1.0
                          ,0.0
                          ,100.0
                          ,ext_sw_supply);

     for (layer = 0; layer < num_layers; layer++)
        {
        dlt_sw_dep[layer] = -ext_sw_supply[layer];
        }
   }
void Soil::plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                           const char *crop_type,            //(INPUT) crop type name
                           const char *uptake_type,          //(INPUT) uptake name
                           float unit_conversion_factor,     //(INPUT) unit conversion factor
                           float uptake_lbound,              //(INPUT) uptake lower limit
                           float uptake_ubound,              //(INPUT) uptake upper limit
                           float *uptake_array)              //(OUTPUT) crop uptake array

/*  Purpose
*     Ask swim for uptakes of water or solute
*
*  Mission Statement
*   Get the soil uptake for %3 from another module
*
*  Notes
*      Bounds should probably be passed in when crops decide what
*      these should be (ie when ini files have limits for uptake
*      in them)
*
*  Changes
*     08-05-1997 - huth - Programmed and Specified
*     20/5/2003 ad converted to BC++
*/
   {
   char uptake_name[80];             // Uptake variable name
   unsigned layer;
   std::vector<float> values;   // Scratch area

   if (strcmp(uptake_source, "apsim") == 0 && *crop_type != '\0')
      {
      // NB - if crop type is blank then swim will know nothing
      // about this crop (eg if not initialised yet)

      sprintf(uptake_name, "uptake_%s_%s", uptake_type, crop_type);

      scienceAPI.get(uptake_name, "", values, uptake_lbound, uptake_ubound);
      for (layer=0; layer< values.size(); layer++)
         uptake_array[layer] = values[layer] * unit_conversion_factor;
      }
   }

void Soil::write()
//=======================================================================================
// Write all parameters as a summary to stdout.
   {
   cout << "                        Root Profile" << endl;
   cout << "         -----------------------------------------------" << endl;
   cout << "          Layer       Kl           Lower    Exploration" << endl;
   cout << "          Depth     Factor         Limit      Factor" << endl;
   cout << "          (mm)         ()        (mm/mm)       (0-1)" << endl;
   cout << "         -----------------------------------------------" << endl;

    float dep_tot, esw_tot;                      // total depth of soil & ll
    char  msg[200];

    dep_tot = esw_tot = 0.0;
    for (int layer = 0; layer < num_layers; layer++)
       {
       sprintf (msg, "     %9.1f%10.3f%15.3f%12.3f"
          , dlayer[layer]
          , kl[layer]
          , divide(ll_dep[layer],dlayer[layer],0.0)
          , xf[layer]);
       cout << msg << endl;
       dep_tot += dlayer[layer];
       esw_tot += dul_dep[layer] - ll_dep[layer];
       }
    cout << "         -----------------------------------------------" << endl;
    sprintf (msg
          , "         Extractable SW: %5.0fmm in %5.0fmm total depth (%3.0f%%)."
          , esw_tot
          , dep_tot
          , fract2pcnt * divide(esw_tot, dep_tot, 0.0));
    cout << msg << endl;
   }

void Soil::getOtherVariables()
//=======================================================================================
// Get data from other modules as required
   {
   std::vector<float> values;               // Scratch area
   scienceAPI.get("sw_dep", "", values, sw_dep_lb, sw_dep_ub);
   for (unsigned int i=0; i< values.size(); i++)
      sw_dep[i] = values[i];

   values.clear();
   if (!scienceAPI.getOptional("no3", "", values, no3_lb, no3_ub))
      {
      // we have no N supply - make non-limiting.
      for (int i = 0; i < num_layers; i++)
         values.push_back(10000.0);
      }
   for (int i = 0; i < num_layers; i++)
      no3gsm[i] = values[i] * kg2gm /ha2sm;

    values.clear();
    if (!scienceAPI.getOptional("nh4", "", values, nh4_lb, nh4_ub))
        {
        // we have no N supply - make non-limiting.
        for (int i = 0; i < num_layers; i++)
           values.push_back(10000.0);
        }
    for (int i = 0; i < num_layers; i++)
       nh4gsm[i] = values[i] * kg2gm /ha2sm;

   }

void Soil::doWaterUptakeInternal (float sw_demand, float root_depth)
//=======================================================================================
// Calculate todays daily water uptake by this root system
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   float sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);

   if ((sw_supply_sum < 0.0) || (sw_demand  < 0.0))
      {
      //we have no uptake - there is no demand or potential
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      }
   else
      {
      // get actual uptake
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      if (sw_demand < sw_supply_sum)
         {
         // demand is less than what roots could take up.
         // water is non-limiting.
         // distribute demand proportionately in all layers.
         for(int layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1.0f * divide (sw_supply[layer], sw_supply_sum, 0.0) * sw_demand;
            }
         }
      else
         {
         // water is limiting - not enough to meet demand so take
         // what is available (potential)
         for(int layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * sw_supply[layer];
            }
         }
      }
   }

float Soil::peswTotal(float root_depth)
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   vector<float> pesw;
   int deepest_layer = find_layer_no(root_depth, dlayer, max_layer);
   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      pesw.push_back(sw_dep[layer] - ll_dep[layer]);
      pesw[layer] = l_bound (pesw[layer], 0.0);
      }
   return std::accumulate(pesw.begin(), pesw.end(), 0.0);
   }

float Soil::swSupply(float root_depth)
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   return sum_real_array (sw_supply, deepest_layer+1);
   }

float Soil::NSupply(float root_depth)
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   return sum_real_array (no3gsm_uptake_pot, deepest_layer+1)+sum_real_array (nh4gsm_uptake_pot, deepest_layer+1);
   }

void Soil::UpdateOtherVariables(string uptake_source)
//=======================================================================================
// Update data owned by other modules that has changed due to calculations by this root system
   {
   if (Str_i_Eq(uptake_source, "calc"))
      {
      vector<float> dltNO3KgHa, dltNH4KgHa, dltSwDep;
      for (int layer = 0; layer< num_layers;layer++)
         {
         dltNO3KgHa.push_back(dlt_no3gsm[layer] * gm2kg /sm2ha);
         dltNH4KgHa.push_back(dlt_nh4gsm[layer] * gm2kg /sm2ha);
         dltSwDep.push_back(dlt_sw_dep[layer]);
         }
      scienceAPI.set("dlt_no3", "kg/ha", dltNO3KgHa);
      scienceAPI.set("dlt_nh4", "kg/ha", dltNH4KgHa);
      scienceAPI.set("dlt_sw_dep", "mm", dltSwDep);
      }
   else if (Str_i_Eq(uptake_source, "swim3"))
      {
      vector<float> dltNO3KgHa, dltNH4KgHa;
      for (int layer = 0; layer< num_layers;layer++)
         {
         dltNO3KgHa.push_back(dlt_no3gsm[layer] * gm2kg /sm2ha);
         dltNH4KgHa.push_back(dlt_nh4gsm[layer] * gm2kg /sm2ha);
         }
      scienceAPI.set("dlt_no3", "kg/ha", dltNO3KgHa);
      scienceAPI.set("dlt_nh4", "kg/ha", dltNH4KgHa);
      }
   else
      {
      // no need to send updates
      }
   }

float Soil::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return (- sum_real_array(dlt_sw_dep, max_layer));;
   }

float Soil::WFPS(int layer)
//=======================================================================================
//
   {
   float wfps;
   wfps = (float)divide(sw_dep[layer] - ll15_dep[layer],
                sat_dep[layer] - ll15_dep[layer], 0.0);
   wfps = bound (wfps, 0.0, 1.0);
   return wfps;
   }

void Soil::doNUptake(string uptake_source, string crop_type, float root_depth, float sumNMax, float sumSoilNDemand, float nDemand, float n_fix_pot)
//=======================================================================================
//       Find nitrogen uptake.
    {
    if (Str_i_Eq(uptake_source, "apsim"))
        {
        // NIH - note that I use a -ve conversion
        // factor FOR NOW to make it a delta.
        plant_get_ext_uptakes(uptake_source.c_str()
                             ,crop_type.c_str()
                             ,"no3"
                             ,-kg2gm/ha2sm
                             ,0.0
                             ,100.0
                             ,dlt_no3gsm);


        }
    else if (n_uptake_option == 1)
        {
        cproc_n_uptake1(no3_diffn_const
                       , dlayer
                       , no3gsm_diffn_pot
                       , no3gsm_mflow_avail
                       , n_fix_pot
                       , n_supply_preference.c_str()
                       , nDemand
                       , sumNMax
                       , root_depth
                       , dlt_no3gsm);
        }
    else if ((n_uptake_option == 2) || (n_uptake_option == 3))
        {
        cproc_n_uptake3(dlayer
                        , no3gsm_uptake_pot
                        , nh4gsm_uptake_pot
                        , n_fix_pot
                        , n_supply_preference.c_str()
                        , sumSoilNDemand
                        , sumNMax
                        , root_depth
                        , dlt_no3gsm
                        , dlt_nh4gsm);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
    }

//+  Purpose
//       Plant transpiration and soil water extraction
void Soil::doWaterUptake (string uptake_source, string crop_type, float SWDemand, float root_depth)
    {
    doWaterSupply(root_depth);

    if (Str_i_Eq(uptake_source,"apsim"))
        {
        doWaterUptakeExternal(uptake_source, crop_type);
        }
    else if (Str_i_Eq(uptake_source,"swim3"))
        {
        doWaterUptakeExternal(uptake_source, crop_type);
        }
    else
        {
        doWaterUptakeInternal(SWDemand, root_depth);
        }
    }

void Soil::plant_nit_supply(float root_depth, float *root_length)
//=======================================================================================
// Calculate Plant Nitrogen Supply
    {
//+  Local Variables
    float no3gsm_min[max_layer];   // minimum allowable NO3 in soil (g/m^2)
    fill_real_array (no3gsm_min, 0.0, max_layer);

    if (n_uptake_option == 1)
        cproc_n_supply1 (dlayer
                         , dlt_sw_dep
                         , no3gsm
                         , no3gsm_min
                         , root_depth
                         , sw_dep
                         , no3gsm_mflow_avail
                         , sw_avail
                         , no3gsm_diffn_pot);

    else if (n_uptake_option == 2)
        cproc_n_supply3 (dlayer
                         , no3gsm
                         , no3gsm_min
                         , no3gsm_uptake_pot
                         , root_depth
                         , root_length
                         , bd
                         , total_n_uptake_max
                         , no3_uptake_max
                         , no3_conc_half_max
                         , sw_avail_pot
                         , sw_avail
                         , plant.phenology().inPhase("n_stress"));

     else if (n_uptake_option == 3)
        {
        float nh4gsm_min[max_layer];   // minimum allowable NH4 in soil (g/m^2)
        fill_real_array (nh4gsm_min, 0.0, max_layer);

        cproc_n_supply4 (dlayer
                             , bd
                             , no3gsm
                             , no3gsm_min
                             , no3gsm_uptake_pot
                             , nh4gsm
                             , nh4gsm_min
                             , nh4gsm_uptake_pot
                             , root_depth
                             , kno3
                             , no3ppm_min
                             , knh4
                             , nh4ppm_min
                             , total_n_uptake_max
                             , sw_avail_pot
                             , sw_avail
                             , plant.phenology().inPhase("n_stress"));
        }
    else
        {
        throw std::invalid_argument ("invalid template N uptake option");
        }

   }



