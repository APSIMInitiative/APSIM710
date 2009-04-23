#include "StdPlant.h"

//===========================================================================
void crop_lai_equilib_light ( float radn_int,
                              float cover_green,
                              float sen_radn_crit,
                              float extinction_coef,
                              float lai,
                              int   day_of_year,
                              int   year,
                              float *lai_eqlb_light)     //(INPUT/OUTPUT) lai threshold for light senescence
//===========================================================================

/*Purpose
 *   Return the lai threshold for light induced senescence.
 */

  {
   //Local Variables
   float lai_eqlb_light_today;   //lai threshold for light senescence
   float radn_canopy;            //radiation reaching canopy mj/m^2)
   float trans_crit;             //critical transmission (0-1)
   //Implementation

   radn_canopy = divide (radn_int, cover_green, 0.0);
   trans_crit = divide (sen_radn_crit, radn_canopy, 0.0);

   if (trans_crit > 0.0)
      {
      // needs rework for row spacing
      lai_eqlb_light_today = -1 * log (trans_crit)/ extinction_coef;
      }
   else
      {
      lai_eqlb_light_today = lai;
      }
   crop_store_value (day_of_year, year, lai_eqlb_light, lai_eqlb_light_today);
   }

//===========================================================================
float crop_leaf_area_sen_frost1(interpolationFunction &frost_fraction,       //(INPUT)
                               float lai,                   //(INPUT)  live plant green lai
                               float mint,                  //(INPUT)  minimum air temperature (o_c)
                               float plants,                //(INPUT)
                               float min_tpla)              //(INPUT)
//===========================================================================

/*Purpose
 *   Return the lai that would senesce on the
 *   current day from low temperatures
 */
   {
   //Local Variables
   float dlt_slai_low_temp;      // lai senesced from low temps
   float sen_fac_temp;           // low temperature factor (0-1)
   float min_lai;
   float max_sen;

   //low temperature factor
   sen_fac_temp = frost_fraction.value(mint);

   dlt_slai_low_temp = sen_fac_temp * lai;
   min_lai = min_tpla * plants * smm2sm;
   max_sen = l_bound (lai - min_lai, 0.0);
   return( bound (dlt_slai_low_temp, 0.0, max_sen));
   }

//===========================================================================
void crop_leaf_area_sen_water2(int   day_of_year,                //(INPUT)  day of year
                               int   year,                       //(INPUT)  year
                               float sen_threshold,              //(INPUT)  supply:demand ratio for onset
                               float sen_water_time_const,       //(INPUT)  delay factor for water senesce
                               int   num_layer,                  //(INPUT)  number of layers in profile
                               float *dlayer,                     //(INPUT)  thickness of soil layer I (mm)
                               float lai,                        //(INPUT)  live plant green lai
                               float *lai_equilib_water,          //(INPUT)  lai threshold for water senesc
                               float root_depth,                 //(INPUT)  depth of roots (mm)
                               float sw_demand,                  //(INPUT)  total crop demand for water
                               float *sw_supply,                  //(INPUT)  potential water to take up
                               float *dlt_slai_water)             // (OUTPUT) water stress senescense
//===========================================================================

/*Purpose
 *   Return the lai that would senesce on the current day from water stress
 */

  {
   //Local Variables
   float ave_lai_equilib_water;     //running mean lai threshold for water se
   int deepest_layer;               //deepest layer in which the roots are gr
   float sw_demand_ratio;           //water supply:demand ratio
   float sw_supply_sum;             //total supply over profile (mm)

   // calculate senescense from water stress
   // NOTE needs rework for multiple crops

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);
   sw_demand_ratio = divide (sw_supply_sum, sw_demand, 1.0);

   if (sw_demand_ratio < sen_threshold)
      {
      ave_lai_equilib_water = crop_running_ave(day_of_year, year, lai_equilib_water, 10);
      *dlt_slai_water = divide((lai - ave_lai_equilib_water), sen_water_time_const, 0.0);
      *dlt_slai_water = l_bound (*dlt_slai_water, 0.0);
      }
   else
      {
      *dlt_slai_water = 0.0;
      }
   *dlt_slai_water = bound (*dlt_slai_water, 0.0, lai);
   }

//===========================================================================
void crop_leaf_area_sen_light2 (float radn_int,                //(INPUT)
                                float radn,                    //(INPUT)
                                float sen_radn_crit,           //(INPUT)
                                int   year,                    //(INPUT)
                                int   day_of_year,             //(INPUT)
                                float *lai_equilib_light,      //(INPUT)
                                float lai,                     //(INPUT)
                                float sen_light_time_const,    //(INPUT)
                                float *dlt_slai_light)         //(OUTPUT) lai senesced by low light
//===========================================================================

/*Purpose
 *   Return the lai that would senesce on the current day from low light
 */

   {
   //Local Variables
   float ave_lai_equilib_light;  //running mean lai threshold for light
                                 //senescence ()
   float radn_transmitted;       //radn transmitted through canopy (mj/m^2)
   //Implementation

   // calculate senescense from water stress
   radn_transmitted = radn - radn_int;

   if (radn_transmitted < sen_radn_crit)
      {
      ave_lai_equilib_light = crop_running_ave (day_of_year, year, lai_equilib_light, 10);

      *dlt_slai_light = divide (lai - ave_lai_equilib_light, sen_light_time_const , 0.0);
      *dlt_slai_light = l_bound (*dlt_slai_light, 0.0);
      }
   else
      {
      *dlt_slai_light = 0.0;
      }
   *dlt_slai_light = bound (*dlt_slai_light, 0.0, lai);
   }

//===========================================================================
void crop_leaf_area_sen_age1 (int emergence,                   //(INPUT)  emergence stage no.
                              int this_stage,                  //(INPUT)  This current stage
                              float /* g_dlt_lai_stressed*/,        //(INPUT)  potential change in live
                              float /* g_dlt_leaf_no*/,             //(INPUT)  actual fraction of oldest leaf
                              float g_dlt_leaf_no_dead,        //(INPUT)  fraction of oldest green leaf
                              float g_lai,                     //(INPUT)  live plant green lai
                              float *g_leaf_area,              //(INPUT)  leaf area of each leaf (mm^2)
                              float *g_leaf_no_dead,           //(INPUT)  no of dead leaves ()
                              float g_plants,                  //(INPUT)  Plant density (plants/m^2)
                              float g_slai,                    //(INPUT)  area of leaf that senesces fro
                              float c_min_tpla,                //(INPUT)
                              float *dlt_slai_age)             //(OUTPUT) new senesced lai from phasic devel.
//===========================================================================

/*Purpose
 *   Return the lai that would senesce on the current day due to ageing
 */

   {
   //Local Variables
   float area_sen_dying_leaf;    // senesced leaf area from
                                 // current leaf dying (mm^2)
   int dying_leaf;               // current leaf number dying ()
   float leaf_no_dead;           // today's number of dead leaves ()
   float slai_age;               // lai senesced by natural ageing
   float min_lai;                // min allowable LAI
   float max_sen;

   //Implementation Section ----------------------------------

   // now calculate the leaf senescence
   // due to normal phenological (phasic) development

   // get highest leaf no. senescing today

   leaf_no_dead = sum_between (emergence-1, this_stage-1, g_leaf_no_dead)
                  + g_dlt_leaf_no_dead;

   dying_leaf = int (leaf_no_dead);

   // get area senesced from highest leaf no.
   area_sen_dying_leaf = (leaf_no_dead - fmod(leaf_no_dead, 1.0))
                        * g_leaf_area[dying_leaf];

   slai_age = (sum_real_array (g_leaf_area, dying_leaf) + area_sen_dying_leaf)  //XXX maybe?? check
                  * smm2sm * g_plants;

   min_lai = c_min_tpla * g_plants * smm2sm;
   max_sen = l_bound (g_lai - min_lai, 0.0);

   *dlt_slai_age = bound (slai_age - g_slai, 0.0, max_sen);
   }

float crop_leaf_area_sen_light1 (float lai_sen_light,
                                float sen_light_slope,
                                float lai,
                                float plants,
                                float min_tpla)   //(OUTPUT) lai senesced by low light

/*Purpose
 *   Return the lai that would senesce on the current day due to shading
 */
   {
   //Local Variables
   float slai_light_fac;        // light competition factor (0-1)
   float max_sen;
   float min_lai;

   //Implementation Section ----------------------------------

   //calculate 0-1 factor for leaf senescence due to
   //competition for light.

   /* this doesnt account for other growing crops
      should be based on reduction of intercepted light and k*lai
      competition for light factor*/

   if (lai > lai_sen_light)
      {
      slai_light_fac = sen_light_slope * (lai - lai_sen_light);
      }
   else
      {
      slai_light_fac = 0.0;
      }
   min_lai = min_tpla * plants * smm2sm;
   max_sen = l_bound (lai - min_lai, 0.0);

   return (bound (lai * slai_light_fac, 0.0, max_sen));
   }


//===========================================================================
float crop_leaf_area_sen_water1 (float sen_rate_water,    //(INPUT)  slope in linear eqn relating soil wat
                                float lai,               //(INPUT)  live plant green lai
                                float swdef_photo,       //(INPUT)
                                float plants,            //(INPUT)
                                float min_tpla)          //(INPUT)
//===========================================================================

/*Purpose
 *   Return the lai that would senesce on the current day due to water stress
 */

   {
   //Local Variables
   float slai_water_fac;  // drought stress factor (0-1)
   float max_sen;
   float min_lai;
   float dlt_slai_water;

   //Implementation Section ----------------------------------

   // drought stress factor
   slai_water_fac = sen_rate_water * (1.0 - swdef_photo);
   dlt_slai_water = lai * slai_water_fac;
   min_lai = min_tpla * plants * smm2sm;
   max_sen = l_bound (lai - min_lai, 0.0);
   dlt_slai_water = bound (dlt_slai_water, 0.0, max_sen);
   return(dlt_slai_water);
   }
#if 0
//===========================================================================
void cproc_leaf_area_sen1 (int emergence,                 // (INPUT)  emergence stage no.
                           int this_stage,                // (INPUT)  This current stage
                           float g_dlt_lai_stressed,      // (INPUT)  potential change in live
                           float g_dlt_leaf_no,           // (INPUT)  actual fraction of oldest leaf
                           float g_dlt_leaf_no_dead,      // (INPUT)  fraction of oldest green leaf
                           float g_lai,                   // (INPUT)  live plant green lai
                           float *g_leaf_area,            // (INPUT)  leaf area of each leaf (mm^2)
                           float *g_leaf_no_dead,         // (INPUT)  no of dead leaves ()
                           float g_plants,                // (INPUT)  Plant density (plants/m^2)
                           float g_slai,                  // (INPUT)  area of leaf that senesces fro
                           float c_min_tpla,              // (INPUT)
                           float *g_dlt_slai_age,         // (OUTPUT) new senesced lai from phasic devel.
                           float c_lai_sen_light,         // (INPUT)
                           float c_sen_light_slope,       // (INPUT)
                           float *g_dlt_slai_light,       // (OUTPUT)
                           float c_sen_rate_water,        // (INPUT)
                           float g_swdef_photo,           // (INPUT)
                           float *g_dlt_slai_water,       // (OUTPUT)
                           float *cXTempSenescence,       // (INPUT)
                           float *cYSenescenceFac,        // (INPUT)
                           int c_num_temp_senescence,     // (INPUT)
                           float g_mint,                  // (INPUT)
                           float *g_dlt_slai_frost,       // (OUTPUT)
                           float *g_dlt_slai)             // (OUTPUT)
//===========================================================================

/*Purpose
 *   Return the lai that would senesce on the current day, being the maximum
 *   of lai senescence due to ageing, shading, water stress or frost.
 */
   {
   //Local variables
   float max1;
   float max2;

   //Implementation Section ----------------------------------

   crop_leaf_area_sen_age1 (emergence, this_stage, g_dlt_lai_stressed,
                          g_dlt_leaf_no, g_dlt_leaf_no_dead, g_lai,
                          g_leaf_area, g_leaf_no_dead, g_plants, g_slai,
                          c_min_tpla, g_dlt_slai_age);

   *g_dlt_slai_light = crop_leaf_area_sen_light1 (c_lai_sen_light, c_sen_light_slope, g_lai,
                            g_plants, c_min_tpla);

   *g_dlt_slai_water = crop_leaf_area_sen_water1 (c_sen_rate_water, g_lai, g_swdef_photo,
                                                 g_plants, c_min_tpla);

   *g_dlt_slai_frost = crop_leaf_area_sen_frost1 (cXTempSenescence, cYSenescenceFac,
                              c_num_temp_senescence, g_lai, g_mint,
                              g_plants, c_min_tpla);

   max1 = max(*g_dlt_slai_age, *g_dlt_slai_light);

   max2 = max(*g_dlt_slai_water, *g_dlt_slai_frost);

   *g_dlt_slai = max(max1, max2);
   }
#endif
//===========================================================================
void cproc_leaf_area_init1 (float c_initial_tpla,     //(INPUT)  initial plant leaf area (mm^2)
                            int   init_stage,         //(INPUT)  initialisation stage
                            float g_current_stage,    //(INPUT)  current phenological stage
                            float */* g_days_tot*/,        // (INPUT)  duration of each phase (days)
                            float  g_plants,          // (INPUT)  Plant density (plants/m^2)
                            float *lai)               //(OUTPUT) total plant leaf area
//===========================================================================

/*Purpose
 *   Initialise leaf area.
 */

   {
   if (on_day_of (init_stage, g_current_stage))
      {
      *lai = c_initial_tpla * smm2sm * g_plants;
      }
   }

//===========================================================================
void cproc_lai_detachment1 (float c_sen_detach_frac,           //(INPUT)
                            float g_slai,                       //(INPUT)
                            float *g_dlt_slai_detached)    //(OUTPUT)
//==========================================================================

/*Purpose
 *   Simulate plant detachment.
 */
  {
   crop_part_fraction_delta(c_sen_detach_frac, g_slai, g_dlt_slai_detached);
   
  }

//===========================================================================
void cproc_canopy_height (float g_canopy_height,          // (INPUT)  canopy height (mm)
                          float *pXStemWt,                //(INPUT)
                          float *pYHeight,                // (INPUT)
                          int   p_num_stem_wt,            // (INPUT)
                          float *g_dm_green,              //(INPUT)  live plant dry weight (biomass
                          float g_plants,                 // (INPUT)  Plant density (plants/m^2)
                          int   stem,                     // (INPUT)  plant part no for stem
                          float *dlt_canopy_height)       //(OUTPUT) canopy height change (mm)
//===========================================================================

/*Purpose
 *   Get change in plant canopy height from stem dry matter per plant
 */

   {
   //Local V ariables
   float dm_stem_plant;          //dry matter of stem (g/plant)
   float new_height;             //new plant height (mm)

   dm_stem_plant = divide (g_dm_green[stem], g_plants, 0.0);
   new_height = linear_interp_real(dm_stem_plant, pXStemWt, pYHeight,
                                    p_num_stem_wt);

   *dlt_canopy_height = new_height - g_canopy_height;
   *dlt_canopy_height = l_bound(*dlt_canopy_height, 0.0);
   }

//=============== ============================================================
void cproc_leaf_no_init1 (float c_leaf_no_at_emerg,       //(INPUT)  leaf number at emergence ()
                          float g_current_stage,          //(INPUT)  current phenological stage
                          int   emerg,                    //(INPUT)  emergence stage no
                          float */* g_days_tot*/,               //(INPUT)  duration of each phase (days)
                          float *leaf_no,                  //(OUTPUT) initial leaf number
                          float *node_no)                  //(OUTPUT) initial node number
//===========================================================================

/*Purpose
 *   Return the the initial number of leaves at emergence.
 */

   {
   //Implementation Section ----------------------------------

   if (on_day_of (emerg, g_current_stage))
      {
      //initialise first leaves
      leaf_no[emerg-1] = c_leaf_no_at_emerg;
      node_no[emerg-1] = c_leaf_no_at_emerg;       /////XXX  really???
      }

   }

/*Purpose
 *   Return the fractional increase in emergence of the oldest
 *   expanding leaf and nodes.  Nodes can initiate from a user-defined
 *   starting stage and leaves from emergence.  The initiation of both
 *   leaves and nodes finishes at a user-defined end stage.
 *   Note ! this does not take account of the other younger leaves
 *   that are currently expanding
 */
void cproc_leaf_no_pot1 (interpolationFunction &node_app_rate_fn,          // (INPUT)
                         interpolationFunction &leaves_per_node_fn,
                         bool   inNodeFormationPhase,
                         bool   inEmergenceDay,
                         float  node_no_now,                // (INPUT) current number of nodes
                         float  g_dlt_tt,                   // (input)
                         float *dlt_leaf_no_pot,            // (OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot)            // (OUTPUT) new fraction of oldest expanding node on main stem
   {
   if (inNodeFormationPhase)
      {
      float node_app_rate = node_app_rate_fn.value(node_no_now);
      *dlt_node_no_pot = divide (g_dlt_tt, node_app_rate, 0.0);
      }
   else
      {
      *dlt_node_no_pot = 0.0;
      }

   if (inEmergenceDay)
      {
      //no leaf growth on first day because initialised elsewhere ???
      *dlt_leaf_no_pot = 0.0;
      }
   else if (inNodeFormationPhase)
      {
      float leaves_per_node = leaves_per_node_fn.value(node_no_now);
      *dlt_leaf_no_pot = *dlt_node_no_pot * leaves_per_node;
      }
   else
      {
      *dlt_leaf_no_pot = 0.0;
      }
   }
#if 0

//===========================================================================
float cproc_leaf_area_pot1 (interpolationFunction &leaf_size_fn, //(INPUT)  leaf size for lookup
                           float  g_node_no,                     //(INPUT)  node number
                           float  c_node_no_correction,          //(INPUT)  corrects for other growing lea
                           float  g_dlt_leaf_no_pot,             //(INPUT)  potential fraction of oldest l
                           float  g_plants)                      //(INPUT)  Plant density (plants/m^2)
//===========================================================================

/*Purpose
 *   Return the potential increase in leaf area development (mm^2)
 *   calculated on an individual leaf basis, limited by temperature
 *   only, with account taken of the area of currently expanding leaves
 *   (node_no_correction).
 */
   {
   //Local Variables
   float node_no_now;
   float leaf_size;

   node_no_now = g_node_no + c_node_no_correction;

   leaf_size = leaf_size_fn.value (node_no_now);

   return (g_dlt_leaf_no_pot * leaf_size * smm2sm * g_plants);
   }

//===========================================================================
void cproc_leaf_area_actual1 (interpolationFunction &sla_max_fn,
                              float  dlt_dm_leaf,        //(INPUT)  leaf biomass growth (g/m^2)
                              float *g_dlt_lai,          //(OUTPUT)  actual change in live plant la
                              float  g_dlt_lai_stressed, //(INPUT)  potential change in live
                              float  g_lai)              //(INPUT)  live plant green lai
//===========================================================================

/*Purpose
 *   Simulate actual crop leaf area development - checks that leaf area
 *   development matches D_m production via a maximum specific leaf area
 *   for the daily increase in LAI. SLA_max changes as a function of LAI.
 */

   {
   //Local Variables
   float sla_max;                //calculated daily max spec leaf area
   float dlt_lai_carbon;         //maximum daily increase in leaf area
                                 //index from carbon supply

   //Implementation Section ----------------------------------
   sla_max = sla_max_fn.value (g_lai);

   dlt_lai_carbon = dlt_dm_leaf * sla_max * smm2sm;
   *g_dlt_lai = min(g_dlt_lai_stressed, dlt_lai_carbon);
   }
#endif

//===========================================================================
void cproc_leaf_no_pot2 (float *c_x_node_no_app,       //(INPUT)
                         float *c_y_node_app_rate,     //(INPUT)
                         float *c_y_leaves_per_node,   //(INPUT)
                         int c_num_node_no_app,       //(INPUT)
                         float g_current_stage,       //(INPUT)  current phenological stage
                         int start_node_app,          //(INPUT)  stage of start of leaf appeara
                         int end_node_app,            //(INPUT)  stage of end of leaf appearanc
                         int emerg,                   //(INPUT)  emergence stage
                         float */* g_days_tot*/,            //(INPUT)  duration of each phase (days)
                         float g_dlt_tt,              //(INPUT)  daily thermal time (growing de
                         float *g_node_no,             //(INPUT)  number of fully expanded nodes
                         float *dlt_leaf_no_pot,       //(OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot)       //(OUTPUT) new fraction of oldest expanding node on main stem
//===========================================================================

/*Purpose
 *   Return the fractional increase in emergence of the oldest
 *   expanding leaf and nodes.  Nodes can initiate from a user-defined
 *   starting stage and leaves from emergence.  The initiation of both
 *   leaves and nodes finishes at a user-defined end stage.
 *   Note ! this does not take account of the other younger leaves
 *   that are currently expanding
 */

   {
   //Local Variables
   float node_no_now;           // number of fully expanded nodes
   float leaves_per_node;
   float node_app_rate;

   //Implementation Section ----------------------------------

   node_no_now = sum_between (start_node_app-1, end_node_app-1, g_node_no);

   node_app_rate = linear_interp_real(node_no_now, c_x_node_no_app,
                                      c_y_node_app_rate, c_num_node_no_app);

   leaves_per_node = linear_interp_real(node_no_now, c_x_node_no_app,
                                        c_y_leaves_per_node, c_num_node_no_app);

   if (stage_is_between (start_node_app, end_node_app, g_current_stage))
      {
      *dlt_node_no_pot = divide (g_dlt_tt, node_app_rate, 0.0);
      }
   else
      {
      *dlt_node_no_pot = 0.0;
      }

   if (on_day_of (emerg, g_current_stage))
      {
      // no leaf growth on first day because initialised elsewhere ???
      *dlt_leaf_no_pot = 0.0;
      }
   else if (stage_is_between (emerg, end_node_app, g_current_stage))
      {
      *dlt_leaf_no_pot = *dlt_node_no_pot * leaves_per_node;
      }
   else
      {
      *dlt_leaf_no_pot = 0.0;
      }
   }

//===========================================================================
void cproc_tpla_max (float g_leaf_no_final,            //(INPUT)final leaf number
                     float g_tiller_no_fertile,        //(INPUT)number of fertile tillers
                     float c_tiller_coef,              //(INPUT)tiller coefficient on TPLAmax
                     float p_main_stem_coef,           //(INPUT)main stem coefficient on TPLAmax
                     float *tpla_max)                  //(OUTPUT) maximum total plant leaf area mm^2
//===========================================================================

/*Purpose
 *   Return the maximum total plant leaf area (mm^2)
 *   Used for sorghum
 */

   {
   if (g_leaf_no_final < 1.0)
      {
      *tpla_max = 0.0;
      }
   else
      {
      *tpla_max = (pow(g_tiller_no_fertile + 1.0, c_tiller_coef) *
                   pow(g_leaf_no_final, p_main_stem_coef)) * scm2smm;
      }
   }

//===========================================================================
void cproc_leaf_area_pot_tpla (int  begin_stage,                 // (INPUT)  stage number of start leaf area growth
                               int  end_stageTPLAPlateau,        // (INPUT)  stage number to stop TPLA growth
                               int  now,                         // (INPUT)  stage number now = max_stage + 1
                               float *g_phase_tt,                // (INPUT)  required thermal time between stages
                               float *g_tt_tot,                  // (INPUT)  elapsed thermal time between stages
                               float */* g_days_tot*/,                // (INPUT)  elapsed days between stages
                               float  g_current_stage,           // (INPUT)  current stage
                               float  c_initial_tpla,            // (INPUT)  initial total plant area (mm2)
                               float  tpla_max,                  // (INPUT)  maximum total plant area (mm2)
                               float  c_tpla_inflection_ratio,   // (INPUT)  fraction of thermal time from begin to
                                                                 //          end leaf area growth where inflexion on
                                                                 //          TPLA occurs
                               float  *g_tpla_today,              // (OUTPUT)  today's total leaf area per plant (mm2)
                               float  g_tpla_yesterday,          // (INPUT)  yesterday's TPLA (mm2)
                               float  p_tpla_prod_coef,          // (INPUT)  TPLA production coefficient (initial slope)
                               float  g_plants,                  // (INPUT)  Plant density (plants/m2)
                               float  *g_lai,                     // (INPUT)  current leaf area index()
                               float *g_dlt_lai_pot)             // (OUTPUT) change in leaf area
//===========================================================================

/*Purpose
 *   Return the potential increase in leaf area development (mm^2)
 *   calculated on a whole plant basis as determined by thermal time
 *   Used for sorghum
 */

  {
   //Local Variables
   float tt_since_begin;         // deg days since begin TPLA Period
   float tpla_inflection;        // inflection adjusted for leaf no.
   float tt_begin_to_endTPLA;    // thermal time for TPLA period
   float temp;                   // temp var to store composite args to functions
                                 // expecting pointers

   //Implementation Section ----------------------------------

   //once leaf no is calculated maximum plant leaf area is determined
   if (on_day_of (begin_stage, g_current_stage))
      {
      *g_lai = c_initial_tpla * smm2sm * g_plants;
      }
   if (stage_is_between (begin_stage, end_stageTPLAPlateau, g_current_stage)
         && g_phase_tt[end_stageTPLAPlateau-1] > 0.0)
      {
      tt_begin_to_endTPLA = sum_between(begin_stage-1, end_stageTPLAPlateau-1, g_phase_tt);
      tt_since_begin = sum_between (begin_stage-1, now-1, g_tt_tot);

     /*  ! scc 10/95 fixing the beta inflection coefficient as halfway to thermal
         ! time of flag_leaf expanded. Code needs work as the halfway point jumps
         ! around a bit as we progress (espec. when final_leaf_no is reset at floral in
         ! Note that tpla_inflection needs to be removed as a 'read-in' parameter
         ! maybe the number is more like .66 of the distance?
         ! can work out from the shape of a leaf area distribution - where is the biggest
         ! leaf appearing...

         !  scc - generalise tpla_inflection  - needs more work
     */

      tpla_inflection = tt_begin_to_endTPLA * c_tpla_inflection_ratio;

      //scc end of changes for tpla (more below)
      temp = 1.0 + exp(-1 * p_tpla_prod_coef * (tt_since_begin - tpla_inflection));
      *g_tpla_today = divide (tpla_max, temp, 0.0);

      if (*g_tpla_today <  g_tpla_yesterday)
         {
         *g_tpla_today = g_tpla_yesterday;
         }
      *g_dlt_lai_pot = (*g_tpla_today - g_tpla_yesterday)* smm2sm * g_plants;
      }
   else  //_beyond TPLA growth stage
      {
      *g_dlt_lai_pot = 0.0;
      }
   }

//===========================================================================
float leaf_size_bellshapecurve (float c_x0_const,
                                float c_x0_slope,
                                float g_leaf_no_final,
                                float c_y0_const,
                                float c_y0_slope,
                                float cAConst,
                                float cASlope1,
                                float cASlope2,
                                float cBConst,
                                float cBSlope1,
                                float cBSlope2,
                                float leaf_no)         //(INPUT) nominated leaf number
//===========================================================================

/*Purpose
 *   Return the leaf area (mm^2) of a specified leaf no.
 */

   {
   //Local Variables
   float area;             // potential area of nominated leaf no (mm^2)
   float area_max;         // potential area of largest leaf (mm^2)
   float breadth;          // breadth coef of leaf
   float largest_leaf;     // leaf no of largeat leaf
   float skewness;         // skewness coef of leaf
                                 // expecting pointers

   //Implementation Section ----------------------------------

     /*  Once leaf no is calculated leaf area of largest expanding leaf
         is determined with quadratic relationship. Coefficients for this
         curve are functions of total leaf no.
     */

   largest_leaf = c_x0_const + (c_x0_slope * g_leaf_no_final);
   area_max = c_y0_const + (c_y0_slope * g_leaf_no_final);

   breadth  = cAConst + divide (cASlope1,
                                1.0 + cASlope2 * g_leaf_no_final, 0.0);

   skewness = cBConst + divide (cBSlope1,
                                 1.0 + cBSlope2 * g_leaf_no_final, 0.0);

   area = area_max * exp (breadth * pow((leaf_no - largest_leaf), 2)
                           + skewness * pow((leaf_no - largest_leaf),3));

   return area;
   }

//===========================================================================
void cproc_leaf_area_pot_bellshapecurve (int  begin_stage,             //
                                         int  now,                     //
                                         float *g_leaf_no,             //
                                         float  c_leaf_no_correction,  //
                                         float  c_x0_const,            //
                                         float  c_x0_slope,            //
                                         float  g_leaf_no_final,       //
                                         float  c_y0_const,            //
                                         float  c_y0_slope,            //
                                         float  cAConst,               //
                                         float  cASlope1,              //
                                         float  cASlope2,              //
                                         float  cBConst,               //
                                         float  cBSlope1,              //
                                         float  cBSlope2,              //
                                         float  g_dlt_leaf_no,         //
                                         float  g_plants,              //
                                         float  /* g_swdef_expansion*/,     //
                                         float *dlt_lai_pot)           // (OUTPUT) change in leaf area
//===========================================================================
/*Purpose
 *   Return the potential increase in LAI development (mm^2)
 *   calculated on an individual leaf basis.
 *   Used for Maize
 */

   {
   //Local Variables
   float area;                   //potential maximum area of oldest expanding
                                 //leaf (mm^2) in today's conditions
   float leaf_no_effective;      //effective leaf no - includes
                                 //younger leaves that have emerged
                                 //after the current one

   //Implementation Section ----------------------------------

   //once leaf no is calculated leaf area of largest
   //expanding leaf is determined

   //glh This should also be from sowing, as above? (changed from emerg (scc))

   leaf_no_effective = sum_between (begin_stage-1, now-1, g_leaf_no)
                          + c_leaf_no_correction;

   area = leaf_size_bellshapecurve (c_x0_const, c_x0_slope, g_leaf_no_final,
                  c_y0_const, c_y0_slope, cAConst, cASlope1, cASlope2,
                  cBConst, cBSlope1, cBSlope2, leaf_no_effective);

   *dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants;

   }

//---------------------------------------------------------------------------


//+  Purpose
//     Calculate extinction coefficient as a function of row spacing

//+  Mission Statement
//     Calculate extinction coefficient as a function of row spacing

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
void legnew_extinct_coef
    (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float *extinct_coef      //OUTPUT
    ) {
    *extinct_coef = linear_interp_real (g_row_spacing
                                        ,c_x_row_spacing
                                        ,c_y_extinct_coef
                                        ,c_num_row_spacing);
    }


//+  Purpose
//     Calculate crop cover

//+  Mission Statement
//     Calculate crop cover

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
//     10-02-1999 - huth - added pod cover component
void legnew_cover (
    float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float canopy_fac                        // skip row factor
    ,float g_lai
    ,float *g_cover_green                    //OUTPUT
    ) {

//+  Constant Values
//+  Local Variables
    float extinct_coef;
    float lai_canopy;                             // lai transformed to solid canopy
    float cover_green_leaf_canopy;                // green leaf cover in canopy

//- Implementation Section ----------------------------------
    if (g_lai > 0.0)
        {
        extinct_coef = linear_interp_real (g_row_spacing
                                           ,c_x_row_spacing
                                           ,c_y_extinct_coef
                                           ,c_num_row_spacing);

//-----light interception modified to give hedgerow effect with skip row

        lai_canopy = g_lai * canopy_fac;          // lai in hedgerow
                                                  // interception on row area basis
        cover_green_leaf_canopy = 1.0 - exp(-extinct_coef*lai_canopy) ;
        *g_cover_green = divide (cover_green_leaf_canopy, canopy_fac
        , 0.0)             ;                      // interception on ground area basis

        }
    else
        {
        *g_cover_green = 0.0;
        }
    }


//+  Purpose
//     Calculate crop cover due to leaf and pod light interception

//+  Mission Statement
//     Calculate crop cover due to leaf and pod light interception

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
//     10-02-1999 - huth - added pod cover component

//+  Purpose
//       Derives number of nodes to result in given cumulative area

//+  Mission Statement
//     Get the number of nodes for a given cumulative area

//+  Changes
//       110298 nih specified and programmed
float legnew_node_no_from_area
    (
     float *g_leaf_area         // (INPUT)  leaf area of each leaf (mm^2)
    ,int   num_nodes            // (INPUT)  number of nodes
    ,float pla                  // (INPUT)  plant leaf area
    ) {
    //+  Local Variables
    int   node_no;                                    // number of nodes containing leaf area (0-max_node)
    float node_area_whole;                            // number of complete nodes ()
    float node_area_part;                             // area from last node (mm^2)
    float node_fract;                                 // fraction of last node (0-1)
    float result;

    node_no = get_cumulative_index_real (pla, g_leaf_area, num_nodes);

    node_area_whole = sum_real_array (g_leaf_area, node_no);
    node_area_part = pla - node_area_whole;
    node_fract = divide (node_area_part, g_leaf_area[node_no], 0.0);

    result = (float) (node_no - 1) + node_fract;
    return (result);
    }


//+  Purpose
//       Derives number of leaves to result in given cumulative area

//+  Mission Statement
//     Gets the number of leaves for given area

//+  Changes
//       110298 nih specified and programmed
float legnew_leaf_no_from_area (
     float  *g_leaf_area                // (INPUT)  leaf area of each leaf (mm^2)
    ,float  *g_leaf_no
    ,int    num_nodes                   // (INPUT)  number of nodes
    ,float  pla                         // (INPUT)  plant leaf area
    ) {
    //+  Local Variables
    int   node_no;                                    // number of nodes containing
    // leaf area (0-max_node)
    float node_area_whole;                            // number of complete nodes ()
    float node_area_part;                             // area from last node (mm^2)
    float node_fract;                                 // fraction of last node (0-1)
    float result;


    node_no = 1+get_cumulative_index_real (pla, g_leaf_area, num_nodes);

    node_area_whole = sum_real_array (g_leaf_area, node_no-1);
    node_area_part = pla - node_area_whole;
    node_fract = divide (node_area_part, g_leaf_area[node_no-1], 0.0);

    result = sum_real_array (g_leaf_no,node_no) + node_fract * g_leaf_no[node_no-1];
//     fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f,%.9f\n",
//             g.day_of_year, (float)node_no,
//             node_area_whole, node_fract, sum_real_array (g_leaf_no,node_no), result);
//     fprintf(stdout, "%d,%.9f,%.9f,%.9f\n",
//             g.day_of_year,g_leaf_no[0], g_leaf_no[1], g_leaf_no[2]);
    return(result);
    }


//------------
//+  Purpose
//     Calculate crop cover

//+  Mission Statement
//     Calculate crop cover

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
//     10-02-1999 - huth - added pod cover component
void legnew_canopy_fac (
     float  g_row_spacing
    ,float  g_plants
    ,float  g_skip_row_fac              // skip row factor
    ,float  g_skip_plant_fac            // skip plant factor
    ,float  g_canopy_width
    ,float  *g_canopy_fac
    ) {


//+  Local Variables
    float area_actual;
    float area_avail;
    float plant_space;
    float radius_intra_row_solid;
    float radius_intra_row_skip;
    float width_intra_row;
    float radius_inter_row_solid;
    float radius_inter_row_skip;
    float width_inter_row;
    float row_spacing_effective;
    float plant_spacing_effective;


    row_spacing_effective = g_row_spacing * g_skip_row_fac;
    plant_space = divide (sm2smm
       , row_spacing_effective * g_plants * g_skip_plant_fac
       , 0.0);
    plant_spacing_effective = plant_space * g_skip_plant_fac;
    radius_intra_row_solid = min(plant_space * 0.5
      , g_canopy_width * 0.5);
    radius_intra_row_skip = min(plant_space * (g_skip_plant_fac - 0.5)
      , g_canopy_width * 0.5);

    width_intra_row = radius_intra_row_skip + radius_intra_row_solid;

    radius_inter_row_solid = min(g_row_spacing * 0.5, g_canopy_width * 0.5);
    radius_inter_row_skip = min(g_row_spacing * (g_skip_row_fac - 0.5), g_canopy_width * 0.5);
    width_inter_row = radius_inter_row_solid + radius_inter_row_skip;

    area_avail = plant_spacing_effective * row_spacing_effective;
    area_actual = width_inter_row * width_intra_row;
    *g_canopy_fac = divide (area_avail, area_actual, 0.0);

    }
//+  Purpose
//       Get change in plant canopy width from stem dry matter per plant

//+  Mission Statement
//   Calculate change in crop canopy width (based upon weight of %7).

//+  Changes
//       230498 nih specified and programmed
void plant_canopy_width
    (
     float  g_canopy_width                   // (INPUT)  canopy height (mm)
    ,float  *p_x_stem_wt
    ,float  *p_y_width
    ,int    p_num_stem_wt
    ,float  *g_dm_green                      // (INPUT)  live plant dry weight (biomass
    ,float  g_plants                         // (INPUT)  Plant density (plants/m^2)
    ,int    stem                             // (INPUT)  plant part no for stem
    ,float  *dlt_canopy_width                 // (OUTPUT) canopy width change (mm)
    ) {
    float dm_stem_plant;                          // dry matter of stem (g/plant)
    float new_width;                              // new plant width (mm)

    dm_stem_plant = divide (g_dm_green[stem], g_plants, 0.0);
    new_width = linear_interp_real(dm_stem_plant
                                  ,p_x_stem_wt
                                  ,p_y_width
                                  ,p_num_stem_wt);

    *dlt_canopy_width = new_width - g_canopy_width;
    *dlt_canopy_width = l_bound (*dlt_canopy_width, 0.0);

    }


//+  Purpose
//       Return the fractional increase in emergence of the oldest
//       expanding leaf and nodes.  Nodes can initiate from a user-defined
//       starting stage and leaves from emergence.  The initiation of both
//       leaves and nodes finishes at a user-defined end stage.
//       Note ! this does not take account of the other younger leaves
//       that are currently expanding

//+  Mission Statement
//   Calculate the potential increase in plant leaf and node number

//+  Changes
//       270598 nih specified and programmed
void cproc_leaf_no_pot3(
     interpolationFunction &node_app_rate_fn                //(INPUT)
    ,interpolationFunction &leaves_per_node_fn              // (INPUT)
    ,bool   inNodeFormationPhase
    ,bool   inEmergenceDay
    ,float  node_no_now                 // (INPUT) current number of nodes
    ,float  g_dlt_tt                          // (INPUT)  daily thermal time (growing de
    ,float  stressFactor
    ,float  *g_leaves_per_node                 // OUTPUT
    ,float  *dlt_leaf_no_pot                   // (OUTPUT) new fraction of oldest expanding leaf
    ,float  *dlt_node_no_pot                   // (OUTPUT) new fraction of oldest expanding node on main stem
    ) {
//+  Local Variables
    float dlt_leaves_per_node;
    float node_app_rate;
    float leaves_per_node_now;

//- Implementation Section ----------------------------------


    if (inNodeFormationPhase)
        {
        node_app_rate = node_app_rate_fn.value(node_no_now);
        *dlt_node_no_pot = divide (g_dlt_tt, node_app_rate, 0.0);
        }
    else
        {
        *dlt_node_no_pot = 0.0;
        }

    if (inEmergenceDay)
        {
        // no leaf growth on first day because initialised elsewhere ???
        *dlt_leaf_no_pot = 0.0;

        *g_leaves_per_node = leaves_per_node_fn.value(node_no_now);
        }
    else if (inNodeFormationPhase)
        {
        // NIH - bit scary using dlt_node_no_POT
        // so make sure that we don't get ahead of ourselves
        // of node number does not increase at potential rate

        leaves_per_node_now = leaves_per_node_fn.value(node_no_now);

        *g_leaves_per_node = min(*g_leaves_per_node, leaves_per_node_now);

        dlt_leaves_per_node = leaves_per_node_fn.value(node_no_now + *dlt_node_no_pot)
                                 - leaves_per_node_now;

        *g_leaves_per_node = (*g_leaves_per_node) + dlt_leaves_per_node * stressFactor;

        *dlt_leaf_no_pot = (*dlt_node_no_pot) * (*g_leaves_per_node);
        }
    else
        {
        *dlt_leaf_no_pot = 0.0;
        }
    }

#if 0
//+  Purpose
//       Return the lai that would senesce on the
//       current day.

//+  Mission Statement
//   Calculate today's leaf area senescence

//+  Changes
//     200498 nih specified and programmed
void legopt_leaf_area_sen1
    (float  g_dlt_lai_stressed               // (INPUT)  potential change in live
    ,float  g_dlt_leaf_no                    // (INPUT)  actual fraction of oldest leaf
    ,float  g_dlt_leaf_no_dead               // (INPUT)  fraction of oldest green leaf
    ,float  g_lai                            // (INPUT)  live plant green lai
    ,float  *g_leaf_area                     // (INPUT)  leaf area of each leaf (mm^2)
    ,float  *g_leaf_no
    ,float  *g_leaf_no_dead                  // (INPUT)  no of dead leaves ()
    ,int    max_node
    ,float  g_plants                         // (INPUT)  Plant density (plants/m^2)
    ,float  g_slai                           // (INPUT)  area of leaf that senesces fro
    ,float  c_min_tpla                       // (INPUT)
    ,float  *g_dlt_slai_age                   // (OUTPUT) new senesced lai from phasic devel.
    ,float  c_lai_sen_light                  // (INPUT)
    ,float  c_sen_light_slope                // (INPUT)
    ,float  *g_dlt_slai_light                 // (OUTPUT)
    ,float  c_sen_rate_water                 // (INPUT)
    ,float  g_swdef_photo                    // (INPUT)
    ,float  *g_dlt_slai_water                 // (OUTPUT)
    ,float  *c_x_temp_senescence             // (INPUT)
    ,float  *c_y_senescence_fac              // (INPUT)
    ,int    c_num_temp_senescence            // (INPUT)
    ,float  g_mint                           // (INPUT)
    ,float  *g_dlt_slai_frost                 // (OUTPUT)
    ,float  *g_dlt_slai                       // (OUTPUT)
    ) {

    //- Implementation Section ----------------------------------
    *g_dlt_slai_age = legopt_leaf_area_sen_age1( g_leaf_no
                              , g_leaf_no_dead
                              , g_dlt_leaf_no_dead
                              , max_node
                              , g_lai
                              , g_slai
                              , c_min_tpla
                              , g_leaf_area
                              , g_plants);


    *g_dlt_slai_light = crop_leaf_area_sen_light1(c_lai_sen_light
                              , c_sen_light_slope
                              , g_lai
                              , g_plants
                              , c_min_tpla);

    *g_dlt_slai_water = crop_leaf_area_sen_water1 (c_sen_rate_water,
                               g_lai,
                               g_swdef_photo,
                               g_plants,
                               c_min_tpla);

    *g_dlt_slai_frost = crop_leaf_area_sen_frost1(c_x_temp_senescence,
                              c_y_senescence_fac,
                              c_num_temp_senescence,
                              g_lai,
                              g_mint,
                              g_plants,
                              c_min_tpla);
//    fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n",
//                g.day_of_year,*g_dlt_slai_age, *g_dlt_slai_light, *g_dlt_slai_water, *g_dlt_slai_frost);

    *g_dlt_slai = max(max(max(*g_dlt_slai_age, *g_dlt_slai_light), *g_dlt_slai_water), *g_dlt_slai_frost);

    }

//+  Purpose
//       Initialise leaf area.

//+  Mission Statement
//   Initialise plant leaf area (on first day of %2)

//+  Changes
//     210498 nih specified and programmed
void legopt_leaf_area_init1(float  c_initial_tpla              // (INPUT)  initial plant leaf area (mm^2)
                           ,float  c_leaf_no_at_emerg          //
                           ,float  g_plants                    // (INPUT)  Plant density (plants/m^2)
                           ,float  *lai                        // (OUTPUT) total plant leaf area
                           ,float  *leaf_area)                  // (OUTPUT)
    {

    *lai = c_initial_tpla * smm2sm * g_plants;

    fill_real_array (leaf_area,
                     divide(c_initial_tpla,c_leaf_no_at_emerg,0.0),
                     (int)c_leaf_no_at_emerg);
    leaf_area[(int)c_leaf_no_at_emerg] =                      //ok
          divide(fmod(c_leaf_no_at_emerg, 1.0) * c_initial_tpla
                 ,c_leaf_no_at_emerg, 0.0);

    }


//+  Purpose
//       Return the the initial number of leaves.

//+  Mission Statement
//   Initialise leaf number (on first day of %3)

//+  Notes
//    NIH - I would prefer to use leaf_no_at_init and init_stage
//          for routine parameters for generalisation

//+  Changes
//       250598 nih specified and programmed
void legopt_leaf_no_init1
    (
     float  c_leaf_no_at_emerg          // (INPUT)  leaf number at emergence ()
    ,float  *leaf_no                    // (OUTPUT) initial leaf number
    ,float  *node_no)                    // (OUTPUT) initial node number
   {
   *node_no = c_leaf_no_at_emerg;
   fill_real_array (leaf_no, 1.0, int(c_leaf_no_at_emerg));
   leaf_no[(int)c_leaf_no_at_emerg]= fmod(c_leaf_no_at_emerg,1.0);
   }
#endif



//---------------------------------------------------------------------------
// Calculate the leaf senescence
// due to normal phenological (phasic, age) development
//---------------------------------------------------------------------------
float legopt_leaf_area_sen_age1
    (float  *g_leaf_no
    ,float  *g_leaf_no_dead                     // (INPUT)  no of dead leaves ()
    ,float  g_dlt_leaf_no_dead                  // (INPUT)  fraction of oldest green leaf
    ,int    max_node
    ,float  g_lai                               // (INPUT)  live plant green lai
    ,float  g_slai                              // (INPUT)  area of leaf that senesces fro
    ,float  c_min_tpla                          // (INPUT)
    ,float  *g_leaf_area                        // (INPUT)  leaf area of each leaf (mm^2)
    ,float  g_plants)                            // (INPUT)  Plant density (plants/m^2)
    {
    float dlt_slai_age = 0.0;                     // return value
    float area_sen_dying_node;                    // senesced leaf area from current node dying (mm^2)
    int   dying_node;                             // current node number dying ()
    float leaf_no_dead;                           // today's number of dead leaves ()
    float slai_age;                               // lai senesced by natural ageing
    float min_lai;                                // min allowable LAI
    float max_sen;


    // get highest leaf no. senescing today
    leaf_no_dead = sum_real_array (g_leaf_no_dead, max_node) + g_dlt_leaf_no_dead;
    dying_node = get_cumulative_index_real (leaf_no_dead, g_leaf_no, max_node);

    // get area senesced from highest leaf no.
    if (dying_node >= 0)
        {
        area_sen_dying_node = divide ( leaf_no_dead - sum_real_array(g_leaf_no, dying_node)
                                      , g_leaf_no[dying_node]
                                      , 0.0) * g_leaf_area[dying_node];

        slai_age = (sum_real_array (g_leaf_area, dying_node)
                      + area_sen_dying_node)
                      * smm2sm * g_plants;

        min_lai = c_min_tpla * g_plants * smm2sm;
        max_sen = l_bound (g_lai - min_lai, 0.0);
        dlt_slai_age = bound (slai_age - g_slai, 0.0, max_sen);
        }
    return dlt_slai_age;
    }



void plant_leaf_detachment (float *leaf_area           // OUT
                                   ,float dlt_slai_detached   // IN
                                   ,float plants              // IN
                                   , int max_node)            //IN
    {
//+  Local Variables
    float area_detached;                          // (mm2/plant)
    int   node;

//- Implementation Section ----------------------------------


    area_detached = dlt_slai_detached / plants * sm2smm;

    for (node = 0; node < max_node; node++)
      {
      if(area_detached > leaf_area[node])
        {
        area_detached = area_detached - leaf_area[node];
        leaf_area[node] = 0.0;
        }
      else
        {
        leaf_area[node] = leaf_area[node] - area_detached;
        break;
        }
      }

    
    }
