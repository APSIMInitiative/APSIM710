#include "StdPlant.h"

//+  Purpose
//       Derives seneseced plant nitrogen (g N/m^2)

//+  Mission Statement
//   Calculate change in senesced plant Nitrogen

//+  Changes
//       121297 nih specified and programmed
void legnew_n_senescence1
    (
     int    num_part               // (INPUT) number of plant part
    ,float  *c_n_sen_conc          // (INPUT)  N concentration of senesced materia  (g/m^2)
    ,float  *g_dlt_dm_senesced     // (INPUT)  plant biomass senescence (g/m^2)
    ,float  *g_n_green             // (INPUT) nitrogen in plant material (g/m^2)
    ,float  *g_dm_green            // (INPUT) plant material (g/m^2)
    ,float  *dlt_n_senesced_trans  // (OUTPUT)  plant N senescence (g/m^2)
    ,float  *dlt_n_senesced        // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2)
    ) {

//+  Local Variables
    int   part;                                   // plant part counter variable
    float green_n_conc;                           // N conc of green material (g/g)
    float sen_n_conc;                             // N conc of senescing material (g/g)
    float dlt_n_in_senescing_part;

//- Implementation Section ----------------------------------


    // first we zero all plant component deltas
    for (part = 0; part < num_part; part++)
       {
       green_n_conc = (float)divide (g_n_green[part]
       ,g_dm_green[part]
       ,0.0);

       dlt_n_in_senescing_part = g_dlt_dm_senesced[part]
       * green_n_conc;

       sen_n_conc = min (c_n_sen_conc[part], green_n_conc);

       dlt_n_senesced[part] = g_dlt_dm_senesced[part]
       * sen_n_conc;

       dlt_n_senesced[part] = u_bound (dlt_n_senesced[part]
       , g_n_green[part]);

       dlt_n_senesced_trans[part] = dlt_n_in_senescing_part
       - dlt_n_senesced[part];

       dlt_n_senesced_trans[part] = l_bound(dlt_n_senesced_trans[part]
       , 0.0);

       }
    }


//============================================================================
void crop_n_detachment(const int num_part,
                       const int root,
                       const int leaf,
                       float dm_leaf_detach_frac,  // (INPUT)  fraction of senesced leaf dry matter detaching from live plant each day (0-1)
                       float *dlt_N_senesced,      //  (INPUT)  actual N loss with senesced plant (g/m^2)
                       float *dlt_N_detached)      // (OUTPUT) actual nitrogen senesced  from plant parts (g/m^2)
//============================================================================

/*  Purpose
*   Calculate today's Nitrogen lost from senesced pools due to detachment
*
*  Mission Statement
*   Calculate today's Nitrogen lost from senesced pools due to detachment
*
*  Changes
*       091294 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*/
   {
   // first we zero all plant component deltas
   fill_real_array (dlt_N_detached, 0.0, num_part);

   dlt_N_detached[leaf] = dlt_N_senesced[leaf] * dm_leaf_detach_frac;
   dlt_N_detached[root] = dlt_N_senesced[root];
   }


//==========================================================================
void crop_n_dead_detachment(int num_part,
                            float *dead_detach_frac, //(INPUT)  fraction of dead plant parts detaching each day (0-1)
                            float *n_dead,           //(INPUT)  plant N content of dead plants (g N/m^2)
                            float *dlt_N_dead_detached)  //(OUTPUT) change in dm of dead plants (g/m^2)
//==========================================================================

/*  Purpose
*      Plant Nitrogen loss from dead plants
*
*  Mission Statement
*   Calculate today's Nitrogen lost from dead pools due to detachment
*
*  Changes
*       091294 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*  Constant Values
*      character  my_name*(*)           ! name of procedure
*      parameter (my_name = 'crop_N_dead_detachment')
*
*  Sub-Program Arguments
*      int *num_part
*      float *dead_detach_frac(*) !
*      float *n_dead(*)           !
*      float *dlt_N_dead_detached(*)   !
*/
   {
   for(int part = 0; part < num_part; part++)
      {
      dlt_N_dead_detached[part] = n_dead[part]* dead_detach_frac[part];
      }
   }

//==========================================================================
float crop_n_dlt_grain_conc(const int grain,
                            float sfac_slope,      //(INPUT)  soil water stress factor slope
                            float sw_fac_max,      //(INPUT)  soil water stress factor maximum
                            float temp_fac_min,    //(INPUT)  temperature stress factor minimum optimum temp
                            float tfac_slope,      //(INPUT)  temperature stress factor slope
                            float maxt,            //(INPUT)  maximum air temperature (oC)
                            float mint,            //(INPUT)  maximum air temperature (oC)
                            float nfact_grain_conc,// (INPUT)
                            float *n_conc_crit,     //(INPUT)  critical N concentration (g N/g biomass)
                            float *n_conc_min,      //(INPUT)  minimum N concentration (g N/g biomass)
                            float swdef_expansion) // (INPUT)
//==========================================================================

/*  Purpose
*     Calculate the nitrogen concentration required to meet the increase
*     from daily grain growth (0-1) as affected by temperature and water stress.
*
*  Mission Statement
*   Calculate the nitrogen concentration required for grain growth.
*
*  Notes
*     First, two factors are calculated and used to estimate the
*     effects of mean temperature and drought stress on the N
*     concentration in grain growth for the day.  High temperature
*     or drought stress can cause the factors to exceed 1.
*     N deficiency can cause nfac < 1.  The net effect of these
*     equations is to allow grain nitrogen concentration to range
*     from less than .01 when N deficiency is severe to about .018
*     stress limit grain growth.
*     Here, optimum N concentration = 1.7%
*
*/
   {
   //  Local Variables
   float N_conc_pot;                   // potential grain N concentration
                                       // (0-1) (g N/g part)
   float N_grain_sw_fac;               // soil water stress factor for N
                                       // uptake
   float N_grain_temp_fac;             // temperature stress factor for N
                                       // uptake
   float ave_temp;                     // mean temperature (oC)

   ave_temp = (maxt + mint) / 2.0f;

   //!!!!!!!!!! return to orig cm
   N_grain_temp_fac = temp_fac_min + tfac_slope * ave_temp;
   N_grain_sw_fac = sw_fac_max - sfac_slope * swdef_expansion ;

   // N stress reduces grain N concentration below critical
   N_conc_pot = n_conc_min[grain] + (n_conc_crit[grain] -
                  n_conc_min[grain]) * nfact_grain_conc;

            // Temperature and water stresses can decrease/increase grain
            // N concentration

            // when there is no N stress, the following can be a higher N conc than
            // the crit and thus the N conc of the grain can exceed N critical.

   return  (N_conc_pot * max (N_grain_temp_fac, N_grain_sw_fac));
   }


//===========================================================================
void crop_n_retrans_avail(const int num_part,
                          const int root,
                          const int grain,
                          float *g_N_conc_min,
                          float *g_dm_green,
                          float *g_N_green,
                          float *N_avail)
//===========================================================================

/*  Purpose
*     Calculate N available for transfer to grain (g/m^2)
*     from each plant part.  By definition, available grain N
*     is set to 0.
*
*  Mission Statement
*   Calculate the Nitrogen available for retranslocation to grain
*
*  Notes
*     N available for translocation to the grain is the sum of
*     N available in the stover.
*     N available in stover is the difference of its N content
*     and the minimum it's allowed to fall to.
*     NB. No translocation from roots.
*
*  Changes
*       080994 jngh specified and programmed
*       970318 slw extracted from Sorg
*
*/
   {
   float N_min;                  // nitrogen minimum level (g/m^2)
   int part;                     // plant part number

   // get grain N potential (supply) -----------
   // now find the available N of each part.
   for(part = 0; part < num_part; part++)
      {
      N_min = g_N_conc_min[part] * g_dm_green[part];
      N_avail[part] = l_bound (g_N_green[part] - N_min, 0.0);
      }
   N_avail[grain]  = 0.0;
   N_avail[root] = 0.0;
   }


//=========================================================================
void cproc_n_senescence1 (const int num_part,         //(INPUT) number of plant part
                          float *c_n_sen_conc,         //(INPUT)  N concentration of senesced materia (g/m^2)
                          float *g_dlt_dm_senesced,   //(INPUT)  plant biomass senescence (g/m^2)
                          float *g_n_green,           //(INPUT) nitrogen in plant material (g/m^2)
                          float *g_dm_green,          //(INPUT) plant material (g/m^2)
                          float *dlt_N_senesced)      // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2)
//=========================================================================

/*  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)
*
*  Mission Statement
*   Calculate change in senesced plant Nitrogen
*
*  Changes
*       121297 nih specified and programmed
*
*/
   {
   //  Local Variables
   int part;            // plant part counter variable
   float green_n_conc;  // N conc of green material (g/g)
   float sen_n_conc;    // N conc of senescing material (g/g)

    // first we zero all plant component deltas
   for(part = 0; part < num_part; part++)
      {
      green_n_conc = (float)divide (g_n_green[part],g_dm_green[part],0.0);
      sen_n_conc = min (c_n_sen_conc[part], green_n_conc);
      dlt_N_senesced[part] = g_dlt_dm_senesced[part] * sen_n_conc;
      dlt_N_senesced[part] = u_bound (dlt_N_senesced[part],g_n_green[part]);
      }
   }

//===========================================================================
void cproc_n_uptake1(float C_no3_diffn_const,   //(INPUT)  time constant for uptake by di
                     float *G_dlayer,            //(INPUT)  thickness of soil layer I (mm)//
                     float *G_no3gsm_diffn_pot,  //(INPUT)  potential NO3 (supply) from so
                     float *G_no3gsm_mflow_avail,// (INPUT)  potential NO3 (supply) from
                     float G_n_fix_pot,         //(INPUT) potential N fixation (g/m2)
                     const char *C_n_supply_preference, //(INPUT)
                     float N_demand,                //(INPUT)  critical plant nitrogen demand
                     float N_max,                   //(INPUT)  maximum plant nitrogen demand
                     float G_root_depth,              // (INPUT)  depth of roots (mm)
                     float *dlt_no3gsm)                // (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2
//===========================================================================

/*  Purpose
*       Return actual plant nitrogen uptake from each soil layer.
*       N uptake is from nitrate only and comes via mass flow and active (diffusion) uptake.
*
*  Mission Statement
*   Calculate crop Nitrogen Uptake
*
*  Changes
*       160498 nih specified and programmed
*
*/
   {

   //  Local Variables
   int deepest_layer;                  // deepest layer in which the roots are
                                       // growing
   float NO3gsm_diffn;                 // actual N available (supply) for
                                       // plant (g/m^2) by diffusion
   float NO3gsm_mflow;                 // actual N available (supply) for
                                       // plant (g/m^2) by mass flow
   float *NO3gsm_diffn_avail = new float[max_layer];   // potential NO3 (supply)
                                       // from soil (g/m^2), by diffusion
   float NO3gsm_diffn_supply;          // total potential N uptake (supply)
                                       // for plant (g/m^2) by diffusion
   float NO3gsm_mflow_supply;          // total potential N uptake (supply)
                                       // for plant (g/m^2) by mass flow
   float diffn_fract;                  // fraction of nitrogen to use (0-1)
                                       // for diffusion
   float mflow_fract;                  // fraction of nitrogen to use (0-1)
                                       // for mass flow
   int layer;                          // soil layer number of profile
   float NO3gsm_uptake;                // plant NO3 uptake from layer (g/m^2)
   float temp1;                  //Vars to pass composite terms to functions
   // Implementation Section ----------------------------------

            // get potential N uptake (supply) from the root profile.
            // get totals for diffusion and mass flow.

   deepest_layer = find_layer_no (G_root_depth, G_dlayer, max_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      NO3gsm_diffn_avail[layer] = G_no3gsm_diffn_pot[layer]- G_no3gsm_mflow_avail[layer];
      NO3gsm_diffn_avail[layer] = l_bound (NO3gsm_diffn_avail[layer], 0.0);
      }
   NO3gsm_mflow_supply = sum_real_array (G_no3gsm_mflow_avail , deepest_layer+1);
   NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail, deepest_layer+1);

            // get actual total nitrogen uptake for diffusion and mass flow.
            // If demand is not satisfied by mass flow, then use diffusion.
            // N uptake above N critical can only happen via mass flow.


   if (NO3gsm_mflow_supply >= N_demand)
      {
      NO3gsm_mflow = NO3gsm_mflow_supply;
      NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max);
      NO3gsm_diffn = 0.0;
      }
   else
      {
      NO3gsm_mflow = NO3gsm_mflow_supply;

      if (strcmp(C_n_supply_preference,"active") == 0)
         {
         temp1 = N_demand - NO3gsm_mflow;
         NO3gsm_diffn = bound (temp1, 0.0, NO3gsm_diffn_supply);
         }
      else if (strcmp(C_n_supply_preference,"fixation") == 0)
         {
         temp1 = N_demand - NO3gsm_mflow - G_n_fix_pot;
         NO3gsm_diffn = bound (temp1, 0.0, NO3gsm_diffn_supply);
         }
      else
         {
         throw std::invalid_argument("bad n supply preference in cproc_n_uptake1()");
         }

      NO3gsm_diffn = (float)divide (NO3gsm_diffn, C_no3_diffn_const, 0.0);

      }

   // get actual change in N contents
   fill_real_array (dlt_no3gsm, 0.0, max_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
               // allocate nitrate
               // Find proportion of nitrate uptake to be taken from layer
               // by diffusion and mass flow

      mflow_fract = (float)divide(G_no3gsm_mflow_avail[layer],NO3gsm_mflow_supply, 0.0);

      diffn_fract = (float)divide(NO3gsm_diffn_avail[layer], NO3gsm_diffn_supply, 0.0);

               // now find how much nitrate the plant removes from
               // the layer by both processes

      NO3gsm_uptake = NO3gsm_mflow * mflow_fract + NO3gsm_diffn * diffn_fract;
      dlt_no3gsm[layer] = -1 * NO3gsm_uptake;
      }
   }


// ====================================================================
void cproc_n_supply1 (float *G_dlayer,                   // (INPUT)
                      float *G_dlt_sw_dep,               // (INPUT)
                      float *G_NO3gsm,                   // (INPUT)
                      float *G_NO3gsm_min,               // (INPUT)
                      float G_root_depth,                // (INPUT)
                      float *G_sw_dep,                   // (INPUT)
                      float *G_NO3gsm_mflow_avail,       // (OUTPUT)
                      float *G_sw_avail,                 // (INPUT)
                      float *G_NO3gsm_diffn_pot)                // (INPUT)
//=========================================================================
/*  Purpose
*      Calculate nitrogen supplies from soil and fixation
*
*  Mission Statement
*   Calculate crop Nitrogen supplies (soil + fixation)
*
*  Changes
*     21-04-1998 - neilh - Programmed and Specified
*/
   {
   crop_n_mass_flow1(max_layer, G_dlayer, G_dlt_sw_dep, G_NO3gsm, G_NO3gsm_min,
            G_root_depth, G_sw_dep, G_NO3gsm_mflow_avail);

   crop_n_diffusion1(max_layer, G_dlayer, G_NO3gsm, G_NO3gsm_min, G_root_depth,
            G_sw_avail, G_sw_dep, G_NO3gsm_diffn_pot);

   }

//=============================================================================
void crop_n_mass_flow1(const int num_layer,          // (INPUT)  number of layers in profile
                       float *dlayer,                // (INPUT)  thickness of soil layer I (mm)
                       float *dlt_sw_dep,            // (INPUT)  water uptake in each layer (mm water)
                       float *no3gsm,                // (INPUT)  nitrate nitrogen in layer L (g N/m^2)
                       float *no3gsm_min,            // (INPUT)  minimum allowable NO3 in soil (g/m^2)
                       float root_depth,             // (INPUT)  depth of roots (mm)
                       float *sw_dep,                // (INPUT)  soil water content of layer L (mm)
                       float *NO3gsm_mflow_pot)      // (OUTPUT) potential plant NO3 uptake (supply) g/m^2 by mass flow
//=============================================================================

/*  Purpose
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)
*
*  Mission Statement
*   Calculate crop nitrogen supply from mass flow, %8.
*
*  Changes
*      090994 jngh specified and programmed
*      970216 slw generalised to avoid common blocks , added num_layer parameter
*
*/
   {
   //  Local Variables
   int deepest_layer;            // deepest layer in which the roots are growing
   int layer;                    // layer number of soil
   float NO3_conc;               // nitrogen concentration (g/m^2/mm)
   float NO3gsm_mflow;           // potential nitrogen uptake (g/m^2)

   float temp1;                  //Vars to pass composite terms to functions

   fill_real_array (NO3gsm_mflow_pot, 0.0, num_layer);

   // only take the layers in which roots occur
   deepest_layer = find_layer_no(root_depth, dlayer, num_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      // get  NO3 concentration
      NO3_conc = (float)divide(no3gsm[layer], sw_dep[layer], 0.0);
      // get potential uptake by mass flow
      NO3gsm_mflow = NO3_conc * (-1 * dlt_sw_dep[layer]);
      temp1 = no3gsm[layer] - no3gsm_min[layer];
      NO3gsm_mflow_pot[layer] = u_bound (NO3gsm_mflow, temp1);
      }
   }


//=============================================================================
void crop_n_diffusion1 (const int num_layer,      // (INPUT)  number of layers in profile
                        float *dlayer,            // (INPUT)  thickness of soil layer I (mm)
                        float *no3gsm,            // (INPUT)  nitrate nitrogen in layer L (g N/m^
                        float *no3gsm_min,        // (INPUT)  minimum allowable NO3 in soil (g/m^
                        float root_depth,         // (INPUT)  depth of roots (mm)
                        float *sw_avail,          // (INPUT)  actual extractable soil water (mm)
                        float *sw_avail_pot,      // (INPUT)  potential extractable soil water (m
                        float *NO3gsm_diffn_pot)  // (OUTPUT) potential plant NO3 by diffusion
//=============================================================================

/*  Purpose
*       Return potential nitrogen uptake (supply) by diffusion
*       for a plant (g/m^2)
*
*  Mission Statement
*   Calculate crop nitrogen supply from active uptake, %8.
*
*  Changes
*      060495 nih taken from template
*      160297 slw generalised to avoid common blocks , added num_layer parameter
*
*/
   {
   //  Local Variables
   int deepest_layer;            // deepest layer in which the roots are growing
   int layer;                    // layer number of soil
   float NO3gsm_diffn;           // potential nitrogen uptake (g/m^2)
   float sw_avail_fract;         // fraction of extractable soil water ()

   float temp2;                  //expecting a pointer

   //- Implementation Section ----------------------------------
   // only take the layers in which roots occur
   fill_real_array(NO3gsm_diffn_pot, 0.0, num_layer);

   deepest_layer = find_layer_no(root_depth, dlayer, num_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      sw_avail_fract = (float)divide(sw_avail[layer], sw_avail_pot[layer], 0.0);
      sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0);

      // get extractable NO3
      // restricts NO3 available for diffusion to NO3 in plant
      // available water range
      NO3gsm_diffn = sw_avail_fract * no3gsm[layer];
      temp2 = no3gsm[layer] - no3gsm_min[layer];
      NO3gsm_diffn_pot[layer] = u_bound(NO3gsm_diffn, temp2);
      }
   }


//============================================================================
void cproc_n_demand1(const int max_part,          // (INPUT)
                     int   *demand_parts,         // (INPUT)
                     const int num_demand_parts,  // (INPUT)
                     float G_dlt_dm,              // (INPUT)  the daily biomass production (
                     float *G_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                     float G_dlt_dm_pot_rue,      // (INPUT)  potential dry matter productio
                     float *G_dlt_n_retrans,       // (INPUT)  nitrogen retranslocated out fr
                     float *G_dm_green,            // (INPUT)  live plant dry weight (biomass
                     float *G_n_conc_crit,         // (INPUT)  critical N concentration (g N/
                     float *G_n_conc_max,          // (INPUT)  maximum N concentration (g N/g
                     float *G_n_green,             // (INPUT)  plant nitrogen content (g N/m^
                     float *N_demand,             // (OUTPUT) critical plant nitrogen demand g/m^2)
                     float *N_max)                // (OUTPUT) max plant nitrogen demand  (g/m^2)
//============================================================================

/*  Purpose
*       Return plant nitrogen demand for each plant component
*
*  Mission Statement
*   Calculate the Nitrogen demand and maximum uptake for each plant pool
*
*  Notes
*           Nitrogen required for grain growth has already been removed
*           from the stover.  Thus the total N demand is the sum of the
*           demands of the stover and roots.  Stover N demand consists of
*           two components:
*           Firstly, the demand for nitrogen by the potential new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative
*
*           NOTE that this routine will not work if the root:shoot ratio
*           is broken. - NIH
*
*  Changes
*     010994 jngh specified and programmed
*     210498 nih  adapted to crop template specifications
*
*/
   {
   //  Local Variables
   int counter;
   float N_crit;                 // critical N amount (g/m^2)
   float N_demand_new ;          // demand for N by new growth
                                 // (g/m^2)
   float N_demand_old;           // demand for N by old biomass
                                 // (g/m^2)
   float N_potential;            // maximum N uptake potential (g/m^2)
   float N_max_new;              // N required by new growth to reach
                                 // N_conc_max  (g/m^2)
   float N_max_old;              // N required by old biomass to reach
                                 // N_conc_max  (g/m^2)
   int part;                     // plant part
   float dlt_dm_pot;             // potential dry weight increase
                                 // (g/m^2)
   float part_fract;             // plant part fraction of dm  (0-1)

   //- Implementation Section ----------------------------------
   fill_real_array (N_demand, 0.0, max_part);
   fill_real_array (N_max, 0.0, max_part);
   for(counter = 0; counter < num_demand_parts; counter++)
      {
      part = demand_parts[counter];

            // need to calculate dm using potential rue not affected by
            // N and temperature

      part_fract = (float)divide (G_dlt_dm_green[part], G_dlt_dm, 0.0);
      dlt_dm_pot = G_dlt_dm_pot_rue * part_fract;
      dlt_dm_pot = bound (dlt_dm_pot, 0.0, G_dlt_dm_pot_rue);
      if (G_dm_green[part] > 0.0)
         {
         // get N demands due to difference between actual N concentrations
         // and critical N concentrations of tops (stover) and roots.
         N_crit       = G_dm_green[part] * G_n_conc_crit[part];
         N_potential  = G_dm_green[part] * G_n_conc_max[part];

         // retranslocation is -ve for outflows
         N_demand_old = N_crit -
                  (G_n_green[part] + G_dlt_n_retrans[part]);
         N_max_old    = N_potential -
                  (G_n_green[part] + G_dlt_n_retrans[part]);

         // get potential N demand (critical N) of potential growth
         N_demand_new = dlt_dm_pot * G_n_conc_crit[part];
         N_max_new    = dlt_dm_pot * G_n_conc_max[part];

         N_demand[part] = N_demand_old + N_demand_new;
         N_max[part]    = N_max_old    + N_max_new ;

         N_demand[part] = l_bound (N_demand[part], 0.0);
         N_max[part]    = l_bound (N_max[part], 0.0);
         }
      else
         {
         N_demand[part] = 0.0;
         N_max[part]    = 0.0;
         }
      }
      // this routine does not allow excess N in one component to move
      // to another component deficient in N
   }


//==========================================================================
void cproc_n_init1(float *C_n_init_conc,  // (INPUT)  initial N concentration (
                   int    max_part,
                   float *G_dm_green,     // (INPUT)  live plant dry weight (biomass
                   float *N_green)        // plant nitrogen (g/m^2)
//===========================================================================

/*  Purpose
*   Initialise plant Nitrogen pools
*
*  Mission Statement
*   Initialise plant Nitrogen pools (on first day of %3)
*
*  Changes
*     210498 nih specified and programmed
*
*/
   {
   for(int part = 0; part < max_part; part++)
         {
         N_green[part] = C_n_init_conc[part] * G_dm_green[part];
         }
      }


//==========================================================================
void cproc_n_detachment1(const int max_part,
                         float *c_sen_detach_frac,
                         float *g_n_senesced,
                         float *g_dlt_n_detached,
                         float *c_dead_detach_frac,
                         float *g_n_dead,
                         float *g_dlt_n_dead_detached)
//==========================================================================

/*  Purpose
*       Simulate plant Nitrogen detachment.
*
*  Mission Statement
*       Calculate plant Nitrogen detachment from senesced and dead pools
*
*  Changes
*      220498 nih specified and programmed
*/
   {
   //- Implementation Section ----------------------------------
   crop_pool_fraction_delta(max_part, c_sen_detach_frac ,g_n_senesced,
            g_dlt_n_detached);

   crop_pool_fraction_delta(max_part, c_dead_detach_frac, g_n_dead,
            g_dlt_n_dead_detached);
   }

//+  Purpose
//       Return actual plant nitrogen uptake from each soil layer.

//+  Mission Statement
//   Calculate crop Nitrogen Uptake

//+  Changes
void cproc_n_uptake3
    (
     float  *g_dlayer                            // (INPUT)  thickness of soil layer I (mm)
    ,float  *g_no3gsm_uptake_pot                 // (INPUT)  potential NO3 (supply)
    ,float  *g_nh4gsm_uptake_pot                 // (INPUT)  potential NH4 (supply)
    ,float  g_n_fix_pot                          // (INPUT) potential N fixation (g/m2)
    ,const char   *c_n_supply_preference         // (INPUT)c_n_supply_preference*(*)
    ,float  n_demand                             // (INPUT)  critical plant nitrogen demand
    ,float  //n_max                                // (INPUT)  maximum plant nitrogen demand
    ,float  g_root_depth                         // (INPUT)  depth of roots (mm)
    ,float  *dlt_no3gsm                          // (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2)
    ,float  *dlt_nh4gsm                          // (OUTPUT) actual plant N uptake from NH4 in each layer (g/m^2)
    ) {

//+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    int   layer;                                  // soil layer number of profile
    float no3gsm_uptake;                          // plant NO3 uptake from layer (g/m^2)
    float nh4gsm_uptake;                          // plant NO3 uptake from layer (g/m^2)
    float ngsm_supply;
    float scalef;

//- Implementation Section ----------------------------------

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);

    ngsm_supply = sum_real_array (g_no3gsm_uptake_pot, deepest_layer+1)
                + sum_real_array (g_nh4gsm_uptake_pot, deepest_layer+1);


    if (Str_i_Eq(c_n_supply_preference,"fixation"))
        {
        n_demand = l_bound (n_demand - g_n_fix_pot, 0.0);
        }

    // get actual change in N contents
    fill_real_array (dlt_no3gsm, 0.0, max_layer);
    fill_real_array (dlt_nh4gsm, 0.0, max_layer);

    if (n_demand > ngsm_supply)
        {
        scalef = 0.99999f  ;                       // avoid taking it all up as it can
                                                  // cause rounding errors to take
                                                  // no3 below zero.
        }
    else
        {
        scalef = divide (n_demand
                         ,ngsm_supply
                         ,0.0);
        }

    for (layer = 0; layer <= deepest_layer; layer++)
        {
         // allocate nitrate
        no3gsm_uptake = g_no3gsm_uptake_pot[layer] * scalef;
        dlt_no3gsm[layer] = - no3gsm_uptake;

         // allocate ammonium
        nh4gsm_uptake = g_nh4gsm_uptake_pot[layer] * scalef;
        dlt_nh4gsm[layer] = - nh4gsm_uptake;
        }
    }


//+  Purpose
//      Calculate nitrogen supplies from soil and fixation

//+  Mission Statement
//   Calculate crop Nitrogen supplies (soil + fixation)

//+  Changes
//     21-04-1998 - neilh - Programmed and Specified
void cproc_n_supply3 (
     float  g_dlayer[]                        // (INPUT)
    ,float  g_no3gsm[]                        // (INPUT)
    ,float  g_no3gsm_min[]                    // (INPUT)
    ,float  *g_no3gsm_uptake_pot
    ,float  g_root_depth                     // (INPUT)
    ,float  *g_root_length
    ,float  g_bd[]
    ,float  c_total_n_uptake_max
    ,float  c_no3_uptake_max
    ,float  c_no3_conc_half_max
    ,float  *g_sw_avail_pot                  // (INPUT)
    ,float  *g_sw_avail
    ,bool   inNStressPhase
    ) {

//+  Local variables
    float no3ppm;
    int   deepest_layer;
    int layer;
    float umax;
    float swfac;
    float total_n_uptake_pot;
    float scalef;

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);
    if (inNStressPhase)
       {
       for (layer = 0; layer<= deepest_layer; layer++)
         {
         no3ppm = (g_no3gsm[layer] - g_no3gsm_min[layer])
            * divide (1000.0, g_bd[layer]*g_dlayer[layer], 0.0);
         umax = c_no3_uptake_max *
              divide(no3ppm
                     ,no3ppm + c_no3_conc_half_max
                     ,0.0);
         umax=l_bound(umax,0.0);
                                                       //**2
         swfac = (float)divide(g_sw_avail[layer],g_sw_avail_pot[layer],0.0) ;
         swfac = bound(swfac,0.0,1.0);

         g_no3gsm_uptake_pot[layer] = umax
                * g_root_length[layer] * 1e6
                * swfac;                                      //mm2 to m2

         g_no3gsm_uptake_pot[layer] =
                    u_bound(g_no3gsm_uptake_pot[layer]
                            ,g_no3gsm[layer]-g_no3gsm_min[layer]);
         }
       }
    else
       {
          // No N stress whilst N is present in soil
          // crop has access to all that it wants early on
          // to avoid effects of small differences in N supply
          // having affect during the most sensitive part
          // of canopy development.

          for (layer = 0; layer <= deepest_layer; layer++)
             {
             g_no3gsm_uptake_pot[layer]
                = l_bound(g_no3gsm[layer]-g_no3gsm_min[layer],0.0);
             }
       }
     total_n_uptake_pot = sum_real_array(g_no3gsm_uptake_pot,deepest_layer+1);
     scalef = (float)divide(c_total_n_uptake_max, total_n_uptake_pot,0.0);
     scalef = bound(scalef,0.0,1.0);
     for (layer = 0; layer <= deepest_layer; layer++)
         {
         g_no3gsm_uptake_pot[layer] = scalef * g_no3gsm_uptake_pot[layer];
         }

    }


//  Purpose
//     Calculate nitrogen supplies from soil and fixation
//
//  Mission Statement
//  Calculate crop Nitrogen supplies (soil + fixation)
void cproc_n_supply4 (float* g_dlayer    //! (INPUT)
               ,float* g_bd                     //
               ,float* g_no3gsm                 //! (INPUT)
               ,float* g_no3gsm_min             //! (INPUT)
               ,float* g_no3gsm_uptake_pot      //  (output)
               ,float* g_nh4gsm                 //! (INPUT)
               ,float* g_nh4gsm_min             //! (INPUT)
               ,float* g_nh4gsm_uptake_pot      //  (output)
               ,float g_root_depth              //! (INPUT)
               ,float c_kno3                     //
               ,float c_no3ppm_min
               ,float c_knh4                     //
               ,float c_nh4ppm_min
               ,float c_total_n_uptake_max      //
               ,float* g_sw_avail_pot           //! (INPUT)
               ,float* g_sw_avail
               ,bool   inNStressPhase)
  {
      float no3ppm;
      float nh4ppm;
      int deepest_layer;
      int layer;
      float swfac;
      float total_n_uptake_pot;
      float scalef;

      deepest_layer = find_layer_no (g_root_depth
                                      ,g_dlayer
                                      ,max_layer);

      if (inNStressPhase)
         {
         for (layer = 0; layer <= deepest_layer; layer++)
            {
            no3ppm = (float)(g_no3gsm[layer] * divide (1000.0, g_bd[layer]*g_dlayer[layer], 0.0));
            nh4ppm = (float)(g_nh4gsm[layer] * divide (1000.0, g_bd[layer]*g_dlayer[layer], 0.0));

           swfac = (float)divide(g_sw_avail[layer],g_sw_avail_pot[layer],0.0); //**2
           swfac = bound (swfac,0.0,1.0);

           g_no3gsm_uptake_pot[layer] = g_no3gsm[layer]
                                      * c_kno3 * (no3ppm - c_no3ppm_min) * swfac;
           g_no3gsm_uptake_pot[layer] = u_bound(g_no3gsm_uptake_pot[layer]
                                               ,g_no3gsm[layer]-g_no3gsm_min[layer]);
           g_no3gsm_uptake_pot[layer] = l_bound(g_no3gsm_uptake_pot[layer], 0.0);

           g_nh4gsm_uptake_pot[layer] = g_nh4gsm[layer]
                                      * c_knh4 * (nh4ppm - c_nh4ppm_min) * swfac;
           g_nh4gsm_uptake_pot[layer] = u_bound(g_nh4gsm_uptake_pot[layer]
                                               ,g_nh4gsm[layer]-g_nh4gsm_min[layer]);
           g_nh4gsm_uptake_pot[layer] = l_bound(g_nh4gsm_uptake_pot[layer], 0.0);
            }
        }
      else
        {
        // No N stress whilst N is present in soil
        // crop has access to all that it wants early on
        // to avoid effects of small differences in N supply
        // having affect during the most sensitive part
        // of canopy development.

        for (layer = 0; layer <= deepest_layer; layer++)
           {
            no3ppm = (float)(g_no3gsm[layer] * divide (1000.0, g_bd[layer]*g_dlayer[layer], 0.0));
            nh4ppm = (float)(g_nh4gsm[layer] * divide (1000.0, g_bd[layer]*g_dlayer[layer], 0.0));


           if (c_kno3>0 && no3ppm>c_no3ppm_min)
              g_no3gsm_uptake_pot[layer] = l_bound(g_no3gsm[layer]-g_no3gsm_min[layer],0.0);
           else
              g_no3gsm_uptake_pot[layer] = 0.0;

           if (c_knh4>0 && nh4ppm>c_nh4ppm_min)
              g_nh4gsm_uptake_pot[layer] = l_bound(g_nh4gsm[layer]-g_nh4gsm_min[layer],0.0);
           else
              g_nh4gsm_uptake_pot[layer] = 0.0;
           }
        }

      total_n_uptake_pot = sum_real_array(g_no3gsm_uptake_pot, deepest_layer+1)
                         + sum_real_array(g_nh4gsm_uptake_pot, deepest_layer+1);
      scalef = (float)divide(c_total_n_uptake_max, total_n_uptake_pot,0.0);
      scalef = bound(scalef,0.0,1.0);
      for (layer = 0; layer <= deepest_layer; layer++)
           {
           g_no3gsm_uptake_pot[layer] = scalef * g_no3gsm_uptake_pot[layer];
           g_nh4gsm_uptake_pot[layer] = scalef * g_nh4gsm_uptake_pot[layer];;
           }

   }

