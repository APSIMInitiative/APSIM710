#include "StdPlant.h"


//============================================================================
void crop_dm_pot_rue (float current_stage,         //(IN)
                      float *rue,                  //(IN)
                      float radn_int,              //(IN)
                      float temp_stress_photo,     //(IN)
                      float nfact_photo,           //(IN)
                      float *dlt_dm_pot)           //(OUTPUT) potential dry matter
                                                   //(carbohydrate) production (g/m^2)
//============================================================================

/*Purpose
 *   Potential biomass (carbohydrate) production from
 *   photosynthesis (g/m^2) limited by temperature and nitrogen stresses
 */

   {
   //Local Variables
   int current_phase;            // current phase number
   float usrue;                  // radiation use efficiency under
                                 // no stress (g biomass/mj)

   //Implementation Section ----------------------------------

   current_phase = int (current_stage);
   usrue = rue[current_phase-1] * min(temp_stress_photo, nfact_photo);

     /*  ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per M_j of intercepted
         ! radiation under stressed conditions.
     */

   *dlt_dm_pot = usrue * radn_int;
   }

//============================================================================
void crop_dm_senescence0(const int num_part,              //(INPUT)  number of plant parts
                         const int root,                  //(INPUT)  number of plant root part
                         const int leaf,                  //(INPUT)  number for plant leaf part
                         const int stem,                  //(INPUT)  number for plant stem part
                         float dm_leaf_sen_frac,         //(INPUT)  fraction of senescing leaf dry
                         float dm_root_sen_frac,         //(INPUT)  fraction of root dry matter se
                         float *dlt_dm_green,             //(INPUT)  plant biomass growth (g/m^2)
                         float *dlt_dm_green_retrans,     //(INPUT)  plant biomass retranslocated
                         float dlt_lai,                  //(INPUT)  actual change in live plant la
                         float dlt_slai,                 //(INPUT)  area of leaf that senesces fro
                         float *dm_green,                 //(INPUT)  live plant dry weight (biomass
                         float lai,                      //(INPUT)  live plant green lai
                         float *dlt_dm_senesced,          //(OUTPUT) actual biomass senesced from plant parts (g/m^2)
                         float *dlt_dm_sen_retrans)       //(OUTPUT) reduction in senesced biomass as a result of retranslocation
//============================================================================

/*Purpose
 *   Derives seneseced plant dry matter (g/m^2) for the day
 *Parameters
 */
   {
   //Local Variables
   float dlt_dm_senescing;      // dm of leaves senesced (g/m^2)
   float dm_green_leaf_today;    // today's green leaf dry matter (g/m^2)
   float lai_today;            // today's green lai
   float sla_today;            // today's specific leaf area (m^2/g)

   // first we zero all plant component deltas

   fill_real_array (dlt_dm_senesced, 0.0, num_part);
   fill_real_array (dlt_dm_sen_retrans, 0.0, num_part);

   //-------------SENESCENCE O_f BIOMASS ---------------------
   lai_today = lai + dlt_lai;

   if (dlt_slai < lai_today)
      {
      dm_green_leaf_today = dm_green[leaf] + dlt_dm_green[leaf]
                                + dlt_dm_green_retrans[leaf]; // -ve outflow
      sla_today = (float)divide (lai_today, dm_green_leaf_today, 0.0);

      dlt_dm_senescing = (float)divide (dlt_slai, sla_today, 0.0);
      }
   else
      {
      dlt_dm_senescing = dm_green[leaf]+ dlt_dm_green[leaf];
      }
   dlt_dm_senesced[leaf] = dlt_dm_senescing;
   dlt_dm_senesced[stem] = 0.0;
   dlt_dm_senesced[root] = dm_green[root] * dm_root_sen_frac;


   //-------------RETRANSLOCATION of BIOMASS FROM SENESCENCE----------
     /* a proportion of senesced leaf dry matter may be
        retranslocated to the stem */

   dlt_dm_sen_retrans[leaf]= dlt_dm_senescing * (1.0f - dm_leaf_sen_frac);
   dlt_dm_green_retrans[stem] = dlt_dm_green_retrans[stem]
                               + dlt_dm_sen_retrans[leaf];
   }

//============================================================================
void crop_dm_dead_detachment(const int num_part,
                             float *dead_detach_frac,         //(INPUT)  fraction of dead plant parts detached
                             float *dm_dead,                  //(INPUT)  dry wt of dead plants (g/m^2)
                             float *dlt_dm_dead_detached)     //(OUTPUT) change in dm of dead plant
//============================================================================

/*Purpose
 *      Plant dry matter loss from dead plants
 *Parameters
 */

   {
   //Implementation Section ----------------------------------

   for (int part=0 ; part < num_part; part++)
      {
      dlt_dm_dead_detached[part] = dm_dead[part] * dead_detach_frac[part];
      }
   }


//============================================================================
void cproc_dm_senescence1 (const int num_part,           //(INPUT)  number of plant parts
                           const int /* max_table*/,          //(INPUT)  max lookup length
                           float independant_variable,   //(INPUT)  independant variable which
                           float **c_x_dm_sen_frac,      //(INPUT)  lookup for independant variabl   is said to drive senescence.
                           float **c_y_dm_sen_frac,      // (INPUT)  fraction of  material senescin
                           int   *c_num_dm_sen_frac,     // (INPUT)  fraction of  material sene
                           float *g_dm_green,            // (INPUT)  live plant dry weight (biomass
                           float *g_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                           float *g_dlt_dm_green_retrans,// (INPUT)  plant biomass retranslocat
                           float *dlt_dm_senesced)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
//============================================================================

/*Purpose
 *   Derives seneseced plant dry matter (g/m^2) for the day
 */
   {
   //Local Variables
   float  fraction_senescing;      // dm senesced (g/m^2)

   //Implementation Section ----------------------------------

   for (int part = 0; part < num_part; part++)
      {
      fraction_senescing
                 = linear_interp_real (independant_variable,
                           c_x_dm_sen_frac[part],
                           c_y_dm_sen_frac[part],
                           c_num_dm_sen_frac[part]);

      fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
      dlt_dm_senesced[part] = (g_dm_green[part] + g_dlt_dm_green[part] +
                                 g_dlt_dm_green_retrans[part])* fraction_senescing;
      }
   }

//============================================================================
void cproc_dm_detachment1( const int max_part,
                           float *c_sen_detach_frac,
                           float *g_dm_senesced,
                           float *g_dlt_dm_detached,
                           float *c_dead_detach_frac,
                           float *g_dm_dead,
                           float *g_dlt_dm_dead_detached)
//============================================================================
/*  Purpose
*       Simulate plant detachment.
*
*  Mission Statement
*   Calculate detachment of senescenced and dead material based on fractional decay rates.
*
*/
   {
   crop_pool_fraction_delta(max_part, c_sen_detach_frac, g_dm_senesced,
                            g_dlt_dm_detached);

   crop_pool_fraction_delta(max_part, c_dead_detach_frac, g_dm_dead,
                            g_dlt_dm_dead_detached);
   }

//========================================================================
void cproc_bio_yieldpart_demand1(float G_current_stage,             // (INPUT)  current phenological stage
                                 int Start_stress_stage,            // (INPUT)
                                 int Start_grainfill_stage,         // (INPUT)
                                 int End_grainfill_stage,           // (INPUT)
                                 int Yield_part,                    // (INPUT)
                                 int Root_part,                     // (INPUT)
                                 int Max_part,                      // (INPUT)
                                 float G_dlt_dm,                   // (INPUT)  the daily biomass production (
                                 float *G_dm_green,                 // (INPUT)  live plant dry weight (biomass
                                 float *G_dm_senesced,              // (INPUT)  senesced plant dry wt (g/m^2)
                                 float *G_days_tot,                 // (INPUT)  duration of each phase (days)
                                 float *G_dm_stress_max,            // (INPUT)  sum of maximum daily stress on
                                 float P_hi_incr,                  // (INPUT)  harvest index increment per da
                                 float *P_x_hi_max_pot_stress,      // (INPUT) Potential Max HI Stress dete
                                 float *P_y_hi_max_pot,             // (INPUT) Potential Max HI
                                 int P_num_hi_max_pot,             // (INPUT) Number of lookup pairs
                                 float *Dlt_dm_yieldpart_demand)    //(OUTPUT) grain dry matter potential (g/m^2)
//========================================================================
/*  Purpose
*        Find grain demand for carbohydrate using daily increase in harvest index (g/m^2)
*
*  Mission Statement
*   Calculate yield component biomass demand using harvest index increments
*
*  Changes
*     010994 jngh specified and programmed
*
*/
   {
   float ave_stress;            // average dm_stress from flowering to gra
   float stress_sum;            // total    "          "     "      "    "
   float days_sum;              // total    days       "     "      "    "
   float dlt_dm_yield;          // grain demand for carbohydrate
                                 // (g/m^2)
   float dm_tops;               // drymatter of tops (g/m^2)
   float harvest_index;         // last harvest index (g grain/g biomass)
   float hi_max_pot;            // max potential HI due to stress
   float dm_tops_new;           // new drymatter  tops (g/m^2)
   float harvest_index_new;     // next harvest index (g grain/g biomass)
   float dm_grain_new ;         // new drymatter grain (g/m^2)
//   float second;           // expecting a pointer


   if (stage_is_between (Start_grainfill_stage, End_grainfill_stage, G_current_stage))
      {
      stress_sum = sum_between (Start_stress_stage-1, Start_grainfill_stage-1, G_dm_stress_max);
      days_sum = sum_between (Start_stress_stage-1, Start_grainfill_stage-1, G_days_tot);
      ave_stress = (float)divide (stress_sum, days_sum, 1.0);
      hi_max_pot = linear_interp_real(ave_stress,
                                      P_x_hi_max_pot_stress,
                                      P_y_hi_max_pot,
                                      P_num_hi_max_pot);

      // effective grain filling period
      dm_tops = sum_real_array (G_dm_green, Max_part) - G_dm_green[Root_part] +
                  sum_real_array (G_dm_senesced, Max_part) - G_dm_senesced[Root_part];
      harvest_index = (float)divide (G_dm_green[Yield_part], dm_tops, 0.0);
      dm_tops_new = dm_tops + G_dlt_dm;


      harvest_index_new = u_bound (harvest_index + P_hi_incr, hi_max_pot);

      dm_grain_new = dm_tops_new * harvest_index_new;
      dlt_dm_yield = dm_grain_new - G_dm_green[Yield_part];
      dlt_dm_yield = bound (dlt_dm_yield, 0.0, dm_grain_new);
      }
   else
      {
      // we are out of grain fill period
      dlt_dm_yield = 0.0;
      }
   *Dlt_dm_yieldpart_demand = dlt_dm_yield;
   }


//===========================================================================
void cproc_yieldpart_demand_stress1(float G_nfact_photo,         // (INPUT)
                                    float G_swdef_photo,         // (INPUT)
                                    float G_temp_stress_photo,   // (INPUT)
                                    float *Dlt_dm_stress_max)    // (OUTPUT) max daily stress (0-1)
//===========================================================================
/* Purpose
*        Find maximum stress on daily dm production (0-1)
*
*  Mission Statement
*   Calculate the stress factor for diminishing potential harvest index
*
*  Assumptions
*       Here we assume that the soil water stress factor has included stress
*       factors that reduce RUE. The stress returned from here is the
*       maximum stress experienced relative to all factors non limiting.
*
*  Changes
*     260598 jngh specified and programmed
*/
   {
   //  Local Variables
   float rue_reduction;          // Effect of non-optimal N and Temp
                                 // conditions on RUE (0-1)

   rue_reduction = min (G_temp_stress_photo, G_nfact_photo);
   *Dlt_dm_stress_max = G_swdef_photo * rue_reduction;
   }


//===========================================================================
void cproc_bio_init1(float *C_dm_init,         //(INPUT)
                     int   Init_stage,         //(INPUT)
                     float G_current_stage,    //(INPUT)  current phenological stage

                     float G_plants,           //(INPUT)  Plant density (plants/m^2)
                     float Max_part,           //(INPUT)
                     float *G_dm_green)        //(INPUT/OUTPUT) plant part weights  (g/m^2)
//===========================================================================
/*  Purpose
*       Initialise plant weights at a specified growth stage
*       at required instances.
*
*  Mission Statement
*   Initialise biomass pools (if the first day of %2)
*
*  Changes
*     260598 nih specified and programmed
*
*/
   {
   if (on_day_of (Init_stage, G_current_stage))
      {
      for(int part = 0; part < Max_part; part++)
         {
         G_dm_green[part] = C_dm_init[part] * G_plants;
         }
      }
   else
      {   // no changes
      }
   }


//==========================================================================
void cproc_rue_n_gradients(int   day,                  // !day of the year
                           float latitude,           //latitude in degree
                           float radiation,          //daily global radiation (MJ/m2/d)
                           float /* tempmax*/,            //daily maximum tempeature (C)
                           float /* tempmin*/,            //daily minimum tempeature (C)
                           float lai_green,          //leaf area index (-)
                           float sln_gradient,      //SLN gradients in canopy (g N/m2 leaf)
                           float pmaxmax,            //potential assimilation rate (SLN ASYMPTOTE) (mg CO2/m2.s)
                           float shadow_projection,  //shadow projection (=0.5)
                           float biomass_conversion, //biomass coversion for biochemical coversion and maintenance respiration (mg DM / mgCO2)
                           float scatter_coeff,      //scattering coefficients (=0.15)
                           float *rue_sln)            //rue based on SLN gradients in the canopy (g DM / MJ)
//===========================================================================
/*  Purpose
*
*	Calculate RUE from SLN and LAI profiles
*	taking account of sun angle and light intensity during the day.
*	Uses Gaussian integration to determine daily RUE.
*
*  Mission Statement
*
*       Calculate the daily RUE of a crop canopy from SLN and LAI profiles
*
*  Changes
*       20001001 ew specified and programmed
*
*
*  Sub-Program Arguments
*      int
*      float
*      float
*      float
*      float
*      float
*      float
*      float
*      float
*      float
*      float
*      float
*
*/
   {
   // Constant Values
   float PI  = 3.14159f;


   //  Local Variable

   //	Glossary of variables (mostly defined by canopy layer)

	float RUE[5];     // radiation use efficiency g/MJ; instantaneous
	float RUEDAY;     // RUE for whole canopy for the day g/MJ
	float SLNGRAD;    // slope of SLN vs SUMLAI line
	float SLN[5];     // specific leaf nitrogen mg N/m2 leaf area
	float AVSLN;      // average SLN for canopy mg N/m2 leaf area
	float LAI[5];     // leaf area index m2 leaf/m2 ground
	float LAICAN;     // LAI of whole canopy
	float LAISUN[5];  // sunlit LAI
	float LAISH[5];   // shaded LAI
//	float IMAX ;      // solar noon total incoming solar radiation MJ/m2/s
	float ITOT;       // total incoming solar radiation MJ/m2/s
	float IDIR ;      // direct beam incoming solar radiation MJ/m2/s
//	float DIRRAD ;    // direct beam solar radiation MJ/m2/day
	float IDIF;       // diffuse incoming solar radiation MJ/m2/s
//	float DIFRAD;     // diffuse solar radiation MJ/m2/day
	float ISUN[5];    // light intensity on sunlit leaves MJ/m2/s/m2 leaf area
	float ISH[5];     // light intensity on shaded leaves MJ/m2/s/m2 leaf area
	float PMAX[5];    // asymptote of photosynthesis-light response curve mg/m2 leaf area/s
	float SUMLAI[5];  // cumulative LAI from top of canopy m2/m2
	float SUMF[5];    // proportion light intercepted to that point in the canopy
	float F[5];       // proportion light intercepted in canopy layer
	float SLAISN[5];  // cumulative sunlit LAI from top of canopy m2/m2
	float CSUN[5];    // photosynthesis by sunlit leaves mg/m2/s
	float CSH[5];     // photosynthesis by shaded leaves mg/m2/s
	float K;          // extinction coefficient
	float G;          // shade projection coefficient
	float ALPHA;      // sun angle (solar elevation above horizon)
	float B;          // conversion coefficient for CHO to biomass g/g
	float BIOMAS[3];  // canopy assimilation g/m2/sec
	float BIO;        // canopy assimilation g/m2/day
	float RADINT[3];  // canopy radiation interception MJ/m2/sec
	float RAD;        // canopy radiation interception MJ/m2/day
	float SCAT;       // leaf light scattering coefficient
	float A;          // Asymptote of Pmax - SLN response curve
	float TIME;       // time of day expressed as fraction of daylength
//	float SOLAR;      // extraterrestrial global radiation (MJ/m2/day)
//	float RATIO;      // radiation attenuation factor for atmosphere; 0.75 if sunny
	float DAYL;       // daylength (hours)
	float LAT;        // latitude (degrees); negative for south
   float SOLARDEC;   // solar declination

   int II;
   int J;
   int L;
   int ITIME;
   float NEWLAT;
//   float ALPHA1;
   float SOLAR1;
   float CON;
   float SLNTOP;
   float SLNBOT;
   float DIR[3];
   float DIF[3];



   //Implementation Section ----------------------------------

	SLNGRAD = sln_gradient;
	G       = shadow_projection;
	B       = biomass_conversion;
	SCAT    = scatter_coeff;
	A       = pmaxmax;
	LAT     = latitude;

   for(int i = 0; i < 5; i++)
      {
      LAI[i]  = (float)(lai_green / 5.0);
      }

        //==========================================================
        //THE FOLLOWING PART ARE ADAPTED FROM GRAEME HAMMER'S CODE
        //==========================================================

//	Set up loop for radiation levels by varying RATIO and DAY
//        90
	   for(II = 0; II < 6; II++)
         {
//         RATIO = 0.25 + 0.1 * float (II - 1);
//
//  	Reduce ratio by 60% for inside a glasshouse
//
//	RATIO = 0.60*RATIO
//	RATIO = 0.25
//
//	Output iteration attributes and headings
//
//	WRITE(20,25) LAT,RATIO,DAY,SLNGRAD
//25	FORMAT(1X,6HLAT = ,F6.1,5X,8HRATIO = ,F5.2,5X,6HDAY = ,F6.1,
//     1	5X,10HSLNGRAD = ,F6.2/1X,46H ETRAD  RADN   DIR   DIF    IMAX   A
//     2VSLN  RUE /)
//
//	Set up loop for average canopy SLN - 0.7 to 3.1 g/m2
//        80
	      for(J = 0; J < 9; J++)
            {
	         AVSLN = (float)(0.7 + 0.3 * float(J - 1));
//
//	Set up loop for Gaussian integration of diurnal response
//	using three times of day
//              65
	         for(ITIME = 0; ITIME < 3; ITIME++)
               {

	            TIME = (float)(0.06 + 0.19 * float(ITIME - 1));
//
//	Calculate global radiation from latitude, day of year, and time of day
//
	            NEWLAT = (PI / 180) * LAT;
	            SOLARDEC = (PI / 180) * 23.45 * sin(double((2 * PI * (284 + day) / 365)));
	            DAYL = acos(-1 * tan(NEWLAT) * tan(SOLARDEC));
//	            SOLAR = 24 * 3600 * 1360 * (DAYL * sin(NEWLAT) * sin(SOLARDEC) +
//                     cos(NEWLAT) * cos(SOLARDEC) * sin(DAYL)) / (PI * 1000000);
	            DAYL = (180 / PI) * (2.0 / 15.0) * DAYL;
	            ALPHA = sin(NEWLAT) * sin(SOLARDEC) + cos(NEWLAT) * cos(SOLARDEC) *
                          cos((PI / 12) * DAYL * (TIME - 0.5));
	            ALPHA = asin(ALPHA);
//	            ALPHA1 = ALPHA * 180 / PI;
//	            SOLAR1 = RATIO * SOLAR;

               SOLAR1 = radiation; //ew - use the actual daily radation for the calculation

//	            IMAX = SOLAR1*(1.0 + sin(2 * PI * 0.5 + 1.5 * PI)) / (DAYL * 60 * 60);
	            ITOT = SOLAR1 * (1.0 + sin(2 * PI * TIME + 1.5 * PI)) / (DAYL * 60 * 60);


//             IDIF = 0.0
	            IDIF = 0.17 * 1360 * sin(ALPHA) / 1000000;
//
//       Increase IDIF by 20% for glasshouse conditions
//
//             IDIF = 1.20*IDIF
	            if(ITOT > IDIF)
                  {
                  IDIF = ITOT;
                  }
	            IDIR = ITOT - IDIF;
//
//	Do calculation by canopy layers; layer 1 is at top of canopy
//
	            for(L = 0; L < 5; L++)
                  {
	               SUMLAI[L] = 0.0;
                  SUMF[L] = 0.0;
	               SLAISN[L] =0.0;
                  }
//
//	Calculate light intercepted and sunlit and shaded leaf area
//	for each canopy layer
//
	            K = G / sin(ALPHA);
	            SUMLAI[0] = LAI[0];
	            SUMF[0] = 1.0 - exp(-1 * K * SUMLAI[0]);
	            F[0] = SUMF[0];
	            SLAISN[0] = SUMF[0] / K ;
	            LAISUN[0] = SLAISN[0];
	            LAISH[0] = LAI[0] - LAISUN[0];

	            for(L = 1; L < 5; L++)
                  {
	               SUMLAI[L] = SUMLAI[L - 1] + LAI[L];
   	            SUMF[L] = 1.0 - exp(-1 * K* SUMLAI[L]);

	               F[L] = max(SUMF[L] - SUMF[L - 1], 0.000001 );
	               SLAISN[L] = SUMF[L] / K;

	               LAISUN[L] = max(SLAISN[L] - SLAISN[L - 1], 0.000001 );
	               LAISH[L] = LAI[L] - LAISUN[L];
                  }
	            LAICAN = SUMLAI[4];
//
//	Calculate light intensity for sunlit and shaded leaf area
//	for each canopy layer
//
	            for(L = 0; L < 5; L++)
                  {
                  ISUN[L] = IDIR * F[L] / LAISUN[L] + IDIF * F[L] / LAI[L];
                  }
	            ISH[0] = IDIF * F[0] / LAI[0] + SCAT*(ISUN[0] * LAISUN[0]) /
                     (LAISH[0] + LAISH[1]);
	            for(L = 1; L < 4; L++)
                  {
                  ISH[L] = IDIF * F[L] / LAI[L] + SCAT*(ISUN[L - 1]*LAISUN[L - 1])/
                           (LAISH[L - 1] + LAISH[L]) + SCAT * (ISUN[L] * LAISUN[L]) /
                           (LAISH[L] + LAISH[L + 1]);
                  }

	            ISH[4] = IDIF * F[4]/LAI[4] + SCAT *
                     (ISUN[3] * LAISUN[3]) / (LAISH[3] + LAISH[3]) +
                     SCAT * (ISUN[4] * LAISUN[4]) / LAISH[4];
//
//	Calculate SLN for each layer using SLNGRAD
//
	            CON = AVSLN + SLNGRAD * (LAICAN / 2.0);
	            SLN[0] = (CON + (CON - SLNGRAD*SUMLAI[0])) / 2.0;

               for(L = 1; L < 5; L++)
                  {
	               SLNTOP = CON - SLNGRAD * SUMLAI[L - 1];
	               SLNBOT = CON - SLNGRAD * SUMLAI[L];
	               SLN[L] = (SLNTOP + SLNBOT) / 2.0;
	               if (SLN[L] <= 0.61)
                     {
                     SLN[L] = 0.61;
                     }
                  }
//
//	Calculate RUE for each canopy layer from
//	photosynthesis of sunlit and shade leaves in each layer
//
	            for(L = 0; L < 5; L++)
                  {
	               PMAX[L] = A * (2.0 / (1.0 + exp(-0.9 * (SLN[L] - 0.6))) - 1.0);
	               CSUN[L] = LAISUN[L]*PMAX[L]*(1.0 - exp(-5000.0 * ISUN[L] / PMAX[L]));
	               CSH[L] = LAISH[L] * PMAX[L] * (1.0 - exp(-5000.0 * ISH[L] / PMAX[L]));
                  RUE[L] = B / 1000.0 * (CSUN[L] + CSH[L])/ (F[L] * ITOT);
                  }
//	Calculate assimilation and radiation intercepted for the entire canopy
//
	            BIOMAS[ITIME - 1] = 0.0;
	            for(L = 0; L < 5; L++)
                  {
	               BIOMAS[ITIME - 1] = BIOMAS[ITIME - 1] + CSUN[L] + CSH[L];
                  }
	            BIOMAS[ITIME - 1] = B / 1000.0 * BIOMAS[ITIME - 1];
	            RADINT[ITIME - 1] = SUMF[4] * ITOT;
               DIR[ITIME - 1] = IDIR;
	            DIF[ITIME - 1] = IDIF;
               }
//
//	Calculate BIO, RAD & RUE for the day; Gaussian integration
//	Calculate DIRRAD & DIFRAD for the day; Gaussian integration
//
            BIO = 3600.0 * DAYL * (BIOMAS[0] + 1.6 * BIOMAS[1] + BIOMAS[2]) / 3.6;
	         RAD = 3600.0 * DAYL * (RADINT[0] + 1.6 * RADINT[1] + RADINT[2]) / 3.6;
	         RUEDAY = BIO / RAD;
//	         DIRRAD = 3600.0 * DAYL * (DIR[0] + 1.6 * DIR[1] + DIR[2]) / 3.6;
//	         DIFRAD = 3600.0 * DAYL * (DIF[0] + 1.6 * DIF[1] + DIF[2]) / 3.6;

//
//	Calculate average SLN for canopy
//
	         AVSLN = 0.0;
	         for(L = 1; L < 5; L++)
               {
	            AVSLN = AVSLN + (LAI[L]* SLN[L]) / SUMLAI[4];
               }
            }
         }


   *rue_sln = RUEDAY;
   }



//=========================================================================
void crop_dm_pot_rue_co2 (float current_stage,
                          float *rue,
                          float radn_int,
                          float temp_stress_photo,
                          float nfact_photo,
                          float co2_modifier,
                          float *dlt_dm_pot)     //(OUTPUT) potential dry matter  (carbohydrate) production (g/m^2
//=========================================================================
/*  Purpose
*       Potential biomass (carbohydrate) production from
*       photosynthesis (g/m^2) affected  by CO2 level
*
*  Mission Statement
*       Calculate water non-limiting biomass production including CO2 effect
*
*  Changes
*       090994 jngh specified and programmed
*       970317 slw templated
*/
   {
   //  Local Variables
   int current_phase;               // current phase number
   float usrue;                     // radiation use efficiency under
                                    // no stress (g biomass/mj)

   // Implementation Section ----------------------------------
   current_phase = int (current_stage);
   usrue = rue[current_phase-1] * co2_modifier *
                  min(temp_stress_photo, nfact_photo);

         // potential dry matter production with temperature
         // and N content stresses is calculated.
         // This is g of dry biomass produced per MJ of intercepted
         // radiation under stressed conditions.

   *dlt_dm_pot = usrue * radn_int;
   }

//  Purpose
//      Derives seneseced plant dry matter (g/m^2) for the day
//
//  Mission Statement
//  Calculate biomass senescence as a function of %3
void plant_dm_senescence (int num_part               // (INPUT) number of plant parts
                               , float // max_Table            // (INPUT) max lookup length
                               , float independant_variable // (INPUT) independant variable which is said to drive senescence.
                               , float **c_x_dm_sen_frac    // (INPUT)  lookup for independant variabl
                               , float **c_y_dm_sen_frac    // (INPUT)  fraction of  material senescin
                               , int *c_num_dm_sen_frac     // (INPUT)  fraction of  material sene
                               , float *g_dm_green          // (INPUT)  live plant dry weight (biomass
                               , float *dlt_dm_senesced)    // (OUTPUT) actual biomass senesced
                                                            //          from plant parts (g/m^2)
   {
     int    part;
     float  fraction_senescing;      // dm senesced (g/m^2)

     for (part = 0; part < num_part; part++)
         {
         fraction_senescing
              = linear_interp_real (independant_variable
                                    , c_x_dm_sen_frac[part]
                                    , c_y_dm_sen_frac[part]
                                    , c_num_dm_sen_frac[part]);

         fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
         dlt_dm_senesced[part] = g_dm_green[part]* fraction_senescing;
         }
   }

//+  Purpose
//       Perform grain filling calculations

//+  Changes
void legnew_bio_yieldpart_demand2(
    float g_grain_no
    ,float p_potential_grain_filling_rate
    ,float g_maxt
    ,float g_mint
    ,float *c_x_temp_grainfill
    ,float *c_y_rel_grainfill
    ,int   c_num_temp_grainfill
    ,float *g_dlt_dm_grain_demand
    ) {
    //+  Local Variables
    float tav;

        // we are in grain filling stage
        tav = (float)((g_maxt+g_mint)/2.0);

        *g_dlt_dm_grain_demand = g_grain_no
                    * p_potential_grain_filling_rate
                    * linear_interp_real(tav
                                         ,c_x_temp_grainfill
                                         ,c_y_rel_grainfill
                                         ,c_num_temp_grainfill);


    }

