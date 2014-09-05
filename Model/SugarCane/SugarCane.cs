﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using ModelFramework;
using CSGeneral;



///<summary>
/// .NET port of the Fortran Sugar model
/// Ported by Shaun Verrall Feb 2013
///</summary> 


public class SugarCane
    {

    [Link]
    public Clock Clock;


    //CONSTANTS


    #region Constants (Hard Coded)


                //!     ================================================================
                //!      sugar_array_sizes
                //!     ================================================================
                //!      array size_of settings
          const  int    max_leaf   = 200;  //! maximum number of plant leaves
          const  int    max_layer  = 100;  //! Maximum number of layers in soil
          const  int    max_table  = 10;   //! Maximum size_of of tables


                //!     ================================================================
                //!      sugar_crop status
                //!     ================================================================
                //!      crop status names
          const  String  crop_alive = "alive";
          const  String  crop_dead  = "dead";
          const  String  crop_out   = "out";


                //!     ================================================================
                //!      sugar_processes_for_stress
                //!     ================================================================
                //!      Process names used for stress
          const  int photo      = 1;    //! photosynthesis flag
          const  int expansion  = 2;    //! cell expansion flag
          const  int pheno      = 3;    //! phenological flag


                //!     ================================================================
                //!      sugar_ plant parts
                //!     ================================================================
                //!      plant part names

          const  int    root    = 0;    //! root
          const  int    leaf    = 1;    //! leaf
          const  int    sstem   = 2;    //! structural stem
          const  int    cabbage = 3;    //! cabbage
          const  int    sucrose = 4;    //! grain

          const  int    max_part = 5;   //! number of plant parts

          string[] part_name = new string[max_part] {"root", "leaf", "sstem", "cabbage", "sucrose"};  


                //!     ================================================================
                //!     sugar_phenological_names
                //!     ================================================================
                //!      Define crop phenological stage and phase names

            //! administration

          const  int    max_stage = 6;              //! number of growth stages
          const  int    now = max_stage + 1;        //! at this point in time ()


            //! mechanical operations

          const  int    crop_end    = 6;        //! crop_end stage
          const  int    fallow = crop_end;      //! fallow phase

          const  int    sowing      = 1;                //! Sowing stage
          const  int    sow_to_sprouting = sowing;      //! seed sow_to_germ phase

          const  int    sprouting   = 2;                //! Germination stage
          const  int    sprouting_to_emerg = sprouting; //! sprouting_to_emerg elongation phase

          const  int    emerg       = 3;                //! Emergence stage
          const  int    emerg_to_begcane = emerg;       //! emergence to start of cane growth

          const  int    begcane     = 4;              
          const  int    begcane_to_flowering = begcane;  

          const  int    flowering   = 5;             
          const  int    flowering_to_crop_end = flowering; 


    #endregion


    #region Module Constants (from SIM file but it gets from INI file)


        #region Upper and Lower Bound Constants (for Sugar, Soil and Met variables)

        //*     ===========================================================
        //      subroutine sugar_read_constants ()
        //*     ===========================================================


            [Param]
            [Output] [Units("()")]
            string             crop_type;    //sv- this will be set to "SugarCane"   //sv- used by Micromet module

            //! upper limit
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            double          tt_emerg_to_begcane_ub;

            //! upper limit
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            double          tt_begcane_to_flowering_ub;

            //! upper limit
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("oC")] 
            double          tt_flowering_to_crop_end_ub;


                //!    sugar_N_uptake

            [Param(MinVal = 1, MaxVal = 2)]
            int             n_uptake_option;


                    //if (n_uptake_option == 1) then

            //! time constant for uptake by  diffusion (days). H van Keulen & NG Seligman. Purdoe 1987. 
            //! This is the time it would take to take up by diffusion the current amount of N if it wasn't depleted between time steps
            [Param(IsOptional=true, MinVal = 0.0, MaxVal = 100.0, Name = "no3_diffn_const")]
            [Units("days")]         
            double          NO3_diffn_const = Double.NaN;


            [Param (IsOptional=true)]
            string          n_supply_preference = "";

                    //else

            [Param(IsOptional=true, MinVal = 0.0, MaxVal = 1.0)]
            [Units("/day")]        
            double          kno3 = Double.NaN;
        
        
            [Param(IsOptional=true, MinVal = 0.0, MaxVal = 10.0)]
            [Units("oC")]         
            double          no3ppm_min = Double.NaN;
        

            [Param(IsOptional=true, MinVal = 0.0, MaxVal = 1.0)]
            [Units("/day")]         
            double          knh4 = Double.NaN;
        
        
            [Param(IsOptional=true, MinVal = 0.0, MaxVal = 10.0)]
            [Units("oC")]
            double          nh4ppm_min = Double.NaN;
        
            [Param(IsOptional=true, MinVal = 0.0, MaxVal = 100.0)]
            [Units("g/m2")]         
            double          total_n_uptake_max = Double.NaN;

                            //endif



             //!    sugar_get_root_params

        //! upper limit of lower limit (mm/mm)
            [Param(MinVal = 0.0, MaxVal = 1000.0)]
            [Units("oC")]      
            double        ll_ub;

            //! upper limit of water uptake factor
            [Param(MinVal = 0.0, MaxVal = 1000.0)]
            [Units("oC")]      
            double        kl_ub;


                     //!    sugar_watck

        //! lowest acceptable value for ll
            [Param(MinVal = 0.0, MaxVal = 1000.0)]  
            double        minsw;



                         //!    sugar_get_other_variables

             //! checking the bounds of the bounds..

        //! upper limit of latitude for model (oL)
            [Param(MinVal = -90.0, MaxVal = 90.0)]
            [Units("oL")]      
            double        latitude_ub;

            //! lower limit of latitude for model(oL)
            [Param(MinVal = -90.0, MaxVal = 90.0)]
            [Units("oL")]      
            double          latitude_lb;



            //! upper limit of maximum temperature (oC)
            [Param(MinVal = 0.0, MaxVal = 60.0)]
            [Units("oC")]      
            double          maxt_ub;

            //! lower limit of maximum temperature (oC)
            [Param(MinVal = 0.0, MaxVal = 60.0)]
            [Units("oC")]      
            double          maxt_lb;



            //! upper limit of minimum temperature (oC)
            [Param(MinVal = 0.0, MaxVal = 40.0)]
            [Units("oC")]      
            double          mint_ub;

            //! lower limit of minimum temperature (oC)
            [Param(MinVal = -100.0, MaxVal = 100.0)]   //TODO: sv-THIS COULD CAUSE PROBLEMS MaxValue should be less then 40.0
            [Units("oC")]      
            double          mint_lb;



            //! upper limit of solar radiation (Mj/m^2)
            [Param(MinVal = 0.0, MaxVal = 100.0)]
            [Units("MJ/m^2")]      
            double          radn_ub;

            //! lower limit of solar radiation (Mj/M^2)
            [Param(MinVal = 0.0, MaxVal = 100.0)]
            [Units("MJ/m^2")]      
            double          radn_lb;



            //! upper limit of layer depth (mm)
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("mm")]      
            double          dlayer_ub;

            //! lower limit of layer depth (mm)
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("mm")]      
            double          dlayer_lb;



            //! upper limit of dul (mm)
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("mm")]      
            double          dul_dep_ub;

            //! lower limit of dul (mm)
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("mm")]      
            double          dul_dep_lb;



            //! upper limit of soilwater depth (mm)
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("mm")]      
            double          sw_dep_ub;

            //! lower limit of soilwater depth (mm)
            [Param(MinVal = 0.0, MaxVal = 10000.0)]
            [Units("mm")]      
            double          sw_dep_lb;



            //! upper limit of soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "no3_ub")]
            [Units("kg/ha")]      
            double          NO3_ub;

            //! lower limit of soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "no3_lb")]
            [Units("kg/ha")]       
            double          NO3_lb;



            //! upper limit of minimum soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "no3_min_ub")]
            [Units("kg/ha")]     
            double          NO3_min_ub;

            //! lower limit of minimum soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "no3_min_lb")]
            [Units("kg/ha")]       
            double          NO3_min_lb;



            //! upper limit of soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "nh4_ub")]
            [Units("kg/ha")]     
            double          NH4_ub;

            //! lower limit of soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "nh4_lb")]
            [Units("kg/ha")]       
            double          NH4_lb;



            //! upper limit of minimum soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "nh4_min_ub")]
            [Units("kg/ha")]        
            double          NH4_min_ub;

            //! lower limit of minimum soil NO3 (kg/ha)
            [Param(MinVal = 0.0, MaxVal = 100000.0, Name = "nh4_min_lb")]
            [Units("kg/ha")]       
            double          NH4_min_lb;


        #endregion 



        #region Crop Constants (for Plant/Ratoon)

            [Param]
            CropConstants plant = new CropConstants();

            [Param]
            CropConstants ratoon = new CropConstants();


            CropConstants crop;

        #endregion



        #region Cultivar Constants (for each cultivar)

            [Link]
            CultivarConstants[] cultivars = null;


            CultivarConstants cult;

        #endregion




        #region Root Constants (different if using (SoilWat or SWIM) or Eo modules in the simulation)

        //*     ===========================================================
        //      subroutine sugar_read_root_params ()        //*     Crop initialisation - get root profile parameters
        //*     ===========================================================
      

                    //! cproc_sw_demand_bound

            [Param(IsOptional =  true, MinVal = 0.0, MaxVal = 100.0)]
            double      eo_crop_factor = 100.0;
                    //double      eo_crop_factor = eo_crop_factor_default;
            //TODO: Either find a way to use this or remove 'eo_crop_factor_default' from the ini file.


            [Param] 
            string      uptake_source;


            //swim3 is an INPUT not a PARAM

            
            [Param(MaxVal = 0.0, MinVal = 1.0)]
            double[]    xf = new double[max_layer];  //! eXtension rate Factor (0-1) 



                    //! sugar_sw_supply

            //TODO: Either find a way to use this or remove 'll_ub' from the ini file.
            //[MinVal = 0.0, MaxVal = ll_ub]
            [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
            double[]    ll = new double[max_layer];


            //ll15 is an INPUT not a PARAM

            
            //TODO: Either find a way to use this or remove 'kl_ub' from the ini file.
            //[MinVal = 0.0, MaxVal = kl_ub]
            [Param(MinVal = 0.0, MaxVal = 1.0)]
            double[]    kl = new double[max_layer];  //! root length density factor for water


            [Param(MinVal = 0.0, MaxVal = 20.0)]
            double[]    rlv_init = new double[max_layer];


        #endregion



    #endregion




    //DAILY INPUTS FROM OTHER MODULES


    #region Meteorology Inputs

        //*     ================================================================
        //      subroutine sugar_get_met_variables ()
        //*     ================================================================


        //*+  Mission Statement
        //*     Get the values of variables/arrays from the met module



        //! INPUT module
        //! ------------

        //[ MinVal=latitude_lb, MaxVal=latitude_ub]
        [Input]
        [Units("oL")]
        double    latitude;


        //[ MinVal=maxt_lb, MaxVal=maxt_ub]
        [Input]
        [Units("oC")]
        double    maxt;


        //[ MinVal=mint_lb, MaxVal=mint_ub]
        [Input]
        [Units("oC")]
        double    mint;


        //[ MinVal=radn_lb, MaxVal=radn_ub]
        [Input]
        [Units("Mj/m^2")]
        double    radn;


        //[ MinVal=0.0, MaxVal=20.0]
        [Input]
        [Units("mm")]
        double    eo;



        //! Canopy Module
        //! -------------

        //TODO: this is supposed to be mapped to fr_intc_radn without the last underscore, but [INPUT] does not allow renaming like [Param] does. So I have rename all fr_intc_radn to fr_intc_radn_
        //need to remove the local variable fr_intc_radn as well. 
        //[ MinVal=0.0, MaxVal=1.0]
        [Input(IsOptional=true)]
        double    fr_intc_radn_;



    #endregion



    #region SoilWat Inputs


        //*     ===========================================================
        //      subroutine sugar_read_root_params ()        //*     Crop initialisation - get root profile parameters
        //*     ===========================================================

        //[MinVal = 0.0, MaxVal = 1.0]
        [Input(IsOptional = true)]
        double      swim3 = Double.NaN;

        //[MinVal = 0.0, MaxVal = ll_ub]
        [Input(IsOptional = true)]
        double[]    ll15 = new double[max_layer];



    
        //*     ================================================================
        //      subroutine sugar_get_soil_variables ()
        //*     ================================================================

        //*+  Purpose
        //*      Get the values of variables/arrays from soil modules.




        //! Soil Water module
        //! -----------------

        //[ MinVal=dlayer_lb, MaxVal=dlayer_ub]
        [Input(IsOptional = true)]
        [Units("mm")]
        double[]    dlayer = new double[max_layer];



        int         num_layers
            {
            get 
                {
                if (dlayer.Length == max_layer)
                    {
                    //! we assume dlayer hasn't been initialised yet.
                    return dlayer.Length;
                    }
                else
                    {
                    return dlayer.Length;   //this is a temporary fix. remove it later.
                    }
                }
            }



        //[ MinVal=0.0, MaxVal=2.65]
        [Input(IsOptional = true)]
        [Units("g/cc")]
        double[]    bd = new double[max_layer];



        //[MinVal=dul_dep_lb, MaxVal=dul_dep_ub]
        [Input(IsOptional = true)]
        [Units("mm")]
        double[]    dul_dep = new double[max_layer];


        //[MinVal=sw_dep_lb, MaxValsw_dep_ub]
        [Input(IsOptional = true)]
        [Units("mm")]
        double[]    sw_dep = new double[max_layer];

        //[MinVal=sw_dep_lb, MaxVal=sw_dep_ub]
        [Input(IsOptional = true)]
        [Units("mm")]
        double[]    sat_dep = new double[max_layer];

        //[MinVal=sw_dep_lb, MaxVal=sw_dep_ub]
        [Input(IsOptional = true)]
        [Units("mm")]
        double[]    ll15_dep = new double[max_layer];

    #endregion



    #region SoilN Inputs

        //! soil nitrogen module
    //! --------------------



        //[ MinVal=NO3_lb, MaxVal=NO3_ub]
        [Input()]
        [Units("kg/ha")]
        double[]    no3 = new double[max_layer];


        //[ MinVal=NO3_min_lb, MaxVal=NO3_min_ub]
        [Input()]
        [Units("kg/ha")]
        double[]    no3_min = new double[max_layer];


        //[ MinVal=NH4_lb, MaxVal=NH4_ub]
        [Input()]
        [Units("kg/ha")]
        double[]    nh4 = new double[max_layer];


        //[ MinVal=NH4_min_lb, MaxVal=NH4_min_ub]
        [Input()]
        [Units("kg/ha")]
        double[]    nh4_min = new double[max_layer];


        //[MinVal=-10., MaxVal=80.]
        //[Input()]
        //[Units("oC")]
        //double[]    st = new double[max_layer];

        //TODO: put 'st' back into sugar_zero_soil_globals() when you uncomment the stuff above.

    #endregion





    //SETTABLE VARIABLES IN THIS MODULE  (THESE ARE ALSO OUTPUTS)


    #region Set My Variables



    [Output]
    [Units("(/m2)")]
    double plants
        {
        get
            {
            return g_plants;
            }
        set
            {
            g_plants = value;

            if (g_current_stage > emerg)
                {
                warning_error("You have updated plant number after emergence");
                }

            bound_check_real_var(value, 0.0, 1000.0, "plants");
            }
        }


    [Output]
    [Units("()")]
    double lodge_redn_photo
        {
        get
            {
            return g_lodge_redn_photo;
            }
        set
            {
            g_lodge_redn_photo = value;  //should we set crop.lodge_redn_photo too?
            bound_check_real_var(value, 0.0, 1.0, "lodge_redn_photo");
            }
        }


    [Output]
    [Units("()")]
    double lodge_redn_sucrose
        {
        get
            {
            return g_lodge_redn_sucrose;
            }
        set
            {
            g_lodge_redn_sucrose = value;  //should we set crop.lodge_redn_sucrose too?
            bound_check_real_var(value, 0.0, 1.0, "lodge_redn_sucrose");
            }
        }


    [Output]
    [Units("()")]
    double lodge_redn_green_leaf
        {
        get
            {
            return g_lodge_redn_green_leaf;
            }
        set
            {
            g_lodge_redn_green_leaf = value;  //should we set crop.lodge_redn_green_leaf too?
            bound_check_real_var(value, 0.0, 1.0, "lodge_redn_green_leaf");
            }
        }


    #endregion





     //MODEL


    #region Local Variables 
            


        //Root parameters that are calculated from values read in from the INI/SIM file
        //see sugar_read_root_params() for where they are calculated.
        double[]    g_ll_dep = new double[max_layer];          //! lower limit of plant-extractable soil water for soil layer L (mm)
        double[]    g_root_length =  new double[max_layer];  




        //!     ================================================================
        //!     Sugar Globals
        //!     ================================================================

      string        g_crop_status;       //! status of crop
      string        g_crop_cultivar;    //! cultivar name
      bool          g_plant_status_out_today;
      double        g_sowing_depth;        //! sowing depth (mm)
    //TODO: OnTick Event sets the year and day_of_year variables below. See SoilWater module for [Link] Clock method of setting thsese values.
      int           g_year; 	//! year
      int           g_day_of_year; 			//! day of year
      //double        sw_avail_fac_deepest_layer;
      double        g_temp_stress_photo;
      double        g_temp_stress_stalk;
      double        g_swdef_expansion;
      double        g_swdef_stalk;
      double        g_swdef_photo;
      double        g_swdef_pheno;
      double        g_swdef_fixation;
      double        g_nfact_expansion;
      double        g_nfact_stalk;
      double        g_nfact_photo;
      double        g_nfact_pheno;
      double        g_lodge_redn_photo;
      double        g_lodge_redn_sucrose;
      double        g_lodge_redn_green_leaf;
      double        g_sucrose_fraction; 				//! fraction of cane C going to sucrose
      double        g_oxdef_photo;
      //double        fr_intc_radn; 				//! fraction of radiation intercepted by canopy
      //double        latitude; 				//! latitude (degrees, negative for southern hemisphere)
      //double        radn; 				//! solar radiation (Mj/m^2/day)
      //double        eo; 				//! potential evapotranspiration (mm)
      //double        mint; 				//! minimum air temperature (oC)
      //double        maxt; 				//! maximum air temperature (oC)
      double[]      g_cnd_photo = new double[max_stage]; 				//! cumulative nitrogen stress type 1
      double[]      g_cswd_photo = new double[max_stage]; 				//! cumulative water stress type 1
      double[]      g_cswd_expansion = new double[max_stage]; 				//! cumulative water stress type 2
      double[]      g_cswd_pheno = new double[max_stage]; 				//! cumulative water stress type 3
      double        g_dlt_tt; 				//! daily thermal time (growing deg day)
      double[]      g_tt_tot= new double[max_stage]; 				//! the sum of growing degree days for a phenological stage (oC d)
      double[]      g_phase_tt= new double[max_stage]; 				//! Cumulative growing degree days required for each stage (deg days)

      double        g_dlt_stage; 				//! change in stage number
      double        g_current_stage; 				//! current phenological stage
      double        g_previous_stage; 				//! previous phenological stage
      double[]      g_days_tot = new double[max_stage]; 				//! duration of each phase (days)
      double        g_dlt_canopy_height; 				//! change in canopy height (mm)
      double        g_canopy_height; 				//! canopy height (mm)
      double        g_phase_devel; 				//! development of current phase ()
      int           g_ratoon_no;
      double        g_plants; 				//! Plant density (plants/m^2)
      double        g_dlt_plants; 				//! change in Plant density (plants/m^2)
      double        g_initial_plant_density; 				//!sowing density (plants/m^2)
      double        g_dlt_root_depth; 				//! increase in root depth (mm)
      double        g_root_depth; 				//! depth of roots (mm)
      bool          g_lodge_flag;
      //double        rue;
      //double[]      uptake_water = new double[max_layer]; 				//! sw uptake as provided by another module in APSIM (mm)
      //int           num_uptake_water; 				//! number of layers in uptake_water()
      //int           num_layers; 				//! number of layers in profile ()
      double        g_transpiration_tot; 				//! cumulative transpiration (mm)
      //double        g_N_uptake_tot; 				//! cumulative total N uptake (g/m^2)
      double        g_n_demand_tot; 				//! sum of N demand since last output (g/m^2)
      double        g_n_conc_act_stover_tot; 				//! sum of tops actual N concentration (g N/g biomass)
      //double        N_conc_crit_stover_tot; 				//! sum of tops critical N concentration (g N/g biomass)
      //double        N_uptake_stover_tot; 				//! sum of tops N uptake (g N/m^2)
      double        g_lai_max; 				//! maximum lai - occurs at flowering
      int           g_isdate; 				//! flowering day number
      //int           mdate; 				//! maturity day number
      //double        dm_graze; 				//! dm removed by grazing
      //double        n_graze; 				//! N removed by grazing

      double[]      g_plant_wc = new double[max_part];
      double[]      g_dlt_plant_wc = new double[max_part];
      //string        uptake_source;
      //double[]      dlayer  = new double[max_layer]; 				//! thickness of soil layer I (mm)
      //double[]      bd = new double[max_layer];
      double[]      g_dlt_sw_dep = new double[max_layer]; 				//! water uptake in each layer (mm water)
      //double[]      sat_dep  = new double[max_layer]; 				//!
      //double[]      dul_dep  = new double[max_layer]; 				//! drained upper limit soil water content for soil layer L (mm water)
      //double[]      ll15_dep  = new double[max_layer]; 				//!
      //double[]      sw_dep  = new double[max_layer]; 				//! soil water content of layer L (mm)
      //double[]      st  = new double[max_layer];
      double        g_sw_demand; 				//! total crop demand for water (mm)
      double        g_sw_demand_te; 				//! sw demand calculated from TE
      double[]      g_sw_avail_pot = new double[max_layer]; 				//! potential extractable soil water (mm)
      double[]      g_sw_avail = new double[max_layer]; 				//! actual extractable soil water (mm)
      double[]      g_sw_supply  = new double[max_layer]; 				//! potential water to take up (supply) from current soil water (mm)
      double[]      g_dlt_root_length = new double[max_layer];
      double[]      g_dlt_root_length_senesced = new double[max_layer];
      //double[]      root_length  = new double[max_layer];
      double        g_dlt_plants_death_drought;
      double        g_dlt_plants_failure_leaf_sen;
      double        g_dlt_plants_failure_emergence;
      double        g_dlt_plants_failure_germ;
      double        g_dlt_plants_death_lodging;
      double        g_dlt_dm; 				//! the daily biomass production (g/m^2)
      double[]      g_dlt_dm_green = new double[max_part]; 				//! plant biomass growth (g/m^2)
      //double[]      dlt_dm_green_pot = new double[max_part]; 				//! plant biomass growth (g/m^2)
      double[]      g_dlt_dm_senesced = new double[max_part]; 				//! plant biomass senescence (g/m^2)
      double[]      g_dlt_dm_realloc  = new double[max_part];
      double[]      g_dlt_dm_detached = new double[max_part]; 				//! plant biomass detached (g/m^2)
      double[]      g_dlt_dm_dead_detached = new double[max_part]; 				//! plant biomass detached from dead plant (g/m^2)
      double[]      g_dlt_dm_green_retrans = new double[max_part]; 				//! plant biomass retranslocated (g/m^2)
      //double[]      dm_stress_max= new double[max_stage]; 				//! sum of maximum daily stress on dm production per phase
      //double[]      dlt_dm_stress_max; 				//! maximum daily stress on dm production (0-1)
      //double[]      dm_green_demand = new double[max_part]; 				//! biomass demand of the plant parts (g/m^2)
      double[]      g_dm_dead = new double[max_part]; 				//! dry wt of dead plants (g/m^2)
      double[]      g_dm_green = new double[max_part]; 				//! live plant dry weight (biomass) (g/m^2)
      double[]      g_dm_senesced = new double[max_part]; 				//! senesced plant dry wt (g/m^2)
      double[]      g_dm_plant_top_tot= new double[max_stage]; 				//! total carbohydrate production in tops per stage (g/plant)
      double        g_partition_xs; 				//! dm used in partitioning that was excess to plant demands.(g/m^2)
      //double        partition_xs_pot; 				//! dm used in partitioning that was excess to plant demands.(g/m^2)
      double        g_dlt_dm_pot_rue;
      double        g_dlt_dm_pot_te;
      double        g_dlt_dm_pot_rue_pot;
      double        g_radn_int;
      double        g_transp_eff;
      double        g_min_sstem_sucrose;
      double        g_slai; 				//! area of leaf that senesces from plant
      double        g_dlt_slai; 				//! area of leaf that senesces from plant
      double        g_dlt_lai; 				//! actual change in live plant lai
      double        g_dlt_lai_pot; 				//! potential change in live plant lai
      double        g_dlt_lai_stressed; 				//! potential change in live plant lai after stresses applied
      double        g_lai; 				//! live plant green lai
      double        g_tlai_dead; 				//! total lai of dead plants
      double        g_dlt_slai_detached; 				//! plant senesced lai detached
      double        g_dlt_tlai_dead_detached; 				//! plant lai detached from dead plant
      //double        dlt_tlai_dead; 				//! plant lai change in dead plant
      double        g_dlt_slai_age; 				//! senesced lai from age
      double        g_dlt_slai_light; 				//! senesced lai from light
      double        g_dlt_slai_water; 				//! senesced lai from water
      double        g_dlt_slai_frost; 				//! senesced lai from frost
      double        g_sla_min; 				//! minimum specific leaf area (mm2/g)
      double[]      g_leaf_no_zb = new double[max_stage]; 				//! number of fully expanded leaves ()
      double[]      g_node_no_zb = new double[max_stage]; 
      double[]      g_node_no_dead_zb= new double[max_stage]; 				//! no of dead leaves ()
      double        g_dlt_leaf_no; 				//! fraction of oldest leaf expanded ()
      double        g_dlt_node_no;
      double        g_dlt_node_no_dead; 				//! fraction of oldest green leaf senesced ()
      //double        leaf_no_final; 				//! total number of leaves the plant produces
      double[]      g_leaf_area_zb= new double[max_leaf];  				//! leaf area of each leaf (mm^2)
      double[]      g_leaf_dm_zb= new double[max_leaf];  				//! dry matter of each leaf (g)
      double        g_node_no_detached_ob; 				//! no of dead leaves detached from records

      double[]      g_n_demand  = new double[max_part]; 				//! plant nitrogen demand (g/m^2)
      //double[]      N_max = new double[max_part]; 				//! max nitrogen demand(g/m^2)
      double[]      g_dlt_n_green = new double[max_part]; 				//! actual N uptake into plant (g/m^2)
      double[]      g_dlt_n_senesced = new double[max_part]; 				//! actual N loss with senesced plant (g/m^2)
      double[]      g_dlt_n_realloc  = new double[max_part];
      double[]      g_dlt_n_detached = new double[max_part]; 				//! actual N loss with detached plant (g/m^2)
      double[]      g_dlt_n_dead_detached = new double[max_part]; 				//! actual N loss with detached dead plant (g/m^2)
      double[]      g_n_dead = new double[max_part]; 				//! plant N content of dead plants (g N/m^2)
      double[]      g_n_green = new double[max_part]; 				//! plant nitrogen content (g N/m^2)
      double[]      g_n_senesced = new double[max_part]; 				//! plant N content of senesced plant (g N/m^2)
      double[]      g_dlt_n_retrans = new double[max_part]; 				//! nitrogen retranslocated out from parts to grain (g/m^2)
      double[]      g_dlt_no3gsm = new double[max_layer]; 				//! actual NO3 uptake from soil (g/m^2)
      double[]      g_dlt_nh4gsm = new double[max_layer]; 				//! actual NO3 uptake from soil (g/m^2)
      double[]      g_no3gsm  = new double[max_layer]; 				//! nitrate nitrogen in layer L (g N/m^2)
      double[]      g_nh4gsm  = new double[max_layer]; 				//! nitrate nitrogen in layer L (g N/m^2)
      double[]      g_no3gsm_min = new double[max_layer]; 				//! minimum allowable NO3 in soil (g/m^2)
      double[]      g_nh4gsm_min = new double[max_layer]; 				//! minimum allowable NO3 in soil (g/m^2)
      //double[]      uptake_no3 = new double[max_layer]; 				//! uptake of no3 as provided by another module in APSIM (kg/ha)
      //double[]      uptake_nh4 = new double[max_layer]; 				//! uptake of no3 as provided by another module in APSIM (kg/ha)
      double[]      g_no3gsm_diffn_pot = new double[max_layer]; 				//! potential NO3 (supply) from soil (g/m^2), by diffusion
      double[]      g_no3gsm_mflow_avail = new double[max_layer]; 				//! potential NO3 (supply) from soil (g/m^2) by mass flow

      double[]      g_no3gsm_uptake_pot = new double[max_layer];
      double[]      g_nh4gsm_uptake_pot = new double[max_layer];

      double        g_n_fix_pot;
      //int           num_uptake_no3; 				//! number of layers in uptake_no3 ()
      //int           num_uptake_nh4; 				//! number of layers in uptake_no3 ()
      double[]      g_n_conc_crit = new double[max_part]; 				//! critical N concentration (g N/g biomass)
      //double[]      N_conc_max = new double[max_part]; 				//! max N concentration (g N/g biomass)
      double[]      g_n_conc_min = new double[max_part]; 				//! minimum N concentration (g N/g biomass)

      //double[]      dm_plant_min = new double[max_part]; 				//! minimum weight of each plant part (g/plant)








 				//sv- I THINK THE FOLLOWING ARE NOT USED, BUT HAVE BEEN ACCIDENTLY LEFT IN THE SOURCE CODE WHEN THEY SHOULD HAVE BEEN REMOVED.
        ////!     ================================================================
        ////!     Sugar Constants
        ////!     ================================================================
        //double      frost_kill;             //! temperature threshold for leaf death (oC)
        //int         num_dead_lfno;
        //int         num_factors;            //! size_of of table
        //int         num_x_swdef_cellxp;        
        //double      leaf_no_min;            //! lower limit of leaf number ()
        //double      leaf_no_max;            //! upper limit of leaf number ()




    #endregion


    #region Functions to Zero Variables


        private void ZeroArray(ref double[] A)
            {
                for (int i = 0; i < A.Length; i++)
                {
                    A[i] = 0.0;
                }
            }




        private void sugar_zero_globals()
            {

        //    //*     ===========================================================
        //    //      subroutine sugar_zero_globals ()
        //    //*     ===========================================================

        //    //*+  Purpose
        //    //*       Zero global variables and arrays


        //        //! zero pools etc.

            ZeroArray(ref g_cnd_photo);
            ZeroArray(ref g_cswd_expansion);
            ZeroArray(ref g_cswd_pheno);
            ZeroArray(ref g_cswd_photo);
            ZeroArray(ref g_days_tot);
            ZeroArray(ref g_dm_dead);
            ZeroArray(ref g_dm_green);
        //    ZeroArray(ref dm_plant_min);
            ZeroArray(ref g_plant_wc);
            ZeroArray(ref g_dm_plant_top_tot);
            ZeroArray(ref g_leaf_area_zb);
            ZeroArray(ref g_leaf_dm_zb);
            ZeroArray(ref g_leaf_no_zb);
            ZeroArray(ref g_node_no_zb);
            ZeroArray(ref g_node_no_dead_zb);
            ZeroArray(ref g_n_conc_crit);
            ZeroArray(ref g_n_conc_min);
            ZeroArray(ref g_n_green);
            ZeroArray(ref g_phase_tt);
            ZeroArray(ref g_tt_tot);
            ZeroArray(ref g_dm_senesced);
            ZeroArray(ref g_n_dead);
            ZeroArray(ref g_n_senesced);
            ZeroArray(ref g_root_length);
            ZeroArray(ref g_dlt_plant_wc);


            g_plant_status_out_today = false;
            g_canopy_height = 0.0;
            g_isdate = 0;
        //    mdate = 0;
        //    leaf_no_final = 0.0;
            g_lai_max = 0.0;
            g_n_conc_act_stover_tot = 0.0;
        //    N_conc_crit_stover_tot = 0.0;
            g_n_demand_tot = 0.0;
        //    N_uptake_stover_tot = 0.0;
        //    g_N_uptake_tot = 0.0;
            g_plants = 0.0;
            //cnh      g%initial_plant_density = 0.0
            g_root_depth = 0.0;
            g_sowing_depth = 0.0;
            g_slai = 0.0;
            g_lai = 0.0;
            g_transpiration_tot = 0.0;
            g_previous_stage = 0.0;
            g_ratoon_no = 0;
            g_node_no_detached_ob = 0.0;
            g_lodge_flag = false;
            g_min_sstem_sucrose = 0.0;
            g_lodge_redn_sucrose = 0.0;
            g_lodge_redn_green_leaf = 0.0;

            }







        private void sugar_zero_daily_variables ()
            {

        //    //*     ===========================================================
        //    //      subroutine sugar_zero_daily_variables ()
        //    //*     ===========================================================

        //    //*+  Purpose
        //    //*       Zero crop daily variables & arrays



        //            //! zero pools etc.

            Console.WriteLine("ZERO DAILY VARIABLES");

              ZeroArray(ref g_dlt_dm_green);
              ZeroArray(ref g_dlt_dm_green_retrans);
              ZeroArray(ref g_dlt_n_green);
              ZeroArray(ref g_dlt_n_retrans);
              ZeroArray(ref g_dlt_no3gsm);
              ZeroArray(ref g_dlt_nh4gsm);
              ZeroArray(ref g_dlt_sw_dep);
              //ZeroArray(ref dm_green_demand);
              ZeroArray(ref g_n_demand);

              ZeroArray(ref g_dlt_dm_dead_detached);
              ZeroArray(ref g_dlt_dm_detached);
              ZeroArray(ref g_dlt_dm_senesced);
              ZeroArray(ref g_dlt_n_dead_detached);
              ZeroArray(ref g_dlt_n_detached);
              ZeroArray(ref g_dlt_n_senesced);
              ZeroArray(ref g_sw_avail);
              ZeroArray(ref g_sw_avail_pot);
              ZeroArray(ref g_sw_supply);


              g_dlt_tlai_dead_detached = 0.0;
              g_dlt_slai_detached = 0.0;
              g_dlt_canopy_height = 0.0;
              g_dlt_dm = 0.0;
              g_partition_xs = 0.0;
              g_dlt_leaf_no = 0.0;
              g_dlt_node_no = 0.0;
              g_dlt_node_no_dead = 0.0;
              g_dlt_plants = 0.0;
              g_dlt_root_depth = 0.0;
              g_dlt_slai = 0.0;
              g_dlt_stage = 0.0;
              g_dlt_lai = 0.0;
              g_dlt_tt = 0.0;

              g_sw_demand = 0.0;
        //      dm_graze = 0.0;
        //      n_graze = 0.0;

              //dlt_min_sstem_sucrose = 0.0;   //sv- turned this into a local variable.

              g_temp_stress_photo = 0.0;
              g_temp_stress_stalk = 0.0;
              g_swdef_expansion = 0.0;
              g_swdef_stalk = 0.0;
              g_swdef_photo = 0.0;
              g_swdef_pheno = 0.0;
              g_swdef_fixation = 0.0;
              g_nfact_expansion = 0.0;
              g_nfact_stalk = 0.0;
              g_nfact_photo = 0.0;
              g_nfact_pheno = 0.0;
              g_oxdef_photo = 0.0;
              g_lodge_redn_photo = 0.0;


            }




        //private void sugar_zero_parameters()
        //    {


        //    //*     ===========================================================
        //    //      subroutine sugar_zero_parameters ()
        //    //*     ===========================================================

        //    //*+  Purpose
        //    //*       Zero parameter variables and arrays

        //    //sv- variables read in from the ini/sim file or calculated from what was read in. 
        //    //sv- Parameter as in [Param]


        //    //! zero pools etc.

        //    ZeroArray(ref g_ll_dep);
        //    ZeroArray(ref xf);
        //    ZeroArray(ref kl);

        //    uptake_source = "";
        //    //cnh      c%crop_type = ' '


        //    }




        private void sugar_zero_variables ()
            {

            //*     ===========================================================
            //      subroutine sugar_zero_variables ()
            //*     ===========================================================

            //*+  Purpose
            //*       Zero crop variables & arrays


                  //! zero pools etc.


              sugar_zero_globals();
              sugar_zero_daily_variables();
              //sugar_zero_parameters();


            }




        private void sugar_zero_soil_globals()
            {


            //*     ===========================================================
            //      subroutine sugar_zero_soil_globals ()
            //*     ===========================================================

            //*+  Purpose
            //*       Zero soil variables & arrays

            //sv- from [INPUTS]


            ZeroArray(ref dlayer);
            ZeroArray(ref sat_dep);
            ZeroArray(ref dul_dep);
            ZeroArray(ref ll15_dep);
            ZeroArray(ref sw_dep);
            //TODO: put st back in once you put the nitrogen [INPUT] tags back in.
            //ZeroArray(ref st);

            //sv- no longer needed since I made num_layers as property that just returns dlayer.Length
            //num_layers = 0;

            }





    #endregion


    #region Bound Checking



    #endregion


    #region Helper Functions


        #region ConvertModule.f90 (FortranInfrastructure)

            public const double smm2m = 1.0 / 1000.0;          //! conversion of mm to m

            public const double sm2smm = 1000000.0;       //! conversion of square metres to square mm

            public const double smm2sm = 1.0 / 1000000.0;      //! conversion factor of mm^2 to m^2

            public const double kg2gm = 1000.0;             //! conversion of kilograms to grams

            public const double ha2sm = 10000.0;            //! conversion of hectares to sq metres

            public const double fract2pcnt = 100.0;          //! convert fraction to percent

            public const double gm2kg = 1.0/1000.0;         //! constant to convert g to kg

            public const double sm2ha = 1.0 / 10000.0;        //! constant to convert m^2 to hectares

            public const double t2g = 1000.0 * 1000.0;        //! tonnes to grams

            public const double g2t = 1.0/ t2g;             //! grams to tonnes


        #endregion




        #region Science.f90 (FortranInfrastructure)

        private int FindLayerNo_ob(double Depth)
                {
                // Find the soil layer in which the indicated depth is located
                // If the depth is not reached, the last element is used
                double depth_cum = 0.0;
                for (int i = 0; i < dlayer.Length; i++)
                    {
                    depth_cum = depth_cum + dlayer[i];
                    if (depth_cum >= Depth)
                        return i + 1;    //convert to one based  
                    }
                return dlayer.Length;   //one based
                }


            public double root_proportion(int Layer_ob, double[] Dlayer, double RootDepth)
                {

                //integer    layer                 ! (INPUT) layer to look at
                //real       dlayr(*)              ! (INPUT) array of layer depths
                //real       root_depth            ! (INPUT) depth of roots

                //!+ Purpose
                //!       returns the proportion of layer that has roots in it (0-1).

                //!+  Definition
                //!     Each element of "dlayr" holds the height of  the
                //!     corresponding soil layer.  The height of the top layer is
                //!     held in "dlayr"(1), and the rest follow in sequence down
                //!     into the soil profile.  Given a root depth of "root_depth",
                //!     this function will return the proportion of "dlayr"("layer")
                //!     which has roots in it  (a value in the range 0..1).

                //!+  Mission Statement
                //!      proportion of layer %1 explored by roots

                double depth_to_layer_bottom;  //! depth to bottom of layer (mm)
                double depth_to_layer_top;     //! depth to top of layer (mm)
                double depth_to_root;          //! depth to root in layer (mm)
                double depth_of_root_in_layer; //! depth of root within layer (mm)




                depth_to_layer_bottom = SumArray(Dlayer, Layer_ob);
                depth_to_layer_top = depth_to_layer_bottom - Dlayer[zb(Layer_ob)];
                depth_to_root = u_bound(depth_to_layer_bottom, RootDepth);

                depth_of_root_in_layer = mu.dim(depth_to_root, depth_to_layer_top);
                return MathUtility.Divide(depth_of_root_in_layer, Dlayer[zb(Layer_ob)], 0.0);

                }


            public bool on_day_of (int stage_no, double current_stage)
                {
                //!     ===========================================================
                //   logical function on_day_of (stage_no, current_stage, phsdur)
                //!     ===========================================================

                //integer    stage_no              ! (INPUT) stage number to test ()
                //real       current_stage         ! (INPUT) last stage number ()
                //real       phsdur(*)             ! (INPUT) duration of phase (days)  //sv- 2013 March, removed this as a parameter. Not used anymore.

                //!+ Purpose
                //!       returns true if on day of stage occurence (at first day of phase)

                //!+  Mission Statement
                //!     on the first day of %1

                //!jh      on_day_of = phsdur(stage_no).le.1.0
                //!jh     :      .and. stage_no.eq.int(current_stage)

                return ((current_stage % 1.0) == 0.0) && (stage_no == (int)current_stage);

                }


            public bool stage_is_between(int start_ob, int finish_ob, double current_stage)
                {
                //!     ===========================================================
                //   logical function stage_is_between (start, finish, current_stage)
                //!     ===========================================================

                //!+ Sub-Program Arguments
                //   integer    finish                ! (INPUT) final stage+ 1
                //   real       current_stage         ! (INPUT) stage number to be tested
                //   integer    start                 ! (INPUT) initial level

                //!+ Purpose
                //!              returns true if last_stage lies at start or up to but not including finish.

                //!+  Definition
                //!     Returns .TRUE. if "current_stage" is greater than or equal
                //!     to "start" and less than "finish", otherwise returns .FALSE..

                //!+  Mission Statement
                //!     %1 is between %2 and %3


                bound_check_integer_var(start_ob, 1, finish_ob - 1, "start");

                return (((int)current_stage >= start_ob)  && ((int)current_stage < finish_ob));

                }


            public double linint_3hrly_temp (double i_tmax, double i_tmin, double[] i_temps, double[] i_y)
                {
                //!     ===========================================================
                //   real function linint_3hrly_temp (tmax, tmin, temps, y, num)
                //!     ===========================================================

                //!+ Sub-Program Arguments
                //   real       tmax                  ! (INPUT) maximum temperature (oC)
                //   real       tmin                  ! (INPUT) maximum temperature (oC)
                //   real       temps(*)              ! (INPUT) temperature array (oC)
                //   real       y(*)                  ! (INPUT) y axis array ()
                //   integer    num                   ! (INPUT) number of values in arrays ()


                //!+ Notes
                //!     Eight interpolations of the air temperature are
                //!     calculated using a three-hour correction factor.
                //!     For each air three-hour air temperature, a value
                //!     is calculated.  The eight three-hour estimates
                //!     are then averaged to obtain the daily value.

                //!+  Mission Statement
                //!      temperature factor (based on 3hourly estimates)


                int    l_num3hr  = 24/3;              //! number of 3 hourly temperatures
   
                //int         period;                //! three hourly period number
                double      l_tot;                   //! sum_of of 3 hr interpolations
                double      l_y_3hour;               //! 3 hr interpolated value
                double      l_tmean_3hour;           //! mean temperature for 3 hr period (oC)
                bool        l_didInterpolate;


                l_tot = 0.0;
                for (int period = 1; period <= l_num3hr; period++)
                    {
                        //! get a three-hour air temperature

                    l_tmean_3hour = temp_3hr(i_tmax, i_tmin, period);
                    l_y_3hour = MathUtility.LinearInterpReal(l_tmean_3hour, i_temps, i_y, out l_didInterpolate);

                    l_tot = l_tot + l_y_3hour;
                    }

            
                return l_tot/ (double)l_num3hr;

                }


            public double temp_3hr(double i_tmax, double i_tmin, int i_period)    
                {

               // !     ===========================================================
               //   real function temp_3hr (tmax, tmin, period)
               //!     ===========================================================

               //!+ Sub-Program Arguments
               //   real       tmax                  ! (INPUT) maximum temperature (oC)
               //   real       tmin                  ! (INPUT) minimum temperature (oC)
               //   integer    period                ! (INPUT) period number of 8 x 3 hour
               //                                    !   periods in the day

               //!+ Purpose
               //!     returns the temperature for a 3 hour period.

               //!+  Mission Statement
               //!      a 3 hourly estimate of air temperature

                double      l_period_no;             //! period number
                double      l_diurnal_range;         //! diurnal temperature range_of for the day (oC)
                double      l_t_deviation;           //! deviation from day's minimum for this 3 hr period
                double      l_t_range_fract;         //! fraction_of of day's range_of for this 3 hr period


                if (i_period < 1)
                    {
                    throw new Exception(" 3 hr. number" + i_period + " is below 1");
                    }
                else if (i_period > 8)
                    {
                    throw new Exception(" 3 hr. number" + i_period + " is above 8");
                    }
                else
                    {
                    l_period_no = (double)i_period;
                    l_t_range_fract = 0.92105 + 0.1140  * l_period_no - 0.0703  * Math.Pow(l_period_no,2) + 0.0053  * Math.Pow(l_period_no,3);

                    l_diurnal_range = i_tmax - i_tmin;
                    l_t_deviation = l_t_range_fract * l_diurnal_range;
                    return i_tmin + l_t_deviation;
                    }

                }


            //public void accumulate (double i_value, ref double[] i_array_zb, double i_index_ob, double i_dlt_index)
            //    {

            //    //!     ===========================================================
            //    //   subroutine accumulate ()
            //    //!     ===========================================================


            //    //!+ Sub-Program Arguments
            //    //   real       value                 ! (INPUT) value to add to array
            //    //   real       array(*)              ! (INPUT/OUTPUT) array to split
            //    //   real       p_index               ! (INPUT) current p_index no
            //    //   real       dlt_index             ! (INPUT) increment in p_index no

            //    //!+ Purpose
            //    //!     Accumulates a value in an array, at the specified index.
            //    //!     If the increment in index value changes to a new index, the value
            //    //!     is distributed proportionately between the two indices of the array.

            //    //!+  Mission Statement
            //    //!      Accumulate %1 (in array %2)

            //    //!+ Changes
            //    //!       250996 jngh changed so it always adds instead of reset at changeover
            //    //!                    to new phase
            //    //!                    corrected to take account of special case when p_index
            //    //!                    is integral no.


 
            //    int             current_index;         //! current index number ()
            //    double          fract_in_old;          //! fraction of value in last index
            //    double          index_devel;           //! fraction_of of current index elapsed ()
            //    int             new_index;             //! number of index just starting ()
            //    double          portion_in_new;        //! portion of value in next index
            //    double          portion_in_old;        //! portion of value in last index

            //    //sv- used (int) cast as a replacement for fortran aint()

            //    current_index = (int)i_index_ob;

            //    //! make sure the index is something we can work with
            //    if(current_index > 0)
            //        {
            //        index_devel = i_index_ob + i_dlt_index - (int)i_index_ob;

            //        if (index_devel >= 1.0)
            //            {
            //                //! now we need to divvy

            //            new_index = (int)(i_index_ob + Math.Min(1.0, i_dlt_index));

            //            if ((i_index_ob % 1.0) == 0.0) 
            //                {
            //                fract_in_old = 1.0 - ((index_devel - 1.0) / i_dlt_index);
            //                portion_in_old = fract_in_old * (i_value + i_array_zb[zb(current_index)] - i_array_zb[zb(current_index)]);
            //                }
            //            else
            //                {
            //                fract_in_old = 1.0 - ((index_devel - 1.0) / i_dlt_index);
            //                portion_in_old = fract_in_old * i_value;
            //                }

            //            portion_in_new = i_value - portion_in_old;

            //            i_array_zb[zb(current_index)] = i_array_zb[zb(current_index)] + portion_in_old;
            //            i_array_zb[zb(new_index)] = i_array_zb[zb(new_index)] + portion_in_new;
            //            }
            //        else
            //            {
            //            i_array_zb[zb(current_index)] = i_array_zb[zb(current_index)] + i_value;
            //            }

            //        }


            //    }


            public void accumulate_ob(double i_value, ref double[] i_array_zb, double i_index_ob, double i_dlt_index)
                {
                double i_index_zb = i_index_ob - 1.0;
                accumulate_zb(i_value, ref i_array_zb, i_index_zb, i_dlt_index);
                }



            public void accumulate_zb(double i_value, ref double[] io_array_zb, double i_index_zb, double i_dlt_index)
                {

                //!     ===========================================================
                //   subroutine accumulate ()
                //!     ===========================================================


                //!+ Sub-Program Arguments
                //   real       value                 ! (INPUT) value to add to array
                //   real       array(*)              ! (INPUT/OUTPUT) array to split
                //   real       p_index               ! (INPUT) current p_index no
                //   real       dlt_index             ! (INPUT) increment in p_index no

                //!+ Purpose
                //!     Accumulates a value in an array, at the specified index.
                //!     If the increment in index value changes to a new index, the value
                //!     is distributed proportionately between the two indices of the array.

                //!+  Mission Statement
                //!      Accumulate %1 (in array %2)

                //!+ Changes
                //!       250996 jngh changed so it always adds instead of reset at changeover
                //!                    to new phase
                //!                    corrected to take account of special case when p_index
                //!                    is integral no.



                int     l_current_index;         //! current index number ()
                double  l_fract_in_old;          //! fraction of value in last index
                double  l_index_devel;           //! fraction_of of current index elapsed ()
                int     l_new_index;             //! number of index just starting ()
                double  l_portion_in_new;        //! portion of value in next index
                double  l_portion_in_old;        //! portion of value in last index


                l_current_index = (int)i_index_zb;

                //! make sure the index is something we can work with (zero based not one based)
                if (l_current_index >= 0)
                    {
                    //http://www.dotnetperls.com/math-truncate
                    l_index_devel = i_index_zb - Math.Truncate(i_index_zb) + i_dlt_index;  //sv- add the delta to the starting index then subtract the integer part of the index.

                    //if the (index's decimal remainder + delta) is large enough to make the value go into a new index then when need to split the value proportionally.
                    if (l_index_devel >= 1.0)   
                        {
                        //! now we need to divvy 

                        l_new_index = (int)(i_index_zb + Math.Min(1.0, i_dlt_index));

                        //if the starting index was an integer then
                        if ((i_index_zb % 1.0) == 0.0)   
                            {
                            l_fract_in_old = 1.0 - ((l_index_devel - 1.0) / i_dlt_index);
                            l_portion_in_old = l_fract_in_old * (i_value + io_array_zb[l_current_index]) - io_array_zb[l_current_index];
                            }
                        else
                            {
                            l_fract_in_old = 1.0 - ((l_index_devel - 1.0) / i_dlt_index);
                            l_portion_in_old = l_fract_in_old * i_value;
                            }

                        l_portion_in_new = i_value - l_portion_in_old;

                        io_array_zb[l_current_index] = io_array_zb[l_current_index] + l_portion_in_old;
                        io_array_zb[l_new_index] = io_array_zb[l_new_index] + l_portion_in_new;
                        }
                    //else just put all of the value into the current index.
                    else
                        {
                        io_array_zb[l_current_index] = io_array_zb[l_current_index] + i_value;
                        }

                    }


                }





        #endregion


        #region Data.f90 (FortranInfrastructure)



            public double error_margin(double Variable)
                {
                /*
                double margin_val;
                //error margin = size of the variable mutiplied by error in the number 0. 
                margin_val = Math.Abs(Variable) * double.Epsilon; 
                //if Variable was zero hence error margin is now zero
                if (margin_val == 0.0)
                {
                //set the error margin to the error in the number 0 by Operating System
                margin_val = Double.Epsilon;
                }
                return margin_val;
                */
                return 0.0001;
                }


            public int get_cumulative_index_real(double cum_sum, double[] A)
                {
                //!     ===========================================================
                //   integer function get_cumulative_index_real()
                //!     ===========================================================


                //!+ Sub-Program Arguments
                //   real       array (*)             ! (INPUT) array to be searched
                //   integer    size_of               ! (INPUT) size_of of array
                //   real       cum_sum               ! (INPUT) sum_of to be found

                //!+ Purpose
                //!     Find the first element of an array where a given value
                //!     is contained with the cumulative sum_of of the elements.
                //!     If sum_of is not reached by the end of the array, then it
                //!     is ok to set it to the last element. This will take
                //!     account of the case of the number of levels being 0.

                //!+  Definition
                //!     Returns ndx where ndx is the smallest value in the range
                //!     1.."size_of" such that the sum of "array"(j), j=1..ndx is
                //!     greater than or equal to "cum_sum".  If there is no such
                //!     value of ndx, then "size_of" will be returned.

                //!+  Mission Statement
                //!      Find index for cumulative %2 = %1


                double      cum;                   //! cumulative sum

                cum = 0.0;

                    //! sum_of each element until sum_of is reached or exceeded

                for (int i = 0; i < A.Length; i++ )
                    {
                    cum = cum + A[i];
                    if (cum >= cum_sum)
                        {
                        return i + 1;  //convert to 1 based.
                        }
                    }

                return A.Length;   //convert to 1 based.


                }


            private int count_of_real_vals(double[] A, int StopLayer_ob)
                {

                //counts the layers until the first layer with a value of 0.0. Used only on dlayer[] to find num_layers.
                int count = 0;

                //sv- make sure that default value of max_layer is not larger than the actual array length.
                if (A.Length < StopLayer_ob)
                    {
                    StopLayer_ob = A.Length;
                    }

                for (int i = 0; i < StopLayer_ob; i++)
                    {
                    if (A[i] != 0.000)
                        {
                        //Console.WriteLine(count);
                        count++;
                        }
                    else
                        {
                        break;
                        }
                    }
                return count; //one based 
                }



            private void AddArray(double[] AddThis_zb, ref double[] ToThis_zb, int NumElemToAdd_ob )
                {

                // subroutine Add_real_array (amount, store, dimen) 
                //!+ Purpose
                //!     add contents of each element of an array to each element of another
                //!     array.

                //!+  Definition
                //!     This subroutine adds each of the "dimen" elements of "amount" to its
                //!     corresponding element of "store".


                //sv- make sure that default value of max_layer is not larger than the actual array length.
                if (ToThis_zb.Length < NumElemToAdd_ob)
                    {
                    NumElemToAdd_ob = ToThis_zb.Length;
                    }


                for (int i=0; i<NumElemToAdd_ob; i++)
                    {
                    ToThis_zb[i] = ToThis_zb[i] + AddThis_zb[i];
                    }

                }

            private void SubtractArray(double[] SubThis_zb, ref double[] FromThis_zb, int NumElemToSub_ob )
                {
                // subroutine subtract_real_array (amount, store, dimen)
                //!+ Purpose
                //!     remove contents of each element of an array from each element of
                //!     another array.

                //!+  Definition
                //!     This subroutine subtracts each of the "dimen" elements of
                //!     "amount" from its corresponding element of "store".


                //sv- make sure that default value of max_layer is not larger than the actual array length.
                if (FromThis_zb.Length < NumElemToSub_ob)
                    {
                    NumElemToSub_ob = FromThis_zb.Length;
                    }


                for (int i=0; i<NumElemToSub_ob; i++)
                    {
                    FromThis_zb[i] = FromThis_zb[i] - SubThis_zb[i];
                    }

                }


            //TODO: Replace this will MathUtility.Sum()
            private double SumArray(double[] A, int StopLayer_ob)
                {
                double sum = 0;
                for (int i = 0; i < StopLayer_ob; i++)
                    {
                    sum = sum + A[i];
                    }
                return sum;
                }
   

            private double sum_between(int start_ob, int finish_ob, double[] array_zb)
                {
                //!     ===========================================================
                //   real function sum_between (start, finish, array)
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //   integer    finish                ! (INPUT) final level+ 1 to be summed to
                //   integer    start                 ! (INPUT) initial level to begin sum_of
                //   real       array (*)             ! (INPUT) array to be summed

                //!+  Purpose
                //!     returns sum_of of part of real array from start to finish-1

                //!+  Definition
                //!     Returns sum of array(j), j="start" .. ("finish"-1).

                //!+  Mission Statement
                //!     |GREEK{S}|SUPERSCRIPT{1}|SUPERSCRIPT{%1}|SUBSCRIPT{%2} %3


                double      tot;                   //! sum_of of array


                bound_check_integer_var (zb(start_ob), 0, zb(finish_ob-1), "start");

                tot = 0.0;

                for (int level = zb(start_ob); level <= zb(finish_ob-1); level++)
                    {
                    tot = tot + array_zb[level];
                    }
                
                return tot;
                
                }



            private double sum_between_zb(int start_zb, int finish_zb, double[] array_zb)
                {

                double tot;                   //! sum_of of array


                bound_check_integer_var(start_zb, 0, (finish_zb-1), "start");

                tot = 0.0;

                for (int level = start_zb; level <= (finish_zb-1); level++)
                    {
                    tot = tot + array_zb[level];
                    }

                return tot;

                }




            public void fill_real_array(ref double[] A_zb, double Value, int StopLayer_ob)
                {
                if (A_zb.Length < StopLayer_ob)
                    {
                    StopLayer_ob = A_zb.Length;
                    }
                for (int i = 0; i < StopLayer_ob; i++)
                    {
                    A_zb[i] = Value;
                    }
                }


            public double l_bound(double A, double MinVal)
                {
                //force A to stay above the MinVal. Set A to MinVal if A is below it.
                return Math.Max(A, MinVal);
                }


            public double u_bound(double A, double MaxVal)
                {
                //force A to stay below the MaxVal. Set A to MaxVal if A is above it.
                return Math.Min(A, MaxVal);
                }


            public double bound(double A, double MinVal, double MaxVal)
                {
                //force A to stay between the MinVal and the MaxVal. Set A to the MaxVal or MinVal if it exceeds them.
                if (MinVal > MaxVal)
                    {
                    ApsimUtil.warning_error("Lower bound " + MinVal + " is > upper bound " + MaxVal + Environment.NewLine
                                       + "        Variable is not constrained");
                    return A;
                    }

                double temp;
                temp = u_bound(A, MaxVal);
                temp = l_bound(temp, MinVal);
                return temp;
                }


    
           //Put this into CSGeneral. The existing Max in MathUtility only works on IEnumerable Collections.
            ///<summary>
            ///Allows any number of parameters (unlike Math.Max()) 
            ///</summary>
            public double max(params double[] A)
                {
                double maximum = A[0];
                for (int i = 1; i < A.Length; i++)
                    {
                    maximum = Math.Max(maximum, A[i]);
                    }
                return maximum;
                }

            ///<summary>
            ///Allows any number of parameters (unlike Math.Min()) 
            ///</summary>
            public double min(params double[] A)
                {
                double minimum = A[0];
                for (int i = 1; i < A.Length; i++)
                    {
                    minimum = Math.Min(minimum, A[i]);
                    }
                return minimum;
                }


        #endregion


            static private void warning_error(string WarningMessage)
                {
                Console.WriteLine("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                Console.WriteLine("                 APSIM Warning Error              ");
                Console.WriteLine("                 -------------------              ");
                Console.WriteLine(WarningMessage);
                Console.WriteLine("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                }



            private void bound_check_real_array(double[] array, double lower_bound, double upper_bound, string array_name, int array_size)
                {
                //! ================================================================
                //   subroutine bound_check_real_array ()
                //! ================================================================

                //!+ Sub-Program Arguments
                //   real       array(*)              ! (INPUT) array to be checked
                //   character  array_name*(*)        ! (INPUT) key string of array
                //   integer    array_size            ! (INPUT) array size_of
                //   real       lower_bound           ! (INPUT) lower bound of values
                //   real       upper_bound           ! (INPUT) upper bound of values

                //!+ Purpose
                //!     check bounds of values in an array

                //!+  Definition
                //!     This subroutine will issue a warning message using the
                //!     name of "array", "name", for each element of "array" that is
                //!     greater than  ("upper" + 2 * error_margin("upper")) or less
                //!     than ("lower" - 2 *error_margin("lower")).  If
                //!     ("lower" - 2 *error_margin("lower")) is greater than "upper",
                //!     then a warning message will be flagged to that effect "size"
                //!     times.

                //!+ Assumptions
                //!      each element has same bounds.

                //!+  Mission Statement
                //!      Check that all %1 lies between %2 and %3


                if (array_size >= 1)
                    {

                    for (int indx = 0; indx < array_size; indx++)
                        {
                        bound_check_real_var(array[indx], lower_bound, upper_bound, array_name);
                        }

                    }


                }



            public void bound_check_integer_var(int value, int lower, int upper, string vname)
                {
                //! ===========================================================
                //   subroutine bound_check_integer_var (value, lower, upper, vname)
                //! ===========================================================

                //!+ Sub-Program Arguments
                //   integer       lower                 ! (INPUT) lower limit of value
                //   integer       upper                 ! (INPUT) upper limit of value
                //   integer       value                 ! (INPUT) value to be validated
                //   character  vname *(*)            ! (INPUT) variable name to be validated

                //!+ Purpose
                //!     checks if a variable lies outside lower and upper bounds.
                //!     Reports an err if it does.

                //!+  Definition
                //!     This subroutine will issue a warning message using the
                //!     name of "value", "vname", if "value" is greater than "upper" or
                //!     less than "lower".  If  "lower" is greater than "upper", then a
                //!     warning message will be flagged to that effect.

                //!+ Notes
                //!     reports err if value > upper or value < lower or lower > upper

                //!+  Mission Statement
                //!      Check that %1 lies between %2 and %3

                double real_lower;          //! real version of lower limit
                double real_upper;          //! real version of upper limit
                double real_val;            //! real version of value


                real_lower = (double)lower;
                real_upper = (double)upper;
                real_val = (double)value;

                bound_check_real_var(real_val, real_lower, real_upper, vname);

                }



            // Unlike u_bound and l_bound, this does not force the variable to be between the bounds. It just warns the user in the summary file.
            protected void bound_check_real_var(double Variable, double LowerBound, double UpperBound, string VariableName)
                {
                string warningMsg = "" ;
                
                if (Variable > UpperBound)
                    {
                    warningMsg = "The variable: \'" + VariableName + "\' is above the expected upper bound of: " + UpperBound;
                    warning_error(warningMsg);
                    }
                if (Variable < LowerBound)
                    {
                    warningMsg = "The variable: \'" + VariableName + "\' is below the expected lower bound of: " + LowerBound;
                    warning_error(warningMsg);
                    }
                
                }



            /// <summary>
            /// Returns Zero Based Index from One Based Index
            /// </summary>
            /// <param name="OneBased"></param>
            /// <returns>Zero Based Index</returns>
            private int zb(int OneBased)
                {
                return OneBased - 1;
                }

            /// <summary>
            /// Returns One Based Index from Zero Based Index
            /// </summary>
            /// <param name="OneBased"></param>
            /// <returns>One Based Index</returns>
            private int ob(int ZeroBased)
                {
                return ZeroBased + 1;
                }

            private double zb_d(double OneBased)
                {
                return OneBased - 1.0;
                }




            private void PrintArray(string Name, double[] A)
                {
                Console.Write(Name + " : ");
                for (int i = 0; i < A.Length; i++)
                    {
                    Console.Write(" , " + A[i]);  
                    }
                Console.WriteLine();
                }


    #endregion



    #region Crop Science Functions (Prepare Event)


        #region Nitrogen Stress Factor Calculations


            void sugar_nfact (double[]  i_dm_green, double[] i_N_conc_crit, double[] i_N_conc_min, double[] i_N_green, double c_k_nfact, ref double o_nfact)
                {

                //*+  Sub-Program Arguments
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
                //      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
                //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
                //      REAL       k_nfact               ! (INPUT)  k value for stress factor
                //      real      nfact                  ! (OUTPUT) N stress factor

                //*+  Purpose
                //*     The concentration of Nitrogen in leaves is used to derive a
                //*     series of Nitrogen stress indices.  The stress indices for
                //*     photosynthesis and cell expansion are calculated from today's
                //*     relative nutritional status between a critical and minimum
                //*     leaf Nitrogen concentration.

                //*+  Mission Statement
                //*     Concentration of nitrogen in the leaves



                double       l_N_conc_leaf;           //! leaf actual N concentration (0-1)
                double       l_N_def;                 //! N factor (0-1)
                double       l_N_conc_leaf_crit;      //! tops (stover) critical N concentration (0-1)
                double       l_N_conc_leaf_min;       //! tops (stover) minimum N concentration (0-1)
                double       l_N_leaf_crit;           //! critical leaf nitrogen (g/m^2)
                double       l_N_leaf_min;            //! minimum leaf nitrogen (g/m^2)
                double       l_N_conc_ratio;          //! available N as fraction of N capacity (0-1)



                    //! calculate actual N concentrations

                l_N_conc_leaf = MathUtility.Divide(i_N_green[leaf], i_dm_green[leaf], 0.0);



                    //! calculate critical N concentrations
                    //       Base N deficiency on leaf N concentrations

                l_N_leaf_crit = i_N_conc_crit[leaf] * i_dm_green[leaf];
                l_N_conc_leaf_crit = MathUtility.Divide(l_N_leaf_crit, i_dm_green[leaf], 0.0);



                    //! calculate minimum N concentrations

                l_N_leaf_min = i_N_conc_min[leaf] * i_dm_green[leaf];
                l_N_conc_leaf_min = MathUtility.Divide(l_N_leaf_min, i_dm_green[leaf], 0.0);



                    //! calculate shortfall in N concentrations

                l_N_conc_ratio = MathUtility.Divide((l_N_conc_leaf - l_N_conc_leaf_min), (l_N_conc_leaf_crit - l_N_conc_leaf_min), 0.0);



                    //! calculate 0-1 N deficiency factors

                l_N_def = c_k_nfact * l_N_conc_ratio;
                o_nfact = bound(l_N_def, 0.0, 1.0);

                }


        #endregion



        #region Temperature Stress Factor Calculations
    

            void sugar_temperature_stress(int c_num_ave_temp , double[] c_x_ave_temp , double[] c_y_stress_photo , double i_maxt , double i_mint , ref double o_tfac)

                {
                //*+  Sub-Program Arguments
                //      INTEGER    C_num_ave_temp        ! (INPUT)  size_of of critical temperatur
                //      REAL       C_x_ave_temp(*)       ! (INPUT)  critical temperatures for phot
                //      REAL       C_y_stress_photo(*)   ! (INPUT)  Factors for critical temperatu
                //      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
                //      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
                //       real tfac

                //*+  Purpose
                //*      Temperature stress factor for photosynthesis.  //sv- also for stem (not just photosynthesis)

                //*+  Mission Statement
                //*     Get the temperature stress factor


                double     l_ave_temp;              //! mean temperature for the day (oC)
                bool       l_DidInterpolate;


                    //! now get the temperature stress factor that reduces photosynthesis (0-1)  //sv- also  for stem (not just photosynthesis)

                l_ave_temp = (i_maxt + i_mint) /2.0;

                o_tfac = MathUtility.LinearInterpReal(l_ave_temp, c_x_ave_temp, c_y_stress_photo, out l_DidInterpolate);
                o_tfac = bound(o_tfac, 0.0, 1.0);

            }


        #endregion



        #region Radiation Interception by Leaves


            double sugar_radn_int(double c_extinction_coef, double i_fr_intc_radn, double i_lai, double i_radn)
                {
                //sv- Also replaces the following function.
                //*     ===========================================================
                //      subroutine sugar_light_supply (Option)
                //*     ===========================================================
                //*+  Purpose
                //*       light supply

                //*+  Mission Statement
                //*     Seek the light intercepted by the leaves



                //*     ===========================================================
                //      subroutine sugar_radn_int
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_extinction_coef     ! (INPUT)  radiation extinction coefficie
                //      REAL       G_fr_intc_radn        ! (INPUT)  fraction of radiation intercep
                //      REAL       G_lai                 ! (INPUT)  live plant green lai
                //      REAL       G_radn                ! (INPUT)  solar radiation (Mj/m^2/day)
                //      real       radn_int              ! (OUTPUT) radiation intercepted by leaves (mj/m^2)

                //*+  Purpose
                //*       This routine returns the radiation intercepted by leaves (mj/m^2)

                //*+  Mission Statement
                //*     Light interception by the leaves

                //*+  Local Variables
                double l_cover;                //! fraction of radn that is intercepted by leaves (0-1) (m^2/m^2)

                //if you are not using the Canopy module in this simulation.

                if (MathUtility.FloatsAreEqual(i_fr_intc_radn, 0.0))
                    {
                    //! we need to calculate our own interception

                    //! this equation implies that leaf interception of solar radiation obeys Beer's law

                    l_cover = 1.0 - Math.Exp(-1 * c_extinction_coef * i_lai);
                    return l_cover * i_radn;
                    }
                else
                    {
                    //! interception has already been calculated for us
                    return i_fr_intc_radn * i_radn;
                    }


                }



        #endregion



        #region Transpiration Efficiency  (based on today's weather)


            double cproc_transp_eff1(double c_svp_fract, double[] c_transp_eff_cf, double i_current_stage, double i_maxt, double i_mint)
                {
                //sv- This function was taken from the "CropTemplate" module which as far as I can tell seems to be a precursor to the "Plant" module. 
                //    It looks like it is a library of plant/crop functions that different crop modules have in common and so use. 
                //    Specifically this function comes from the crp_wtr.f90 file (Line 864). 

                //sv- Also replaces the following function.
                //*     ===========================================================
                //      subroutine sugar_transpiration_eff (Option)
                //*     ===========================================================
                //*+  Purpose
                //*       Calculate today's transpiration efficiency from min and max
                //*       temperatures and converting mm water to g dry matter
                //*       (g dm/m^2/mm water)
                //*+  Mission Statement
                //*     Calculate transpiration efficiency


                //!     ===========================================================
                //      subroutine cproc_transp_eff1(svp_fract, transp_eff_cf,          &
                //                 current_stage,maxt, mint, transp_eff)
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL       svp_fract     ! (INPUT)  fraction of distance between svp at mi
                //      REAL       transp_eff_cf(*) ! (INPUT)  transpiration efficiency coefficien
                //      REAL       current_stage ! (INPUT)
                //      REAL       maxt          ! (INPUT)  maximum air temperature (oC)
                //      REAL       mint          ! (INPUT)  minimum air temperature (oC)
                //      REAL       transp_eff    ! (OUTPUT)

                //!+  Purpose
                //!       Calculate today's transpiration efficiency from the transpiration
                //!       efficiency coefficient and vapour pressure deficit, which is calculated
                //!       from min and max temperatures.

                //!+  Mission Statement
                //!   Calculate today's transpiration efficiency from VPD

                //!+  Assumptions
                //!       the temperatures are > -237.3 oC for the svp function.

                //!+  Notes
                //!       Average saturation vapour pressure for ambient temperature
                //!       during transpiration is calculated as part-way between that
                //!       for minimum temperature and that for the maximum temperature.
                //!       Tanner & Sinclair (1983) used .75 and .67 of the distance as
                //!       representative of the positive net radiation (rn).  Daily SVP
                //!       should be integrated from about 0900 hours to evening when Radn
                //!       becomes negative.

                //!+  Local Variables       
                double      l_vpd;           //! vapour pressure deficit (kpa)
                int         l_current_phase;



                l_current_phase = (int)g_current_stage;


                        //! get vapour pressure deficit (kPa)
                l_vpd = vapour_pressure_deficit(c_svp_fract, i_maxt, i_mint);



                        //sv- taken from FortranInfrastructure module, ConvertModule.f90
                const double g2mm = 1.0e3 / 1.0e6;          //! convert g water per m^2 to mm water   
                                                            //! 1 g water = 1 cm^3 water = 1,000 cubic mm, and
                                                            //! 1 sq m = 1,000,000 sq mm


                // "transp_eff" units are (g/m^2/mm) or (g carbo per m^2 / mm water)
                //  because all other dm weights are in (g/m^2)

                //! "transp_eff_cf" ("cf" stands for coefficient) is used to convert vpd to transpiration efficiency. 
                //! Although the units are sometimes soley expressed as a pressure (kPa) it is really in the form:  
                //      kPa * kg carbo per m^2 / kg water per m^2   and this can be converted to 
                //      = kPa * g carbo per m^2 / g water per m^2     
                //      = kPa * g carbo per m^2 * g2mm / g water per m^2 * g2mm
                //      = (kPa * g carbo per m^2 / mm water) * g2mm 
                //      = kPa * transp_eff * g2mm  

                //hence, 
                //     transp_eff = transp_eff_cf / kPa /g2mm
                //                = transp_eff_cf / vpd /g2mm


                return MathUtility.Divide(c_transp_eff_cf[zb(l_current_phase)], l_vpd, 0.0) /g2mm;

                }



            double vapour_pressure_deficit(double c_svp_fract, double i_maxt, double i_mint)
                {

                //!+  Notes
                //!       Average saturation vapour pressure for ambient temperature
                //!       during transpiration is calculated as part-way between that
                //!       for minimum temperature and that for the maximum temperature.
                //!       Tanner & Sinclair (1983) used .75 and .67 of the distance as
                //!       representative of the positive net radiation (rn).  Daily SVP
                //!       should be integrated from about 0900 hours to evening when Radn
                //!       becomes negative.

                double l_vpd;

                    //! get vapour pressure deficit when net radiation is positive.
                l_vpd = c_svp_fract * (svp(maxt) - svp(mint));

                l_vpd = l_bound(l_vpd, 0.01);

                return l_vpd;
                }



            double svp(double temp_arg)
                {

                //! function to get saturation vapour pressure for a given temperature in oC (kpa)
                // sv- This function was extracted from inside the  cproc_transp_eff1() function below (see svp local variable)

                //!+  Assumptions
                //!       the temperatures are > -237.3 oC for the svp function.

                //! convert pressure mbar to kpa  //sv- taken from FortranInfrastructure module, ConvertModule.f90
                double mb2kpa = 100.0 / 1000.0;    //! 1000 mbar = 100 kpa   
                  
                return 6.1078 * Math.Exp(17.269 * temp_arg / (237.3 + temp_arg)) * mb2kpa;
                }


        #endregion



        #region SW Demand (Atomospheric Potential)

         
            double sugar_water_demand(double i_dlt_dm_pot_rue, double i_transp_eff, double i_lai, double i_eo)
                {

                double l_cover_green;

                //cproc_sw_demand1() 
                //! Return crop water demand from soil by the crop (mm) calculated by dividing (biomass production limited by radiation) by transpiration efficiency.       
                //! get potential transpiration from potential carbohydrate production and transpiration efficiency
                        //Start - cproc_sw_demand1() 
                        g_sw_demand_te = MathUtility.Divide(i_dlt_dm_pot_rue, i_transp_eff, 0.0);
                        //End - cproc_sw_demand1()  

                l_cover_green = 1.0 - Math.Exp(-crop.extinction_coef * i_lai);

                //!         Constrain sw demand according to atmospheric potential.
                return cproc_sw_demand_bound(g_sw_demand_te, eo_crop_factor, i_eo, l_cover_green);

                }




            double cproc_sw_demand_bound (double i_sw_demand_unbounded, double i_eo_crop_factor, double  i_eo, double i_cover_green)
                {
                //sv- This function was taken from the "CropTemplate" module which as far as I can tell seems to be a precursor to the "Plant" module. 
                //    It looks like it is a library of plant/crop functions that different crop modules have in common and so use. 
                //    Specifically this function comes from the crp_wtr.f90 file (Line 813). 

                //!     ===========================================================
                //      subroutine cproc_sw_demand_bound ()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL sw_demand_unbounded ! (INPUT)  Unbounded sw demand (mm)
                //      REAL eo_crop_factor      ! (INPUT) crop factor for eo   (-)
                //      REAL eo                  ! (INPUT) eo                  (mm)
                //      REAL cover_green         ! (INPUT) green crop cover    (-)
                //      real sw_demand_bounded   ! (OUTPUT) bounded sw demand (mm)

                //!+  Purpose
                //!       Calculated a bounded value of crop sw demand by constraining sw demand to some fraction/multiple of atmospheric potential (Eo).

                //!+  Mission Statement
                //!   Constrain sw demand according to atmospheric potential.


                //!+  Local Variables
                double l_sw_demand_max;   //! maximum sw demand as constrained by atmospheric potential (mm)

                l_sw_demand_max = i_eo_crop_factor * i_eo * i_cover_green;

                return u_bound(i_sw_demand_unbounded, l_sw_demand_max);


                }

        #endregion



        #region Nitrogen Demand

            void sugar_nit_demand_est(double[] i_dm_green, double i_dlt_dm_pot_rue_pot, double[] i_N_conc_crit, double[] i_N_green, ref double[] o_N_demand)
                {
                //* ====================================================================
                //       subroutine sugar_nit_demand_est (Option)
                //* ====================================================================


                //*+  Purpose
                //*      Calculate an approximate nitrogen demand for today's growth.
                //*      The estimate basically = n to fill the plant up to maximum
                //*      nitrogen concentration.

                //*+  Mission Statement
                //*     Calculate nitrogen demand for growth


                //*+  Constant Values
                //      integer num_demand_parts
                //      parameter (num_demand_parts = 4)

                //*+  Local Variables
                double[]    l_dlt_dm_green_pot = new double[max_part]; //! potential (est) dlt dm green
                double      l_dm_green_tot;            //! total dm green
                //double[]    l_dlt_N_retrans = new double[max_part];


                //! assume that the distribution of plant
                //! C will be similar after today and so N demand is that
                //! required to raise all plant parts to max N conc.

                //! calculate potential new shoot and root growth
                l_dm_green_tot = SumArray(i_dm_green, max_part);

                for (int part=0; part < max_part; part++)
                    {
                    l_dlt_dm_green_pot[part] = i_dlt_dm_pot_rue_pot * MathUtility.Divide(i_dm_green[part], l_dm_green_tot, 0.0);
                    //l_dlt_N_retrans[part] = 0.0;
                    }

                sugar_N_demand(l_dlt_dm_green_pot, i_dlt_dm_pot_rue_pot, i_dm_green, i_N_conc_crit, i_N_green, ref o_N_demand);

                }



            void sugar_N_demand(double[] i_dlt_dm_green_pot, double i_dlt_dm_pot_rue_pot, double[] i_dm_green, double[] i_N_conc_crit, double[] i_N_green, ref double[] o_N_demand)
                {
                //*     ===========================================================
                //      subroutine sugar_N_demand
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       G_dlt_dm_green_pot(*) ! (INPUT)  plant biomass growth (g/m^2)
                //      REAL       G_dlt_dm_pot_rue_pot  ! (INPUT)
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
                //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
                //      real       N_demand (*)          ! (OUTPUT) plant nitrogen demand
                //                                       ! (g/m^2)

                //*+  Purpose
                //*       Return plant nitrogen demand for each plant component.  The
                //*       demand for Nitrogen for each plant pool occurs as the plant
                //*       tries to maintain a critical nitrogen concentration in each
                //*       plant pool.

                //*+  Mission Statement
                //*     Get the nitrogen demand for each plant part

                //*+  Notes
                //*           N demand consists of two components:
                //*           Firstly, the demand for nitrogen by the potential new growth.
                //*           Secondly, the demand due to the difference between
                //*           the actual N concentration and the critical N concentration
                //*           of the tops (stover), which can be positive or negative


                //*+  Local Variables

                double      l_N_crit;                //! critical N amount (g/m^2)
                double      l_N_demand_new;          //! demand for N by new growth (g/m^2)
                double      l_N_demand_old;          //! demand for N by old biomass (g/m^2)



                //! g_dlt_dm_pot is above ground biomass only so leave roots out of comparison
                bound_check_real_var((SumArray(i_dlt_dm_green_pot, max_part) - i_dlt_dm_green_pot[root]), 0.0, i_dlt_dm_pot_rue_pot, "dlt_dm_pot - dlt_dm_pot(root)");


                //! NIH - note stem stuff is redone down later.

                for (int part=0; part < max_part; part++)
                    {
                    if (i_dm_green[part] > 0.0) 
                        {

                        //! get N demands due to difference between actual N concentrations
                        //! and critical N concentrations of tops (stover) and roots.

                        l_N_crit = i_dm_green[part] * i_N_conc_crit[part];
                        l_N_demand_old = l_N_crit - i_N_green[part];


                            //! get potential N demand (critical N) of potential growth

                        l_N_demand_new = i_dlt_dm_green_pot[part] * i_N_conc_crit[part];

                        o_N_demand[part] = l_N_demand_old + l_N_demand_new;
                        o_N_demand[part] = l_bound(o_N_demand[part], 0.0);
                        }
                    else
                        {
                        o_N_demand[part] = 0.0;
                        }

                    }

                //cnh I am not 100% happy with this but as this is a first attempt at fully
                //cnh utilizing a sucrose pool I shall put in this quick fix for now and
                //cnh re-evaluate later.  Note that g_N_conc_crit(Sstem) is really the crit.
                //cnh conc for CANE.

                //! SStem demand for N is based on N conc in cane (i.e SStem+sucrose)

                l_N_crit = (i_dm_green[sstem] + i_dm_green[sucrose]) * i_N_conc_crit[sstem];
                l_N_demand_old = l_N_crit - i_N_green[sstem];
                l_N_demand_new = (i_dlt_dm_green_pot[sstem] + i_dlt_dm_green_pot[sucrose]) * i_N_conc_crit[sstem];
                o_N_demand[sstem] = l_N_demand_old + l_N_demand_new;
                o_N_demand[sstem] = l_bound(o_N_demand[sstem], 0.0);

                }

 

        #endregion



    #endregion



    #region Crop Science Functions (Process Event)




        #region Root Growth


            void sugar_root_depth_growth (double[] i_dlayer, 
                                            double[] c_x_sw_ratio, double[] c_y_sw_fac_root, 
                                            double[] c_x_afps, double[] c_y_afps_fac, 
                                            double[] i_dul_dep, double[] i_sw_dep, double[] i_ll_dep, 
                                            double[] c_root_depth_rate, double i_current_stage, double[] i_xf, 
                                            out double o_dlt_root_depth, double i_root_depth)
                {
                //*     ===========================================================
                //      subroutine sugar_root_depth (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Plant root depth calculations

                //*+  Mission Statement
                //*     Calculates the plant root depth


                //!     ===========================================================
                //        subroutine sugar_root_depth_growth ()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      real    g_dlayer(*)             ! (INPUT)  layer thicknesses (mm)
                //      integer C_num_sw_ratio          ! (INPUT) number of sw lookup pairs
                //      real    C_x_sw_ratio(*)         ! (INPUT) sw factor lookup x
                //      real    C_y_sw_fac_root(*)      ! (INPUT) sw factor lookup y
                //      real    c_x_afps(*)
                //      real    c_y_afps_fac(*)
                //      integer c_num_afps
                //      real    G_dul_dep(*)            ! (INPUT) DUL (mm)
                //      real    G_sw_dep(*)             ! (INPUT) SW (mm)
                //      real    P_ll_dep(*)             ! (INPUT) LL (mm)
                //      real    C_root_depth_rate(*)    ! (INPUT) root front velocity (mm)
                //      real    G_current_stage         ! (INPUT) current growth stage
                //      real    p_xf(*)                 ! (INPUT) exploration factor
                //      real    g_dlt_root_depth        ! (OUTPUT) increase in rooting depth (mm)
                //      real    g_root_Depth            ! (OUTPUT) root depth (mm)

                //!+  Purpose
                //!       Calculate plant rooting depth through time limited by soil water content
                //!       in layer through which roots are penetrating.

                //!+  Mission Statement
                //!   Calculate today's rooting depth


                int     l_deepest_layer_ob;         //! deepest layer in which the roots are growing
                double  l_sw_avail_fac_deepest_layer_ob;  
                double  l_afps_fac_deepest_layer_ob;
                double  l_sw_fac_deepest_layer_ob;



                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

                l_sw_avail_fac_deepest_layer_ob = crop_sw_avail_fac(c_x_sw_ratio, c_y_sw_fac_root, i_dul_dep, i_sw_dep, i_ll_dep, l_deepest_layer_ob);

                l_afps_fac_deepest_layer_ob = sugar_afps_fac(l_deepest_layer_ob);

                l_sw_fac_deepest_layer_ob = u_bound(l_afps_fac_deepest_layer_ob, l_sw_avail_fac_deepest_layer_ob);

                o_dlt_root_depth = crop_root_depth_increase(c_root_depth_rate, i_current_stage, i_dlayer, i_root_depth, l_sw_fac_deepest_layer_ob, i_xf);

                }


            double crop_sw_avail_fac(double[] c_x_sw_ratio, double[] c_y_sw_fac_root, double[] i_dul_dep, double[] i_sw_dep, double[] i_ll_dep, int i_layer_ob)
                {
                //!     ===========================================================
                //      real function crop_sw_avail_fac()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      INTEGER    num_sw_ratio        ! (INPUT)
                //      REAL       x_sw_ratio(*)       ! (INPUT)
                //      REAL       y_sw_fac_root(*)    ! (INPUT)
                //      REAL       dul_dep(*)          ! (INPUT) drained upper limit for layer L (mm water)
                //      REAL       sw_dep(*)           ! (INPUT) soil water content of layer L (mm)
                //      REAL       ll_dep(*)           ! (INPUT) lower limit of plant-extractable soil
                //                                     ! water for soil layer L (mm)
                //      INTEGER    layer               ! (INPUT) soil profile layer number

                //!+  Purpose
                //!      Get the soil water availability factor in a layer.  For a layer,
                //!      it is 1.0 unless the plant-extractable soil water declines
                //!      below a fraction of plant-extractable soil water capacity for
                //!      that layer.

                //!+  Mission Statement
                //!   Determine root hospitality factor for moisture (for layer %7)


                double       l_pesw;                //! plant extractable soil-water (mm/mm)
                double       l_pesw_capacity;       //! plant extractable soil-water capacity (mm/mm)
                double       l_sw_avail_ratio;      //! soil water availability ratio (0-1)
                bool         l_didInterpolate;

                //!- Implementation Section ----------------------------------

                l_pesw = i_sw_dep[zb(i_layer_ob)] - i_ll_dep[zb(i_layer_ob)];
                l_pesw_capacity = i_dul_dep[zb(i_layer_ob)] - i_ll_dep[zb(i_layer_ob)];
                l_sw_avail_ratio = MathUtility.Divide(l_pesw, l_pesw_capacity, 10.0);
                return MathUtility.LinearInterpReal(l_sw_avail_ratio, c_x_sw_ratio, c_y_sw_fac_root, out l_didInterpolate);
                }



            double sugar_afps_fac(int i_layer_ob)
                {
                //      ===========================================================
                //      real function Sugar_afps_fac(layer)
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      integer layer

                //*+  Purpose
                //*       Calculate factor for Air Filled Pore Space (AFPS)
                //*       on root function with a given layer.

                double    l_afps;
                bool      l_didInterpolate;

                l_afps = MathUtility.Divide((sat_dep[zb(i_layer_ob)] - sw_dep[zb(i_layer_ob)]), dlayer[zb(i_layer_ob)], 0.0);

                return MathUtility.LinearInterpReal(l_afps, crop.x_afps, crop.y_afps_fac, out l_didInterpolate);

            
                }


            double crop_root_depth_increase(double[] c_root_depth_rate, double i_current_stage, double[] i_dlayer, double i_root_depth, 
                                            double i_sw_avail_fac_deepest_layer_ob, double[] i_xf)
                {

                //!     ===========================================================
                //      subroutine crop_root_depth_increase()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL       C_root_depth_rate(*)  ! (INPUT)  root growth rate potential (mm
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL       G_sw_avail_fac_deepest_layer ! (INPUT)
                //      REAL       P_XF (*)              ! (INPUT) eXploration Factor (0-1)
                //      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

                //!+  Purpose
                //!       Return the increase in root depth (mm)

                //!+  Notes
                //!         there is a discrepency when the root crosses into another layer. - cr380



                int     l_current_phase;         //! current phase number
                double  l_root_depth_max;        //! maximum depth to which roots can go (mm)
                int     l_current_layer_ob;         //! layer of root front
                int     l_deepest_layer_ob;         //! deepest layer for rooting
                double  l_dlt_root_depth;



                l_current_layer_ob = FindLayerNo_ob(i_root_depth);
                l_current_phase =  (int)i_current_stage;  

                    //! this equation allows soil water in the deepest
                    //! layer in which roots are growing
                    //! to affect the daily increase in rooting depth.

                l_dlt_root_depth  = c_root_depth_rate[zb(l_current_phase)]  * i_sw_avail_fac_deepest_layer_ob  * i_xf[zb(l_current_layer_ob)];

                    //! constrain it by the maximum
                    //! depth that roots are allowed to grow.

                l_deepest_layer_ob = count_of_real_vals(i_xf, i_xf.Length);
                l_root_depth_max =  SumArray(i_dlayer, l_deepest_layer_ob);
                return u_bound(l_dlt_root_depth, l_root_depth_max - i_root_depth);

                }



            void sugar_init_root_depth(double[] i_dlayer, double[] i_root_length, double i_root_depth, ref double o_dlt_root_depth)
                {
                //*     ===========================================================
                //      subroutine sugar_root_depth_init (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Plant root depth calculations

    
                //*     ===========================================================
                //      subroutine sugar_init_root_depth()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_root_length(*)              ! (INPUT)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

                //*+  Purpose
                //*       This routine returns the increase in root depth.  The
                //*       approach used here utilises a potential root front velocity
                //*       affected by relative moisture content at the rooting front.

                //*+  Mission Statement
                //*     Gets the increase in plant root depth

                //*+  Notes
                //*         there is a discrepency when the root crosses into another layer. - cr380

                int    l_num_root_layers;       

            
                if (i_root_depth == 0.0)
                    {
                        //! initialise root depth
                        //! this version does not take account of sowing depth.
                        //cnh it used to do this on first day of sprouting
                        //cnh         dlt_root_depth = c_initial_root_depth

                        //cnh now I say roots are at bottom of deepest layer that user said had a value for rlv at initialisation.
                        l_num_root_layers = count_of_real_vals(i_root_length, i_root_length.Length);
                        o_dlt_root_depth = SumArray(i_dlayer, l_num_root_layers) - i_root_depth;
                    }
                else  
                    {
                    //! we have no root growth
                     //! do nothing
                    }

                }



        #endregion




        #region Potential Water Available to Roots (Water Supply)


            void GetSupplyFromSWIM()
                {

                throw new Exception(" Sugar can't get 'supply' from  SWIM yet");   //TODO: Remove this when you figure it out.
                //    //! Use the water uptake values given by some other
                //    //! module in the APSIM system. (eg APSWIM)
                //    //! KEEP other variables calculated above.

                //    crop_get_ext_uptakes(
                //                        g % uptake_source   //! uptake flag
                //                        , c % crop_type       //! crop type
                //                        , "water"           //! uptake name
                //                        , 1.0               //! unit conversion factor
                //                        , 0.0               //! uptake lbound
                //                        , 100.0             //! uptake ubound
                //                        , g % sw_supply       //! uptake array
                //                        , max_layer         //! array dim
                //                         );

                }



            void cproc_sw_supply1(double c_sw_lb, double[] i_dlayer, double[] i_ll_dep, double[] i_dul_dep, double[] i_sw_dep, double i_root_depth, double[] i_kl, 
                                    ref double[] o_sw_avail, ref double[] o_sw_avail_pot, ref double[] o_sw_supply)
                {
                //! ====================================================================
                //       subroutine cproc_sw_supply1 ()
                //! ====================================================================


                //!+  Sub-Program Arguments
                //      real    C_sw_lb            ! (INPUT)
                //      real    G_dlayer (*)       ! (INPUT)
                //      real    P_ll_dep (*)       ! (INPUT)
                //      real    G_dul_dep (*)      ! (INPUT)
                //      real    G_sw_dep (*)       ! (INPUT)
                //      integer max_layer          ! (INPUT)
                //      real    g_root_depth       ! (INPUT)
                //      real    p_kl (*)           ! (INPUT)
                //      real    g_sw_avail (*)     ! (OUTPUT)
                //      real    g_sw_avail_pot (*) ! (OUTPUT)
                //      real    g_sw_supply (*)    ! (OUTPUT)

                //!+  Purpose
                //!     Calculate the crop water supply based on the KL approach.

                //!+  Mission Statement
                //!   Calculate today's soil water supply

                crop_check_sw(c_sw_lb, i_dlayer, i_dul_dep, i_sw_dep, i_ll_dep);
                crop_sw_avail_pot(i_dlayer, i_dul_dep, i_root_depth, i_ll_dep, ref o_sw_avail_pot);         //! potential extractable sw
                crop_sw_avail(i_dlayer, i_root_depth, i_sw_dep, i_ll_dep, ref o_sw_avail);                  //! actual extractable sw (sw-ll)
                crop_sw_supply(i_dlayer, i_root_depth, i_sw_dep, i_kl, i_ll_dep, ref o_sw_supply);

                }


            void crop_check_sw(double c_minsw, double[] i_dlayer, double[] i_dul_dep, double[] i_sw_dep,double[] i_ll_dep)
                {
                //!     ===========================================================
                //      subroutine crop_check_sw()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      REAL       minsw             ! (INPUT)  lowest acceptable value for ll
                //      REAL       dlayer(*)         ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       dul_dep(*)        ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                //      INTEGER    max_layer         ! (INPUT)  number of layers in profile ()
                //      REAL       sw_dep(*)         ! (INPUT)  soil water content of layer L (mm)
                //      REAL       ll_dep(*)         ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)

                //!+  Purpose
                //!       Check validity of soil water parameters for all soil profile layers.

                //!+  Notes
                //!           Reports an error if
                //!           - ll_dep and dul_dep are not in ascending order
                //!           - ll is below c_minsw
                //!           - sw < c_minsw

                double      l_dul;            //! drained upper limit water content of layer (mm water/mm soil)
                string      l_err_messg;     //! error message
                double      l_ll;                //! lower limit water content of layer (mm water/mm soil)
                double      l_sw;                //! soil water content of layer l (mm water/mm soil)


           
                for (int layer = 0; layer < i_dlayer.Length; layer++)
                    {

                    l_sw = MathUtility.Divide(i_sw_dep[layer], i_dlayer[layer], 0.0);
                    l_dul = MathUtility.Divide(i_dul_dep[layer], i_dlayer[layer], 0.0);
                    l_ll = MathUtility.Divide(i_ll_dep[layer], i_dlayer[layer], 0.0);

                    if (l_ll + error_margin(l_ll) < c_minsw) 
                        {
                        l_err_messg =  " lower limit of " + l_ll + " in layer " + ob(layer) + Environment.NewLine + "         is below acceptable value of " + c_minsw;
                        Console.WriteLine(l_err_messg);
                        }

                    if (l_dul + error_margin(l_dul) < l_ll) 
                        {
                        l_err_messg = " Drained upper limit of " + l_dul + " in layer " + ob(layer) + Environment.NewLine + "         is below lower limit of " + ll;
                        Console.WriteLine(l_err_messg);
                        }

                    if (l_sw + error_margin(l_sw) < c_minsw) 
                        {
                        l_err_messg = " Soil water of " + l_sw + " in layer " + ob(layer) + Environment.NewLine + "         is below acceptable value of " + c_minsw;
                        Console.WriteLine(l_err_messg);
                        }
                    }

                }


            void crop_sw_avail_pot(double[] i_dlayer, double[] i_dul_dep, double i_root_depth, double[] i_ll_dep, ref double[] o_sw_avail_pot)
                {

                //!     ===========================================================
                //      subroutine crop_sw_avail_pot()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      INTEGER    num_layer        ! (INPUT)  number of layers in profile
                //      REAL       dlayer(*)        ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       dul_dep(*)       ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                //      REAL       root_depth       ! (INPUT)  depth of roots (mm)
                //      REAL       ll_dep(*)        ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
                //      REAL       sw_avail_pot(*)  ! (OUTPUT) crop water potential uptake for each full layer (mm)

                //!+  Purpose
                //!       Return potential available soil water from each layer in the root zone.

                //!+  Mission Statement
                //!   Calculate the potentially (or maximum) available soil water

                //!+  Notes
                //!       see cr474 for limitations and potential problems.


                int    l_deepest_layer_ob;    //! deepest layer in which the roots are growing
                



                //! get potential uptake

                fill_real_array(ref o_sw_avail_pot, 0.0, o_sw_avail_pot.Length);

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                for (int layer=0; layer < l_deepest_layer_ob; layer++)
                    {
                    o_sw_avail_pot[layer] = i_dul_dep[layer] - i_ll_dep[layer];
                    }

                    //! correct bottom layer for actual root penetration
                o_sw_avail_pot[zb(l_deepest_layer_ob)] = o_sw_avail_pot[zb(l_deepest_layer_ob)] * root_proportion(l_deepest_layer_ob, i_dlayer, i_root_depth);

                }

    
            void crop_sw_avail(double[] i_dlayer, double i_root_depth, double[]  i_sw_dep, double[] i_ll_dep, ref double[] o_sw_avail)
                {
                //!     ===========================================================
                //      subroutine crop_sw_avail()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      INTEGER    num_layer        ! (INPUT)  number of layers in profile
                //      REAL       dlayer(*)        ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       root_depth       ! (INPUT)  depth of roots (mm)
                //      REAL       sw_dep(*)        ! (INPUT)  soil water content of layer L (mm)
                //      REAL       ll_dep(*)        ! (INPUT)  lower limit of plant-extractable
                //                                  ! soil water for soil layer L (mm)
                //      real       sw_avail(*)      ! (OUTPUT) crop water potential uptake
                //                                  ! for each full layer (mm)

                //!+  Purpose
                //!       Return actual water available for extraction from each layer in the
                //!       soil profile by the crop (mm water)

                //!+  Mission Statement
                //!   Calculate the available soil water

                //!+  Notes
                //!       see cr474 for limitations and potential problems.


                int    l_deepest_layer_ob;    //! deepest layer in which the roots are growing


                            //! get potential uptake

                fill_real_array(ref o_sw_avail, 0.0, o_sw_avail.Length);

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                for (int layer=0; layer < l_deepest_layer_ob; layer++)
                    {
                    o_sw_avail[layer] = i_sw_dep[layer] - i_ll_dep[layer];
                    o_sw_avail[layer] = l_bound(o_sw_avail[layer], 0.0);
                    }

                            //! correct bottom layer for actual root penetration
                 o_sw_avail[zb(l_deepest_layer_ob)] = o_sw_avail[zb(l_deepest_layer_ob)] * root_proportion(l_deepest_layer_ob, i_dlayer, i_root_depth);
                 }

        
            void crop_sw_supply(double[] idlayer, double i_root_depth, double[] i_sw_dep, double[] i_kl, double[] i_ll_dep, ref double[] o_sw_supply)
                {


                //!     ===========================================================
                //      subroutine crop_sw_supply()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      INTEGER    num_layer       ! (INPUT)  number of layers in profile
                //      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       root_depth      ! (INPUT)  depth of roots (mm)
                //      REAL       sw_dep(*)       ! (INPUT)  soil water content of layer L (mm)
                //      REAL       kl(*)           ! (INPUT)  root length density factor for water
                //      REAL       ll_dep(*)       ! (INPUT)  lower limit of plant-extractable soi
                //      real       sw_supply(*)    ! (OUTPUT) potential crop water uptake
                //                                 ! from each layer (mm) (supply to roots)

                //!+  Purpose
                //!       Return potential water uptake from each layer of the soil profile
                //!       by the crop (mm water). This represents the maximum amount in each
                //!       layer regardless of lateral root distribution but takes account of
                //!       root depth in bottom layer.

                //!+  Mission Statement
                //!   Calculate today's soil water supply

                //!+  Notes
                //!      This code still allows water above dul to be taken - cnh


                int         l_deepest_layer_ob;   //! deepest layer in which the roots are growing
                double      l_sw_avail;        //! water available (mm)


                    //! get potential uptake

                fill_real_array (ref o_sw_supply, 0.0, o_sw_supply.Length);

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                    {
                    l_sw_avail = (i_sw_dep[layer] - i_ll_dep[layer]);
                    o_sw_supply[layer] = l_sw_avail * i_kl[layer];
                    o_sw_supply[layer] = l_bound(o_sw_supply[layer], 0.0);
                    }

                    //! now adjust bottom layer for depth of root
                o_sw_supply[zb(l_deepest_layer_ob)] = o_sw_supply[zb(l_deepest_layer_ob)] * root_proportion(l_deepest_layer_ob, idlayer, i_root_depth);

                }



        #endregion



        #region Actual Water Extraction by Plant (Water Uptake)  (Limited by Roots ability to extract and Plant Demand (ie.SW Demand))


            void GetUptakeFromSWIM()
                {

                //TODO: FIGURE OUT HOW TO DO THIS LATER.
                throw new Exception(" Sugar can't get 'uptake' from SWIM yet");   //TODO: Remove this when you figure it out.
                //! use the water uptake values already given by some other
                //! module in the apsim system. (eg apswim)
                //for (int layer = 0; layer < dlayer.length; layer++)
                //    {
                //    g % dlt_sw_dep[layer] = -1.0 * g % sw_supply[layer];
                //    }

                }



            void cproc_sw_uptake1(double[] i_dlayer, double i_root_depth, double i_sw_demand, double[] i_sw_supply, ref double[] o_dlt_sw_dep)
                {
                //!     ===========================================================
                //      subroutine cproc_sw_uptake1()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      INTEGER    num_layer       ! (INPUT)  number of layers in profile
                //      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       root_depth      ! (INPUT)  depth of roots (mm)
                //      REAL       sw_demand       ! (INPUT)  total crop demand for water (mm)
                //      REAL       sw_supply(*)    ! (INPUT)  potential water to take up (supply)
                //      real       dlt_sw_dep (*)  ! (OUTPUT) root water uptake (mm)

                //!+  Purpose
                //!       Returns actual water uptake from each layer of the soil
                //!       profile by the crop (mm).

                //!+  Mission Statement
                //!   Calculate the crop uptake of soil water


                int         l_deepest_layer_ob;   //! deepest layer in which the roots are growing
                double      l_sw_supply_sum;   //! total potential over profile (mm)



                    //! find total root water potential uptake as sum of all layers

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                l_sw_supply_sum = SumArray(i_sw_supply, l_deepest_layer_ob);

                if ((l_sw_supply_sum <= 0.0) || (i_sw_demand <= 0.0))
                    {
                    //! we have no uptake - there is no demand or potential
                    fill_real_array(ref o_dlt_sw_dep, 0.0, i_dlayer.Length);
                    }
                else
                    {
                    //! get actual uptake

                    fill_real_array(ref o_dlt_sw_dep, 0.0, i_dlayer.Length);
                    if (i_sw_demand < l_sw_supply_sum)
                        {
                        //! demand is less than what roots could take up.
                        //! water is non-limiting.
                        //! distribute demand proportionately in all layers.

                        for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                            {
                            o_dlt_sw_dep[layer] = -MathUtility.Divide(i_sw_supply[layer], l_sw_supply_sum, 0.0) * i_sw_demand;
                            }
                        }
                    else
                        {
                        //! water is limiting - not enough to meet demand so take what is available (potential)

                        for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                            {
                            o_dlt_sw_dep[layer] = -i_sw_supply[layer];
                            }

                        }
                    }

                }


        #endregion




        #region Water Stress Factor Calculations



            void crop_swdef_expansion(double[] c_x_sw_demand_ratio, double[] c_y_swdef_leaf,  
                                          double i_root_depth, double i_sw_demand, double[] i_sw_supply, ref double o_swdef)
                {

                //!+  Sub-Program Arguments
                //      INTEGER num_sw_demand_ratio  ! (INPUT)
                //      REAL    x_sw_demand_ratio(*) ! (INPUT)
                //      REAL    y_swdef_leaf(*)      ! (INPUT)
                //      INTEGER num_layer            ! (INPUT)  number of layers in profile
                //      REAL    dlayer(*)            ! (INPUT)  thickness of soil layer I (mm)
                //      REAL    root_depth           ! (INPUT)  depth of roots (mm)
                //      REAL    sw_demand            ! (INPUT)  total crop demand for water (mm)
                //      REAL    sw_supply(*)         ! (INPUT)  potential water to take up (supply) from current soil water (mm)
                //      REAL    swdef                ! (OUTPUT) sw stress factor (0-1)

                //!+  Purpose
                //!       Get the soil water to demand ratio and calculate the 0-1 stress factor for leaf expansion.
                //!       1 is no stress, 0 is full stress.

                //!+  Mission Statement
                //!   Calculate the soil water stress factor for leaf expansion


                int         l_deepest_layer_ob;       //! deepest layer in which the roots are growing
                double      l_sw_demand_ratio;     //! water supply:demand ratio
                double      l_sw_supply_sum;       //! total supply over profile (mm)
                bool        l_didInterpolate;

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

                    //! get potential water that can be taken up when profile is full

                l_sw_supply_sum = SumArray(i_sw_supply, l_deepest_layer_ob);
                l_sw_demand_ratio = MathUtility.Divide(l_sw_supply_sum, i_sw_demand, 10.0);
                o_swdef = MathUtility.LinearInterpReal(l_sw_demand_ratio, c_x_sw_demand_ratio, c_y_swdef_leaf, out l_didInterpolate);

                }



            void sugar_swdef_demand_ratio ( double[] c_x_sw_demand_ratio, double[] c_y_swdef_leaf, 
                                        double i_root_depth, double i_sw_demand, double[] i_sw_supply, ref double o_swdef)
                {


                //*+  Sub-Program Arguments
                //      INTEGER    C_num_sw_demand_ratio ! (INPUT)
                //      REAL       C_x_sw_demand_ratio(*) ! (INPUT)
                //      REAL       C_y_swdef_leaf(*)     ! (INPUT)
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL       G_sw_demand           ! (INPUT)  total crop demand for water (m
                //      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (su
                //      real      swdef                 ! (OUTPUT) sw stress factor (0-1)

                //*+  Purpose
                //*       Get the soil water availability factor (0-1), commonly
                //*       called soil water deficit factor. 1 is no stress, 0 is full stress.

                //*+  Mission Statement
                //*     Calculates the soil water availability factor


                int         l_deepest_layer_ob;         //! deepest layer in which the roots are growing
                double      l_sw_demand_ratio;       //! water supply:demand ratio
                double      l_sw_supply_sum;         //! total supply over profile (mm)
                bool        l_didInterpolate;

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);


                    //! get potential water that can be taken up when profile is full

                l_sw_supply_sum = SumArray(i_sw_supply, l_deepest_layer_ob);
                l_sw_demand_ratio = MathUtility.Divide(l_sw_supply_sum, i_sw_demand, 10.0);

                o_swdef = MathUtility.LinearInterpReal(l_sw_demand_ratio, c_x_sw_demand_ratio, c_y_swdef_leaf, out l_didInterpolate);

                }



            void crop_swdef_pheno(double[] c_x_sw_avail_ratio, double[] c_y_swdef_pheno,  
                                double i_root_depth, double[] i_sw_avail, double[] i_sw_avail_pot, ref double o_swdef)
                {


                //!+  Sub-Program Arguments
                //      INTEGER num_sw_avail_ratio  ! (INPUT)
                //      REAL    x_sw_avail_ratio(*) ! (INPUT)
                //      REAL    y_swdef_pheno(*)    ! (INPUT)
                //      INTEGER num_layer           ! (INPUT)  number of layers in profile
                //      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL    root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
                //      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (mm)
                //      REAL    swdef               ! (OUTPUT) sw stress factor (0-1)

                //!+  Purpose
                //!       Get the soil water availability ratio in the root zone
                //!       and calculate the 0 - 1 stress factor for phenology.
                //!       1 is no stress, 0 is full stress.

                //!+  Mission Statement
                //!   Calculate the soil water stress factor for phenological development



                int         deepest_layer_ob;       //! deepest layer in which the roots are growing
                double      sw_avail_ratio;      //! water availability ratio
                double      sw_avail_pot_sum;    //! potential extractable soil water (mm)
                double      sw_avail_sum;        //! actual extractable soil water (mm)
                bool        didInterpolate;

                deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                sw_avail_pot_sum = SumArray(i_sw_avail_pot, deepest_layer_ob);
                sw_avail_sum = SumArray(i_sw_avail, deepest_layer_ob);
                sw_avail_ratio = MathUtility.Divide(sw_avail_sum, sw_avail_pot_sum, 1.0); //!???
                sw_avail_ratio = bound(sw_avail_ratio , 0.0, 1.0);
                o_swdef = MathUtility.LinearInterpReal(sw_avail_ratio, c_x_sw_avail_ratio, c_y_swdef_pheno, out didInterpolate);

                }



            void crop_swdef_photo(double i_root_depth, double i_sw_demand, double[] i_sw_supply, ref double o_swdef)
                {


                //!+  Sub-Program Arguments
                //      INTEGER num_layer          ! (INPUT)  number of layers in profile
                //      REAL dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL sw_demand           ! (INPUT)  total crop demand for water (mm)
                //      REAL sw_supply(*)        ! (INPUT)  potential water to take up (supply) from current soil water (mm)
                //      REAL swdef                 ! (OUTPUT) sw stress factor (0-1)

                //!+  Purpose
                //!       Calculate the soil water supply to demand ratio and therefore the 0-1 stress factor
                //!       for photosysnthesis. 1 is no stress, 0 is full stress.

                //!+  Mission Statement
                //!   Calculate the soil water stress factor for photosynthesis


                int         l_deepest_layer_ob;   //! deepest layer in which the roots are growing
                double      l_sw_demand_ratio; //! water supply:demand ratio
                double      l_sw_supply_sum;   //! total supply over profile (mm)



                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                    //! get potential water that can be taken up when profile is full
                l_sw_supply_sum = SumArray(i_sw_supply, l_deepest_layer_ob);
                l_sw_demand_ratio = MathUtility.Divide(l_sw_supply_sum, i_sw_demand, 1.0);
                o_swdef = bound(l_sw_demand_ratio , 0.0, 1.0);

                }




        #endregion 



        #region  Set Minimum stuctural stem sucrose
 

            void sugar_min_sstem_sucrose(ref double io_min_sstem_sucrose)
                {

                //*     ===========================================================
                //      subroutine sugar_min_sstem_sucrose (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Set limit on SStem for start of sucrose partitioning

                //*+  Mission Statement
                //*     Minimum stuctural stem sucrose

                 //! These ideally should go in new routines but it seems overkill for
                 //! a simple.  This is a patch job

                double dlt_min_sstem_sucrose;

                 if (on_day_of(begcane, g_current_stage))
                    {
                    io_min_sstem_sucrose = cult.min_sstem_sucrose;
                    }

                 if (stage_is_between(begcane, crop_end, g_current_stage))
                    {
                    dlt_min_sstem_sucrose = cult.min_sstem_sucrose_redn * (1.0 - Math.Min(g_nfact_stalk, g_swdef_stalk));
                    dlt_min_sstem_sucrose = u_bound(dlt_min_sstem_sucrose, io_min_sstem_sucrose);
                    io_min_sstem_sucrose = io_min_sstem_sucrose - dlt_min_sstem_sucrose;
                    }

                }


        #endregion



        #region Phenological Growth (Process Event)




            void sugar_phen_init(double c_shoot_lag, double  c_shoot_rate, double i_current_stage, double i_sowing_depth, double i_ratoon_no, 
                                double c_tt_begcane_to_flowering, double c_tt_emerg_to_begcane, double c_tt_flowering_to_crop_end, ref double[] io_phase_tt)
                {

                //*     ===========================================================
                //      subroutine sugar_phenology_init ()
                //*     ===========================================================

                //*+  Purpose
                //*     Use temperature, photoperiod and genetic characteristics
                //*     to determine when the crop begins a new growth phase.
                //*     The initial daily thermal time and height are also set.

                //*+  Mission Statement
                //*     Initialise crop growth phases


                //*     ===========================================================
                //      subroutine sugar_phen_init()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_shoot_lag           ! (INPUT)  minimum growing degree days fo
                //      REAL       C_shoot_rate          ! (INPUT)  growing deg day increase with
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
                //      INTEGER    G_Ratoon_no           ! (INPUT)  ratoon no (mm)
                //      REAL       P_tt_begcane_to_flowering ! (INPUT)
                //      REAL       P_tt_emerg_to_begcane ! (INPUT)
                //      REAL       P_tt_flowering_to_crop_end ! (INPUT)
                //      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                //                                       ! degree days required for
                //                                       ! each stage (deg days)

                //*+  Purpose
                //*       Returns cumulative thermal time targets required for the
                //*       individual growth stages.

                //*+  Mission Statement
                //*     Calculate the thermal time targets for individual growth stages


                if (on_day_of(sprouting, i_current_stage))
                    {
                    if (i_ratoon_no == 0) 
                        {
                        io_phase_tt[zb(sprouting_to_emerg)] = c_shoot_lag + i_sowing_depth*c_shoot_rate;
                        }
                    else
                        {
                        //! Assume the mean depth of shooting is half way between the set depth and the soil surface.
                        io_phase_tt[zb(sprouting_to_emerg)] = c_shoot_lag + i_sowing_depth/2.0 * c_shoot_rate;
                        }
                    }
                else if (on_day_of(emerg, i_current_stage)) 
                    {
                    io_phase_tt[zb(emerg_to_begcane)] = c_tt_emerg_to_begcane;
                    }
                else if (on_day_of(begcane, i_current_stage))
                    {
                    io_phase_tt[zb(begcane_to_flowering)] = c_tt_begcane_to_flowering;
                    }
                else if (on_day_of (flowering, i_current_stage))
                    {
                    io_phase_tt[zb(flowering_to_crop_end)] = c_tt_flowering_to_crop_end;
                    }
         
                }




            //void cproc_phenology1(double g_previous_stage, double g_current_stage, int sowing_stage, int germ_stage, int end_development_stage, int start_stress_stage, int end_stress_stage , int max_stage,        
            //                        double[] c_x_temp, double[] c_y_tt, double g_maxt, double g_mint, double g_nfact_pheno, double g_swdef_pheno, double c_pesw_germ, double[] c_fasw_emerg, double[] c_rel_emerg_rate, int c_num_fasw_emerg,
            //                        double[] g_dlayer, int max_layer, double g_sowing_depth, 
            //                        double[] g_sw_dep , double[] g_dul_dep , double[] p_ll_dep, 
            //                        double g_dlt_tt, double[] g_phase_tt, double g_phase_devel, double g_dlt_stage, double[] g_tt_tot, double[] g_days_tot)
            //    {
    
            //sv- MERGED INTO THE ONPROCESS EVENT HANDLER. 

            //    //*     ===========================================================
            //    //      subroutine sugar_phenology ()
            //    //*     ===========================================================

            //    //*+  Purpose
            //    //*     Use temperature, photoperiod and genetic characteristics
            //    //*     to determine when the crop begins a new growth phase.
            //    //*     The initial daily thermal time and height are also set.

            //    //*+  Mission Statement
            //    //*     Calculate the crop growth stages


            //    //!     ===========================================================
            //    //      subroutine cproc_phenology1 ()
            //    //!     ===========================================================

            //    //!+  Purpose
            //    //!     Use temperature, photoperiod and genetic characteristics
            //    //!     to determine when the crop begins a new growth phase.
            //    //!     The initial daily thermal time and height are also set.

            //    //!+  Mission Statement
            //    //!   Calculate crop phenological development using thermal time targets.


            //    g_previous_stage = g_current_stage;

            //    //! get thermal times

            //    g_dlt_tt = crop_thermal_time(c_x_temp, c_y_tt, g_current_stage, g_maxt, g_mint, start_stress_stage, end_stress_stage, g_nfact_pheno, g_swdef_pheno);

            //    g_phase_devel = crop_phase_devel(sowing_stage, germ_stage, end_development_stage, 
            //                                        c_pesw_germ, c_fasw_emerg, c_rel_emerg_rate, c_num_fasw_emerg, g_current_stage, g_days_tot,
            //                                        g_dlayer, max_layer, g_sowing_depth, g_sw_dep, g_dul_dep, p_ll_dep, g_dlt_tt, g_phase_tt, g_tt_tot);

            //    crop_devel(g_current_stage, max_stage, g_phase_devel, out g_dlt_stage, ref g_current_stage);

            //    //! update thermal time states and day count

            //    accumulate(g_dlt_tt, ref g_tt_tot, g_previous_stage, g_dlt_stage);

            //    accumulate(1.0, ref g_days_tot, g_previous_stage, g_dlt_stage);

            //    }



            double crop_thermal_time(double[] c_x_temp, double[] c_y_tt, double i_current_stage, double i_maxt, double i_mint, 
                                    int i_start_stress_stage, int i_end_stress_stage, double i_nfact_pheno, double i_swdef_pheno)
                {

                //!     ===========================================================
                //      subroutine crop_thermal_time()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      INTEGER    C_num_temp            ! (INPUT)  size_of table
                //      REAL       C_x_temp(*)           ! (INPUT)  temperature table for photosyn
                //      REAL       C_y_tt(*)             ! (INPUT)  degree days
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
                //      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
                //      INTEGER    start_stress_stage    ! (INPUT)
                //      INTEGER    end_stress_stage      ! (INPUT)
                //      REAL       G_nfact_pheno         ! (INPUT)
                //      REAL       G_swdef_pheno         ! (INPUT)
                //      real       g_dlt_tt              ! (OUTPUT) daily thermal time (oC)

                //!+  Purpose
                //!     Growing degree day (thermal time) is calculated. Daily thermal time is reduced
                //!     if water or nitrogen stresses occur.

                //!+  Mission Statement
                //!   Calculate today's thermal time, %11.

                //!+  Notes
                //!     Eight interpolations of the air temperature are
                //!     calculated using a three-hour correction factor.
                //!     For each air three-hour air temperature, a value of growing
                //!     degree day is calculated.  The eight three-hour estimates
                //!     are then averaged to obtain the daily value of growing degree
                //!     days.


                double       l_dly_therm_time;        //! thermal time for the day (deg day)


                l_dly_therm_time = linint_3hrly_temp(i_maxt, i_mint, c_x_temp, c_y_tt);

                if (stage_is_between(i_start_stress_stage, i_end_stress_stage, i_current_stage))
                    {
                    return l_dly_therm_time * Math.Min(i_swdef_pheno, i_nfact_pheno);
                    }
                else
                    {
                    return l_dly_therm_time;
                    }

                }



            double crop_phase_devel(int i_sowing_stage, int i_germ_stage, int i_end_development_stage, double c_pesw_germ, double[] c_fasw_emerg, double[] c_rel_emerg_rate, int c_num_fasw_emerg, 
                                    double i_current_stage, double[] i_days_tot, double[] i_dlayer,  int i_max_layer, 
                                    double i_sowing_depth, double[] i_sw_dep, double[] i_dul_dep, double[] i_ll_dep, ref double io_dlt_tt, double[] i_phase_tt, double[] i_tt_tot)
                {
                //!     ===========================================================
                //      subroutine crop_phase_devel()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      INTEGER    sowing_Stage          ! (INPUT)
                //      INTEGER    germ_Stage            ! (INPUT)
                //      INTEGER    end_development_stage ! (INPUT)
                //      REAL       C_pesw_germ           ! (INPUT)  plant extractable soil water i
                //      REAL       C_fasw_emerg(*)       ! (INPUT)
                //      REAL       c_rel_emerg_rate(*)   ! (INPUT)
                //      INTEGER    c_num_fasw_emerg      ! (INPUT)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      INTEGER    max_layer             ! (INPUT)
                //      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
                //      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
                //      REAL       G_dul_dep(*)          ! (INPUT)
                //      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
                //      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
                //      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
                //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
                //      real       phase_devel           ! (OUTPUT) fraction of current phase
                //                                       ! elapsed ()

                //!+  Purpose
                //!     Determine the fraction of current phase elapsed ().

                //!+  Mission statement
                //!   Determine the progress through the current growth stage.


                  if (stage_is_between(i_sowing_stage, i_germ_stage, i_current_stage))
                        {
                        return crop_germination(i_sowing_stage, i_germ_stage, c_pesw_germ, i_current_stage, i_dlayer, i_sowing_depth, i_sw_dep, i_ll_dep);
                        }
                  else if (stage_is_between (i_germ_stage, i_end_development_stage, i_current_stage))
                        {

                        crop_germ_dlt_tt(c_fasw_emerg, c_rel_emerg_rate, i_current_stage, i_germ_stage, i_sowing_depth, i_sw_dep, i_ll_dep, i_dul_dep, ref io_dlt_tt);

                        return crop_phase_tt(io_dlt_tt, i_phase_tt, i_tt_tot, i_current_stage);
                        }
                else
                    {
                    return i_current_stage % 1.0;
                    }

                }
        


            double crop_germination(int i_sowing_stage, int i_germ_stage, double c_pesw_germ, double i_current_stage, double[] i_dlayer, double  i_sowing_depth, 
                                    double[] i_sw_dep, double[] i_ll_dep)
                {
                //!     ===========================================================
                //      real function crop_germination()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      INTEGER    sowing_stage          ! (INPUT)
                //      INTEGER    germ_stage            ! (INPUT)
                //      REAL       C_pesw_germ           ! (INPUT)  plant extractable soil water i
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      INTEGER    max_layer             ! (INPUT)
                //      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
                //      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
                //      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab

                //!+  Purpose
                //!      Determine germination based on soil water availability

                //!+  Mission statement
                //!   Determine germination based upon soil moisture status.


                int         l_layer_no_seed;         //! seedling layer number
                double      l_pesw_seed;             //! plant extractable soil water in seedling layer available for germination ( mm/mm)


                    //! determine if soil water content is sufficient to allow germination.
                    //! Soil water content of the seeded layer must be > the
                    //! lower limit to be adequate for germination.

                if (stage_is_between (i_sowing_stage,i_germ_stage,i_current_stage)) 
                    {
                    l_layer_no_seed = FindLayerNo_ob(i_sowing_depth);
                    l_pesw_seed = MathUtility.Divide(i_sw_dep[zb(l_layer_no_seed)] - i_ll_dep[zb(l_layer_no_seed)], i_dlayer[zb(l_layer_no_seed)], 0.0);

                    //! can't germinate on same day as sowing, because miss out on day of sowing else_where

                    if ((l_pesw_seed > c_pesw_germ)  && !(on_day_of(i_sowing_stage,i_current_stage)))
                        {
                        //! we have germination
                        //! set the current stage so it is on the point of germination
                        return 1.0 + (i_current_stage % 1.0);
                        }
                    else
                        {
                        //! no germination yet but indicate that we are on the way.
                        return 0.999;
                        }
                    }
                else
                    {
                        //! no sowing yet
                    return 0.0;
                    }

                }




            void crop_germ_dlt_tt(double[] c_fasw_emerg, double[] c_rel_emerg_rate, double i_current_stage, int i_germ_phase,
                        double i_sowing_depth, double[] i_sw_dep, double[] i_ll_dep, double[] i_dul_dep, ref double io_dlt_tt)
                {
                //!     ===========================================================
                //      subroutine crop_germ_dlt_tt()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL       C_fasw_emerg(*)       ! (INPUT)  plant extractable soil water i
                //      REAL       c_rel_emerg_rate(*)   ! (INPUT)
                //      INTEGER    c_num_fasw_emerg      ! (INPUT)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      INTEGER    germ_phase            ! (INPUT)
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      INTEGER    max_layer             ! (INPUT)
                //      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
                //      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
                //      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
                //      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit(mm)
                //      real       g_dlt_tt

                //!+  Purpose
                //!      Calculate daily thermal time for germination to emergence
                //!      limited by soil water availability in the seed layer.

                //!+  Mission statement
                //!   Calculate emergence adjusted thermal time (based on moisture status)

                int     l_layer_no_seed;         //! seedling layer number

                double  l_fasw_seed;
                double  l_rel_emerg_rate;        //! relative emergence rate (0-1)
                int     l_current_phase;
                bool    l_didInterpolate;


              l_current_phase = (int)i_current_stage;

              if (l_current_phase == i_germ_phase)
                  {
                  l_layer_no_seed = FindLayerNo_ob(i_sowing_depth);
                  l_fasw_seed = MathUtility.Divide(i_sw_dep[zb(l_layer_no_seed)] - i_ll_dep[zb(l_layer_no_seed)], i_dul_dep[zb(l_layer_no_seed)] - i_ll_dep[zb(l_layer_no_seed)], 0.0);
                  l_fasw_seed = bound(l_fasw_seed, 0.0, 1.0);

                  l_rel_emerg_rate = MathUtility.LinearInterpReal(l_fasw_seed, c_fasw_emerg, c_rel_emerg_rate, out l_didInterpolate);
                  io_dlt_tt = io_dlt_tt * l_rel_emerg_rate;
                  }
              else
                  {
                  //io_dlt_tt = io_dlt_tt;
                  }

                }



            double crop_phase_tt(double i_dlt_tt, double[] i_phase_tt, double[] i_tt_tot, double i_stage_no)
                {
                //!     ===========================================================
                //      real function crop_phase_tt()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
                //      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
                //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
                //      real       stage_no              ! (INPUT) stage number

                //!+  Purpose
                //!       Return fraction of thermal time we are through the current phenological phase (0-1)

                //!+  Mission statement
                //!   the fractional progress through growth stage %4


                int     l_phase;                 //! phase number containing stage

                l_phase = (int)i_stage_no;

                return MathUtility.Divide(i_tt_tot[zb(l_phase)] + i_dlt_tt, i_phase_tt[zb(l_phase)], 1.0);

                }



            void crop_devel(double i_current_stage, int i_max_stage, double i_phase_devel, out double o_dlt_stage, ref double io_current_stage)
                {

                //!     ===========================================================
                //      subroutine crop_devel()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      INTEGER    max_stage             ! (INPUT)
                //      REAL       G_phase_devel         ! (INPUT)  development of current phase (
                //      real       dlt_stage             ! (OUTPUT) change in growth stage
                //      real       current_stage         ! (OUTPUT) new stage no.

                //!+  Purpose
                //!     Determine the curent stage of development.

                //!+  Mission statement
                //!   Determine the current stage of crop development.


                double      l_new_stage;             //! new stage number

                 //! mechanical operation - not to be changed

                 //! now calculate the new delta and the new stage

                //sv- replaced aint() fortran with explicit cast to int. Apparently it is supposed to work.

                l_new_stage = (int)i_current_stage + i_phase_devel;   //add the phase development to the current stage to get the new stage.
                o_dlt_stage = l_new_stage - i_current_stage;

                if (i_phase_devel >= 1.0)
                    {
                    io_current_stage = (int)(io_current_stage + 1.0);
                    if ((int)io_current_stage == i_max_stage)
                        {
                        io_current_stage = 1.0;
                        }
                    }

                else
                    {
                    io_current_stage = l_new_stage;
                    }
            
                }



        #endregion



        #region Crop Height  (Process Event)

            double cproc_canopy_height(double i_canopy_height, double[] i_x_stem_wt, double[] i_y_height, double[] i_dm_green, double i_plants, int i_stem)
                {

                //*     ===========================================================
                //      subroutine sugar_height (Option)
                //*     ===========================================================

                //*+  Mission Statement
                //*     Calculate canopy height

                //!     ===========================================================
                //      subroutine cproc_canopy_height ()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
                //      REAL       p_x_stem_wt(*)
                //      REAL       p_y_height(*)
                //      INTEGER    p_num_stem_wt
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      integer    stem                  ! (INPUT)  plant part no for stem
                //      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)   //sv- I think this should be an output.

                //!+  Purpose
                //!       Get change in plant canopy height from stem dry matter per plant

                //!+  Mission Statement
                //!   Calculate change in crop canopy height (based upon weight of %7).


                double      l_dm_stem_plant;         //! dry matter of stem (g/plant)
                double      l_new_height;            //! new plant height (mm)
                bool        l_didInterpolate;

                double      l_dlt_canopy_height;


                l_dm_stem_plant = MathUtility.Divide(i_dm_green[i_stem], i_plants, 0.0);
                l_new_height = MathUtility.LinearInterpReal(l_dm_stem_plant, i_x_stem_wt, i_y_height, out l_didInterpolate);

                l_dlt_canopy_height = l_new_height - i_canopy_height;
                l_dlt_canopy_height = l_bound(l_dlt_canopy_height, 0.0);

                return l_dlt_canopy_height;
                }

        #endregion



        #region Leaf Number



            void cproc_leaf_no_init1(double c_leaf_no_at_emerg, double i_current_stage, int i_emerg, ref double[] o_leaf_no_zb, ref double[] o_node_no_zb)
                {

                //*     ===========================================================
                //      subroutine sugar_leaf_no_init (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Leaf number development

                //*+  Mission Statement
                //*     Initialise leaf number development

                            //! Plant leaf development

                //!     ===========================================================
                //      subroutine cproc_leaf_no_init1()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL       C_leaf_no_at_emerg    ! (INPUT)  leaf number at emergence ()
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      real       leaf_no(*)            ! (OUTPUT) initial leaf number
                //      real       node_no(*)            ! (OUTPUT) initial node number
                //      integer    emerg                 ! (INPUT)  emergence stage no

                //!+  Purpose
                //!       Return the the initial number of leaves at emergence.

                //!+  Mission Statement
                //!   Initialise leaf number (on first day of %3)

                //!+  Notes
                //!    NIH - I would prefer to use leaf_no_at_init and init_stage
                //!          for routine parameters for generalisation


                if (on_day_of(i_emerg, i_current_stage))
                    {
                        //! initialise first leaves

                    o_leaf_no_zb[zb(i_emerg)] = c_leaf_no_at_emerg;
                    o_node_no_zb[zb(i_emerg)] = c_leaf_no_at_emerg;
                    }
                else
                    {
                    //! no inital leaf no
                    }

                }



            void cproc_leaf_no_pot1(double[] c_x_node_no_app, double[] c_y_node_app_rate, double[] c_x_node_no_leaf, double[] c_y_leaves_per_node,
                                    double i_current_stage, int i_start_node_app, int i_end_node_app, int i_emerg, double i_dlt_tt, double[] i_node_no_zb,
                                    out double o_dlt_leaf_no_pot, out double o_dlt_node_no_pot)
                {
                //*     ===========================================================
                //      subroutine sugar_leaf_no_pot ()
                //*     ===========================================================

                //*+  Purpose
                //*       Leaf number development


                //!     ===========================================================
                //      subroutine cproc_leaf_no_pot1()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      REAL       C_x_node_no_app(*)    !(INPUT)
                //      REAL       C_y_node_app_rate(*)  !(INPUT)
                //      INTEGER    c_num_node_no_app     ! (INPUT)
                //      REAL       c_x_node_no_leaf(*)   ! (INPUT)
                //      REAL       C_y_leaves_per_node(*)! (INPUT)
                //      INTEGER    c_num_node_no_leaf    ! (INPUT)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      INTEGER    start_node_app        ! (INPUT)  stage of start of leaf appeara
                //      INTEGER    end_node_app          ! (INPUT)  stage of end of leaf appearanc
                //      INTEGER    emerg                 ! (INPUT)  emergence stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
                //      REAL       G_node_no(*)          ! (INPUT)  number of fully expanded nodes
                //      real       dlt_leaf_no_pot       ! (OUTPUT) new fraction of oldest
                //                                       ! expanding leaf
                //      real       dlt_node_no_pot       ! (OUTPUT) new fraction of oldest
                //                                       ! expanding node on main stem

                //!+  Purpose
                //!       Return the fractional increase in emergence of the oldest
                //!       expanding leaf and nodes.  Nodes can initiate from a user-defined
                //!       starting stage and leaves from emergence.  The initiation of both
                //!       leaves and nodes finishes at a user-defined end stage.
                //!       Note ! this does not take account of the other younger leaves
                //!       that are currently expanding

                //!+  Mission Statement
                //!   Calculate the potential increase in plant leaf and node number


                double      l_node_no_now_ob;           //! number of fully expanded nodes
                double      l_leaves_per_node;
                double      l_node_app_rate;
                bool        l_didInterpolate;


                l_node_no_now_ob = sum_between_zb(zb(i_start_node_app), zb(i_end_node_app), i_node_no_zb);


                l_node_app_rate = MathUtility.LinearInterpReal(l_node_no_now_ob, c_x_node_no_app, c_y_node_app_rate, out l_didInterpolate);

                l_leaves_per_node = MathUtility.LinearInterpReal(l_node_no_now_ob, c_x_node_no_leaf, c_y_leaves_per_node, out l_didInterpolate);


                if (stage_is_between (i_start_node_app, i_end_node_app, i_current_stage))
                    {
                    o_dlt_node_no_pot = MathUtility.Divide(i_dlt_tt, l_node_app_rate, 0.0);
                    }
                else
                    {
                    o_dlt_node_no_pot = 0.0;
                    }


                if (on_day_of(i_emerg, i_current_stage))
                    {
                    //! no leaf growth on first day because initialised elsewhere ???
                    o_dlt_leaf_no_pot = 0.0;
                    }
                else if (stage_is_between(i_emerg, i_end_node_app, i_current_stage)) 
                    {
                    o_dlt_leaf_no_pot = o_dlt_node_no_pot * l_leaves_per_node;
                    }
                else
                    {
                    o_dlt_leaf_no_pot = 0.0;
                    }




                }

        #endregion



        #region Leaf Area (Potential)

            //void sugar_init_leaf_area(double c_initial_tpla, double i_current_stage, double i_plants, ref double o_lai, ref double[] o_leaf_area)
            //    {
            //    //*     ===========================================================
            //    //      subroutine sugar_leaf_area_init (Option)
            //    //*     ===========================================================

            //    //*+  Purpose
            //    //*     Set the initial plant leaf area

            //    //*+  Mission Statement
            //    //*     Initialise plant leaf area


            //                //! Plant leaf development


            //    //*     ===========================================================
            //    //      subroutine sugar_init_leaf_area()
            //    //*     ===========================================================

            //    //*+  Sub-Program Arguments
            //    //      REAL       C_initial_tpla        ! (INPUT)  initial plant leaf area (mm^2)
            //    //      REAL       G_current_stage       ! (INPUT)  current phenological stage
            //    //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
            //    //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
            //    //      real       lai                   ! (OUTPUT) total plant leaf area
            //    //      real       leaf_area(*)          ! (OUTPUT) plant leaf areas

            //    //*+  Purpose
            //    //*       Initialise leaf area.

            //    //*+  Mission Statement
            //    //*     Calculate the initial leaf area


            //    if (on_day_of(emerg, i_current_stage)) 
            //        {
            //        o_lai = c_initial_tpla * smm2sm * i_plants;
            //        o_leaf_area[0] = c_initial_tpla;
            //        }
            //    }


            double sugar_leaf_area_devel(double c_leaf_no_correction, double i_dlt_leaf_no, double[] i_leaf_no_zb, double i_plants, 
                                        double[] c_leaf_size, double[] c_leaf_size_no, 
                                        double[] c_tillerf_leaf_size, double[] c_tillerf_leaf_size_no)
                {

                //*     ===========================================================
                //      subroutine sugar_leaf_area_potential ()
                //*     ===========================================================

                //*+  Purpose
                //*       Simulate potential crop leaf area development - may be limited by
                //*       DM production in subsequent routine

                //*+  Mission Statement
                //*     Get the potential leaf area development


                //            ! Plant leaf development


                //*     ===========================================================
                //      subroutine sugar_leaf_area_devel()
                //*     ===========================================================


                //*+  Sub-Program Arguments
                //      REAL       C_leaf_no_correction  ! (INPUT)  corrects for other growing lea
                //      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
                //      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       C_leaf_size(*)        ! (INPUT)
                //      REAL       C_leaf_size_no(*)     ! (INPUT)
                //      INTEGER    C_num_leaf_size       ! (INPUT)
                //      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
                //      REAL       C_tillerf_leaf_size(*) ! (INPUT)
                //      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
                //      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

                //*+  Purpose
                //*       Return the potential increase in leaf area development (mm^2)
                //*       calculated on an individual leaf basis.

                //*+  Mission Statement
                //*     Calculate potential leaf area development based on individual leaves

                //*+  Changes
                //*     070495 nih taken from template
                //*     120196 nih made this really a potential dlt_lai by removing
                //*                stress factors.  g_dlt_lai_pot can now be used
                //*                in different places and stress applied only when
                //*                required.


                double       l_area;                  //! potential maximum area of oldest expanding leaf (mm^2) in today's conditions
                double       l_leaf_no_effective_ob;     //! effective leaf no - includes younger leaves that have emerged after the current one



                    //! once leaf no is calculated leaf area of largest expanding leaf is determined

                l_leaf_no_effective_ob = sum_between_zb(zb(emerg), zb(now), i_leaf_no_zb) + c_leaf_no_correction;

                l_area = sugar_leaf_size(c_leaf_size, c_leaf_size_no, c_tillerf_leaf_size, c_tillerf_leaf_size_no, l_leaf_no_effective_ob);

                //Console.WriteLine("dlt_leaf_no, area, plants: " + i_dlt_leaf_no + " , " + l_area + " , " + i_plants);  

                return i_dlt_leaf_no * l_area * smm2sm * i_plants;

                }


            double sugar_leaf_size(double[] c_leaf_size, double[] c_leaf_size_no, double[] c_tillerf_leaf_size, double[] c_tillerf_leaf_size_no, double i_leaf_no_ob)
                {
                //*     ===========================================================
                //      real function sugar_leaf_size()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_leaf_size(*)        ! (INPUT)
                //      REAL       C_leaf_size_no(*)     ! (INPUT)
                //      INTEGER    C_num_leaf_size       ! (INPUT)
                //      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
                //      REAL       C_tillerf_leaf_size(*) ! (INPUT)
                //      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
                //      real       leaf_no               ! (INPUT) nominated leaf number

                //*+  Purpose
                //*       Return the leaf area (mm^2) of a specified leaf no.

                //*+  Mission Statement
                //*     Calculate the specific leaf area


                double      l_leaf_size;
                double      l_tiller_factor;
                bool        l_didInterpolate;


                l_leaf_size = MathUtility.LinearInterpReal(i_leaf_no_ob, c_leaf_size_no, c_leaf_size, out l_didInterpolate);

                l_tiller_factor = MathUtility.LinearInterpReal(i_leaf_no_ob, c_tillerf_leaf_size_no, c_tillerf_leaf_size, out l_didInterpolate);

                //l_leaf_size = au.linear_interp_real(i_leaf_no_ob, c_leaf_size_no, c_leaf_size, cult.num_leaf_size);

                //Console.WriteLine("c_leaf_size: " + c_leaf_size[0] + " , " + c_leaf_size[1] +" , " + c_leaf_size[2]);  
                //Console.WriteLine(" c_leaf_size: " + c_leaf_size.Length);

                //Console.WriteLine("leaf no, num leaf size: " + i_leaf_no_ob + " , " + cult.num_tillerf_leaf_size);  

                //l_tiller_factor = au.linear_interp_real(i_leaf_no_ob, c_tillerf_leaf_size_no, c_tillerf_leaf_size, cult.num_tillerf_leaf_size);

                //Console.WriteLine("leaf size, tiller_factor: " + l_leaf_size + " , " + l_tiller_factor);  

                return l_leaf_size * l_tiller_factor;

                }



        #endregion




        #region Potential Transpiration (reduced by available water supply from roots)





             double cproc_bio_water1(double i_root_depth, double[] i_sw_supply, double i_transp_eff)
                 {
                //*     ===========================================================
                //      subroutine sugar_bio_water (Option)
                //*     ===========================================================



                //!     ===========================================================
                //      subroutine cproc_bio_water1(num_layer, dlayer, root_depth,          &
                //                          sw_supply, transp_eff, dlt_dm_pot_te)
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      INTEGER    num_layer       ! (INPUT)  number of layers in profile
                //      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       root_depth      ! (INPUT)  depth of roots (mm)
                //      REAL       sw_supply(*)    ! (INPUT)  potential water to take up (supply)
                //      REAL       transp_eff      ! (INPUT)  transpiration efficiency (g dm/m^2/m
                //      real       dlt_dm_pot_te   !(OUTPUT) potential dry matter production
                //                                 ! by transpiration (g/m^2)

                //!+  Purpose
                //!   Calculate the potential biomass production based upon today's water supply.


                int         l_deepest_layer_ob;   //! deepest layer in which the roots are growing
                double      l_sw_supply_sum;   //! Water available to roots (mm)

                 //transpiration efficiency is grams of carbo / mm of water

                 //when water stress occurs the plant is not able to get all the water it needs.
                 //it can only get what is available (ie. sw_supply)
                 //So based on the water that the plant was able to get, 
                 //we need to work out how much biomass it could grow. 
                 //biomass able to be grown(g) = transp_eff(g/mm) * supply(mm)


                    //! potential (supply) by transpiration

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                l_sw_supply_sum = SumArray(i_sw_supply, l_deepest_layer_ob);
                return l_sw_supply_sum * i_transp_eff;

                 }




        #endregion




        #region Water Logging



            double crop_oxdef_photo1(double[] c_oxdef_photo, double[] c_oxdef_photo_rtfr, 
                                    double[] i_ll15_dep, double[] i_sat_dep, double[] i_sw_dep, double[] i_dlayer, double[] i_root_length, double  i_root_depth)
                {
                //*     ===========================================================
                //      subroutine sugar_water_log (Option)
                //*     ===========================================================

                //*+  Purpose
                //*         Get current water stress factors (0-1)

                //*+  Mission Statement
                //*     Get the water stress factors for photosynthesis


                //! ====================================================================
                //       subroutine crop_oxdef_photo1 ()
                //! ====================================================================


                //!+  Sub-Program Arguments
                //      INTEGER    C_num_oxdef_photo     ! (INPUT)
                //      REAL       C_oxdef_photo(*)      ! (INPUT)
                //      REAL       C_oxdef_photo_rtfr(*) ! (INPUT)
                //      REAL       G_ll15_dep(*)         ! (INPUT)
                //      REAL       G_sat_dep(*)          ! (INPUT)
                //      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_root_length(*)      ! (INPUT)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      INTEGER    max_layer             ! (INPUT)
                //      real       oxdef_photo           ! (OUTPUT)

                //!+  Purpose
                //!   Calculate 0-1 factor for water logging effect on growth

                //!+  Mission Statement
                //!   Calculate the oxygen deficit factor for photosynthesis


                int         l_num_root_layers;
                double      l_wet_root_fr;
                double      l_wfps;
                double[]    l_root_fr = new double[i_dlayer.Length];
                double      l_tot_root_fr;
                bool        l_didInterpolate;



                l_tot_root_fr = 1.0;
                crop_root_dist(i_dlayer, i_root_length, i_root_depth, ref l_root_fr, l_tot_root_fr);

                l_num_root_layers = count_of_real_vals(l_root_fr, i_dlayer.Length);

                l_wet_root_fr = 0.0;

                for (int layer = 0; layer < l_num_root_layers; layer++)
                    {
                    l_wfps = MathUtility.Divide(i_sw_dep[layer] - i_ll15_dep[layer], i_sat_dep[layer] - i_ll15_dep[layer], 0.0);
                    l_wfps = bound(l_wfps, 0.0, 1.0);

                    l_wet_root_fr = l_wet_root_fr + l_wfps * l_root_fr[layer];
                    }
    

                return MathUtility.LinearInterpReal(l_wet_root_fr, c_oxdef_photo_rtfr, c_oxdef_photo, out l_didInterpolate);

                }



            void crop_root_dist(double[] i_dlayer, double[] i_root_length, double i_root_depth, ref double[] o_root_array, double i_root_sum)
                {

                //!     ===========================================================
                //      subroutine crop_root_dist()
                //!     ===========================================================


                //!+  Sub-Program Arguments
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_root_length(*)      ! (INPUT)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      real       root_array(*)         ! (OUTPUT) array to contain
                //                                       ! distributed material
                //      real       root_sum              ! (INPUT) Material to be distributed
                //      integer    max_layer             ! (INPUT) max number of soil layers

                //!+  Purpose
                //!       Distribute root material over profile based upon root length distribution.

                //!+  Mission Statement
                //!   Distribute %5 over the profile according to root distribution


                int         l_deepest_layer_ob;         //! deepest layer in which the roots are growing
                double      l_root_length_sum;       //! sum of root distribution array
        


                //! distribute roots over profile to root_depth

                fill_real_array(ref o_root_array, 0.0, i_dlayer.Length);

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

                l_root_length_sum = SumArray(i_root_length, l_deepest_layer_ob);

                for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                    {
                    o_root_array[layer] = i_root_sum * MathUtility.Divide(i_root_length[layer], l_root_length_sum, 0.0);
                    }

                }














        #endregion



        #region Biomass (Dry Matter) Growth


            double sugar_dm_pot_rue(double[] c_rue, double i_current_stage, double i_radn_int, double i_nfact_photo, double i_temp_stress_photo, double i_oxdef_photo, double i_lodge_redn_photo)
                {
                //sv- Also partly replaces the following function.
                //*     ===========================================================
                //      subroutine sugar_bio_RUE (Option)
                //*     ===========================================================
                //*+  Mission Statement
                //*     Biomass radiation use efficiency


                //*     ===========================================================
                //      subroutine sugar_dm_pot_rue
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_radn_int            ! (INPUT)
                //      REAL       G_nfact_photo         ! (INPUT)
                //      REAL       G_temp_stress_photo   ! (INPUT)
                //      REAL       G_oxdef_photo         ! (INPUT)
                //      REAL       G_lodge_redn_photo    ! (INPUT)
                //      real       dlt_dm_pot            ! (OUTPUT) potential dry matter(carbohydrate) production (g/m^2)

                //*+  Purpose
                //*       This routine calculates the potential biomass (carbohydrate)
                //*       production for conditions where soil supply is limiting.


                //*+  Local Variables
                int l_current_phase;         //! current phase number
                double l_rue;                   //! radiation use efficiency under stress (g biomass/mj)


                l_current_phase = (int)(i_current_stage);
                l_rue = c_rue[zb(l_current_phase)] * sugar_rue_reduction(i_nfact_photo, i_temp_stress_photo, i_oxdef_photo, i_lodge_redn_photo);

                //! potential dry matter production with temperature
                //! and N content stresses is calculated.
                //! This is g of dry biomass produced per MJ of intercepted
                //! radiation under stressed conditions.

                return l_rue * i_radn_int;
                }


            double sugar_rue_reduction (double i_nfact_photo, double i_temp_stress_photo, double i_oxdef_photo, double i_lodge_redn_photo)
                {

                //*+  Sub-Program Arguments
                //      REAL       G_nfact_photo         ! (INPUT)
                //      REAL       G_temp_stress_photo   ! (INPUT)
                //      REAL       G_oxdef_photo         ! (INPUT)
                //      REAL       G_lodge_redn_photo    ! (INPUT)

                //*+  Purpose
                //*       The overall fractional effect of non-optimal N, Temperature, lodging
                //*       and water logging conditions on radiation use efficiency.

                //*+  Mission Statement
                //*     Get the fractional effect of non-optimal N, Temperature and water on radiation use efficiency


                return min(i_temp_stress_photo, i_nfact_photo, i_oxdef_photo, i_lodge_redn_photo);

                }


            double sugar_dm_pot_rue_pot(double[] c_rue, double i_current_stage, double i_radn_int)
                {
                //sv- Also partly replaces the following function.
                //*     ===========================================================
                //      subroutine sugar_bio_RUE (Option)
                //*     ===========================================================
                //*+  Mission Statement
                //*     Biomass radiation use efficiency

                //*     ===========================================================
                //      subroutine sugar_dm_pot_rue_pot
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm/mj)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_radn_int            ! (INPUT)
                //      real       dlt_dm_pot            ! (OUTPUT) potential dry matter(carbohydrate) production (g/m^2)

                //*+  Purpose
                //*       This routine calculates the potential biomass (carbohydrate)
                //*       production for conditions where soil supply is non-limiting.

                //*+  Local Variables
                int         l_current_phase;         //! current phase number
                double      l_rue;                   //! radiation use efficiency under no stress (g biomass/mj)


                l_current_phase = (int)(i_current_stage);
                l_rue = c_rue[zb(l_current_phase)];

                return l_rue * i_radn_int;
                }


        #endregion



        #region Biomass(Dry Matter) Growth Actual

            void sugar_dm_init(double c_dm_cabbage_init, double c_dm_leaf_init, double c_dm_sstem_init, double c_dm_sucrose_init, double c_specific_root_length, 
                                double i_current_stage, double[] i_dlayer, double i_plants, double[] i_root_length, 
                                ref double[] io_dm_green, ref double[] o_leaf_dm)
                {
                //*     ===========================================================
                //      subroutine sugar_bio_actual (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Simulate crop biomass processes.

                //*+  Mission Statement
                //*     Calculate crop biomass processes


                //*     ===========================================================
                //      subroutine sugar_dm_init()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_dm_cabbage_init     ! (INPUT)  cabbage "    "        "        "
                //      REAL       C_dm_leaf_init        ! (INPUT)  leaf growth before emergence (g/plant)
                //      REAL       C_dm_sstem_init       ! (INPUT)  stem growth before emergence (g/plant)
                //      REAL       C_dm_sucrose_init     ! (INPUT)  sucrose "    "        "        "
                //      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (mm/g)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       G_root_length(*)      ! (INPUT)
                //      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                //                                       ! (g/m^2)
                //      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                //                                       ! plant part (g/plant)
                //      real       leaf_dm(*)            ! (OUTOUT) leaf wts

                //*+  Purpose
                //*       Initialise plant weights and plant weight minimums
                //*       at required instances.

                //*+  Mission Statement
                //*     Get initial plant weights and plant weight minimums


                int       l_num_layers;
                double    l_root_wt_layer;
                double    l_root_length_layer;



                    //! initialise plant weight
                    //! initialisations - set up dry matter for leaf, stem,..etc,
                    //! and root

                if (on_day_of(emerg, i_current_stage))
                    {
                        //! seedling has just emerged.

                        //! we initialise root_wt no by adding all root together
                        //! as specified by the rlv given by user at sowing.
                    l_num_layers = count_of_real_vals (i_dlayer, max_layer);
                    io_dm_green[root] = 0.0;
                    for (int layer = 0; layer < l_num_layers; layer++)
                        {
                        l_root_length_layer = i_root_length[layer] * sm2smm;
                        l_root_wt_layer  = MathUtility.Divide(l_root_length_layer, c_specific_root_length, 0.0);
                        io_dm_green[root] = io_dm_green[root] +   l_root_wt_layer;
                        }

                    io_dm_green[sstem] = c_dm_sstem_init * i_plants;
                    io_dm_green[leaf] = c_dm_leaf_init * i_plants;
                    o_leaf_dm[0] = c_dm_leaf_init;
                    io_dm_green[cabbage] = c_dm_cabbage_init * i_plants;
                    io_dm_green[sucrose] = c_dm_sucrose_init * i_plants;

                    }
                else
                    {
                    //cnh     NO MINIMUMS SET AS YET
                    //! no changes
                    }

          }








        #endregion





        #region Leaf Area Stressed


            //*     ===========================================================
            //      subroutine sugar_leaf_area_stressed (Option)
            //*     ===========================================================

            //*+  Purpose
            //*       Simulate potential stressed crop leaf area development - may
            //*       be limited by DM production in subsequent routine

            //*+  Mission Statement
            //*     Calculate potential stressed leaf area development


                    //! Plant leaf development

            //! ====================================================================
            //       subroutine cproc_leaf_area_stressed1 ()
            //! ====================================================================

            //!+  Sub-Program Arguments
            //       real g_dlt_lai_pot
            //       real g_swdef_expansion
            //       real g_nfact_expansion
            //       real g_dlt_lai_stressed

            //!+  Purpose
            //!     Calculate the biomass non-limiting leaf area development from the
            //!     potential daily increase in lai and the stress factors for water and nitrogen.

            //!+  Mission Statement
            //!   Calculate biomass non-limiting leaf area development


          //g_dlt_lai_stressed = g_dlt_lai_pot          &
          //             * min(g_swdef_expansion          &
          //                  ,g_nfact_expansion)











        #endregion





        #region Biomass(Dry Matter) Partitioning



            //void sugar_bio_partition()
            //    {

            //    //*     ===========================================================
            //    //      subroutine sugar_bio_partition (Option)
            //    //*     ===========================================================

            //    //*+  Purpose
            //    //*       Partition biomass.

            //    //*+  Mission Statement
            //    //*     Get biomass partitioning data



            //    double  l_leaf_no_today;
            //    bool    l_didInterpolate;


            //    l_leaf_no_today = sum_between(emerg, now, g_leaf_no) + g_dlt_leaf_no;

            //    g_sla_min = MathUtility.LinearInterpReal((double)l_leaf_no_today, crop.sla_lfno, crop.sla_min, out l_didInterpolate);   // sugar_sla_min()  //Return the minimum specific leaf area (mm^2/g) of a specified leaf no.

            //    g_sucrose_fraction = sugar_sucrose_fraction(cult.stress_factor_stalk, cult.sucrose_fraction_stalk, g_swdef_stalk, g_nfact_stalk, g_temp_stress_stalk, g_lodge_redn_sucrose);


            //    sugar_dm_partition_rules(cult.cane_fraction, crop.leaf_cabbage_ratio, g_min_sstem_sucrose, crop.ratio_root_shoot, cult.sucrose_delay,
            //                              g_current_stage, g_dm_green, g_sla_min, g_sucrose_fraction, g_tt_tot, g_dlt_dm, g_dlt_lai_stressed, ref g_dlt_dm_green, ref g_partition_xs); //sugar_dm_partition()

            //    }




            double sugar_sucrose_fraction(double[] c_stress_factor_stalk, double[] c_sucrose_fraction_stalk, 
                                            double i_swdef_stalk, double i_nfact_stalk, double i_temp_stress_stalk, double i_lodge_redn_sucrose)

                {

                //*     ===========================================================
                //      subroutine sugar_sucrose_fraction()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      INTEGER    c_num_stress_factor_Stalk ! (INPUT)
                //      REAL       C_stress_factor_stalk(*) ! (INPUT)
                //      REAL       C_Sucrose_fraction_stalk(*) ! (INPUT)
                //      REAL       G_swdef_stalk     ! (INPUT)
                //      REAL       G_nfact_stalk     ! (INPUT)
                //      REAL       G_temp_stress_stalk     ! (INPUT)
                //      REAL       G_lodge_Redn_sucrose
                //      real       sucrose_fraction      ! (OUTPUT) fraction of cane C
                //                                       ! partitioned to sucrose (0-1)

                //*+  Purpose
                //*     Returns the fraction of Cane C partioned to sucrose based
                //*     upon severity of water stress(cell expansion)

                //*+  Mission Statement
                //*     Calculate fraction of Cane C partioned to sucrose


                double      l_stress_factor_min;     //! minimum of all 0-1 stress factors on stalk growth
                double      l_sucrose_fraction; 
                bool        l_didInterpolate;


                l_stress_factor_min = Math.Min(i_swdef_stalk, Math.Min(i_nfact_stalk, i_temp_stress_stalk));   //get the minimum of any of the 3 values. 

                //! this should give same results as old version for now

                l_sucrose_fraction = MathUtility.LinearInterpReal(l_stress_factor_min, c_stress_factor_stalk, c_sucrose_fraction_stalk, out l_didInterpolate) * i_lodge_redn_sucrose;

                bound_check_real_var(l_sucrose_fraction, 0.0, 1.0, "fraction of Cane C to sucrose");

                l_sucrose_fraction = bound (l_sucrose_fraction, 0.0, 1.0);

                return l_sucrose_fraction;

                }




            void sugar_dm_partition_rules(double c_cane_fraction, double c_leaf_cabbage_ratio, double i_min_sstem_sucrose, double[] c_ratio_root_shoot, double c_sucrose_delay, double i_current_stage, 
                                            double[] i_dm_green, double i_sla_min, double i_sucrose_fraction, double[] i_tt_tot, double i_dlt_dm, double i_dlt_lai_pot, 
                                            ref double[] o_dlt_dm_green, ref double o_partition_xs)
                {

                //*     ===========================================================
                //      subroutine sugar_dm_partition()
                //*     ===========================================================

                //*     ===========================================================
                //      subroutine sugar_dm_partition_rules()
                //*     ===========================================================


                //*+  Sub-Program Arguments
                //      REAL       C_cane_fraction       ! (INPUT)
                //      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
                //      REAL       G_min_sstem_sucrose   ! (INPUT)
                //      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
                //      REAL       C_sucrose_delay       ! (INPUT)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
                //      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
                //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
                //      real       dlt_dm                ! (INPUT) dry matter to partition
                //      real       dlt_lai_pot           ! (INPUT) increase in lai if unconstrained by carbon supply.
                //      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
                //      real       partition_xs          ! xs dry matter to that required to supply all demands. (g/m^2)  (sv- Excess dry matter)

                //*+  Purpose
                //*       Partitions assimilate between individual plant pools.  The rules
                //*       for partitioning change with stage of crop growth.

                //*+  Mission Statement
                //*     Calculate the partition assimilate between individual plant pools

                //*+  Changes
                //*       060495 nih taken from template
                //*       110196 nih added dlt_dm to argument list to make this routine
                //*                  more like a utility routine for partioning dry matter



                int         l_current_phase;         //! current phase no.
                double      l_dlt_dm_green_tot;      //! total of partitioned dm (g/m^2)
                double      l_dlt_leaf_max;          //! max increase in leaf wt (g/m2)
                double      l_dlt_cane;              //! increase in cane wt (g/m2)
                double      l_dlt_cane_min;          //! min increase in cane wt (g/m2)
                double      l_tt_since_begcane;      //! thermal time since the beginning of cane growth (deg days)


                 //! Root must be satisfied. The roots don't take any of the
                 //! carbohydrate produced - that is for tops only.  Here we assume
                 //! that enough extra was produced to meet demand. Thus the root
                 //! growth is not removed from the carbo produced by the model.

                 //! first we zero all plant component deltas

                fill_real_array(ref o_dlt_dm_green, 0.0, max_part);
                o_partition_xs = 0.0;

                    //! now we get the root delta for all stages - partition scheme specified in coeff file

                l_current_phase = (int)i_current_stage;
                o_dlt_dm_green[root] = c_ratio_root_shoot[zb(l_current_phase)] * i_dlt_dm;


                l_dlt_leaf_max = MathUtility.Divide(i_dlt_lai_pot, i_sla_min*smm2sm, 0.0);

                if (stage_is_between (emerg, begcane, i_current_stage)) 
                    {
                    //! we have leaf and cabbage development only

                    o_dlt_dm_green[leaf] = i_dlt_dm * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0));

                    o_dlt_dm_green[leaf] = u_bound(o_dlt_dm_green[leaf],l_dlt_leaf_max);

                    o_dlt_dm_green[cabbage] = o_dlt_dm_green[leaf]/c_leaf_cabbage_ratio;

                    o_partition_xs = i_dlt_dm - o_dlt_dm_green[leaf] - o_dlt_dm_green[cabbage];

                    //! Put the excess dry matter in sstem
                    o_dlt_dm_green[sstem] = o_partition_xs;
                    }

                else if (stage_is_between (begcane, crop_end, i_current_stage)) 
                    {
                    //! if leaf component makes leaves too thick extra goes to sstem

                    l_dlt_cane_min = c_cane_fraction * i_dlt_dm;

                    o_dlt_dm_green[leaf] = (i_dlt_dm - l_dlt_cane_min) * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0));

                    o_dlt_dm_green[leaf] = u_bound(o_dlt_dm_green[leaf],l_dlt_leaf_max);
                    o_dlt_dm_green[cabbage] = o_dlt_dm_green[leaf]/c_leaf_cabbage_ratio;
                    l_dlt_cane = i_dlt_dm - o_dlt_dm_green[leaf] - o_dlt_dm_green[cabbage];

                    l_tt_since_begcane = sum_between_zb(zb(begcane),zb(now),i_tt_tot);

                    if ((l_tt_since_begcane > c_sucrose_delay) && (i_dm_green[sstem] > i_min_sstem_sucrose))
                        {
                        //! the SStem pool gets (1 - c_sucrose_fraction) of the DEMAND
                        //! for C. Extra C above the demand for cane goes only into
                        //! the sucrose pool.

                        o_dlt_dm_green[sstem] = l_dlt_cane_min * (1.0 - i_sucrose_fraction);
                        o_dlt_dm_green[sucrose] = l_dlt_cane_min * i_sucrose_fraction;

                        o_partition_xs = l_dlt_cane - l_dlt_cane_min;
                        o_dlt_dm_green[sucrose] = o_dlt_dm_green[(sucrose)] + o_partition_xs;
                        }
                    else
                        {
                        //! nih - should excess C go into sucrose here too even though
                        //! we have not started into the sugar accumulation phase????
                        o_dlt_dm_green[sstem] = l_dlt_cane;
                        o_partition_xs = l_dlt_cane - l_dlt_cane_min;
                        }
                    }

                else
                    {
                    //! no partitioning
                    }

                            // cnh Due to small rounding errors I will say that small errors are ok

                    //! do mass balance check - roots are not included
                l_dlt_dm_green_tot = SumArray(o_dlt_dm_green, max_part) - o_dlt_dm_green[root];

                bound_check_real_var(l_dlt_dm_green_tot, (i_dlt_dm - 0.000001), (i_dlt_dm + 0.000001), "dlt_dm_green_tot mass balance");

                    //! check that deltas are in legal range

                bound_check_real_array(o_dlt_dm_green, -0.000001, (i_dlt_dm + 0.000001), "dlt_dm_green", max_part);

                }

   


        #endregion



        #region BIOMASS(DRY MATTER) RETRANSLOCATION


        //*     ===========================================================
        //      subroutine sugar_bio_retrans (Option)
        //*     ===========================================================

        //*+  Purpose
        //*       Retranslocate biomass.

        //*+  Mission Statement
        //*     Get the biomass retranslocation information

 

        //*     ===========================================================
        //      subroutine sugar_dm_retranslocate
        //     :               (
        //     :                dm_retranslocate
        //     :               )
        //*     ===========================================================


        //*+  Sub-Program Arguments
        //      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
        //                                       ! weights due to translocation (g/m^2)

        //*+  Purpose
        //*     Calculate plant dry matter delta's due to retranslocation (g/m^2)

        //*+  Mission Statement
        //*     Calculate plant dry matter change due to retranslocation




        #endregion




        #region Leaf Area Actual


            double sugar_leaf_area(double[] i_dlt_dm_green, double i_dlt_lai_stressed, double i_dlt_leaf_no, double[] i_leaf_no_zb, 
                                    double[] c_sla_lfno, double[] c_sla_max)
                {
            
                //*     ===========================================================
                //      subroutine sugar_leaf_actual (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Simulate actual crop leaf area development - checks that leaf area
                //*       development matches DM production.

                //*+  Mission Statement
                //*      Get the actual leaf area development infomation


                //! limit the delta leaf area by carbon supply

 
                //*     ===========================================================
                //      subroutine sugar_leaf_area()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
                //      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
                //      REAL       G_dlt_lai_stressed    ! (INPUT)  potential change in live plant
                //      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
                //      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
                //      INTEGER    C_num_sla_lfno        ! (INPUT)
                //      REAL       C_sla_lfno(*)         ! (INPUT)
                //      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for

                //*+  Purpose
                //*       Simulate actual crop leaf area development - checks that leaf area
                //*       development matches DM production.

                //*+  Mission Statement
                //*     Calculate actual crop leaf area development


                double      l_dlt_lai_carbon;     //! maximum daily increase in leaf area index from carbon supply
                double      l_leaf_no_today_ob;      //! total number of leaves today
                double      l_sla_max;            //! maximum allowable specific leaf area (cm2/g)
                bool        l_didInterpolate;


                    //! limit the delta leaf area by carbon supply
                    //! and stress factors

                l_leaf_no_today_ob = sum_between_zb(zb(emerg), zb(now), i_leaf_no_zb) + i_dlt_leaf_no;

                l_sla_max = MathUtility.LinearInterpReal((double)l_leaf_no_today_ob, c_sla_lfno, c_sla_max, out l_didInterpolate);  //sugar_sla_max()  //Return the maximum specific leaf area (mm^2/g) of a specified leaf no.

                l_dlt_lai_carbon = i_dlt_dm_green[leaf] * l_sla_max * smm2sm;

                return Math.Min(i_dlt_lai_stressed, l_dlt_lai_carbon);

                }




        #endregion




        #region Root Distribution


            void cproc_root_length_growth1(ref double[] o_dlt_root_length,
                                            double c_specific_root_length, double[] i_dlayer, 
                                            double i_dlt_root_wt, double i_dlt_root_depth, double i_root_depth, double[] i_root_length, double i_plants, double[] i_xf,
                                            double[] c_x_sw_ratio, double[] c_y_sw_fac_root, double[] c_x_plant_rld, double[] c_y_rel_root_rate, 
                                            double[] i_dul_dep, double[] i_sw_dep, double[] i_ll_dep, int i_max_layer)
                {

                //*     ===========================================================
                //      subroutine sugar_root_dist (Option)
                //*     ===========================================================

                //*+  Mission Statement
                //*     Calculate plant root distribution

                //! ====================================================================
                //       subroutine cproc_root_length_growth1()
                //! ====================================================================


                //!+  Sub-Program Arguments
                //      REAL       C_specific_root_length ! (INPUT) length of root per unit wt (mm
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_dlt_root_wt         ! (INPUT)  plant root biomass growth (g/m
                //      REAL       G_dlt_root_length(*)  ! (OUTPUT) increase in root length (mm/mm
                //      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL       g_root_length(*)      ! (INPUT)
                //      REAL       g_plants              ! (INPUT)
                //      REAL       P_xf(*)               ! (INPUT)  eXtension rate Factor (0-1)
                //      INTEGER    C_num_sw_ratio        ! (INPUT)
                //      REAL       C_x_sw_ratio(*)       ! (INPUT)
                //      REAL       C_y_sw_fac_root(*)    ! (INPUT)
                //      REAL       c_x_plant_rld (*)     ! (INPUT)
                //      REAL       c_y_rel_root_rate(*)  ! (INPUT)
                //      INTEGER    c_num_plant_rld       ! (INPUT)
                //      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water
                //      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
                //      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
                //      INTEGER    max_layer             ! (INPUT)  maximum number of soil laye

                //!+  Purpose
                //!   Calculate the increase in root length density in each rooted
                //!   layer based upon soil hospitality, moisture and fraction of
                //!   layer explored by roots.

                //!+  Mission Statement
                //!   Calculate the root length growth for each layer

                const int   l_crop_max_layer = 100;        //sv- taken from, FortranInfrastructure\ConstantsModule.f90

                int         l_deepest_layer_ob;      //! deepest rooted later
                double      l_dlt_length_tot;     //! total root length increase (mm/m^2)
                double[]    l_rlv_factor = new double[l_crop_max_layer];          //! relative rooting factor for a layer
                double      l_rlv_factor_tot;     //! total rooting factors across profile
                double      l_branching_factor;    
                double      l_plant_rld;
                double      l_rld;
                bool        l_didInterpolate;


                if (i_max_layer > l_crop_max_layer)
                    {
                    throw new Exception("Too many layers for crop routines");
                    }
                else
                    {
                    fill_real_array(ref o_dlt_root_length, 0.0, i_max_layer);

                    l_deepest_layer_ob = FindLayerNo_ob(i_root_depth + i_dlt_root_depth);
                    l_rlv_factor_tot = 0.0;

                    for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                        {

                        l_rld = MathUtility.Divide(i_root_length[layer], i_dlayer[layer], 0.0);

                        l_plant_rld = MathUtility.Divide(l_rld, i_plants, 0.0);

                        l_branching_factor = MathUtility.LinearInterpReal(l_plant_rld, c_x_plant_rld, c_y_rel_root_rate, out l_didInterpolate);

                        l_rlv_factor[layer] =  crop_sw_avail_fac(c_x_sw_ratio, c_y_sw_fac_root, i_dul_dep, i_sw_dep, i_ll_dep, ob(layer)) 
                                                                * l_branching_factor         //! branching factor
                                                                * i_xf[layer]                //! growth factor
                                                                * MathUtility.Divide(i_dlayer[layer], i_root_depth, 0.0);   //! space weighting factor
                                          
                        l_rlv_factor[layer] = l_bound(l_rlv_factor[layer], 1e-6);
                        l_rlv_factor_tot = l_rlv_factor_tot + l_rlv_factor[layer];
                        }

                    l_dlt_length_tot = i_dlt_root_wt / sm2smm * c_specific_root_length;

                    for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                        {
                        o_dlt_root_length[layer] = l_dlt_length_tot * MathUtility.Divide(l_rlv_factor[layer], l_rlv_factor_tot, 0.0);
                        }

                    }

                }



        #endregion



        #region Leaf Senescence


            double sugar_leaf_death_grass(double c_green_leaf_no, double i_lodge_redn_green_leaf, double i_current_stage,
                                            double i_dlt_leaf_no, double[] i_leaf_no, double[] i_node_no_dead)
                {
                //*     ===========================================================
                //      subroutine sugar_leaf_death ()
                //*     ===========================================================

                //*+  Purpose
                //*       Return the fractional death of oldest green leaf.

                //*     ===========================================================
                //      subroutine sugar_leaf_death_grass()
                //*     ===========================================================


                //*+  Sub-Program Arguments
                //      REAL       C_green_leaf_no       ! (INPUT)
                //      REAL       G_lodge_redn_green_leaf !(INPUT)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
                //      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
                //      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
                //      real       dlt_node_no_dead      ! (OUTPUT) new fraction of oldest
                //                                       ! green leaf

                //*+  Purpose
                //*       Return the fractional death of oldest green leaf.


                double       l_leaf_no_today;
                double       l_node_no_dead_today;       //! total number of dead leaves today
                double       l_node_no_dead_yesterday;   //! total number of dead leaves yesterday
                double       l_total_leaf_no;            //! total number of leaves today



                l_node_no_dead_yesterday = sum_between_zb(zb(emerg), zb(now), i_node_no_dead);

                if (stage_is_between (emerg, crop_end, i_current_stage))
                    {
                    //! this approach won't work if the growing point gets killed
                    //! we will require an approach that integrates the app rate
                    //! function to create a dlfno vs tt curve.
                    //! this is quick and dirty to allow testing of green leaf
                    //! approach

                    l_leaf_no_today = sum_between_zb(zb(emerg), zb(now), i_leaf_no) + i_dlt_leaf_no;
                    l_node_no_dead_today = l_leaf_no_today - c_green_leaf_no * i_lodge_redn_green_leaf;
                    l_node_no_dead_today = l_bound(l_node_no_dead_today, 0.0);
                    }
                else if (on_day_of(crop_end, i_current_stage)) 
                    {
                    l_total_leaf_no = sum_between_zb(zb(emerg), zb(now), i_leaf_no);
                    l_node_no_dead_today = l_total_leaf_no;
                    }
                else
                    {
                    l_node_no_dead_today = 0.0;
                    }


                l_node_no_dead_today = bound (l_node_no_dead_today, l_node_no_dead_yesterday, (double)max_leaf);

                return (l_node_no_dead_today - l_node_no_dead_yesterday);

                }

 


            double sugar_leaf_area_sen_age0(double i_dlt_node_no_dead, double i_lai, double[] i_leaf_area_zb, double[] i_node_no_dead_zb, double i_plants, 
                                            double i_slai, double i_node_no_detached_ob, double c_leaf_no_at_emerg)
                {
                //*     ===========================================================
                //      subroutine sugar_leaf_area_sen_age0()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       G_dlt_node_no_dead    ! (INPUT)  fraction of oldest green leaf
                //      REAL       G_lai                 ! (INPUT)  live plant green lai
                //      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
                //      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
                //      REAL       G_node_no_detached    ! (INPUT)  number of detached leaves
                //      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence
                //      real       dlt_slai_age          ! (OUTPUT) new senesced lai from phasic devel.

                //*+  Purpose
                //*       Return the lai that would senesce on the current day due to ageing

                //*+  Mission Statement
                //*     Calculate the LAI seneced due to ageing


                double      l_dlt_leaf_area;         //! potential senesced leaf area from highest leaf no. senescing (mm^2)
                int         l_node_no_dead_ob;          //! current leaf number dying ()
                double      l_slai_age;              //! lai senesced by natural ageing
                double      l_dead_fr_highest_dleaf;



                    //! now calculate the leaf senescence
                    //! due to normal phenological (phasic) development

                    //! get highest leaf no. senescing today

        //c      leaf_no_dead = int (1.0
        //c     :                   + sum_between (emerg, now, g_leaf_no_dead))

                //! note that the first leaf record really contains
                //! 1+c_leaf_no_at_emerg leaves in it - not 1.

                l_node_no_dead_ob = (int)(1.0 + sum_between_zb(zb(emerg), zb(now), i_node_no_dead_zb) - i_node_no_detached_ob - c_leaf_no_at_emerg);
                l_node_no_dead_ob = Math.Max(l_node_no_dead_ob, 1);

                l_dead_fr_highest_dleaf = (1.0 + sum_between_zb(zb(emerg), zb(now), i_node_no_dead_zb) - i_node_no_detached_ob - c_leaf_no_at_emerg) % 1.0;

                    //! get area senesced from highest leaf no.

                l_dlt_leaf_area = (i_dlt_node_no_dead % 1.0) * i_leaf_area_zb[zb(l_node_no_dead_ob)];

                l_slai_age = (SumArray(i_leaf_area_zb, l_node_no_dead_ob - 1) + l_dead_fr_highest_dleaf * i_leaf_area_zb[zb(l_node_no_dead_ob)] + l_dlt_leaf_area) * smm2sm * i_plants;

                return bound(l_slai_age - i_slai, 0.0, i_lai);

                }



            double crop_leaf_area_sen_water1(double i_sen_rate_water, double i_lai, double i_swdef_photo, double i_plants, double i_min_tpla)
                {
                //!     ===========================================================
                //      subroutine crop_leaf_area_sen_water1()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL sen_rate_water       ! (INPUT)  slope in linear eqn relating soil wat
                //      REAL lai                  ! (INPUT)  live plant green lai
                //      REAL swdef_photo          ! (INPUT)
                //      REAL plants               ! (INPUT)
                //      REAL min_tpla             ! (INPUT)
                //      REAL dlt_slai_water       ! (OUTPUT) water stress senescense

                //!+  Purpose
                //!       Return the lai that would senesce on the current day due to water stress

                //!+  Mission Statement
                //!   Calculate today's leaf area senescence due to water stress.


                double      l_slai_water_fac;    //! drought stress factor (0-1)
                double      l_max_sen;
                double      l_min_lai;
                double      l_dlt_slai_water;

                //! drought stress factor
                l_slai_water_fac = i_sen_rate_water * (1.0 - i_swdef_photo);
                l_dlt_slai_water = i_lai * l_slai_water_fac;

                l_min_lai = i_min_tpla * i_plants * smm2sm;
                l_max_sen = l_bound(i_lai - l_min_lai, 0.0);
                
                l_dlt_slai_water = bound(l_dlt_slai_water, 0.0, l_max_sen);

                return l_dlt_slai_water;
                }



            double crop_leaf_area_sen_light1(double i_lai_sen_light, double i_sen_light_slope, double i_lai, double i_plants, double i_min_tpla)
                {
                //!     ===========================================================
                //      subroutine crop_leaf_area_sen_light1 ()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //       real lai_sen_light
                //       real sen_light_slope
                //       real lai
                //       real plants
                //       real min_tpla
                //       real dlt_slai_light        ! (OUTPUT) lai senesced by low light

                //!+  Purpose
                //!       Return the lai that would senesce on the current day due to shading

                //!+  Mission Statement
                //!   Calculate today's leaf area senescence due to shading.


                double      l_slai_light_fac;        //! light competition factor (0-1)
                double      l_max_sen;
                double      l_min_lai;

                double      l_dlt_slai_light;

                         //! calculate 0-1 factor for leaf senescence due to
                         //! competition for light.

                //!+!!!!!!!! this doesnt account for other growing crops
                //!+!!!!!!!! should be based on reduction of intercepted light and k*lai
                //         ! competition for light factor

                if (i_lai > i_lai_sen_light)
                    {                               
                    l_slai_light_fac = i_sen_light_slope * (i_lai - i_lai_sen_light);
                    }
                else
                    {
                    l_slai_light_fac = 0.0;
                    } 

                l_dlt_slai_light = i_lai * l_slai_light_fac;
                l_min_lai = i_min_tpla * i_plants * smm2sm;
                l_max_sen = l_bound (i_lai - l_min_lai, 0.0);
                l_dlt_slai_light = bound(l_dlt_slai_light, 0.0, l_max_sen);

                return  l_dlt_slai_light;
                }



            double crop_leaf_area_sen_frost1(double[] i_frost_temp, double[] i_frost_fraction, double i_lai, double i_mint, double i_plants, double i_min_tpla)
                {
                //!     ===========================================================
                //      subroutine crop_leaf_area_sen_frost1()
                //!     ===========================================================

                //!+  Sub-Program Arguments
                //      REAL    frost_temp(*)     ! (INPUT)
                //      REAL    frost_fraction(*) ! (INPUT)
                //      INTEGER num_frost_temp    ! (INPUT)
                //      REAL    lai               ! (INPUT)  live plant green lai
                //      REAL    mint              ! (INPUT)  minimum air temperature (oC)
                //      REAL    plants            ! (INPUT)
                //      REAL    min_tpla          ! (INPUT)
                //      real    dlt_slai_frost    ! (OUTPUT) lai frosted today

                //!+  Purpose
                //!       Return the lai that would senesce on the current day from low temperatures

                double      l_dlt_slai_low_temp;    //! lai senesced from low temps
                double      l_sen_fac_temp;         //! low temperature factor (0-1)
                double      l_min_lai;
                double      l_max_sen;
                bool        l_didInterpolate;


                    //! low temperature factor
                l_sen_fac_temp = MathUtility.LinearInterpReal(i_mint, i_frost_temp, i_frost_fraction, out l_didInterpolate);

                l_dlt_slai_low_temp = l_sen_fac_temp * i_lai;
                l_min_lai = i_min_tpla * i_plants * smm2sm;
                l_max_sen = l_bound (i_lai - l_min_lai, 0.0);

                return bound(l_dlt_slai_low_temp, 0.0, l_max_sen);

                }


        #endregion




        #region Biomass Senescence



            void sugar_dm_senescence(double c_dm_root_sen_frac, double c_leaf_cabbage_ratio, double c_cabbage_sheath_fr, 
                                        double[] i_dlt_dm_green, double i_dlt_lai, double i_dlt_slai, double[] i_dm_green, double[] i_dm_senesced, 
                                        double i_lai, double[] i_leaf_dm, double i_plants, double i_slai, double[] i_leaf_area, ref double[] o_dlt_dm_senesced)
                {

                //*     ===========================================================
                //      subroutine sugar_sen_bio (Option)
                //*     ===========================================================

                //*+  Purpose
                //*       Simulate plant senescence.

                //*+  Mission Statement
                //*     Get the plant senecence information



                //*     ===========================================================
                //      subroutine sugar_dm_senescence()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       C_dm_root_sen_frac    ! (INPUT)  fraction of root dry matter senescing each day (0-1)
                //      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt ()
                //      REAL       C_cabbage_sheath_fr   ! (INPUT)  fraction of cabbage that is leaf sheath (0-1)
                //      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
                //      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant lai
                //      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces from plant
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
                //      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
                //      REAL       G_lai                 ! (INPUT)  live plant green lai
                //      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       G_slai                ! (INPUT)  area of leaf that senesces from plant
                //      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
                //*
                //      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                //                                       ! from plant parts (g/m^2)

                //*+  Purpose
                //*       Derives seneseced plant dry matter (g/m^2)

                //*+  Mission Statement
                //*     Calculate seneced plant dry matter


                double       l_dm_senesced_leaf;       //! today's dm of senesced leaves (g/m^2)
                double       l_dm_senesced_leaf_plant; //! today's dm of senesced leaves (g/plant)
                double       l_lai_today;             //! today's green lai
                double       l_slai_today;            //! today's senesced lai
                double       l_leaf_no_senesced;      //! number of senesced leaves today
                int          l_leaf_no_senescing;     //! leaf number senescing today



                    //! first we zero all plant component deltas

                fill_real_array(ref o_dlt_dm_senesced, 0.0, max_part);

                l_lai_today = i_lai + i_dlt_lai;

                if (i_dlt_slai < l_lai_today) 
                    {
                    l_slai_today = i_slai + i_dlt_slai;
                    l_leaf_no_senesced = sugar_leaf_no_from_lai(i_leaf_area, i_plants, l_slai_today);
                    l_leaf_no_senescing = (int)(l_leaf_no_senesced + 1.0);
                    l_dm_senesced_leaf_plant = SumArray(i_leaf_dm, (int)l_leaf_no_senesced) + (l_leaf_no_senesced % 1.0) * i_leaf_dm[zb(l_leaf_no_senescing)];

                    l_dm_senesced_leaf = l_dm_senesced_leaf_plant * i_plants;
                    l_dm_senesced_leaf = l_bound(l_dm_senesced_leaf, i_dm_senesced[leaf]);

                    o_dlt_dm_senesced[leaf] = l_dm_senesced_leaf - i_dm_senesced[leaf];

                    //! Take related cabbage with the dying leaf

                    o_dlt_dm_senesced[cabbage] = MathUtility.Divide(o_dlt_dm_senesced[leaf], c_leaf_cabbage_ratio, 0.0) * c_cabbage_sheath_fr;

        //c         dlt_dm_senesced(cabbage) =
        //c     :         u_bound(dlt_dm_senesced(cabbage),
        //c     :         g_dm_green(cabbage)+g_dlt_dm_green(cabbage))
                    }
                else
                    {
                    o_dlt_dm_senesced[leaf] = i_dm_green[leaf] + i_dlt_dm_green[leaf];

                    o_dlt_dm_senesced[cabbage] = i_dm_green[cabbage] + i_dlt_dm_green[cabbage];
                    }

                o_dlt_dm_senesced[root] = i_dm_green[root] * c_dm_root_sen_frac;



                }



        double sugar_leaf_no_from_lai(double[] i_leaf_area, double i_plants, double i_lai)
            {

            //*     ===========================================================
            //      real function sugar_leaf_no_from_lai()
            //*     ===========================================================

            //*+  Sub-Program Arguments
            //      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
            //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
            //*
            //      real       lai                   ! (INPUT) lai of leaves

            //*+  Purpose
            //*       Derives leaf no from lai and leaf area


            double      l_leaf_area;             //! plant leaf area from lai (mm^2)
            int         l_leaf_no;               //! number of leaves containing leaf
                                            // leaf area (0-max_leaf)
            double      l_leaf_area_whole;       //! number of complete leaves ()
            double      l_leaf_area_part;        //! area from last leaf (mm^2)
            double      l_leaf_fract;            //! fraction of last leaf (0-1)


            l_leaf_area = MathUtility.Divide(i_lai, i_plants, 0.0) * sm2smm;
            l_leaf_no = get_cumulative_index_real(l_leaf_area, i_leaf_area);

            l_leaf_area_whole = SumArray(i_leaf_area,(l_leaf_no - 1));
            l_leaf_area_part = l_leaf_area - l_leaf_area_whole;
            l_leaf_fract = MathUtility.Divide(l_leaf_area_part, i_leaf_area[zb(l_leaf_no)], 0.0);
            return (double)(l_leaf_no - 1) + l_leaf_fract;

            }







        #endregion



        #region Root Length Senescence


           void cproc_root_length_senescence1(double c_specific_root_length, double[] i_dlayer, double i_dlt_root_dm_senesced, double[] i_root_length, double i_root_depth, 
                                                ref double[] o_dlt_root_length_senesced)
                {
                //*     ===========================================================
                //      subroutine sugar_sen_root_length (Option)
                //*     ===========================================================
                //*+  Mission Statement
                //*     Calculate senesced root length

                //! ====================================================================
                //       subroutine cproc_root_length_senescence1()
                //! ====================================================================
                //!+  Sub-Program Arguments
                //      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (m
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_dlt_root_dm_senesced ! (INPUT)  plant biomass senescence (g/m^2)
                //      REAL       G_root_length(*)       ! (INPUT)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL       G_dlt_root_length_senesced (*) ! (OUTPUT) root length lost from each layer (mm/mm^2)
                //      INTEGER    max_layer             ! (INPUT)  maximum layer number

                //!+  Purpose
                //!     Calculate root length senescence based upon changes in senesced root biomass and the specific root length.

                //!+  Notes
                //!   nih - I know there is a simpler way of doing this but if we make the
                //!         calculation of senescence rate more complex this aproach will
                //!         automatically handle it.

                double      l_senesced_length;           //! length of root to senesce (mm/m2)

                fill_real_array(ref o_dlt_root_length_senesced, 0.0, o_dlt_root_length_senesced.Length);

                l_senesced_length = i_dlt_root_dm_senesced/sm2smm * c_specific_root_length;

                crop_root_dist(i_dlayer, i_root_length, i_root_depth, ref o_dlt_root_length_senesced, l_senesced_length);

                }


        #endregion




        #region Nitrogen Retranslocation


            void sugar_N_retranslocate(double[] i_dm_green, double[] i_n_conc_min, double[] i_n_green, ref double[] o_dlt_N_retrans)
                {

                //*     ===========================================================
                //      subroutine sugar_N_retranslocate()
                //*     ===========================================================

                //*+  Sub-Program Arguments
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
                //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
                //      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from plant parts (g N/m^2)


                double[]     l_N_avail = new double[max_part];     //! N available for transfer to grain (g/m^2)


                sugar_N_retrans_avail(i_dm_green, i_n_conc_min, i_n_green, ref l_N_avail);  //! grain N potential (supply)

                //TODO: THIS LOOKS LIKE A BUG IN THE SUGAR CODE. They just zero it, they don't set dlt_N_retrans to N_avail.

                //! limit retranslocation to total available N
                fill_real_array(ref o_dlt_N_retrans, 0.0, max_part);


               //! just check that we got the maths right.
               for (int part=0; part<max_part; part++)
                   {
                   bound_check_real_var(Math.Abs(o_dlt_N_retrans[part]), 0.0, l_N_avail[part], "dlt_N_retrans(part)");
                   }

                }



            void sugar_N_retrans_avail(double[] i_dm_green, double[] i_N_conc_min, double[] i_N_green, ref double[] o_N_avail)
                {

                //*     ===========================================================
                //      subroutine sugar_N_retrans_avail()
                //*     ===========================================================


                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
                //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
                //      real       N_avail (*)           ! (OUTPUT) total N available for
                //                                       ! transfer to grain (g/m^2)

                //*+  Purpose
                //*     Calculate N available for transfer (g/m^2)
                //*     from each plant part.

                //*+  Mission Statement
                //*     Calculate N available for transfer

                //*+  Notes
                //*     NB. No translocation from roots.


                double      l_N_min;           //! nitrogen minimum level (g/m^2)


                //! now find the available N of each part.

                for(int part=0; part<max_part; part++)
                    {
                    l_N_min = i_N_conc_min[part] * i_dm_green[part];
                    o_N_avail[part] = l_bound(i_N_green[part] - l_N_min, 0.0);
                    }

                o_N_avail[sucrose] = 0.0;
                o_N_avail[root] = 0.0;

                }



        #endregion


        #region Nitrogen Supply


            void sugar_nit_supply(int i_option)
                {
                //*===========================================================
                // subroutine sugar_nit_supply (Option)
                //*===========================================================

                //*+  Purpose
                //*       Find nitrogen supply.

                double l_fixation_determinant;


                //! find potential N uptake (supply, available N)
                if (i_option == 1)
                    {
                    l_fixation_determinant = SumArray(g_dm_green, max_part) - g_dm_green[root];

                    cproc_n_supply2(dlayer, max_layer, 
                                    g_dlt_sw_dep, g_no3gsm, g_no3gsm_min, g_root_depth, sw_dep, ref g_no3gsm_mflow_avail, 
                                    g_sw_avail, g_sw_avail_pot, ref g_no3gsm_diffn_pot, 
                                    g_current_stage, crop.n_fix_rate, l_fixation_determinant, g_swdef_fixation, ref g_n_fix_pot);
                    }
                else if (i_option == 2) 
                    {
                    l_fixation_determinant = SumArray(g_dm_green, max_part) - g_dm_green[root];

                    cproc_n_supply4 (dlayer, bd, max_layer, 
                                    g_no3gsm, g_no3gsm_min, ref g_no3gsm_uptake_pot, 
                                    g_nh4gsm, g_nh4gsm_min, ref g_nh4gsm_uptake_pot, 
                                    g_root_depth, 
                                    1.0,           //!c%n_stress_start_stage
                                    kno3, no3ppm_min, 
                                    knh4, nh4ppm_min,
                                    total_n_uptake_max, 
                                    g_sw_avail_pot, g_sw_avail, 
                                    g_current_stage, crop.n_fix_rate, l_fixation_determinant, g_swdef_fixation, ref g_n_fix_pot);
                    }

                else
                    {
                    throw new Exception("Invalid template option");
                    }

                }





            void cproc_n_supply2 (double[] i_dlayer, int i_max_layer, 
                                double[] i_dlt_sw_dep, double[] i_no3gsm, double[] i_no3gsm_min, double i_root_depth, double[] i_sw_dep, ref double[] o_NO3gsm_mflow_avail, 
                                double[] i_sw_avail, double[] i_sw_avail_pot, ref double[] o_no3gsm_diffn_pot, 
                                double i_current_stage, double[] c_n_fix_rate, double i_fixation_determinant, double i_swdef_fixation, ref double o_n_fix_pot)
                {

                //!+  Sub-Program Arguments
                //      real g_dlayer(*)             ! (INPUT)
                //      integer max_layer            ! (INPUT)
                //      real g_dlt_sw_dep(*)         ! (INPUT)
                //      real g_NO3gsm(*)             ! (INPUT)
                //      real g_NO3gsm_min(*)         ! (INPUT)
                //      real g_root_depth            ! (INPUT)
                //      real g_sw_dep(*)             ! (INPUT)
                //      real g_NO3gsm_mflow_avail(*) ! (OUTPUT)
                //      real g_sw_avail(*)           ! (INPUT)
                //      real g_sw_avail_pot(*)       ! (INPUT)
                //      real g_NO3gsm_diffn_pot(*)   ! (OUTPUT)
                //      real G_current_stage         ! (INPUT)
                //      real C_n_fix_rate(*)         ! (INPUT)
                //      real fixation_determinant    ! (INPUT)
                //      real G_swdef_fixation        ! (INPUT)
                //      real g_N_fix_pot             ! (INPUT)  //sv-actually should be (OUTPUT)

                //!+  Purpose
                //!      Calculate nitrogen supplys from soil and fixation

                //!+  Mission Statement
                //!      Calculate nitrogen supplys from soil and fixation


                 crop_N_mass_flow1(i_max_layer, i_dlayer, i_dlt_sw_dep, i_no3gsm, i_no3gsm_min, i_root_depth, i_sw_dep, ref o_NO3gsm_mflow_avail);

                 crop_N_diffusion1(i_max_layer, i_dlayer, i_no3gsm, i_no3gsm_min, i_root_depth, i_sw_avail, i_sw_avail_pot, ref o_no3gsm_diffn_pot);

                          //! determine N from fixation

                 crop_N_fixation_pot1(i_current_stage, c_n_fix_rate, i_fixation_determinant, i_swdef_fixation, ref o_n_fix_pot);

                }



            void crop_N_mass_flow1(int i_num_layer, double[] i_dlayer, double[] i_dlt_sw_dep, double[] i_no3gsm, double[] i_no3gsm_min, double i_root_depth, double[] i_sw_dep, 
                                    ref double[] o_no3gsm_mflow_pot)
                {
                //!+  Sub-Program Arguments
                //      INTEGER num_layer        ! (INPUT)  number of layers in profile
                //      REAL    dlayer(*)         ! (INPUT)  thickness of soil layer I (mm)
                //      REAL    dlt_sw_dep(*)     ! (INPUT)  water uptake in each layer (mm water)
                //      REAL    no3gsm(*)         ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
                //      REAL    no3gsm_min(*)     ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
                //      REAL    root_depth        ! (INPUT)  depth of roots (mm)
                //      REAL    sw_dep(*)         ! (INPUT)  soil water content of layer L (mm)
                //      real NO3gsm_mflow_pot(*) ! (OUTPUT) potential plant NO3
                //                                              ! uptake (supply) g/m^2,
                //                                              ! by mass flow

                //!+  Purpose
                //!       Return potential nitrogen uptake (supply) by mass flow (water
                //!       uptake) (g/m^2)

                //!+  Mission Statement
                //!   Calculate crop nitrogen supply from mass flow, %8.


                int     l_deepest_layer_ob;    //! deepest layer in which the roots are growing
                double  l_NO3_conc;            //! nitrogen concentration (g/m^2/mm)
                double  l_NO3gsm_mflow;        //! potential nitrogen uptake (g/m^2)


                fill_real_array (ref o_no3gsm_mflow_pot, 0.0, i_num_layer);
                         //! only take the layers in which roots occur
                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                    {
                            //! get  NO3 concentration
                         l_NO3_conc = MathUtility.Divide(i_no3gsm[layer], i_sw_dep[layer], 0.0);
                            //! get potential uptake by mass flow
                         l_NO3gsm_mflow = l_NO3_conc * (-i_dlt_sw_dep[layer]);
                         o_no3gsm_mflow_pot[layer] = u_bound (l_NO3gsm_mflow, i_no3gsm[layer] - i_no3gsm_min[layer]);
                    }


                }






        void crop_N_diffusion1 (int i_num_layer, double[] i_dlayer, double[] i_no3gsm, double[] i_no3gsm_min, double i_root_depth, double[] i_sw_avail, double[] i_sw_avail_pot, 
                                ref double[] o_no3gsm_diffn_pot)   
            {


            //use convertmodule       ! ha2sm, kg2gm


            //!+  Sub-Program Arguments
            //      INTEGER num_layer           ! (INPUT)  number of layers in profile
            //      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
            //      REAL    no3gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^
            //      REAL    no3gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^
            //      REAL    root_depth          ! (INPUT)  depth of roots (mm)
            //      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
            //      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (m
            //      real    NO3gsm_diffn_pot(*) ! (OUTPUT) potential plant NO3
            //                                              ! uptake (supply) g/m^2,
            //                                              !  by diffusion

            //!+  Purpose
            //!       Return potential nitrogen uptake (supply) by diffusion
            //!       for a plant (g/m^2)

            //!+  Mission Statement
            //!   Calculate crop nitrogen supply from active uptake, %8.

            int     l_deepest_layer_ob;       //! deepest layer in which the roots are growing
            double  l_NO3gsm_diffn;        //! potential nitrogen uptake (g/m^2)
            double  l_sw_avail_fract;      //! fraction of extractable soil water ()


                //! only take the layers in which roots occur
            fill_real_array(ref o_no3gsm_diffn_pot, 0.0, i_num_layer);

            l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
            for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                {
                l_sw_avail_fract = MathUtility.Divide(i_sw_avail[layer], i_sw_avail_pot[layer], 0.0);
                l_sw_avail_fract = bound(l_sw_avail_fract, 0.0, 1.0);
                //! get extractable NO3
                //! restricts NO3 available for diffusion to NO3 in plant
                //! available water range
                l_NO3gsm_diffn = l_sw_avail_fract * i_no3gsm[layer];
                o_no3gsm_diffn_pot[layer] = u_bound(l_NO3gsm_diffn, i_no3gsm[layer] - i_no3gsm_min[layer]);
                }

          }





        void crop_N_fixation_pot1(double i_current_stage, double[] c_n_fix_rate, double i_fixation_determinant, double i_swdef_fixation, ref double o_n_fix_pot)
            {

            //!+  Sub-Program Arguments
            //      REAL       G_Current_stage       ! (INPUT) Current stage
            //      REAL       C_n_fix_rate(*)       ! (INPUT)  potential rate of N fixation (
            //      REAL       fixation_determinant  ! (INPUT)
            //      REAL       G_swdef_fixation      ! (INPUT)
            //      real       N_fix_pot                   ! (OUTPUT) N fixation potential (g/

            //!+  Purpose
            //!          Calculate the quantity of atmospheric nitrogen fixed
            //!          per unit standing crop biomass (fixation_determinant) and
            //!          limited by the soil water deficit factor for fixation.

            //!+  Mission Statement
            //!   Calculate crop nitrogen supply from fixation, %5.


            int l_current_phase;                //! guess


            l_current_phase = (int)i_current_stage;

            o_n_fix_pot = c_n_fix_rate[zb(l_current_phase)] * i_fixation_determinant * i_swdef_fixation;

            }





    
       void cproc_n_supply4 (double[] i_dlayer, double[] i_bd, int i_max_layer, 
                            double[] i_no3gsm, double[] i_no3gsm_min, ref double[] o_no3gsm_uptake_pot, 
                            double[] i_nh4gsm, double[] i_nh4gsm_min, ref double[] o_nh4gsm_uptake_pot, 
                            double i_root_depth, double c_n_stress_start_stage, 
                            double c_kno3, double c_no3ppm_min, 
                            double c_knh4, double c_nh4ppm_min,
                            double c_total_n_uptake_max, double[] i_sw_avail_pot, double[] i_sw_avail,
                            double i_current_stage, double[] c_n_fix_rate, double i_fixation_determinant, double i_swdef_fixation, ref double o_n_fix_pot)
            {

            //!+  Sub-Program Arguments
            //    real g_dlayer(*)             ! (INPUT)
            //    integer max_layer            ! (INPUT)
            //    real g_bd(*)
            //    real g_NO3gsm(*)             ! (INPUT)
            //    real g_NO3gsm_min(*)         ! (INPUT)
            //    real g_no3gsm_uptake_pot(*)
            //    real g_NH4gsm(*)             ! (INPUT)
            //    real g_NH4gsm_min(*)         ! (INPUT)
            //    real g_nH4gsm_uptake_pot(*)
            //    real g_root_depth            ! (INPUT)
            //    real c_n_stress_start_stage
            //    real c_kno3
            //    real c_no3ppm_min
            //    real c_knh4
            //    real c_nh4ppm_min
            //    real c_total_n_uptake_max
            //    real g_sw_avail(*)           ! (INPUT)
            //    real g_sw_avail_pot(*)       ! (INPUT)
            //    real G_current_stage         ! (INPUT)
            //    real C_n_fix_rate(*)         ! (INPUT)
            //    real fixation_determinant    ! (INPUT)
            //    real G_swdef_fixation        ! (INPUT)
            //    real g_N_fix_pot             ! (INPUT)


            double  l_no3ppm;
            double  l_nh4ppm;
            int     l_deepest_layer_ob;
            double  l_swfac;
            double  l_total_n_uptake_pot;
            double  l_scalef;

            l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

            if (i_current_stage >= c_n_stress_start_stage)
                {

                for (int layer=0; layer < l_deepest_layer_ob; layer++)
                    {

                    l_no3ppm = i_no3gsm[layer] * MathUtility.Divide(1000.0,i_bd[layer]*i_dlayer[layer], 0.0);
                    l_nh4ppm = i_nh4gsm[layer] * MathUtility.Divide (1000.0, i_bd[layer]*i_dlayer[layer] , 0.0);

                    l_swfac = MathUtility.Divide(i_sw_avail[layer], i_sw_avail_pot[layer], 0.0);
                    l_swfac = bound(l_swfac,0.0,1.0);

                    o_no3gsm_uptake_pot[layer] = i_no3gsm[layer] * c_kno3 * (l_no3ppm - c_no3ppm_min) * l_swfac;
                    o_no3gsm_uptake_pot[layer] = u_bound(o_no3gsm_uptake_pot[layer],i_no3gsm[layer]-i_no3gsm_min[layer]);
                    o_no3gsm_uptake_pot[layer] = l_bound(o_no3gsm_uptake_pot[layer], 0.0);

                    o_nh4gsm_uptake_pot[layer] = i_nh4gsm[layer] * c_knh4 * (l_nh4ppm - c_nh4ppm_min) * l_swfac;
                    o_nh4gsm_uptake_pot[layer] = u_bound(o_nh4gsm_uptake_pot[layer],i_nh4gsm[layer]-i_nh4gsm_min[layer]);
                    o_nh4gsm_uptake_pot[layer] = l_bound(o_nh4gsm_uptake_pot[layer], 0.0);
                    }
                }
            else
                {
                //! No N stress whilst N is present in soil
                //! crop has access to all that it wants early on
                //! to avoid effects of small differences in N supply
                //! having affect during the most sensitive part
                //! of canopy development.

                for (int layer=0; layer < l_deepest_layer_ob; layer++)
                    {
                    l_no3ppm = i_no3gsm[layer] * MathUtility.Divide(1000.0, i_bd[layer]*i_dlayer[layer], 0.0);
                    l_nh4ppm = i_nh4gsm[layer] * MathUtility.Divide(1000.0, i_bd[layer]*i_dlayer[layer], 0.0);

                    if ((c_kno3 > 0) && (l_no3ppm > c_no3ppm_min))
                        o_no3gsm_uptake_pot[layer] = l_bound(i_no3gsm[layer]-i_no3gsm_min[layer],0.0);
                    else
                        o_no3gsm_uptake_pot[layer] = 0.0;


                    if ((c_knh4 > 0) && (l_nh4ppm > c_nh4ppm_min))
                        o_nh4gsm_uptake_pot[layer] = l_bound(i_nh4gsm[layer]-i_nh4gsm_min[layer],0.0);
                    else
                        o_nh4gsm_uptake_pot[layer] = 0.0;
   
                    }
                }


            l_total_n_uptake_pot = SumArray(o_no3gsm_uptake_pot, l_deepest_layer_ob) + SumArray(o_nh4gsm_uptake_pot, l_deepest_layer_ob);
            l_scalef = MathUtility.Divide(c_total_n_uptake_max, l_total_n_uptake_pot,0.0);
            l_scalef = bound(l_scalef,0.0,1.0);
            for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                {
                o_no3gsm_uptake_pot[layer] = l_scalef * o_no3gsm_uptake_pot[layer];
                o_nh4gsm_uptake_pot[layer] = l_scalef * o_nh4gsm_uptake_pot[layer];
                }

            //! determine N from fixation
            crop_N_fixation_pot1(i_current_stage, c_n_fix_rate, i_fixation_determinant, i_swdef_fixation, ref o_n_fix_pot);

           }





        #endregion


        #region Nitrogen Initalisation



      void sugar_N_init(double c_N_cabbage_init_conc, double c_N_leaf_init_conc, double c_N_root_init_conc, double c_N_sstem_init_conc, double i_current_stage, 
                            double[] i_days_tot, double[] i_dm_green, ref double[] o_N_green)

          {
            //*+  Sub-Program Arguments
            //      REAL       C_n_cabbage_init_conc ! (INPUT)     "   cabbage    "
            //      REAL       C_n_leaf_init_conc    ! (INPUT)  initial leaf N concentration (
            //      REAL       C_n_root_init_conc    ! (INPUT)  initial root N concentration (
            //      REAL       C_n_sstem_init_conc   ! (INPUT)  initial stem N concentration (
            //      REAL       G_current_stage       ! (INPUT)  current phenological stage
            //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
            //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
            //      real       N_green(*)            ! plant nitrogen (g/m^2)   //sv- (OUTPUT) I am guessing


            //*+  Mission Statement
            //*     Initialise plant nitrogen



            if (on_day_of(emerg, i_current_stage)) 
                {

                if (o_N_green[root] == 0.0) 
                    {
                    //! There is no root system currently operating from a previous crop
                    o_N_green[root] = c_N_root_init_conc * i_dm_green[root];
                    }
                else
                    {
                    //! There IS a root system currently operating from a previous crop
                    }

                o_N_green[sstem] = c_N_sstem_init_conc * i_dm_green[sstem];
                o_N_green[leaf] = c_N_leaf_init_conc * i_dm_green[leaf];
                o_N_green[cabbage] = c_N_cabbage_init_conc * i_dm_green[cabbage];
                o_N_green[sucrose] = 0.0;

                }


          }




       #endregion


        #region Nitrogen Uptake



            void sugar_nit_uptake (int i_option)
                {

                //*+  Sub-Program Arguments
                //      integer    Option                ! (INPUT) option number


                //*+  Mission Statement
                //*     Get the nitrogen uptake information

                //TODO: Uncomment Below to let SWIM do Nitrogen Uptake. Also need to uncomment crop_get_ext_uptakes() subroutine below.
                //if (uptake_source == "apsim") 
                //    {
                //        //! NIH - note that I use a -ve conversion factor FOR NOW to make it a delta.
                //        crop_get_ext_uptakes(
                //                   uptake_source   //! uptake flag
                //                  ,crop_type       //! crop type
                //                  ,"no3"             //! uptake name
                //                  ,(-kg2gm/ha2sm)      //! unit conversion factor
                //                  ,0.0               //! uptake lbound
                //                  ,100.0             //! uptake ubound
                //                  ,g%dlt_no3gsm      //! uptake array
                //                  ,max_layer         //! array dim
                //                  );
                //    }
                //else if (i_option == 1) 
                if (i_option == 1)
                    {
                    cproc_N_uptake1(NO3_diffn_const, 
                                        dlayer, max_layer, 
                                        g_no3gsm_diffn_pot, g_no3gsm_mflow_avail, 
                                        g_n_fix_pot, n_supply_preference, 
                                        g_n_demand, g_n_demand,   //!sugar does not have n_max
                                        max_part, g_root_depth, 
                                        ref g_dlt_no3gsm);
                    }
                else if (i_option == 2) 
                    {
                    cproc_n_uptake3(dlayer, max_layer, 
                                        g_no3gsm_uptake_pot, g_nh4gsm_uptake_pot, 
                                        g_n_fix_pot, n_supply_preference, 
                                        g_n_demand, g_n_demand, 
                                        max_part, g_root_depth, 
                                        ref g_dlt_no3gsm, ref g_dlt_nh4gsm);
                    }
                else
                    {
                    throw new Exception("Invalid template option");
                    }

                }






            //void crop_get_ext_uptakes (string i_uptake_source, string i_crop_type , string i_uptake_type, double i_unit_conversion_factor, 
            //                            double i_uptake_lbound, double i_uptake_ubound, ref double[] o_uptake_array, int i_max_layer)
            //    {


            //    //!+  Sub-Program Arguments
            //    //      character uptake_source*(*)   !(INPUT) uptake flag
            //    //      character crop_type*(*)       !(INPUT) crop type name
            //    //      character uptake_type*(*)     !(INPUT) uptake name
            //    //      real      unit_conversion_factor!(INPUT) unit conversion factor
            //    //      real      uptake_lbound       !(INPUT) uptake lower limit
            //    //      real      uptake_ubound       !(INPUT) uptake upper limit
            //    //      real      uptake_array(*)     !(OUTPUT) crop uptake array
            //    //      integer   max_layer           !(INPUT) max layer number

            //    //!+  Purpose
            //    //!      Ask swim for uptakes of water or solute

            //    //!+  Mission Statement
            //    //!   Get the soil uptake for %3 from another module

            //    //!+  Notes
            //    //!      Bounds should probably be passed in when crops decide what
            //    //!      these should be (ie when ini files have limits for uptake
            //    //!      in them)



            //    int       layer;                      //! layer counter
            //    int       num_uptakes;                //! num uptake vals
            //    string    uptake_name;                //! Uptake variable name



            //    if ((i_uptake_source == "apsim")  && (i_crop_type != "")) 
            //        {
            //         //! NB - if crop type is blank then swim will know nothing
            //         //! about this crop (eg if not initialised yet)

            //        uptake_name = "uptake_" + i_uptake_type + "_" + i_crop_type;


            //         call get_real_array (unknown_module          &
            //                       ,uptake_name          &
            //                       ,i_max_layer          &
            //                       ,'()")]          &
            //                       ,uptake_array          &
            //                       ,num_uptakes          &
            //                       ,i_uptake_lbound          &
            //                       ,i_uptake_ubound)

            //         do 100 layer = 1, num_uptakes
            //            uptake_array(layer) = uptake_array(layer) * i_unit_conversion_factor;
            //  100    continue
            //        }

            //}




            void cproc_N_uptake1(double c_no3_diffn_const, double[] i_dlayer, int i_max_layer, double[] i_no3gsm_diffn_pot, double[] i_no3gsm_mflow_avail, 
                                double i_n_fix_pot, string c_n_supply_preference, double[] i_n_demand, double[] i_n_max, int i_max_part, double i_root_depth, 
                                ref double[] o_dlt_NO3gsm)

                {

                //!+  Sub-Program Arguments
                //      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      INTEGER    max_layer             ! (INPUT)  max number of soil layers
                //      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
                //      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
                //      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
                //      CHARACTER  c_n_supply_preference*(*) !(INPUT)
                //      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
                //      INTEGER    max_part              ! (INPUT)  number of plant parts
                //      REAL       G_n_max(*)            ! (INPUT)  maximum plant nitrogen demand
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
                //                                       ! from NO3 in each layer (g/m^2)

                //!+  Purpose
                //!       Return actual plant nitrogen uptake from each soil layer.
                //!       N uptake is from nitrate only and comes via mass flow and active (diffusion) uptake.

                //!+  Mission Statement
                //!   Calculate crop Nitrogen Uptake


                const int   l_crop_max_layer = 100;        //sv- taken from, FortranInfrastructure\ConstantsModule.f90

                int           l_deepest_layer_ob;         //! deepest layer in which the roots are growing
                double        l_NO3gsm_diffn;          //! actual N available (supply) for plant (g/m^2) by diffusion
                double        l_NO3gsm_mflow;          //! actual N available (supply) for plant (g/m^2) by mass flow
                double[]      l_NO3gsm_diffn_avail = new double[l_crop_max_layer];   //! potential NO3 (supply) from soil (g/m^2), by diffusion
                double        l_NO3gsm_diffn_supply;   //! total potential N uptake (supply) for plant (g/m^2) by diffusion
                double        l_NO3gsm_mflow_supply;   //! total potential N uptake (supply) for plant (g/m^2) by mass flow
                double        l_diffn_fract;           //! fraction of nitrogen to use (0-1) for diffusion
                double        l_mflow_fract;           //! fraction of nitrogen to use (0-1) for mass flow
                double        l_N_demand_tot;             //! total nitrogen demand (g/m^2)
                double        l_NO3gsm_uptake;         //! plant NO3 uptake from layer (g/m^2)
                double        l_N_max_tot;                 //! potential N uptake per plant (g/m^2)



                //! get potential N uptake (supply) from the root profile.
                //! get totals for diffusion and mass flow.

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);
                for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                    {
                    l_NO3gsm_diffn_avail[layer] = i_no3gsm_diffn_pot[layer] - i_no3gsm_mflow_avail[layer];
                    l_NO3gsm_diffn_avail[layer] = l_bound(l_NO3gsm_diffn_avail[layer], 0.0);
                    }

                l_NO3gsm_mflow_supply = SumArray(i_no3gsm_mflow_avail, l_deepest_layer_ob);
                l_NO3gsm_diffn_supply = SumArray(l_NO3gsm_diffn_avail, l_deepest_layer_ob);

                    //! get actual total nitrogen uptake for diffusion and mass flow.
                    //! If demand is not satisfied by mass flow, then use diffusion.
                    //! N uptake above N critical can only happen via mass flow.

                l_N_demand_tot = SumArray(i_n_demand, i_max_part);  
                l_N_max_tot    = SumArray(i_n_max, i_max_part);  //sv- n_max is the same as n_demand, see the value passed in as a parameter

                if (l_NO3gsm_mflow_supply >= l_N_demand_tot)
                    {
                    l_NO3gsm_mflow = l_NO3gsm_mflow_supply;
                    l_NO3gsm_mflow = u_bound(l_NO3gsm_mflow, l_N_max_tot);
                    l_NO3gsm_diffn = 0.0;
                    }

                else
                    {
                    l_NO3gsm_mflow = l_NO3gsm_mflow_supply;

                    if (c_n_supply_preference.ToLower() == "active") 
                        {
                        l_NO3gsm_diffn = bound (l_N_demand_tot - l_NO3gsm_mflow, 0.0, l_NO3gsm_diffn_supply);
                        }
                    else if (c_n_supply_preference.ToLower() == "fixation") 
                        {
                        l_NO3gsm_diffn = bound (l_N_demand_tot - l_NO3gsm_mflow - i_n_fix_pot, 0.0, l_NO3gsm_diffn_supply);
                        }
                    else
                        {
                        throw new Exception("bad n supply preference");
                        }

                    l_NO3gsm_diffn = MathUtility.Divide(l_NO3gsm_diffn, c_no3_diffn_const, 0.0);
                    }



                //! get actual change in N contents

                fill_real_array(ref o_dlt_NO3gsm, 0.0, i_max_layer);

                for (int layer = 0; layer < l_deepest_layer_ob; layer++)
                    {

                        //! allocate nitrate
                        //! Find proportion of nitrate uptake to be taken from layer
                        //! by diffusion and mass flow

                    l_mflow_fract = MathUtility.Divide(i_no3gsm_mflow_avail[layer], l_NO3gsm_mflow_supply, 0.0);

                    l_diffn_fract = MathUtility.Divide(l_NO3gsm_diffn_avail[layer], l_NO3gsm_diffn_supply, 0.0);

                        //! now find how much nitrate the plant removes from
                        //! the layer by both processes

                    l_NO3gsm_uptake = (l_NO3gsm_mflow * l_mflow_fract) + (l_NO3gsm_diffn * l_diffn_fract);
                    o_dlt_NO3gsm[layer] = - l_NO3gsm_uptake;
                    }

                }




            void cproc_n_uptake3(double[] i_dlayer, int i_max_layer, double[] i_no3gsm_uptake_pot, double[] i_nh4gsm_uptake_pot, double i_n_fix_pot, string c_n_supply_preference, 
                            double[] i_soil_n_demand, double[] i_n_max, int i_max_part, double i_root_depth, ref double[] o_dlt_no3gsm, ref double[] o_dlt_nh4gsm)
                {


                //real      g_dlayer(*)
                //integer   max_layer
                //real      g_no3gsm_uptake_pot(*)
                //real      g_nh4gsm_uptake_pot(*)
                //real      g_n_fix_pot
                //character c_n_supply_preference*(*)
                //real      g_soil_n_demand(*)
                //real      g_n_max(*)
                //integer   max_part
                //real      g_root_depth
                //real      dlt_no3gsm(*)
                //real      dlt_nh4gsm(*)


                int       l_deepest_layer_ob;                    //! deepest layer in which the roots are growing
                double    l_N_demand;                         //! total nitrogen demand (g/m^2)
                double    l_NO3gsm_uptake;                    //! plant NO3 uptake from layer (g/m^2)
                double    l_NH4gsm_uptake;                    //! plant NO3 uptake from layer (g/m^2)
                double    l_N_max;                            //! potential N uptake per plant (g/m^2)
                double    l_Ngsm_supply;
                double    l_scalef;

         

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

                l_Ngsm_supply = SumArray(i_no3gsm_uptake_pot, l_deepest_layer_ob) + SumArray(i_nh4gsm_uptake_pot, l_deepest_layer_ob);

                l_N_demand = SumArray(i_soil_n_demand, i_max_part);

                if (c_n_supply_preference == "fixation") 
                    {
                    l_N_demand = l_bound (l_N_demand - i_n_fix_pot, 0.0);
                    }

                // !get actual change in N contents
                fill_real_array(ref o_dlt_no3gsm, 0.0, i_max_layer);
                fill_real_array(ref o_dlt_nh4gsm, 0.0, i_max_layer);

                if (l_N_demand > l_Ngsm_supply)
                    {
                    l_scalef = 0.99999;     // !avoid taking it all up as it can cause rounding errors to takeno3 below zero.
                    }
                else
                    {
                    l_scalef = MathUtility.Divide(l_N_demand,l_Ngsm_supply,0.0);
                    }

                for (int layer = 1; layer < l_deepest_layer_ob; layer++)
                    {

                        //! allocate nitrate
                    l_NO3gsm_uptake = i_no3gsm_uptake_pot[layer] * l_scalef;
                    o_dlt_no3gsm[layer] = - l_NO3gsm_uptake;

                        //! allocate ammonium
                    l_NH4gsm_uptake = i_nh4gsm_uptake_pot[layer] * l_scalef;
                    o_dlt_nh4gsm[layer] = - l_NH4gsm_uptake;
                    }


                }


      #endregion



        #region Nitrogen Partitioning


            void sugar_N_partition(double[] i_dlayer, double[] i_dlt_NO3gsm, double[] i_dlt_NH4gsm, double[] i_N_demand, double i_root_depth, ref double[] o_dlt_N_green)

                {

                //*+  Sub-Program Arguments
                //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
                //      REAL       G_dlt_no3gsm(*)       ! (INPUT)  actual NO3 uptake from soil (g
                //      REAL       G_dlt_nh4gsm(*)       ! (INPUT)  actual NO3 uptake from soil (g
                //      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                //                                       ! into each plant part (g/m^2)

                //*+  Purpose
                //*       Return actual plant nitrogen uptake to each plant part and from
                //*       each soil layer.

                //*+  Mission Statement
                //*     Calculate nitrogen uptake to each plant part from soil

                //*+  Changes
                //*       130396 nih added fix to stop N above critical conc evaporating


                int         l_deepest_layer_ob;         //! deepest layer in which the roots are growing
                double      l_plant_part_fract;      //! fraction of nitrogen to use (0-1) for plant part                
                double      l_N_demand;              //! total nitrogen demand (g/m^2)
                double      l_N_uptake_sum;          //! total plant N uptake (g/m^2)


                fill_real_array(ref o_dlt_N_green, 0.0, max_part);
                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

                        //! find proportion of uptake to be
                        //! distributed to each plant part and distribute it.
                l_N_uptake_sum = - SumArray(i_dlt_NO3gsm, l_deepest_layer_ob) - SumArray(i_dlt_NH4gsm, l_deepest_layer_ob);
                l_N_demand = SumArray(i_N_demand, max_part);

                //! Partition N, according to relative demand, to each plant
                //! part but do not allow supply to exceed demand.  Any excess
                //! supply is to go into cane. - NIH 13/3/96

                for (int part = 0; part < max_part; part++)  //! plant part number
                    {
                    l_plant_part_fract = MathUtility.Divide(i_N_demand[part], l_N_demand, 0.0);
                    o_dlt_N_green[part] = Math.Min(l_N_uptake_sum, l_N_demand) * l_plant_part_fract;
                    }

                if (l_N_uptake_sum > l_N_demand) 
                    {
                    o_dlt_N_green[sstem] = o_dlt_N_green[sstem] + (l_N_uptake_sum - l_N_demand);
                    }

                }

        #endregion



        #region Water Content of Cane


       void sugar_water_content(double[] c_cane_dmf_tt, double[] c_cane_dmf_min, double[] c_cane_dmf_max, int c_num_cane_dmf, double c_cane_dmf_rate, 
           double i_swdef_stalk, double i_nfact_stalk, double i_temp_stress_stalk, double[] i_dlt_dm_green, double[] i_dm_green, double[] i_dlt_plant_wc, 
           ref double[] o_plant_wc, double[] i_tt_tot)

            {

            //*+  Sub-Program Arguments
            //      REAL       C_cane_dmf_tt(*)      ! (INPUT)
            //      REAL       C_cane_dmf_min(*)     ! (INPUT)
            //      REAL       C_cane_dmf_max(*)     ! (INPUT)
            //      INTEGER    C_num_cane_dmf        ! (INPUT)
            //      REAL       C_cane_dmf_rate       ! (INPUT)
            //      REAL       G_swdef_stalk         ! (INPUT)
            //      REAL       G_nfact_stalk         ! (INPUT)
            //      REAL       G_temp_stress_stalk   ! (INPUT)
            //      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
            //      REAL       G_dm_green(*)         ! (INPUT)
            //      REAL       G_dlt_plant_wc(*)     ! (INPUT)
            //      REAL       G_plant_wc(*)         ! (OUTPUT)
            //      REAL       G_tt_tot(*)           ! (INPUT)


            //*+  Mission Statement
            //*     Calculate plant water content

            //*+  Notes
            //*   NIH - Eventually this routine will need to be broken down into subroutines.



            double  l_tt;                  //! thermal time (deg. day)
            double  l_cane_dmf_max;        //! max dm fraction in cane(sstem+sucrose)
            double  l_cane_dmf_min;        //! min dm fraction in  cane(sstem+sucrose)
            double  l_cane_dmf;
            double  l_stress_factor_min;
            double  l_sucrose_fraction;
            bool    l_didInterpolate; 



            fill_real_array(ref i_dlt_plant_wc, 0.0, max_part);

            l_tt = sum_between_zb(zb(begcane), zb(now), i_tt_tot);

            l_cane_dmf_max = MathUtility.LinearInterpReal(l_tt, c_cane_dmf_tt, c_cane_dmf_max, out l_didInterpolate);

            l_cane_dmf_min = MathUtility.LinearInterpReal(l_tt, c_cane_dmf_tt, c_cane_dmf_min, out l_didInterpolate);

            l_stress_factor_min = min(i_swdef_stalk, i_nfact_stalk, i_temp_stress_stalk);

            l_cane_dmf = l_cane_dmf_max - l_stress_factor_min * (l_cane_dmf_max-l_cane_dmf_min);

            l_sucrose_fraction = MathUtility.Divide(i_dlt_dm_green[sucrose], i_dlt_dm_green[sstem] + i_dlt_dm_green[sucrose] ,0.0);

            i_dlt_plant_wc[sstem] = MathUtility.Divide(i_dlt_dm_green[sstem], l_cane_dmf, 0.0) * (1.0 - l_sucrose_fraction);

            //! Approach above is unstable - assume DMF is fixed
            l_cane_dmf = 0.3;
            i_dlt_plant_wc[sstem] = (i_dlt_dm_green[sstem] + i_dlt_dm_green[sucrose]) * (1.0 - l_cane_dmf)/l_cane_dmf;

            }


        #endregion



        #region Plant Death



            void sugar_plant_death()
                {

                //*+  Purpose
                //*      Determine plant death in crop


                sugar_failure_germination(crop.days_germ_limit, g_current_stage, g_days_tot, g_plants, ref g_dlt_plants_failure_germ);

                sugar_failure_emergence(crop.tt_emerg_limit, g_current_stage, g_plants, g_tt_tot, ref g_dlt_plants_failure_emergence);

                sugar_failure_leaf_sen(g_current_stage, g_lai, g_plants, ref g_dlt_plants_failure_leaf_sen);

                sugar_death_drought(crop.leaf_no_crit, crop.swdf_photo_limit, crop.swdf_photo_rate, g_cswd_photo, g_leaf_no_zb, g_plants, g_swdef_photo, ref g_dlt_plants_death_drought);

                sugar_death_lodging(g_lodge_flag, g_swdef_photo, g_oxdef_photo, crop.stress_lodge, crop.death_fr_lodge, crop.num_stress_lodge, g_plants, ref g_dlt_plants_death_lodging);


                //sugar_death_actual()
                //*+  Purpose
                //*      Determine actual plant death.
                g_dlt_plants = min(g_dlt_plants_failure_germ, g_dlt_plants_failure_emergence, g_dlt_plants_failure_leaf_sen, g_dlt_plants_death_drought, g_dlt_plants_death_lodging);



                if ( mu.reals_are_equal(g_dlt_plants + g_plants, 0.0) || ((g_dlt_plants + g_plants) < 0.0) )   //sv- I added the less than 0 test, because this is an error waiting to happen.
                    {

                    sugar_kill_crop(ref g_crop_status, g_dm_dead , g_dm_green , g_dm_senesced);

                    }

                }




            void sugar_failure_germination(double c_days_germ_limit, double i_current_stage, double[] i_days_tot, double i_plants, ref double o_dlt_plants)
                {

                //*+  Sub-Program Arguments
                //      REAL       C_days_germ_limit     ! (INPUT)  maximum days allowed after sowing for germination to take place (days)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      real       dlt_plants            ! (OUTPUT) change in plant number


                //*+  Mission Statement
                //*     Crop death due to lack of germination


                if (stage_is_between(sowing, sprouting, i_current_stage) && (sum_between_zb(zb(sowing), zb(now), i_days_tot) >= c_days_germ_limit)) 
                    {
                     o_dlt_plants = - i_plants;
                     Console.WriteLine(" crop failure because of lack of" + "/n" + "         germination within" + c_days_germ_limit + " days of sowing");
                    }

                  else
                    {
                    o_dlt_plants = 0.0;
                    }
      

                }


            void sugar_failure_emergence(double c_tt_emerg_limit, double i_current_stage, double i_plants, double[] i_tt_tot, ref double o_dlt_plants)
                {

                //*+  Sub-Program Arguments
                //      REAL       C_tt_emerg_limit      ! (INPUT)  maximum degree days allowed for emergence to take place (deg day)
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
                //      real       dlt_plants            ! (OUTPUT) change in plant number


                //*+  Mission Statement
                //*     Crop death due to lack of emergence


                if (stage_is_between(sprouting, emerg, i_current_stage) && (sum_between_zb(zb(sprouting), zb(now), i_tt_tot) > c_tt_emerg_limit)) 
                    {
                     o_dlt_plants = - i_plants;
                    Console.WriteLine(" failed emergence due to deep planting");
                    }
                  else
                    {
                     o_dlt_plants = 0.0;
                    }
   
                }



          void sugar_failure_leaf_sen(double i_current_stage, double i_lai, double i_plants, ref double o_dlt_plants)
                {

                //*+  Sub-Program Arguments
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       G_lai                 ! (INPUT)  live plant green lai
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      real       dlt_plants            ! (OUTPUT) change in plant number

                //*+  Purpose
                //*      Determine plant death from all leaf area senescing.


                //if ((i_lai <= 0.0) && (stage_is_between(emerg, crop_end, i_current_stage)))
                if ( (mu.reals_are_equal(i_lai, 0.0) || (i_lai < 0.0)) && stage_is_between(emerg, crop_end, i_current_stage) ) //sv- I changed this because what if i_lai is 0.0000001, it is still not equal to 0.0
                    {
                    o_dlt_plants = - i_plants;
                    i_lai = 0.0;

                    Console.WriteLine(" crop failure because of total leaf senescence.");
                    }
                else
                    {
                    o_dlt_plants = 0.0;
                    }

                }



            void sugar_death_drought(double c_leaf_no_crit, double c_swdf_photo_limit, double c_swdf_photo_rate, double[] i_cswd_photo, 
                                    double[] i_leaf_no, double i_plants, double i_swdef_photo, ref double o_dlt_plants)

                {

                //*+  Sub-Program Arguments
                //      REAL       C_leaf_no_crit        ! (INPUT)  critical number of leaves below which portion of the crop may die due to water stress
                //      REAL       C_swdf_photo_limit    ! (INPUT)  critical cumulative photosynthesis water stress above which the crop partly fails (unitless)
                //      REAL       C_swdf_photo_rate     ! (INPUT)  rate of plant reduction with photosynthesis water stress
                //      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
                //      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leaves ()
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       G_swdef_photo         ! (INPUT)
                //      real       dlt_plants            ! (OUTPUT) change in plant number

                //*+  Purpose
                //*      Determine plant death from drought.


                double       l_cswd_photo;            //! cumulative water stress for photoperiod
                double       l_leaf_no;               //! number of leaves
                double       l_killfr;                //! fraction of crop population to kill
   

                l_cswd_photo = sum_between_zb(zb(emerg), zb(crop_end), i_cswd_photo);
                l_leaf_no = sum_between_zb(zb(emerg), zb(now), i_leaf_no);

                if ((l_leaf_no < c_leaf_no_crit) && (l_cswd_photo > c_swdf_photo_limit) && (i_swdef_photo < 1.0)) 
                    {
                    l_killfr = c_swdf_photo_rate * (l_cswd_photo - c_swdf_photo_limit);
                    l_killfr = bound(l_killfr, 0.0, 1.0);
                    o_dlt_plants = - i_plants  *l_killfr;

                    Console.WriteLine("plant_kill." + Math.Truncate(l_killfr * 100.0) + "% failure because of water stress.");
                    }

                else
                    {
                    o_dlt_plants = 0.0;
                    }


                }



            void sugar_death_lodging(bool i_lodge_flag, double i_swdef_photo, double i_oxdef_photo, double[] c_stress_lodge, double[] c_death_fr_lodge, 
                            int c_num_stress_lodge, double i_plants, ref double o_dlt_plants_death_lodging)
                {


                //*+  Sub-Program Arguments
                //      logical g_lodge_flag
                //      real    g_swdef_photo
                //      real    g_oxdef_photo
                //      real    c_stress_lodge(*)
                //      real    c_death_fr_lodge(*)
                //      integer c_num_stress_lodge
                //      real    g_plants
                //      real    g_dlt_plants_death_lodging

                //*+  Mission Statement
                //*     Crop death due to lodging


                double      l_min_stress_factor;
                double      l_death_fraction;
                bool        l_didInterpolate;

                if (i_lodge_flag) 
                    {
                    l_min_stress_factor = Math.Min(i_swdef_photo, i_oxdef_photo);

                    l_death_fraction = MathUtility.LinearInterpReal(l_min_stress_factor,c_stress_lodge,c_death_fr_lodge, out l_didInterpolate);

                    o_dlt_plants_death_lodging = - i_plants * l_death_fraction;
                    }
                else
                    {
                    o_dlt_plants_death_lodging = 0.0;
                    }

                }




        #endregion



        #region Reallocate Cabbage


            void sugar_realloc_cabbage(int i_leaf, int i_cabbage, int i_sstem, int i_max_part, 
                double c_cabbage_sheath_fr, double[] i_dm_green, double[] i_dlt_dm_senesced, double[] i_n_green, 
                ref double[] o_dlt_dm_realloc, ref double[] o_dlt_n_realloc)
                {

                //*+  Sub-Program Arguments
                //      integer leaf
                //      integer cabbage
                //      integer sstem
                //      integer max_part
                //      real C_cabbage_sheath_fr
                //      real G_dm_green(*)
                //      real g_dlt_dm_senesced(*)
                //      real G_n_green(*)
                //      real g_dlt_dm_realloc(*)
                //      real g_dlt_n_realloc(*)

                //*+  Purpose
                //*       Reallocate cabbage to cane as plant develops to maintain a fixed leaf:cabbage ratio

                //*+  Mission Statement
                //*     Calcuate reallocation of cabbage to cane


                double      l_realloc_wt;
                double      l_realloc_n;


                fill_real_array(ref o_dlt_dm_realloc, 0.0, i_max_part);
                fill_real_array(ref o_dlt_n_realloc, 0.0, i_max_part);

                l_realloc_wt = i_dlt_dm_senesced[i_cabbage] * (MathUtility.Divide(1.0,c_cabbage_sheath_fr,0.0) - 1.0);
                o_dlt_dm_realloc[i_cabbage] = - l_realloc_wt;
                o_dlt_dm_realloc[i_sstem] = l_realloc_wt;

                //! this is not 100% accurate but swings and round-abouts will look after it - I hope (NIH)
                l_realloc_n = MathUtility.Divide(i_n_green[i_cabbage], i_dm_green[i_cabbage], 0.0) * l_realloc_wt;
                o_dlt_n_realloc[i_cabbage] = - l_realloc_n;
                o_dlt_n_realloc[i_sstem] = l_realloc_n;

                }




        #endregion



        #region Detachment (of Leaf and Cabbage)



        //void sugar_detachment()
        //    {

        //    //*+  Mission Statement
        //    //*     Calculate plant detachment


        //    if (!mu.reals_are_equal(crop.sen_detach_frac[leaf], crop.sen_detach_frac[cabbage]) )
        //        {
        //        Console.WriteLine("Invalid detachment for leaf and cabbage ratio.");
        //        }


        //    cproc_dm_detachment1(max_part, 
        //                        crop.sen_detach_frac, g_dm_senesced, ref g_dlt_dm_detached, 
        //                        crop.dead_detach_frac, g_dm_dead, ref g_dlt_dm_dead_detached);

        //    cproc_n_detachment1(max_part, 
        //                        crop.sen_detach_frac, g_n_senesced, ref g_dlt_n_detached, 
        //                        crop.dead_detach_frac, g_n_dead, ref g_dlt_n_dead_detached);

        //    cproc_lai_detachment1(leaf, 
        //                        crop.sen_detach_frac, g_slai, ref g_dlt_slai_detached, 
        //                        crop.dead_detach_frac, g_tlai_dead, ref g_dlt_tlai_dead_detached);


        //    }





        void  cproc_dm_detachment1(int i_max_part, 
                                double[] c_sen_detach_frac, double[] i_dm_senesced, ref double[] o_dlt_dm_detached, 
                                double[] c_dead_detach_frac, double[] i_dm_dead, ref double[] o_dlt_dm_dead_detached)
            {
            //!+  Sub-Program Arguments
            //      integer max_part
            //      real    c_sen_detach_frac (*)
            //      real    g_dm_senesced (*)
            //      real    g_dlt_dm_detached (*)
            //      real    c_dead_detach_frac (*)
            //      real    g_dm_dead (*)
            //      real    g_dlt_dm_dead_detached (*)

            //!+  Mission Statement
            //!   Calculate detachment of senescenced and dead material based on fractional decay rates.


            crop_pool_fraction_delta(i_max_part, c_sen_detach_frac, i_dm_senesced, ref o_dlt_dm_detached);


            crop_pool_fraction_delta(i_max_part, c_dead_detach_frac, i_dm_dead, ref o_dlt_dm_dead_detached);

            }



           void cproc_n_detachment1(int i_max_part, 
                                double[] c_sen_detach_frac, double[] i_n_senesced, ref double[] o_dlt_n_detached, 
                                double[] c_dead_detach_frac, double[] i_n_dead, ref double[] o_dlt_n_dead_detached)
            {

            //!+  Sub-Program Arguments
            //      integer max_part
            //      real    c_sen_detach_frac (*)
            //      real    g_n_senesced (*)
            //      real    g_dlt_n_detached (*)
            //      real    c_dead_detach_frac (*)
            //      real    g_n_dead (*)
            //      real    g_dlt_n_dead_detached (*)


            //!+  Mission Statement
            //!       Calculate plant Nitrogen detachment from senesced and dead pools


            crop_pool_fraction_delta(i_max_part, c_sen_detach_frac, i_n_senesced, ref o_dlt_n_detached);


            crop_pool_fraction_delta(i_max_part, c_dead_detach_frac, i_n_dead, ref o_dlt_n_dead_detached);

            }





        void crop_pool_fraction_delta (int i_num_part_ob, double[] i_fraction_zb, double[] i_pool_zb, ref double[] o_dlt_pool_zb)
            {

            //!+  Sub-Program Arguments
            //      INTEGER    num_part      ! (INPUT)  number of plant parts
            //      REAL       fraction(*)   ! (INPUT)  fraction of pools to detach
            //      REAL       pool(*)       ! (INPUT)  plant pool for detachment (g/m^2)
            //      real       dlt_pool(*)   ! (OUTPUT) change in plant pool

            //!+  Purpose
            //!      Multiply plant pool array by a part fraction array.

            //!+  Mission Statement
            //!   Calculate change in %3 based on fractional decay rates.


            for (int part = 0; part < i_num_part_ob; part++)
                {
                o_dlt_pool_zb[part] = i_pool_zb[part] * i_fraction_zb[part];
                }

            }





        void cproc_lai_detachment1 (int i_leaf_zb, 
                            double[] c_sen_detach_frac, double i_slai, ref double o_dlt_slai_detached, 
                            double[] c_dead_detach_frac, double i_tlai_dead, ref double o_dlt_tlai_dead_detached)
            {

            //!+  Sub-Program Arguments
            //      integer leaf
            //      real    c_sen_detach_frac(*)
            //      real    g_slai
            //      real    g_dlt_slai_detached
            //      real    c_dead_detach_frac(*)
            //      real    g_tlai_dead
            //      real    g_dlt_tlai_dead_detached


            //!+  Mission Statement
            //!   Calculate detachment of lai (based upon fractional decay rates)


            crop_part_fraction_delta(i_leaf_zb, c_sen_detach_frac, i_slai, ref o_dlt_slai_detached);

            crop_part_fraction_delta(i_leaf_zb, c_dead_detach_frac, i_tlai_dead, ref o_dlt_tlai_dead_detached);

            }





        void crop_part_fraction_delta (int i_part_no_zb, double[] i_fraction_zb, double i_part, ref double o_dlt_part)
            {

            //!+  Sub-Program Arguments
            //      integer    part_no
            //      REAL       fraction(*)     ! (INPUT)  fraction for each part
            //      REAL       part            ! (INPUT)  part value to use
            //      real       dlt_part        ! (OUTPUT) change in part

            //!+  Purpose
            //!      Calculate change in a particular plant pool

            //!+  Mission Statement
            //!   Calculate change in %3 for %1 based on fractional decay rates.


            o_dlt_part = i_part * i_fraction_zb[i_part_no_zb];

            }




        #endregion 



        #region Cleanup after processes (Do some Housekeeping)



            #region Update Variables

        void sugar_update(ref double io_canopy_height, ref double[] io_cnd_photo, ref double[] io_cswd_expansion, ref double[] io_cswd_pheno, ref double[] io_cswd_photo, 
                            double i_dlt_canopy_height, 
                            double i_dlt_dm, double[] i_dlt_dm_dead_detached, double[] i_dlt_dm_detached, double[] i_dlt_dm_green, 
                            double[] i_dlt_dm_green_retrans, double[] i_dlt_dm_senesced, double[] i_dlt_dm_realloc, 
                            double i_dlt_lai, double i_dlt_leaf_no, double i_dlt_node_no, double i_dlt_node_no_dead, 
                            double[] i_dlt_n_dead_detached, double[] i_dlt_n_detached, double[] i_dlt_n_green, 
                            double[] i_dlt_n_retrans, double[] i_dlt_n_senesced, double[] i_dlt_n_realloc, 
                            double i_dlt_plants, double[] i_dlt_plant_wc, double[] i_dlt_root_length, double[] i_dlt_root_length_senesced, double i_dlt_root_depth, 
                            double i_dlt_slai, double i_dlt_slai_detached,  double i_dlt_stage, double i_dlt_tlai_dead_detached, 
                            ref double[] io_dm_dead, ref double[] io_dm_green, ref double[] io_dm_plant_top_tot, ref double[] io_dm_senesced, ref double io_lai, 
                            ref double[] io_leaf_area, ref double[] io_leaf_dm, ref double[] io_leaf_no_zb, 
                            ref double[] io_node_no_zb, ref double[] io_node_no_dead_zb, double i_nfact_photo, ref double[] io_n_conc_crit, ref double[] io_n_conc_min, 
                            ref double[] io_n_dead, ref double[] io_n_green, ref double[] io_n_senesced, ref double io_plants, ref double[] io_plant_wc, 
                            double i_previous_stage, 
                            ref double[] io_root_length, ref double io_root_depth, ref double io_slai, 
                            double i_swdef_expansion, double i_swdef_pheno, double i_swdef_photo, 
                            ref double io_tlai_dead, 
                            double c_n_conc_crit_root, double c_n_conc_min_root, 
                            double[] c_x_stage_code, double[] c_y_n_conc_crit_cabbage, double[] c_y_n_conc_crit_cane, double[] c_y_n_conc_crit_leaf, double[] c_y_n_conc_min_cabbage, 
                            double[] c_y_n_conc_min_cane, double[] c_y_n_conc_min_leaf, 
                            double i_current_stage, double[] c_stage_code_list, double[] i_phase_tt, double[] i_tt_tot, 
                            ref double io_node_no_detached_ob, 
                            double c_leaf_no_at_emerg )
                {

                
                //*+  Sub-Program Arguments
                //      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
                //      REAL       G_cnd_photo(*)        ! (INPUT)  cumulative nitrogen stress typ
                //      REAL       G_cswd_expansion(*)   ! (INPUT)  cumulative water stress type 2
                //      REAL       G_cswd_pheno(*)       ! (INPUT)  cumulative water stress type 3
                //      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
                //      REAL       G_dlt_canopy_height   ! (INPUT)  change in canopy height (mm)
                //      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
                //      REAL       G_dlt_dm_dead_detached(*) ! (INPUT)  plant biomass detached fro
                //      REAL       G_dlt_dm_detached(*)  ! (INPUT)  plant biomass detached (g/m^2)
                //      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
                //      REAL       G_dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocat
                //      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
                //      REAL       G_dlt_dm_realloc(*)   ! (INPUT)
                //      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
                //      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
                //      REAL       G_dlt_node_no         ! (INPUT)
                //      REAL       G_dlt_node_no_dead    ! (INPUT)  fraction of oldest green leaf
                //      REAL       G_dlt_n_dead_detached(*) ! (INPUT)  actual N loss with detached
                //      REAL       G_dlt_n_detached(*)   ! (INPUT)  actual N loss with detached pl
                //      REAL       G_dlt_n_green(*)      ! (INPUT)  actual N uptake into plant (g/
                //      REAL       G_dlt_n_retrans(*)    ! (INPUT)  nitrogen retranslocated out fr
                //      REAL       G_dlt_n_senesced(*)   ! (INPUT)  actual N loss with senesced pl
                //      REAL       G_dlt_n_realloc(*)   ! (INPUT)
                //      REAL       G_dlt_plants          ! (INPUT)  change in Plant density (plant
                //      REAL       G_dlt_plant_wc(*)     ! (INPUT)
                //      REAL       G_dlt_root_length(*)  ! (INPUT)
                //      REAL       G_dlt_root_length_senesced(*) ! (INPUT)
                //      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
                //      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces fro
                //      REAL       G_dlt_slai_detached   ! (INPUT)  plant senesced lai detached
                //      REAL       G_dlt_stage           ! (INPUT)  change in stage number
                //      REAL       G_dlt_tlai_dead_detached ! (INPUT)  plant lai detached from dea
                //      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
                //      REAL       G_dm_plant_top_tot(*) ! (INPUT)  total carbohydrate production
                //      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
                //      REAL       G_lai                 ! (INPUT)  live plant green lai
                //      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
                //      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
                //      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
                //      REAL       G_node_no(*)
                //      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
                //      REAL       G_nfact_photo         ! (INPUT)
                //      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
                //      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
                //      REAL       G_n_dead(*)           ! (INPUT)  plant N content of dead plants
                //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
                //      REAL       G_n_senesced(*)       ! (INPUT)  plant N content of senesced pl
                //      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
                //      REAL       G_plant_wc(*)         ! (INPUT)
                //      REAL       G_previous_stage      ! (INPUT)  previous phenological stage
                //      REAL       G_root_length(*)      ! (INPUT)
                //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
                //      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
                //      REAL       G_swdef_expansion     ! (INPUT)
                //      REAL       G_swdef_pheno         ! (INPUT)
                //      REAL       G_swdef_photo         ! (INPUT)
                //      REAL       G_tlai_dead           ! (INPUT)  total lai of dead plants
                //      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
                //      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
                //      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
                //      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
                //      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
                //      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
                //      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
                //      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
                //      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
                //      REAL       G_current_stage       ! (INPUT)  current phenological stage
                //      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
                //      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
                //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
                //      REAL       G_node_no_detached    ! (INPUT)  number of detached leaves
                //      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence
             

                //*+  Mission Statement
                //*     Update states


                double       l_dlt_dm_plant;          //! dry matter increase (g/plant)
                double       l_dlt_leaf_area;         //! leaf area increase (mm^2/plant)
                double       l_dlt_leaf_area_sen;         //! leaf area increase (mm^2/plant)
                double       l_dlt_leaf_dm_sen;           //!
                double       l_dlt_dm_green_dead;     //! dry matter of green plant part dying (g/m^2)
                double       l_dlt_dm_senesced_dead;  //! dry matter of senesced plant part dying (g/m^2)
                double       l_dlt_dm_plant_leaf;     //! increase in plant leaf dm (g/plant)
                double       l_dlt_N_green_dead;      //! N content of green plant part dying (g/m^2)
                double       l_dlt_N_senesced_dead;   //! N content of senesced plant part dying (g/m^2)
                double       l_dlt_lai_dead;          //! lai of green leaf of plants dying ()
                double       l_dlt_slai_dead;         //! lai of senesced leaf of plant dying ()
                double       l_dying_fract;           //! fraction op population dying (0-1)
                double       l_node_no_ob;               //

            

                //! Note.
                //! Accumulate is used to add a value into a specified array element.
                //! If a specified increment of the element indicates a new element
                //! is started, the value is distributed proportionately between the
                //! two elements of the array

                //! Add is used to add the elements of one array into the corresponding
                //! elements of another array.

                //! now update with deltas

                //! The following table describes the transfer of material that should
                //! take place
                //!                        POOLS
                //!                 green senesced  dead
                //! dlt_green         +                     (incoming only)
                //! dlt_retrans       +-
                //! dlt_senesced      -      +
                //! dlt_dead          -      -       +
                //! dlt_detached             -       -      (outgoing only)


                //! take out water with detached stems
                io_plant_wc[sstem] = io_plant_wc[sstem] * (1.0 - MathUtility.Divide(i_dlt_dm_dead_detached[sstem], io_dm_dead[sstem]+io_dm_green[sstem],0.0));
                AddArray(i_dlt_plant_wc, ref io_plant_wc, max_part);

          
                    //! transfer N

                l_dying_fract = MathUtility.Divide(-i_dlt_plants, io_plants, 0.0);

                for (int part = 0; part < max_part; part++)
                    {
                    l_dlt_N_green_dead = io_n_green[part] * l_dying_fract;
                    io_n_green[part] = io_n_green[part] - l_dlt_N_green_dead;
                    io_n_dead[part] = io_n_dead[part] + l_dlt_N_green_dead;

                    l_dlt_N_senesced_dead = io_n_senesced[part] * l_dying_fract;
                    io_n_senesced[part] = io_n_senesced[part] - l_dlt_N_senesced_dead;
                    io_n_dead[part] = io_n_dead[part] + l_dlt_N_senesced_dead;
                    }

                SubtractArray(i_dlt_n_dead_detached, ref io_n_dead, max_part);

                AddArray(i_dlt_n_green, ref io_n_green, max_part);
                AddArray(i_dlt_n_retrans, ref io_n_green, max_part);
                AddArray(i_dlt_n_realloc, ref io_n_green, max_part);
                SubtractArray(i_dlt_n_senesced, ref io_n_green, max_part);

                AddArray(i_dlt_n_senesced, ref io_n_senesced, max_part);
                SubtractArray(i_dlt_n_detached, ref io_n_senesced, max_part);



                    //! Transfer plant dry matter

                l_dlt_dm_plant = MathUtility.Divide(i_dlt_dm, io_plants, 0.0);

                accumulate_zb(l_dlt_dm_plant, ref io_dm_plant_top_tot, zb_d(i_previous_stage), i_dlt_stage);

                for (int part = 0; part < max_part; part++)
                    {
                    l_dlt_dm_green_dead = io_dm_green[part] * l_dying_fract;
                    io_dm_green[part] = io_dm_green[part] - l_dlt_dm_green_dead;
                    io_dm_dead[part] = io_dm_dead[part] + l_dlt_dm_green_dead;

                    l_dlt_dm_senesced_dead = io_dm_senesced[part] * l_dying_fract;
                    io_dm_senesced[part] = io_dm_senesced[part] - l_dlt_dm_senesced_dead;
                    io_dm_dead[part] = io_dm_dead[part] + l_dlt_dm_senesced_dead;
                    }

                SubtractArray(i_dlt_dm_dead_detached, ref io_dm_dead, max_part);

                AddArray(i_dlt_dm_green, ref io_dm_green, max_part);
                AddArray(i_dlt_dm_green_retrans, ref io_dm_green, max_part);
                AddArray(i_dlt_dm_realloc, ref io_dm_green, max_part);
                SubtractArray(i_dlt_dm_senesced, ref io_dm_green, max_part);

                AddArray(i_dlt_dm_senesced, ref io_dm_senesced, max_part);
                SubtractArray(i_dlt_dm_detached, ref io_dm_senesced, max_part);



        //c      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
        //c     :           - g_dlt_dm_detached(root))
        //c      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
        //c     :          - g_dlt_N_detached(root))
        //c
        //c      call sugar_top_residue (dm_residue, N_residue)

        //c             //! put roots into root residue

        //c      call sugar_root_incorp (g_dlt_dm_detached(root)
        //c     :                    , g_dlt_N_detached(root))

                //SEND EVENTS TO OTHER MODULES
                //****************************
                sugar_update_other_variables(i_dlt_dm_detached, i_dlt_dm_dead_detached,     
                                            i_dlt_n_detached, i_dlt_n_dead_detached,
                                            g_root_length, g_root_depth);



                    //! transfer plant leaf area

                l_dlt_lai_dead  = io_lai  * l_dying_fract;
                l_dlt_slai_dead = io_slai * l_dying_fract;

                io_lai = io_lai + i_dlt_lai - l_dlt_lai_dead - i_dlt_slai;
                io_slai = io_slai + i_dlt_slai - l_dlt_slai_dead - i_dlt_slai_detached;
                io_tlai_dead = io_tlai_dead + l_dlt_lai_dead + l_dlt_slai_dead - i_dlt_tlai_dead_detached;


                    //! plant leaf development
                    //! need to account for truncation of partially developed leaf (add 1)
                    //   c      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
                    //! need to add leaf at emergence because we now remove records of detach
                    //! and so whereever detached leaves are used we need to account for the
                    //! are set at emergence as these offset the records.
                    //! THIS NEEDS CHANGING!!!!
                l_node_no_ob = 1.0 + sum_between_zb(zb(emerg), zb(now), io_node_no_zb) - io_node_no_detached_ob - c_leaf_no_at_emerg;

                l_node_no_ob = l_bound(l_node_no_ob, 1.0);

       

                l_dlt_leaf_area = MathUtility.Divide(i_dlt_lai, io_plants, 0.0) * sm2smm;
                //Console.WriteLine("leaf area before accumulate : " + io_leaf_area[0] + " , " + l_dlt_leaf_area + " , " + l_node_no_ob + " , " + i_dlt_node_no);
                accumulate_zb(l_dlt_leaf_area, ref io_leaf_area, zb_d(l_node_no_ob), i_dlt_node_no);
                //Console.WriteLine("leaf area after accumulate : " + io_leaf_area[0]);

                l_dlt_dm_plant_leaf = MathUtility.Divide(i_dlt_dm_green[leaf], io_plants, 0.0);
                accumulate_zb(l_dlt_dm_plant_leaf, ref io_leaf_dm, zb_d(l_node_no_ob), i_dlt_node_no);

                accumulate_zb(i_dlt_leaf_no, ref io_leaf_no_zb, zb_d(i_previous_stage), i_dlt_stage);
                accumulate_zb(i_dlt_node_no, ref io_node_no_zb, zb_d(i_previous_stage), i_dlt_stage);
                accumulate_zb(i_dlt_node_no_dead, ref io_node_no_dead_zb, zb_d(i_previous_stage), i_dlt_stage);



                //! detached leaf area needs to be accounted for

                //sv-work out delta in "leaf area" and "leaf dm" caused by senescence. (per plant)  
                l_dlt_leaf_area_sen = MathUtility.Divide(i_dlt_slai_detached, io_plants, 0.0) * sm2smm;    
                l_dlt_leaf_dm_sen = MathUtility.Divide(i_dlt_dm_detached[leaf], io_plants, 0.0);  

                int l_num_leaves;            //! number of leaves on plant
                l_num_leaves = count_of_real_vals(io_leaf_area, max_leaf);   //sv- this is pointless, considering what the next line does.
                l_num_leaves = max_leaf;

                bool l_found_last_senesced = false;
                int l_rec_last_senesced = 0;       //! number of empty leaf records    
                //int l_leaf_rec;                   //! leaf record number

                //sv- keep looping through all the leaves starting from the bottom, until delta is all used up.
                for (int leaf_rec = 0; leaf_rec < l_num_leaves; leaf_rec++)   
                    {
                    //DO LEAF AREA DELTA
                    //sv- if the area of this leaf is less than or equal to the delta  
                    if (io_leaf_area[leaf_rec] <= l_dlt_leaf_area_sen)                  
                        {
                        //sv- if leaf area is zero, this will execute but have no effect.                                 
                        l_dlt_leaf_area_sen = l_dlt_leaf_area_sen - io_leaf_area[leaf_rec];     //sv- then reduce the delta by the area of this leaf. 
                        io_leaf_area[leaf_rec] = 0.0;                                   //sv- set the leaf area of this leaf to zero because it has fully senesced.
                        }
                    else
                        {
                        io_leaf_area[leaf_rec] = io_leaf_area[leaf_rec] - l_dlt_leaf_area_sen;  //sv- then reduce the leaf area by the delta.
                        l_dlt_leaf_area_sen = 0.0;                                              //sv- delta is now used up.
                        }
        
                    //DO LEAF DRY MATTER DELTA
                    //sv- same as above.
                    if (io_leaf_dm[leaf_rec] <= l_dlt_leaf_dm_sen)                      
                        {
                        l_dlt_leaf_dm_sen = l_dlt_leaf_dm_sen - io_leaf_dm[leaf_rec];
                        io_leaf_dm[leaf_rec] = 0.0;
                        }   
                    else
                        {
                        io_leaf_dm[leaf_rec] = io_leaf_dm[leaf_rec] - l_dlt_leaf_dm_sen;
                        l_dlt_leaf_dm_sen = 0.0;
                        }

                    //SET THE LAST SENESCED LEAF
                    //sv- if this leaf has not fully senesced, and it is the first one we have come across that has not. (starting from the bottom leaf)
                    //    (due to the loop not terminating, We will come across all the other leaves above this leaf, that are not senesced as well.)                                                              
                    if ((io_leaf_dm[leaf_rec] > 0.0) && (l_found_last_senesced == false))  
                        {                                                        
                        l_found_last_senesced = true;
                        l_rec_last_senesced = leaf_rec - 1;         //sv- the leaf below, the leaf that has not fully senesced.
                        }
                    }

                //sv- Remove the leaf, by removing it's information from leaf_area and leaf_dm, 
                //    by moving all the elements in the array down by one index, and then throwing away the first element.
                if (l_found_last_senesced && (l_rec_last_senesced >= 0))
                    {
                    io_node_no_detached_ob = io_node_no_detached_ob + ob(l_rec_last_senesced);
                    //!kludgy solution for now
                
                    //for all the leaves greater then the last senesced leaf
                    for (int leaf_rec = l_rec_last_senesced; leaf_rec < (l_num_leaves-1); leaf_rec++)  
                        {
                        //sv- starts at 1 and goes up, eg. 2,3 etc.

                        //sv- move the values down one index.
                        io_leaf_dm[leaf_rec] = io_leaf_dm[leaf_rec+1];   
                        io_leaf_area[leaf_rec] = io_leaf_area[leaf_rec+1];

                        //sv- zero the old indexes 
                        io_leaf_dm[leaf_rec+1] = 0.0;                       
                        io_leaf_area[leaf_rec+1] = 0.0;
                        }
                    }

                    //for (int leaf_rec = l_rec_last_senesced+1; leaf_rec < l_num_leaves; leaf_rec++)  
                    //    {
                    //    //sv- "leaf_rec_new" starts at 1 and goes up, eg. 2,3 etc.
                    //    leaf_rec_new = leaf_rec - l_rec_last_senesced;    

                    //    //sv- move the values down one index.
                    //    io_leaf_dm[leaf_rec_new] = io_leaf_dm[leaf_rec];   
                    //    io_leaf_area[leaf_rec_new] = io_leaf_area[leaf_rec];

                    //    //sv- zero the old indexes 
                    //    io_leaf_dm[leaf_rec] = 0.0;                       
                    //    io_leaf_area[leaf_rec] = 0.0;
                    //    }
                    //}





                    //! plant stress

                accumulate_zb(1.0 - i_swdef_photo, ref io_cswd_photo, zb_d(i_previous_stage), i_dlt_stage);
                accumulate_zb(1.0 - i_swdef_expansion, ref io_cswd_expansion, zb_d(i_previous_stage), i_dlt_stage);
                accumulate_zb(1.0 - i_swdef_pheno, ref io_cswd_pheno, zb_d(i_previous_stage), i_dlt_stage);

                accumulate_zb(1.0 - i_nfact_photo, ref io_cnd_photo, zb_d(i_previous_stage), i_dlt_stage);



                    //! other plant states

                io_canopy_height = io_canopy_height + i_dlt_canopy_height;
                io_plants = io_plants + i_dlt_plants;
                io_root_depth = io_root_depth + i_dlt_root_depth;
                AddArray(i_dlt_root_length, ref io_root_length, max_layer);
                SubtractArray(i_dlt_root_length_senesced, ref io_root_length, max_layer);

                sugar_N_conc_limits(c_n_conc_crit_root, c_n_conc_min_root, c_x_stage_code, 
                                        c_y_n_conc_crit_cabbage, c_y_n_conc_crit_cane, c_y_n_conc_crit_leaf, 
                                        c_y_n_conc_min_cabbage, c_y_n_conc_min_cane, c_y_n_conc_min_leaf, 
                                        i_current_stage, c_stage_code_list, i_phase_tt, i_tt_tot, 
                                        ref io_n_conc_crit, ref io_n_conc_min );  //! plant N concentr


            }





        void sugar_N_conc_limits(double c_n_conc_crit_root , double c_n_conc_min_root, 
                                double[] c_x_stage_code , 
                                double[] c_y_n_conc_crit_cabbage , double[] c_y_n_conc_crit_cane , double[] c_y_n_conc_crit_leaf, 
                                double[] c_y_n_conc_min_cabbage , double[] c_y_n_conc_min_cane , double[] c_y_n_conc_min_leaf,
                                double i_current_stage, double[] c_stage_code_list, double[] i_phase_tt, double[] i_tt_tot, 
                                ref double[] o_n_conc_crit, ref double[] o_n_conc_min)
            {
            //*+  Sub-Program Arguments
            //      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
            //      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
            //      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
            //      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
            //      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
            //      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
            //      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
            //      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
            //      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
            //      REAL       G_current_stage       ! (INPUT)  current phenological stage
            //      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
            //      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
            //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
            //      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
            //                                       ! (g N/g part)
            //      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
            //                                       ! (g N/g part)

            //*+  Purpose
            //*       Calculate the critical N concentration below which plant growth
            //*       is affected.  Also minimum N concentration below which it is not
            //*       allowed to fall.  These are analogous to the water concentrations
            //*       of dul and ll.

            //*+  Mission Statement
            //*     Calculate critical N concentration below which plant growth affected


            int         l_numvals;               //! number of values in stage code table
            double      l_stage_code;            //! interpolated current stage code
            bool        l_didInterpolate;



            fill_real_array (ref o_n_conc_crit, 0.0, max_part);
            fill_real_array (ref o_n_conc_min, 0.0, max_part);

            if (stage_is_between (emerg, crop_end, i_current_stage)) 
                {
                o_n_conc_crit[root] = c_n_conc_crit_root;
                o_n_conc_min[root] = c_n_conc_min_root;

                    //! the tops critical N percentage concentration is the stover
                    //! (non-grain shoot) concentration below which N concentration
                    //! begins to affect plant growth.

                l_numvals = count_of_real_vals (c_x_stage_code, max_stage);
    
                l_stage_code = sugar_stage_code(c_stage_code_list, i_phase_tt, i_tt_tot, i_current_stage, c_x_stage_code, l_numvals);

                //! nih - I put cane critical conc in the sstem element of the
                //! array because there is no 'cane' (sstem+sucrose) pool
                o_n_conc_crit[sstem] = MathUtility.LinearInterpReal(l_stage_code, c_x_stage_code, c_y_n_conc_crit_cane, out l_didInterpolate);
                o_n_conc_crit[leaf] = MathUtility.LinearInterpReal(l_stage_code, c_x_stage_code, c_y_n_conc_crit_leaf, out l_didInterpolate);
                o_n_conc_crit[cabbage] = MathUtility.LinearInterpReal(l_stage_code, c_x_stage_code, c_y_n_conc_crit_cabbage, out l_didInterpolate);

                //    ! the  minimum N concentration is the N concentration
                //    ! below which N does not fall.

                //! nih - I put cane minimum conc in the sstem element of the
                //! array because there is no 'cane' (sstem+sucrose) pool
                o_n_conc_min[sstem] = MathUtility.LinearInterpReal(l_stage_code, c_x_stage_code, c_y_n_conc_min_cane, out l_didInterpolate);

                o_n_conc_min[leaf] = MathUtility.LinearInterpReal(l_stage_code, c_x_stage_code, c_y_n_conc_min_leaf, out l_didInterpolate);

                o_n_conc_min[cabbage] = MathUtility.LinearInterpReal(l_stage_code, c_x_stage_code, c_y_n_conc_min_cabbage, out l_didInterpolate);
                }

            }



      double sugar_stage_code(double[] c_stage_code_list, double[] i_phase_tt, double[] i_tt_tot, double i_stage_no, double[] i_stage_table, int i_numvals)
            {

            //*+  Sub-Program Arguments
            //      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
            //      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days required for each stage (deg days)
            //      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
            //      real       stage_no              ! (INPUT) stage number to convert
            //      real       stage_table(*)        ! (INPUT) table of stage codes
            //      integer    numvals               ! (INPUT) size_of of table

            //*+  Purpose
            //*       Return an interpolated stage code from a table of stage_codes
            //*       and a nominated stage number. Returns 0 if the stage number is not
            //*       found. Interpolation is done on thermal time.

            //*+  Mission Statement
            //*     Get thermal time interpolated stage code from table


            double    l_phase_tt;              //! required thermal time between stages (oC)
            string    l_warn_message;     //! error message
            double    l_fraction_of;           //!
            int       l_next_stage;            //! next stage number to use
            double    l_tt_tot;                //! elapsed thermal time between stages (oC)
            int       l_this_stage;            //! this stage to use
            double    l_x_stage_code = 0.0;          //! interpolated stage code



            if (i_numvals >= 2) 
                {
                //! we have a valid table
                l_this_stage = stage_no_of_ob(i_stage_table[0] , c_stage_code_list, max_stage);

                for (int i = 1; i < i_numvals; i++)
                    {
                    l_next_stage = stage_no_of_ob(i_stage_table[i], c_stage_code_list, max_stage);

                    if (stage_is_between (l_this_stage, l_next_stage, i_stage_no))
                        {
                                //! we have found its place
                        l_tt_tot = sum_between_zb(zb(l_this_stage), zb(l_next_stage), i_tt_tot);
                        l_phase_tt = sum_between_zb(zb(l_this_stage), zb(l_next_stage), i_phase_tt);
                        l_fraction_of = MathUtility.Divide(l_tt_tot, l_phase_tt, 0.0);
                        l_x_stage_code = i_stage_table[i-1] + (i_stage_table[i] - i_stage_table[i-1]) * l_fraction_of;
                        break;
                        }
                    else
                        {
                        l_x_stage_code = 0.0;
                        l_this_stage = l_next_stage;
                        }

                    }
       
                }
            else
                {
                    //! we have no valid table

                 l_x_stage_code = 0.0;

                 l_warn_message =  "Invalid lookup table - number of values =" + i_numvals;
                 warning_error(l_warn_message);
                }

              return l_x_stage_code;

          }





      int stage_no_of_ob (double i_stage_code, double[] i_stage_code_list, int i_list_size)
            {
            //!+ Sub-Program Arguments
            //   real       stage_code            ! (INPUT) stage code to look up
            //   real       stage_code_list(*)    ! (INPUT) list of stage codes
            //   integer    list_size             ! (INPUT) size_of of stage code list

            //!+ Purpose
            //!     Returns stage number of a stage code from a list of stage codes.
            //!     Returns 0 if not found.

            //!+  Definition
            //!     "stage_code_list" is an array of "list_size" stage codes.
            //!     This function returns the index of the first element of
            //!     "stage_code_list" that is equal to "stage_code".  If there
            //!     are no elements in "stage_code_list" equal to
            //!     "stage_code", then a warning error is flagged and zero is
            //!     returned.

            //!+  Mission Statement
            //!      stage number of %1


          //sv- IMPORTANT - Once again to avoid any confusion I changed the error code from 0 to -1 
          //sv-             even though stage no's are 1 based and 0 does not cause an issue as an error return value

            string      l_warn_message;     //! err message
            int         l_position_zb;         //! position found in array
            int         l_returnValue_ob; 


  
            l_position_zb = position_in_real_array_zb(i_stage_code, i_stage_code_list);

            if (l_position_zb > -1)
                {
                l_returnValue_ob =  ob(l_position_zb);  
                }
            else
                {
                l_returnValue_ob = 0;

                l_warn_message = "Stage code not found in code list." + " Code number =" + i_stage_code;
                warning_error (l_warn_message);

                }

            return l_returnValue_ob;

            }



      int position_in_real_array_zb(double i_Number, double[] i_Array)
          {

          //!+ Sub-Program Arguments
          //   real       Array(*)              ! (INPUT) Array to search
          //   integer    Array_size            ! (INPUT) Number of elements in array
          //   real       Number                ! (INPUT) Number to search for

          //!+ Purpose
          //!       returns the index number of the first occurrence of specified value

          //!+  Definition
          //!     Returns the index of the first element of of the
          //!     "array_size" elements of "array" that compares equal to
          //!     "number".  Returns 0 if there are none. 

          //!+  Mission Statement
          //!      position of %1 in array %2

          //sv- IMPORTANT - I changed this to return the Zero Based index rather than the 1 Based index.
          //sv-           - Now Returns -1 if there are none because 0 is a valid index.

          int position;              //! position of number in array



          position = -1; 

          for (int index = 0; index < i_Array.Length; index++)
              {
              if (mu.reals_are_equal(i_Number, i_Array[index]))
                  {
                  position = index;  
                  break;
                  }
              else
                  {
                  //! Not found
                  }
              }

          return position;
         
          }



            #endregion




            #region Calculate Totals Variables


      void sugar_totals(double i_current_stage, double[] i_days_tot, int i_day_of_year, double[] i_dlayer, double[] i_dlt_sw_dep, 
                double[] i_dm_green, ref int o_isdate, double i_lai, ref double io_lai_max, ref double o_n_conc_act_stover_tot, 
                double[] i_n_demand, ref double io_n_demand_tot, double[] i_n_green, double i_root_depth, ref double io_transpiration_tot )
            {


            //*+  Sub-Program Arguments
            //      REAL       G_current_stage       ! (INPUT)  current phenological stage
            //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
            //      INTEGER    G_day_of_year         ! (INPUT)  day of year
            //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
            //      REAL       G_dlt_sw_dep(*)       ! (INPUT)  water uptake in each layer (mm water)
            //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
            //      INTEGER    G_isdate              ! (INPUT)  flowering day number                        //sv- actually an OUTPUT
            //      REAL       G_lai                 ! (INPUT)  live plant green lai
            //      REAL       G_lai_max             ! (INPUT)  maximum lai - occurs at flowering           //sv- actually an OUTPUT
            //      REAL       G_n_conc_act_stover_tot ! (INPUT)  sum of tops actual N concentration (g N/g biomass)    //sv- actually an OUTPUT
            //      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
            //      REAL       G_n_demand_tot        ! (INPUT)  sum of N demand since last output (g/m^2)   //sv- actually an OUTPUT
            //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
            //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
            //      REAL       G_transpiration_tot   ! (INPUT)  cumulative transpiration (mm)               //sv- actually an OUTPUT

            //*+  Purpose
            //*         Collect totals of crop variables for output



            double      l_N_conc_stover;         //! tops actual N concentration (g N/g part)
            int         l_deepest_layer_ob;         //! deepest layer in which the roots are growing
            double      l_N_green_demand;        //! plant N demand (g/m^2)




            //cnh I have removed most of the variables because they were either calculated wrongly or irrelevant.

                //! get totals
            l_N_conc_stover = MathUtility.Divide((i_n_green[leaf] + i_n_green[sstem] + i_n_green[cabbage] + i_n_green[sucrose]), (i_dm_green[leaf] + i_dm_green[sstem] + i_dm_green[cabbage] + i_dm_green[sucrose]) , 0.0);

          //! note - g_N_conc_crit should be done before the stages change
            //cnh wrong!!!
            //c      N_conc_stover_crit = (g_N_conc_crit(leaf) + g_N_conc_crit(stem)) * 0.5
            l_N_green_demand = SumArray(i_n_demand, max_part);

            l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

            if (on_day_of(sowing, i_current_stage))
                {
                 io_transpiration_tot = - SumArray(i_dlt_sw_dep, l_deepest_layer_ob);
                 o_n_conc_act_stover_tot = l_N_conc_stover;
                 io_n_demand_tot = l_N_green_demand;
                }
            else
                {
                io_transpiration_tot = io_transpiration_tot + (-SumArray(i_dlt_sw_dep, l_deepest_layer_ob));
                o_n_conc_act_stover_tot = l_N_conc_stover;
                io_n_demand_tot = io_n_demand_tot + l_N_green_demand;
                }

            io_lai_max = Math.Max(io_lai_max, i_lai);

            if (on_day_of(flowering, i_current_stage)) 
                {
                o_isdate = i_day_of_year;
                }



            }


            #endregion




            #region Report Events That Occurred Today and Status of Variables


      void sugar_event(double[] c_stage_code_list, string[] c_stage_names, double i_current_stage, double[] i_days_tot, int i_day_of_year, 
                            double[] i_dlayer, double[] i_dm_dead, double[] i_dm_green, double[] i_dm_senesced, double i_lai, 
                            double[] i_n_green, double i_root_depth, double[] i_sw_dep, int i_year, double[] i_ll_dep)
            {

            //*+  Sub-Program Arguments
            //      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
            //      CHARACTER  C_stage_names(*)*(*)  ! (INPUT)  full names of stages for repor
            //      REAL       G_current_stage       ! (INPUT)  current phenological stage
            //      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
            //      INTEGER    G_day_of_year         ! (INPUT)  day of year
            //      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
            //      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
            //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
            //      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
            //      REAL       G_lai                 ! (INPUT)  live plant green lai
            //      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
            //      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
            //      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
            //      INTEGER    G_year                ! (INPUT)  year
            //      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab

            //*+  Purpose
            //*       Report occurence of event and the current status of specific variables.

            //*+  Mission Statement
            //*     Report event and current status of specific variables


            double      l_biomass;               //! total above ground plant wt (g/m^2)
            int         l_deepest_layer_ob;         //! deepest layer in which the roots are growing
            double      l_pesw_tot;              //! total plant extractable sw (mm)
            double[]    l_pesw = new double[max_layer];       //! plant extractable soil water (mm)
            double      l_N_green;               //! plant nitrogen of tops (g/m^2) less flower
            double      l_dm_green;              //! plant wt of tops (g/m^2) less flower
            int         l_stage_no;              //! stage number at beginning of phase
            double      l_N_green_conc_percent;  //! n% of tops less flower (incl grain)


             //sv- I added this to mimic what the fortran infrastructure does.
            //"13 July 1991(Day of year=194), sugar: ");
            //http://msdn.microsoft.com/en-us/library/8kb3ddd4.aspx
            Console.WriteLine("{0}{1}{2}{3}", Clock.Today.ToString("d MMMM yyy").ToString(), "(Day of year=", g_day_of_year, "), SugarCane: ");


            l_stage_no = (int)i_current_stage;
            if (on_day_of(l_stage_no, i_current_stage))
                {
                    //! new phase has begun.
                Console.WriteLine("{0}{1,6:F1} {2}"," stage " , c_stage_code_list[zb(l_stage_no)], c_stage_names[zb(l_stage_no)]);

                //write (string, '(a, f6.1, 1x, a)') ' stage ', c_stage_code_list(stage_no), c_stage_names(stage_no)


                l_biomass = SumArray(i_dm_green, max_part) - i_dm_green[root] 
                        + SumArray(i_dm_senesced, max_part) - i_dm_senesced[root]
                        + SumArray(i_dm_dead, max_part) - i_dm_dead[root];

                l_dm_green = SumArray(i_dm_green, max_part) - i_dm_green[root];

                l_N_green = SumArray(i_n_green, max_part) - i_n_green[root];

                l_N_green_conc_percent = MathUtility.Divide(l_N_green, l_dm_green, 0.0) * fract2pcnt;

                l_deepest_layer_ob = FindLayerNo_ob(i_root_depth);

                for(int layer = 0; layer < l_deepest_layer_ob; layer++)
                    {
                    l_pesw[layer] = i_sw_dep[layer] - i_ll_dep[layer];
                    l_pesw[layer] = l_bound (l_pesw[layer], 0.0);
                    }
                l_pesw_tot = SumArray(l_pesw, l_deepest_layer_ob);

                if (stage_is_between(emerg, crop_end, i_current_stage))
                    {
                    Console.WriteLine("{0}{1,16:F7}{2}{3,16:F7}", "                     biomass =       ", l_biomass, "   lai = ", i_lai);
                    Console.WriteLine("{0}{1,16:F7}{2}{3,16:F7}", "                     stover N conc = ", l_N_green_conc_percent, "   extractable sw =", l_pesw_tot);

                        //       write (string, '(2(a, g16.7e2), a, 2(a, g16.7e2))')
                        //:              '                     biomass =       '
                        //:            , biomass
                        //:            , '   lai = '
                        //:            , g_lai
                        //:            , new_line
                        //:            ,'                     stover N conc ='
                        //:            , N_green_conc_percent
                        //:            , '   extractable sw ='
                        //:            , pesw_tot

                    }

                }


            }




            #endregion



        #endregion



    #endregion






      //OUTPUTS FROM THIS MODULE


    #region Functions used in Send My Variables



        double sugar_profile_fasw()
            {

            //*+  Mission Statement
            //*     Fraction of available soil water in profile

            double      asw;
            double      asw_pot;
            int         deepest_layer_ob;

            deepest_layer_ob = FindLayerNo_ob(g_root_depth);
            asw_pot = 0.0;
            asw     = 0.0;
            for (int layer = 0; layer < deepest_layer_ob; layer++)
                {
                asw_pot = asw_pot + g_sw_avail_pot[layer];
                asw = asw + u_bound(g_sw_avail[layer], g_sw_avail_pot[layer]);
                }

            return MathUtility.Divide(asw, asw_pot, 0.0);

            }


    #endregion



    #region Send My Variables


//*     ================================================================
//      subroutine sugar_send_my_variable (variable_name)
//*     ================================================================

//*+  Purpose
//*      Return the value of a variable requested by other modules.

//*+  Mission Statement
//*     Provide data to a requesting module


//*+  Local Variables
//      real       act_N_up              ! cumulative total N uptake by plant
//                                       ! (kg/ha)
//      real       biomass               ! above ground biomass (alive+dead)
//      real       biomass_n             ! N in above ground biomass (alive+dead)
//      real       cane_dmf              !
//      real       cane_wt               ! cane weight (sstem + sucrose)
//      real       ccs                   ! commercial cane sugar(g/g)
//      real       cover                 ! crop cover fraction (0-1)
//      real       das                   ! days after sowing
//      integer    das1                  ! days after sowing (rounded integer of das)
//      real       radn_int              ! daily radn intercepted (MJ)
//      integer    deepest_layer         ! deepest layer in which the roots are
//                                       ! growing
//      real       fasw
//      real       lai_sum               ! leaf area index of all leaf material
//                                       ! live + dead
//      real       lai_dead              ! dead leaf area index
//                                       ! (m^2 leaf/m^2 soil)
//      integer    num_layers            ! number of layers in profile
//      integer    stage_no              ! current stage no.
//      real       NO3gsm_tot            ! total NO3 in the root profile (g/m^2)
//      real       N_demand              ! sum N demand for plant parts (g/m^2)
//      real       N_supply              ! N supply for grain (g/m^2)
//cbak
//      real       conc_n_leaf           ! Current N concentration in leaves
//      real       conc_n_cab            ! Current N concentration in cabbage
//      real       conc_n_cane           ! Current N concentration in cane
//*
//*
//      real       n_leaf_crit           ! Weight of N in leaves at the critical c
//      real       n_leaf_min            ! Weight of N in leaves at the min concen
//      real       green_biomass_n       ! Weight of N in green tops (g/m^2)
//      real       plant_n_tot           ! Total plant N including roots (g/m2)
//cmjr
//      real       canefw                ! Weight of fresh cane at 30% dry matter
//      real       scmstf                ! sucrose conc in fresh millable stalk
//      real       scmst                 ! sucrose conc in dry millable stalk
//      real       temp
//      real       tla
//      integer    layer
//      real       rwu(max_layer)        ! root water uptake (mm)
//      real       rlv(max_layer)
//      real       ep
//      real       esw(max_layer)
//*- Implementation Section ----------------------------------




    [Output] [Units("(days)")] int daysaftersowing
 	    { 
 	 	 get 
 	 	 	{
            double l_das = sum_between_zb(zb(sowing), zb(now), g_days_tot);
            int l_das1 = (int)l_das;
            return l_das1;
            }
         }


    [Output] [Units("()")] string  crop_status
 	    { get { return g_crop_status;} }


    [Output] [Units("()")] double stage
        { get  { return g_current_stage;}  }


    [Output] [Units("()")] double stage_code
 	    { 
 	 	 get 
 	 	 	{
            if (g_crop_status != crop_out)
                {
                int l_stage_no = (int)g_current_stage;
                return crop.stage_code_list[zb(l_stage_no)];
                }
            else
                {
                return crop_end;  
                }
            }
        }

    [Output] [Units("()")] string stagename
 	    { 
 	 	 get 
 	 	 	 {
             if (g_crop_status != crop_out)
                 {
                 int l_stage_no = (int)g_current_stage;
                 return crop.stage_names[zb(l_stage_no)];
                 }
             else
                 {
                 //return "crop_end";
                 return "fallow";   //this is a better name than "crop_end" even though they are the same stage.
                 }
             }
        }

    //See -> Module constants read in from the ini file.

    //[Output] [Units("()")] string crop_type
    //    { get { return crop_type; } }


    //See -> Set My Variables

     //[Output] [Units("(/m2)")] double plants
     //   { get {return g_plants;} }


     [Output] [Units("()")] int ratoon_no
 	 	 { get { return g_ratoon_no;} }


     [Output]  [Units("(oC)")] double[] phase_tt
 	 	 { get { return g_phase_tt;} }


     [Output] [Units("(oC)")] double[] tt_tot
 	 	 { get { return g_tt_tot;} }


     [Output] [Units("()")] double[] leaf_no
         { get { return g_leaf_no_zb;} }


     [Output] [Units("()")] double[]  node_no_dead
 	 	 { get { return g_node_no_dead_zb;} }


     [Output] [Units("()")]double[]  leaf_area
 	 	 { get { return g_leaf_area_zb;} }


     [Output] [Units("()")] double[]  leaf_dm
 	 	 { get { return g_leaf_dm_zb;} }


     [Output] [Units("(mm)")]double height
 	 	 { get { return g_canopy_height;} }


     [Output]   [Units("(mm)")] double root_depth
 	 	 { get { return g_root_depth;} }


     [Output] [Units("()")] double cover_green
 	    { 
 	 	 get 
 	 	 	 {
             if (g_crop_status != crop_out)
                 return 1.0 - Math.Exp(-crop.extinction_coef * g_lai);
             else
                 return 0.0;
             }
         }


     [Output] [Units("(mj/m2)")] double radn_int
 	    { 
 	 	 get 
 	 	 	{ 
            //double l_cover = 1.0 - Math.Exp(-crop.extinction_coef * g_lai);
            double l_radn_int = cover_green * radn;
            return l_radn_int;
            }
         }


     [Output] [Units("()")] double cover_tot
 	    { 
 	 	 get 
 	 	 	 {
             if (g_crop_status != crop_out)
                 {
                 double l_lai_dead = g_slai + g_tlai_dead;
                 return 1.0 - Math.Exp(-crop.extinction_coef * g_lai - crop.extinction_coef_dead * l_lai_dead);
                 }
             else
                 {
                 return 0.0;
                 }
            }
         }


     [Output] [Units("()")] double lai_sum 
 	    { 
 	 	 get 
 	 	 	 { 
             double l_lai_sum = g_lai + g_slai + g_tlai_dead;
             return l_lai_sum;
             }
         }


     [Output] [Units("()")] double tlai
 	 	 { get { return g_lai + g_slai;} }


     [Output]  [Units("()")] double tla    
 	    { 
 	 	 get 
 	 	 	 { 
             double l_tla = SumArray(g_leaf_area_zb, max_leaf);
             return l_tla;
             }
         }


     [Output] [Units("()")] double slai
 	 	 { get { return g_slai;} }


     [Output] [Units("(m^2/m^2)")] double lai
 	 	 { get { return g_lai;} }


     [Output] [Units("(mm/mm3)")] double[] rlv
         {
         get
             {
             int num_layers = count_of_real_vals(dlayer, max_layer);
             double[] l_rlv = new double[num_layers];
             for (int layer = 0; layer < num_layers; layer++)
                 {
                 l_rlv[layer] = MathUtility.Divide(g_root_length[layer], dlayer[layer], 0.0) * sugar_afps_fac(layer);
                 }
             return l_rlv;
             }
         }


     [Output] [Units("(mm/mm3)")] double[] rlv_tot
         {
         get
             {
             int num_layers = count_of_real_vals(dlayer, max_layer);
             double[] l_rlv = new double[num_layers];
             for (int layer = 0; layer < num_layers; layer++)
                 {
                 l_rlv[layer] = MathUtility.Divide(g_root_length[layer], dlayer[layer], 0.0);
                 }
             return l_rlv;
             }
         }


     [Output] [Units("(mm)")] double[] ll_dep
        { 
         get 
            {
            int num_layers = count_of_real_vals(dlayer, max_layer);
            double[] l_ll_dep = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                {
                l_ll_dep[layer] = g_ll_dep[layer];
                }
            return l_ll_dep; 
            } 
         }


    //TODO: Grazing is no longer supported in Sugar module

     //[Output] [Units("(g/m^2)")] double dm_graze
     //{ get { return g_dm_graze; } }


     //[Output] [Units("(g/m^2)")] double n_graze
     //{ get { return g_N_graze; } }


     [Output] [Units("(g/m^2)")] double lai2
         {
         get
             {
             double l_temp = SumArray(g_leaf_area_zb, max_leaf);
             l_temp = l_temp * g_plants / 1000000.0;
             return l_temp;
             }
         }


     [Output] [Units("(g/m^2)")] double leaf_wt2
         {
         get
             {
             double l_temp = SumArray(g_leaf_dm_zb, max_leaf);
             l_temp = l_temp * g_plants;
             return l_temp;
             }
         }





            //! plant biomass
            //***************


     [Output]  [Units("(g/m^2)")] double rootgreenwt
 	 	 { get { return g_dm_green[root]; } }


     [Output]   [Units("(g/m^2)")] double leafgreenwt
 	 	 { get { return g_dm_green[leaf]; } }


     [Output] [Units("(g/m^2)")] double sstem_wt
 	 	 { get { return g_dm_green[sstem] + g_dm_dead[sstem]; } }  //! Add dead pool for lodged crops


     [Output] [Units("(0-1)")]
     double cane_dmf
         {
         get
             {
             double l_cane_dmf = MathUtility.Divide(g_dm_green[sstem] + g_dm_green[sucrose],
                                                  g_dm_green[sstem] + g_dm_green[sucrose] + g_plant_wc[sstem],
                                                  0.0);
             return l_cane_dmf;
             }
         }


     [Output][Units("(t/ha)")] double canefw
         {
 	 	  get 
 	 	 	{   
            double l_canefw = (g_dm_green[sstem] + g_dm_green[sucrose]
                    +  g_dm_dead[sstem] + g_dm_dead[sucrose]    //! Add dead pool for lodged crops
                    +  g_plant_wc[sstem]) * g2t/sm2ha;
            return l_canefw;
            }
         }



     [Output] [Units("(%)")] double ccs
 	    { 
 	 	 get 
 	 	 	 { 
             double l_canefw = (g_dm_green[sstem] + g_dm_green[sucrose] + g_plant_wc[sstem]);  //TODO: is this missing the dead pool for lodged crops as in canefw property above/ also g2t conversion?
             double l_scmstf = MathUtility.Divide(g_dm_green[sucrose], l_canefw, 0.0);
             double l_ccs = 1.23 * l_scmstf - 0.029;
             l_ccs = l_bound(l_ccs, 0.0);
             l_ccs = l_ccs * 100.0;          //! convert to %             
             return l_ccs;
             }
         }


     [Output] [Units("(g/g)")] double scmstf
 	    { 
 	 	 get 
 	 	 	{
            double l_canefw = (g_dm_green[sstem] + g_dm_green[sucrose] + g_plant_wc[sstem]);
            double l_scmstf = MathUtility.Divide(g_dm_green[sucrose], l_canefw, 0.0);
            return l_scmstf;
            }
         }


     [Output] [Units("(g/g)")] double scmst
 	    { 
 	 	 get 
 	 	 	{
            double l_cane_wt = g_dm_green[sstem] + g_dm_green[sucrose];
            double l_scmst = MathUtility.Divide(g_dm_green[sucrose], l_cane_wt, 0.0);
            return l_scmst;
            }
         }


     [Output] [Units("(g/m^2)")] double sucrose_wt
 	 	 { get { return g_dm_green[sucrose] + g_dm_dead[sucrose]; } }  //! Add dead pool to allow for lodged stalks


     [Output][Units("(g/m^2)")] double cabbage_wt
 	 	 { get { return g_dm_green[cabbage]; } }


     [Output]  [Units("(g/m^2)")] double cane_wt
 	    { 
 	 	 get 
 	 	 	 { 
             double l_cane_wt = g_dm_green[sstem]+ g_dm_green[sucrose] 
                            + g_dm_dead[sstem]+ g_dm_dead[sucrose];   //! Add dead pool for lodged crops
             return l_cane_wt;
             }
         }

     [Output] [Units("(g/m^2)")] double biomass
 	    { 
 	 	 get 
 	 	 	{
            double l_biomass =    SumArray(g_dm_green,max_part) - g_dm_green[root]
                              + SumArray(g_dm_senesced,max_part) - g_dm_senesced[root]
                              + SumArray(g_dm_dead,max_part) - g_dm_dead[root];
             return l_biomass;
             }
         }


     [Output] [Units("(g/m^2)")] double green_biomass
 	    { 
 	 	get 
 	 	 	{  
            double l_biomass = SumArray(g_dm_green,max_part) - g_dm_green[root]
                             + g_dm_dead[sstem] + g_dm_dead[sucrose];  //! Add dead pool for lodged crops
            return l_biomass;
            }
         }


     [Output]  [Units("(g/m^2)")] double greenwt
 	 	 { get { return SumArray(g_dm_green, max_part); } }


     [Output] [Units("(g/m^2)")] double senescedwt
 	 	 { get { return SumArray(g_dm_senesced, max_part); } }


     [Output][Units("(g/m^2)")] double dm_dead
 	 	 { get { return SumArray(g_dm_dead, max_part); } }


     [Output] [Units("(g/m^2)")] double dlt_dm
 	 	 { get { return g_dlt_dm; } }


     [Output] [Units("(g/m^2)")] double partition_xs
 	 	 { get { return g_partition_xs; } }


     [Output]  [Units("(g/m^2)")] double dlt_dm_green
 	 	 { get { return SumArray(g_dlt_dm_green, max_part); } }


     [Output] [Units("(g/m^2)")] double[]  dlt_dm_detached
 	 	 { get { return g_dlt_dm_detached; } }




            // Reporting of N concentrations
            //******************************


     [Output] [Units("(g/g)")] double[] n_critical
 	 	 { get { return g_n_conc_crit; } }

     [Output] [Units("(g/g)")] double[] n_minimum
 	 	 { get { return g_n_conc_min; } }


     [Output] [Units("(g/m^2)")] double n_conc_leaf
 	 	 { 
         get 
            {
             double l_Conc_N_leaf = MathUtility.Divide(g_n_green[leaf], g_dm_green[leaf], 0.0);
             return l_Conc_N_leaf;
             }
         }

     [Output] [Units("(g/m^2)")] double n_conc_cab
 	    { 
 	 	 get 
 	 	 	{
            double l_Conc_N_cab = MathUtility.Divide(g_n_green[cabbage], g_dm_green[cabbage], 0.0);
            return l_Conc_N_cab;
             }
         }

     [Output]  [Units("(g/m^2)")] double n_conc_cane
 	    { 
 	 	 get 
 	 	 	 {
             double l_Conc_N_cane = MathUtility.Divide(g_n_green[sstem]+g_n_green[sucrose], g_dm_green[sstem]+g_dm_green[sucrose], 0.0);
             return l_Conc_N_cane;
             }
         }

            //Weights of N in plant

     [Output] [Units("(g/m^2)")] double n_leaf_crit
 	    { 
 	 	get 
 	 	    {
            double l_N_leaf_crit = g_n_conc_crit[leaf] * g_dm_green[leaf];
            return l_N_leaf_crit;
            }
         }

     [Output] [Units("(g/m^2)")] double n_leaf_min
 	    { 
 	 	 get 
 	 	 	{
             double l_N_leaf_min = g_n_conc_min[leaf] * g_dm_green[leaf];
             return l_N_leaf_min;
             }
         }


     [Output]  [Units("(g/m^2)")] double biomass_n
 	    { 
 	 	 get 
 	 	 	{
            double l_biomass_n =  SumArray(g_n_green,max_part) - g_n_green[root]
                              + SumArray(g_n_senesced,max_part) - g_n_senesced[root]
                              + SumArray(g_n_dead,max_part) - g_n_dead[root];
             return l_biomass_n;
             }
         }


     [Output]  [Units("(g/m^2)")] double plant_n_tot
 	    { 
 	 	 get 
 	 	 	 { 
            double l_plant_n_tot = SumArray(g_n_green,max_part)
                                + SumArray(g_n_senesced,max_part)
                                + SumArray(g_n_dead,max_part);
            return l_plant_n_tot;
             }
         }


     [Output] [Units("(g/m^2)")] double green_biomass_n
 	    { 
 	 	 get 
 	 	 	 {
             double l_green_biomass_n = SumArray(g_n_green, max_part) - g_n_green[root];
             return l_green_biomass_n;
             }
         }


     [Output] [Units("(g/m^2)")] double[] n_green
 	 	 { get { return g_n_green; } }


     [Output] [Units("(g/m^2)")] double greenn 
 	 	 { get { return  SumArray(g_n_green, max_part); } }


     [Output][Units("(g/m^2)")] double senescedn
        { get { return  SumArray(g_n_senesced, max_part); } }


            //Delta N in plant tops

     [Output] [Units("(g/m^2)")] double[] dlt_n_green
 	 	 { get { return g_dlt_n_green; } }




        //Stress Outputs
        //**************


     [Output] [Units("()")] double swdef_pheno
 	 	 { get { return g_swdef_pheno; } }


     [Output]  [Units("()")] double swdef_photo
 	 	 { get { return g_swdef_photo; } }


     [Output] [Units("()")] double swdef_expan
 	 	 { get { return g_swdef_expansion; } }


     [Output]  [Units("()")] double swdef_stalk
 	 	 { get { return g_swdef_stalk; } }


     [Output]  [Units("()")] double nfact_photo
 	 	 { get { return g_nfact_photo; } }


     [Output]  [Units("()")]double nfact_expan
 	 	 { get { return g_nfact_expansion; } }

     //See -> Set My Variables for the following 3

     //[Output]  [Units("()")]double lodge_redn_photo
     //    { get { return g_lodge_redn_photo; } }


     //[Output]  [Units("()")] double lodge_redn_sucrose
     //    { get { return g_lodge_redn_sucrose; } }


     //[Output]  [Units("()")] double lodge_redn_green_leaf
     //    { get { return g_lodge_redn_green_leaf; } }


     [Output] [Units("(0-1)")] double oxdef_photo
 	 	 { get { return g_oxdef_photo; } }



        //Water Outputs
        //*************


     [Output]  [Units("(mm)")] double ep
 	    { 
 	 	 get 
 	 	 	 { 
             int num_layers = count_of_real_vals (dlayer, max_layer);
             double l_ep = Math.Abs(SumArray(g_dlt_sw_dep, num_layers));
             return l_ep;
             }
         }

    
    [Output] [Units("(mm)")]double cep
 	 	 { get { return - g_transpiration_tot; } }


     [Output]  [Units("(mm)")] double[] sw_uptake
 	    { 
 	 	 get 
 	 	 	{
            int num_layers = count_of_real_vals (dlayer, max_layer);
            double[] l_rwu = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                {
                l_rwu[layer] = - g_dlt_sw_dep[layer];
                }
            return l_rwu;
            }
         }


     [Output][Units("(mm)")] double sw_demand
 	 	 { get { return g_sw_demand; } }


     [Output]  [Units("(mm)")] double sw_demand_te
 	 	 { get { return g_sw_demand_te; } }


     [Output] [Units("(0-1)")] double fasw
 	    { 
 	 	 get 
 	 	 	 {
             double fasw = sugar_profile_fasw();
             return fasw;
             }
         }

     [Output]  [Units("(mm)")]double[] esw_layr
        { 
 	 	 get 
 	 	 	{
            int num_layers = count_of_real_vals (dlayer, max_layer);
            double[] l_esw_layr = new double[num_layers];

            if (g_crop_status != crop_out) 
                {   
                for (int layer = 0; layer < num_layers; layer++)
                    {
                    l_esw_layr[layer] = Math.Max(0.0, sw_dep[layer] - g_ll_dep[layer]);
                    }
                }
             else
                {
                ZeroArray(ref l_esw_layr);
                }


            return l_esw_layr;
             }
         }





            //! plant nitrogen
            //****************

    //TODO: This is always 0 because g_N_uptake_tot is always 0 because no accumulation is actually ever done. 
    //Obviously no one uses this output variable because no one has compalined about this bug. Probably should be n_uptake = no3_uptake + nh4_uptake
    //if you remove it, also remove g_N_uptake_tot

     //[Output][Units("(kg/ha)")] double n_uptake
     //   { 
     //    get 
     //        {
     //        double act_N_up = g_N_uptake_tot * gm2kg /sm2ha;
     //        return act_N_up;
     //        }
     //    }


     [Output]  [Units("(g/m^2)")] double no3_tot
 	    { 
 	 	 get 
 	 	 	{ 
            int deepest_layer_ob = FindLayerNo_ob(g_root_depth);
            double l_NO3gsm_tot = SumArray(g_no3gsm, deepest_layer_ob);             
            return l_NO3gsm_tot;
             }
         }


     [Output]  [Units("(g/m^2)")] double n_demand
 	    { 
 	 	 get 
 	 	 	{ 
            double l_N_demand = SumArray (g_n_demand, max_part);
            return l_N_demand;
            }
         }


     [Output] [Units("(kg/ha)")] double no3_demand
 	    { 
 	 	 get 
 	 	 	{
            double l_N_demand = SumArray(g_n_demand, max_part)*10.0;
            return l_N_demand;
            }
         }


     [Output]  [Units("(g/m^2)")] double n_supply
 	    { 
 	 	 get 
 	 	 	{
            double l_N_supply = SumArray( g_dlt_n_green, max_part) - g_dlt_n_green[root];
            return l_N_supply;
            }
         }


     [Output] [Units("(g/m2)")] double[] no3_uptake
 	    { 
         get 
            { 
            int num_layers = count_of_real_vals (dlayer, max_layer);
            double[] l_NO3_uptake = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                {
                l_NO3_uptake[layer] = - g_dlt_no3gsm[layer];
                }
             return l_NO3_uptake;
             }
         }
                               

     [Output] [Units("(g/m2)")] double[] nh4_uptake
 	    { 
         get 
            { 
            int num_layers = count_of_real_vals (dlayer, max_layer);
            double[] l_NH4_uptake = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                {
                l_NH4_uptake[layer] = - g_dlt_nh4gsm[layer];
                }
             return l_NH4_uptake;
             }
         }



     [Output]  [Units("(g/m2)")] double[] no3_uptake_pot
 	    { 
         get 
             {
             int num_layers = count_of_real_vals (dlayer, max_layer);
             double[] l_NO3_uptake_pot = new double[num_layers];
             Array.Copy(g_no3gsm_uptake_pot, l_NO3_uptake_pot, num_layers);
             return l_NO3_uptake_pot; 
             } 
         }


     [Output]  [Units("(g/m2)")] double[] nh4_uptake_pot
 	    { 
         get  
             {
             int num_layers = count_of_real_vals(dlayer, max_layer);
             double[] l_NH4_uptake_pot = new double[num_layers];
             Array.Copy(g_nh4gsm_uptake_pot, l_NH4_uptake_pot, num_layers);
             return l_NH4_uptake_pot;
             } 
         }





    #endregion






     //EVENT HANDLERS


    #region Clock Event Handlers



      #region Functions used in Clock Handlers


          void CheckAllNUptakeOptionalsReadIn()
              {
              //sugar_read_constants () has an "if" statment in it. 
              //I have replicated this by trying to read in both lots of variables in (for either case of the if statement) from the ini file. 
              //This function is then used to make sure that given the specific "n_uptake_option" that all the variables this specific option needs has non zero values. 
              if (n_uptake_option == 1)
                  {
                  if ((NO3_diffn_const == Double.NaN) || (n_supply_preference == ""))
                      throw new Exception("Using n_uptake_option == 1 and missing either 'NO3_diffn_const' or 'n_supply_preference' from ini file");
                  }
              else
                  {
                  if ((kno3 == Double.NaN) || (no3ppm_min == Double.NaN) || (knh4 == Double.NaN) || (nh4ppm_min == Double.NaN) || (total_n_uptake_max == Double.NaN))
                      throw new Exception("Using n_uptake_option == 2 and missing either 'kno3', 'no3ppm_min', 'knh4', 'nh4ppm_min' or 'total_n_uptake_max' from ini file");
                  }
              }

    


          void sugar_get_soil_variables(ref double[] o_no3gsm, ref double[] o_no3gsm_min, ref double[] o_nh4gsm, ref double[] o_nh4gsm_min)
              {


                //! Soil Water module
                //! -----------------


              //TODO: implement the erosion stuff using a .net event handler rather than just doing another get and seeing if the number of layers has changed.

              //sv- See Erosion Event Handler for where this has been moved to and commented out. 
              
              //Figure out a .NET event way to implement this.



              
              //! soil nitrogen module
              //! --------------------


              // had to put these here because can do it in the INPUTS -> //! soil nitrogen module
              //todays value of NO3, NO3_min, NH4, NH4_min should have been read in from the SoilN module before the Prepare Event is fired.
              for (int layer = 0; layer < num_layers; layer++)
                  {
                  o_no3gsm[layer] = no3[layer] * kg2gm / ha2sm;
                  }

              for (int layer = 0; layer < num_layers; layer++)
                  {
                  o_no3gsm_min[layer] = no3_min[layer] * kg2gm / ha2sm;
                  }

              for (int layer = 0; layer < num_layers; layer++)
                  {
                  o_nh4gsm[layer] = nh4[layer] * kg2gm / ha2sm;
                  }

              for (int layer = 0; layer < num_layers; layer++)
                  {
                  o_nh4gsm_min[layer] = nh4_min[layer] * kg2gm / ha2sm;
                  }
              }






      #endregion



        [EventHandler]
        public void OnInitialised()
            {
            //sv-taken from OnTick event handler
            g_day_of_year = Clock.Today.DayOfYear;
            g_year = Clock.Today.Year;


            //*     ===========================================================
            //      subroutine sugar_init ()
            //*     ===========================================================

            //*+  Purpose
            //*       Crop initialisation


            sugar_zero_variables();
            sugar_zero_soil_globals();

            Console.WriteLine(" Initialising");

                //! initialize crop variables

            CheckAllNUptakeOptionalsReadIn();   //sugar_read_constants() - (this is what is left of sugar_read_constants() that can't be done using [Param])

            g_current_stage = Convert.ToDouble(crop_end);
            g_crop_status = crop_out;

            }



        [EventHandler]
        public void OnPrepare()
            {

            //sv- taken from OnTick event handler
            g_day_of_year = Clock.Today.DayOfYear;
            g_year = Clock.Today.Year;
            sugar_zero_daily_variables();

            Console.WriteLine("day of year: " + g_day_of_year);




            //* ====================================================================
            //       subroutine sugar_prepare ()
            //* ====================================================================

            //*+  Purpose
            //*     APSim allows modules to perform calculations in preparation for
            //*     the standard APSim timestep.  This model uses this opportunity
            //*     to calculate potential growth variables for the coming day
            //*     and phenological development.

            //*+  Mission Statement
            //*     Perform preparatory calculations for the next timestep



            if (g_crop_status == crop_alive)
                {

                sugar_get_soil_variables(ref g_no3gsm, ref g_no3gsm_min, ref g_nh4gsm, ref g_nh4gsm_min);


                //NITROGEN STRESS FACTOR CALCULATIONS    ( Get current Nitrogen stress "Factors" (0-1) )

                //sugar_nit_stress_photo(1);           //*     Nitrogen stress factors for photosynthesis
                sugar_nfact(g_dm_green, g_n_conc_crit, g_n_conc_min, g_n_green, crop.k_nfact_photo, ref g_nfact_photo);
                //sugar_nit_stress_expansion(1);       //*     Nitrogen stress factors for cell expansion
                sugar_nfact(g_dm_green, g_n_conc_crit, g_n_conc_min, g_n_green, crop.k_nfact_expansion, ref g_nfact_expansion);
                //sugar_nit_stress_pheno(1);           //*     Nitrogen stress factors for phenology
                sugar_nfact(g_dm_green, g_n_conc_crit, g_n_conc_min, g_n_green, crop.k_nfact_pheno, ref g_nfact_pheno);
                //sugar_nit_stress_stalk(1);           //*     Nitrogen stress factors for stalk
                sugar_nfact(g_dm_green, g_n_conc_crit, g_n_conc_min, g_n_green, crop.k_nfact_stalk, ref g_nfact_stalk);



                //TEMPERATURE STRESS FACTOR CALCULATIONS   ( Get current temperature stress factors (0-1) )

                //sugar_temp_stress_photo(1);          //*     temperature stress factors for photosynthesis
                sugar_temperature_stress(crop.num_ave_temp, crop.x_ave_temp, crop.y_stress_photo, maxt, mint, ref g_temp_stress_photo);
                //sugar_temp_stress_stalk(1);          //*     temperature stress factors for stalk
                sugar_temperature_stress(crop.num_ave_temp_stalk, crop.x_ave_temp_stalk, crop.y_stress_stalk, maxt, mint, ref g_temp_stress_stalk);



                //LODGING REDUCTION FACTOR CALCULATION    ( Get current effect of lodging on photosynthesis (0-1) )

                //sugar_lodge_redn_photo(1);           //*     current effect of lodging on photosynthesis
                if (g_lodge_flag)
                    g_lodge_redn_photo = crop.lodge_redn_photo;
                else
                    g_lodge_redn_photo = 1.0;

                //sugar_lodge_redn_sucrose(1);         //*     current effect of lodging on sucrose growth
                if (g_lodge_flag)
                    g_lodge_redn_sucrose = crop.lodge_redn_sucrose;
                else
                    g_lodge_redn_sucrose = 1.0;

                //sugar_lodge_redn_green_leaf(1);      //*     current effect of lodging on green leaf number
                if (g_lodge_flag)
                    g_lodge_redn_green_leaf = crop.lodge_redn_green_leaf;
                else
                    g_lodge_redn_green_leaf = 1.0;





                //RADIATION INTERCEPTION BY LEAVES

                //sugar_light_supply(1);
                g_radn_int = sugar_radn_int(crop.extinction_coef, fr_intc_radn_, g_lai, radn);



                //WATER LOGGING     //sv- also called in Process Event.

                //sugar_water_log(1);       
                g_oxdef_photo = crop_oxdef_photo1(crop.oxdef_photo, crop.oxdef_photo_rtfr, ll15_dep, sat_dep, sw_dep, dlayer, g_root_length, g_root_depth);



                //BIOMASS(DRY MATTER) PRODUCTION     //sv- also called in Process Event.

                //sugar_bio_RUE(1);         
                g_dlt_dm_pot_rue = sugar_dm_pot_rue(crop.rue, g_current_stage, g_radn_int, g_nfact_photo, g_temp_stress_photo, g_oxdef_photo, g_lodge_redn_photo);
                g_dlt_dm_pot_rue_pot = sugar_dm_pot_rue_pot(crop.rue, g_current_stage, g_radn_int);



                //TRANSPIRATION EFFICENCY (based on today's weather)

                //sugar_transpiration_eff(1);
                g_transp_eff = cproc_transp_eff1(crop.svp_fract, crop.transp_eff_cf, g_current_stage, maxt, mint);



                //SW DEMAND (Atomospheric Potential)

                //sugar_water_demand(1);
                g_sw_demand = sugar_water_demand(g_dlt_dm_pot_rue, g_transp_eff, g_lai, eo);




                //NITROGEN DEMAND

                //sugar_nit_demand_est (1)
                sugar_nit_demand_est(g_dm_green, g_dlt_dm_pot_rue_pot, g_n_conc_crit, g_n_green, ref g_n_demand);

                }

            else
                {
                sugar_get_soil_variables(ref g_no3gsm, ref g_no3gsm_min, ref g_nh4gsm, ref g_nh4gsm_min);
                }


            }





        [EventHandler]
        public void OnProcess()
            {


            if (g_crop_status != crop_out)
                {


                sugar_get_soil_variables(ref g_no3gsm, ref g_no3gsm_min, ref g_nh4gsm, ref g_nh4gsm_min);



                //ROOT GROWTH

                //sugar_root_depth(1)
                sugar_root_depth_growth(dlayer,
                                         crop.x_sw_ratio, crop.y_sw_fac_root,
                                         crop.x_afps, crop.y_afps_fac,
                                         dul_dep, sw_dep, g_ll_dep,
                                         crop.root_depth_rate, g_current_stage, xf,
                                         out g_dlt_root_depth, g_root_depth);


                //sugar_root_Depth_init(1)  //! after because it sets the delta //!NOTE THIS IS STILL THE DELTA
                sugar_init_root_depth(dlayer, g_root_length, g_root_depth, ref g_dlt_root_depth);





                //POTENTIAL WATER AVAILABLE TO ROOTS

                //sugar_water_supply(1)

                //c+!!!!!!!!! check order dependency of delta
                cproc_sw_supply1(sw_dep_lb, dlayer, g_ll_dep, dul_dep, sw_dep, g_root_depth, kl
                                , ref g_sw_avail
                                , ref g_sw_avail_pot
                                , ref g_sw_supply
                                );
               


                //TODO: FIGURE OUT HOW TO DO THIS LATER WHEN YOU FIGURE OUT HOW TO TALK WITH SWIM
                if ((uptake_source == "apsim") || (uptake_source == "swim3"))
                    {
                    GetSupplyFromSWIM();
                    }





                //ACTUAL WATER EXTRATION BY PLANT (Limited by Roots ability to extract and Plant Demand)

                //sugar_water_uptake(1)

               

                if (uptake_source == "calc")
                    {
                    cproc_sw_uptake1(dlayer, g_root_depth, g_sw_demand, g_sw_supply, ref g_dlt_sw_dep);
                    }
                else
                    {
                    GetUptakeFromSWIM();
                    }
             



                //WATER STRESS FACTOR CALCULATIONS     ( Get current water stress factors (0-1) )

                //sugar_water_stress_expansion(1)       //*     water stress factors for expansion
                crop_swdef_expansion(crop.x_sw_demand_ratio, crop.y_swdef_leaf, g_root_depth, g_sw_demand, g_sw_supply, ref g_swdef_expansion);
                //sugar_water_stress_stalk(1)           //*     water stress factors for stalk
                sugar_swdef_demand_ratio(crop.x_demand_ratio_stalk, crop.y_swdef_stalk, g_root_depth, g_sw_demand, g_sw_supply, ref g_swdef_stalk);
                //sugar_water_stress_pheno (1)          //*     water stress factors for phenology
                crop_swdef_pheno(crop.x_sw_avail_ratio, crop.y_swdef_pheno, g_root_depth, g_sw_avail, g_sw_avail_pot, ref g_swdef_pheno);
                //sugar_water_stress_photo (1)          //*     water stress factors for photosynthesis
                crop_swdef_photo(g_root_depth, g_sw_demand, g_sw_supply, ref g_swdef_photo);





                if (g_crop_status == crop_alive)
                    {

                    //SET MINIMUM STRUCTURAL STEM SUCROSE

                    //sugar_min_sstem_sucrose(1)
                    sugar_min_sstem_sucrose(ref g_min_sstem_sucrose);



                    //PHENOLOGY

                    //sugar_phenology_init(1)
                    sugar_phen_init(crop.shoot_lag, crop.shoot_rate, g_current_stage, g_sowing_depth, g_ratoon_no,
                        cult.tt_begcane_to_flowering, cult.tt_emerg_to_begcane, cult.tt_flowering_to_crop_end, ref g_phase_tt);

                   

                    //sugar_phenology(1)  //cproc_phenology1 ()
                    //Start - cproc_phenology1 ()

                    g_previous_stage = g_current_stage;

                    //! get thermal times

                 
                    g_dlt_tt = crop_thermal_time(crop.x_temp, crop.y_tt, g_current_stage, maxt, mint, emerg, flowering, g_nfact_pheno, g_swdef_pheno);
                    

                    g_phase_devel = crop_phase_devel(sowing, sprouting, flowering,
                                                        crop.pesw_germ, crop.fasw_emerg, crop.rel_emerg_rate, crop.num_fasw_emerg, g_current_stage, g_days_tot,
                                                        dlayer, max_layer, g_sowing_depth, sw_dep, dul_dep, g_ll_dep, ref g_dlt_tt, g_phase_tt, g_tt_tot);
                   
                   
                    crop_devel(g_current_stage, max_stage, g_phase_devel, out g_dlt_stage, ref g_current_stage);


                    //! update thermal time states and day count

                    accumulate_ob(g_dlt_tt, ref g_tt_tot, g_previous_stage, g_dlt_stage);

                    accumulate_ob(1.0, ref g_days_tot, g_previous_stage, g_dlt_stage);
                    Console.WriteLine("das: " + sum_between_zb(zb(sowing), zb(now), g_days_tot));
                    PrintArray("days_tot: ", g_days_tot);
                    Console.WriteLine("dlt_stage: " + g_dlt_stage);

                    //End - cproc_phenology1 ()
                  


                    //CROP HEIGHT

                    //sugar_height(1)
                    g_dlt_canopy_height = cproc_canopy_height(g_canopy_height, crop.x_stem_wt, crop.y_height, g_dm_green, g_plants, sstem);





                    //LEAF NUMBER

                    //sugar_leaf_no_init(1)
                    cproc_leaf_no_init1(crop.leaf_no_at_emerg, g_current_stage, emerg, ref g_leaf_no_zb, ref g_node_no_zb);     //! initialise total leaf number

                    //sugar_leaf_no_pot(1)
                    cproc_leaf_no_pot1(crop.x_node_no_app, crop.y_node_app_rate, crop.x_node_no_leaf, crop.y_leaves_per_node,
                                        g_current_stage, emerg, flowering, emerg, g_dlt_tt, g_node_no_zb,
                                        out g_dlt_leaf_no, out g_dlt_node_no);

                  


                    //LEAF AREA POTENTIAL

                    //sugar_leaf_area_init(1)
                    if (on_day_of(emerg, g_current_stage))
                        {
                        g_lai = crop.initial_tpla * smm2sm * g_plants;
                        g_leaf_area_zb[0] = crop.initial_tpla;
                        }

                    //sugar_leaf_area_potential(1)
                    g_dlt_lai_pot = sugar_leaf_area_devel(crop.leaf_no_correction, g_dlt_leaf_no, g_leaf_no_zb, g_plants,
                                        cult.leaf_size, cult.leaf_size_no,
                                        cult.tillerf_leaf_size, cult.tillerf_leaf_size_no);

                   




                    //BIOMASS(DRY MATTER) PRODUCTION (restricted by water stress -> biomass (grams) = transp_eff(g/mm) * supply(mm))

                    //sugar_bio_water(1)
                    g_dlt_dm_pot_te = cproc_bio_water1(g_root_depth, g_sw_supply, g_transp_eff);



                    //WATER LOGGING

                    //sugar_water_log(1)
                    g_oxdef_photo = crop_oxdef_photo1(crop.oxdef_photo, crop.oxdef_photo_rtfr, ll15_dep, sat_dep, sw_dep, dlayer, g_root_length, g_root_depth);



                    //BIOMASS(DRY MATTER) PRODUCTION (with no water stress but with every other type of stress)

                    //sugar_bio_RUE(1);
                    g_dlt_dm_pot_rue = sugar_dm_pot_rue(crop.rue, g_current_stage, g_radn_int, g_nfact_photo, g_temp_stress_photo, g_oxdef_photo, g_lodge_redn_photo);
                    g_dlt_dm_pot_rue_pot = sugar_dm_pot_rue_pot(crop.rue, g_current_stage, g_radn_int);

                    //sugar_bio_actual(1); //sv- get initial value
                    sugar_dm_init(crop.dm_cabbage_init, crop.dm_leaf_init, crop.dm_sstem_init, crop.dm_sucrose_init, crop.specific_root_length,
                                g_current_stage, dlayer, g_plants, g_root_length,
                                ref g_dm_green, ref g_leaf_dm_zb);


                    //BIOMASS(DRY MATTER) PRODUCTION (choose between "water stressed" and "non water stressed")  

                

                    //! use whichever is limiting
                    //sugar_bio_actual(1);
                    g_dlt_dm = Math.Min(g_dlt_dm_pot_rue, g_dlt_dm_pot_te);
                  



                    //LEAF AREA STRESSED

                    //sugar_leaf_area_stressed(1)      //cproc_leaf_area_stressed1()
                    g_dlt_lai_stressed = g_dlt_lai_pot * Math.Min(g_swdef_expansion, g_nfact_expansion);

 



                    //BIOMASS(DRY MATTER) PARTITIONING

                    //sugar_bio_partition(1)

                    double l_leaf_no_today_ob;
                    bool l_didInterpolate;


                    l_leaf_no_today_ob = sum_between_zb(zb(emerg), zb(now), g_leaf_no_zb) + g_dlt_leaf_no;

                    g_sla_min = MathUtility.LinearInterpReal((double)l_leaf_no_today_ob, crop.sla_lfno, crop.sla_min, out l_didInterpolate);   // sugar_sla_min()  //Return the minimum specific leaf area (mm^2/g) of a specified leaf no.

                    g_sucrose_fraction = sugar_sucrose_fraction(cult.stress_factor_stalk, cult.sucrose_fraction_stalk, 
                                                                g_swdef_stalk, g_nfact_stalk, g_temp_stress_stalk, g_lodge_redn_sucrose);


                    sugar_dm_partition_rules(cult.cane_fraction, crop.leaf_cabbage_ratio, g_min_sstem_sucrose, crop.ratio_root_shoot, cult.sucrose_delay,
                                              g_current_stage, g_dm_green, g_sla_min, g_sucrose_fraction, g_tt_tot, g_dlt_dm, g_dlt_lai_stressed, 
                                              ref g_dlt_dm_green, ref g_partition_xs); //sugar_dm_partition()

         




                    //BIOMASS(DRY MATTER) RETRANSLOCATION         

                    //sugar_bio_retrans(1)     //sugar_dm_retranslocate()

                    //! now translocate carbohydrate between plant components

                    fill_real_array(ref g_dlt_dm_green_retrans, 0.0, max_part);

                    //! now check that we have mass balance

                    double l_mass_balance = SumArray(g_dlt_dm_green_retrans, max_part);   //! sum of translocated carbo (g/m^2)

                    bound_check_real_var(l_mass_balance, 0.0, 0.0, "dm_retranslocate mass balance");



                    //LEAF AREA ACTUAL (Limited by Carbon Supply)

                    //sugar_leaf_actual(1)

                    g_dlt_lai = sugar_leaf_area(g_dlt_dm_green, g_dlt_lai_stressed, g_dlt_leaf_no, g_leaf_no_zb, crop.sla_lfno, crop.sla_max);




                    //ROOT DISTRIBUTION

                    //sugar_root_dist(1)
                    cproc_root_length_growth1(ref g_dlt_root_length,
                                             crop.specific_root_length, dlayer,
                                             g_dlt_dm_green[root], g_dlt_root_depth, g_root_depth, g_root_length, g_plants, xf,
                                             crop.x_sw_ratio, crop.y_sw_fac_root, crop.x_plant_rld, crop.y_rel_root_rate,
                                             dul_dep, sw_dep, g_ll_dep, max_layer);




                    //LEAF AREA SENESCENCE (leaf AREA lost) (leaf AREA as opposed to leaf DRY MATTER)  //!!!! LEAF AREA SEN !!!!

                    //sugar_leaf_death(1)
                    g_dlt_node_no_dead = sugar_leaf_death_grass(cult.green_leaf_no, g_lodge_redn_green_leaf, g_current_stage, g_dlt_leaf_no, g_leaf_no_zb, g_node_no_dead_zb);
                  


                    //sugar_leaf_area_sen(1)
                    //Start - sugar_leaf_area_sen(1)

                    //*+  Purpose
                    //*      Calculate Leaf Area Senescence

                    g_dlt_slai_age = sugar_leaf_area_sen_age0(g_dlt_node_no_dead, g_lai, g_leaf_area_zb, g_node_no_dead_zb, g_plants, g_slai, g_node_no_detached_ob, crop.leaf_no_at_emerg);

                    g_dlt_slai_water = crop_leaf_area_sen_water1(crop.sen_rate_water, g_lai, g_swdef_photo, g_plants, 0.0);

                    g_dlt_slai_light = crop_leaf_area_sen_light1(crop.lai_sen_light, crop.sen_light_slope, g_lai, g_plants, 0.0);

                    g_dlt_slai_frost = crop_leaf_area_sen_frost1(crop.frost_temp, crop.frost_fraction, g_lai, mint, g_plants, 0.0);

                    //! now take largest of deltas
                    g_dlt_slai = max(g_dlt_slai_age, g_dlt_slai_light, g_dlt_slai_water, g_dlt_slai_frost);

                    //End - sugar_leaf_area_sen(1)




                    //BIOMASS SENESCENCE (dm lost) (eg. leaf, cabbage, root) (nb. Leaf is the weight senesced not the area [as above]) 

                    //sugar_sen_bio(1)
                    sugar_dm_senescence(crop.dm_root_sen_frac, crop.leaf_cabbage_ratio, crop.cabbage_sheath_fr,
                                                g_dlt_dm_green, g_dlt_lai, g_dlt_slai, g_dm_green, g_dm_senesced,
                                                g_lai, g_leaf_dm_zb, g_plants, g_slai, g_leaf_area_zb, ref g_dlt_dm_senesced);



                    //NITROGEN SENESCENCE (nitrogen lost when dry matter is lost)

                    //sugar_sen_nit(1)
                    //Start - sugar_sen_nit(1)
                    //*+  Mission Statement
                    //*     Get the senesced plant nitrogen content

                    //      sugar_N_senescence()
                    //*+  Purpose
                    //*       Derives seneseced plant nitrogen (g N/m^2)

                    //! first we zero all plant component deltas
                    fill_real_array(ref g_dlt_n_senesced, 0.0, max_part);

                    //dlt_N_senesced[] is actual nitrogen senesced from plant parts (g/m^2)
                    g_dlt_n_senesced[leaf] = g_dlt_dm_senesced[leaf] * crop.n_leaf_sen_conc;
                    g_dlt_n_senesced[cabbage] = g_dlt_dm_senesced[cabbage] * crop.n_cabbage_sen_conc;
                    g_dlt_n_senesced[root] = g_dlt_dm_senesced[root] * crop.n_root_sen_conc;

                    //cnh what checks are there that there is enough N in plant to provide this

                    //End - sugar_sen_nit(1)



                    //ROOT LENGTH SENESCENCE (root length lost)

                    //sugar_sen_root_length (1)
                    cproc_root_length_senescence1(crop.specific_root_length, dlayer, g_dlt_dm_senesced[root], g_root_length, g_root_depth, ref g_dlt_root_length_senesced);



                    //NITROGEN PROCESSES

                    //sugar_nit_retrans (1)
                    sugar_N_retranslocate(g_dm_green, g_n_conc_min, g_n_green, ref g_dlt_n_retrans);

                    //sugar_nit_Demand(1)   //defunct code

                    //sugar_nit_supply (c%n_uptake_option)
                    sugar_nit_supply(n_uptake_option);

                    //sugar_nit_init (1)
                    //*+  Mission Statement
                    //    *     Get the initial plant nitrogen information
                    sugar_N_init(crop.n_cabbage_init_conc, crop.n_leaf_init_conc, crop.n_root_init_conc, crop.n_sstem_init_conc, g_current_stage, g_days_tot, g_dm_green, ref g_n_green);

                    //sugar_nit_uptake (c%n_uptake_option)
                    sugar_nit_uptake(n_uptake_option);

                    //sugar_nit_partition (1)
                    //*+  Mission Statement
                    //*     Get the nitrogen partitioning information
                    sugar_N_partition(dlayer, g_dlt_no3gsm, g_dlt_nh4gsm, g_n_demand, g_root_depth, ref g_dlt_n_green);



                    //WATER CONTENT OF CANE

                    //sugar_water_content_cane (1)
                    sugar_water_content(crop.cane_dmf_tt, crop.cane_dmf_min, crop.cane_dmf_max, crop.num_cane_dmf, crop.cane_dmf_rate,
                        g_swdef_stalk, g_nfact_stalk, g_temp_stress_stalk, g_dlt_dm_green, g_dm_green, g_dlt_plant_wc,
                        ref g_plant_wc, g_tt_tot);



                    //PLANT DEATH

                    //sugar_plant_death (1)
                    sugar_plant_death();


                    //REALLOCATE BETWEEN LEAF AND CABBAGE (needed for: DETACH LEAF AND CABBAGE)

                    //call sugar_realloc (1)
                    //*+  Purpose
                    //*       Reallocate cabbage to cane as plant develops to maintain a fixed leaf:cabbage ratio
                    sugar_realloc_cabbage(leaf, cabbage, sstem, max_part, crop.cabbage_sheath_fr, g_dm_green, g_dlt_dm_senesced, g_n_green, ref g_dlt_dm_realloc, ref g_dlt_n_realloc);

                    }



                //DETACH LEAF AND CABBAGE

                //sugar_detachment (1)
                //*     Calculate plant detachment


                if (!mu.reals_are_equal(crop.sen_detach_frac[leaf], crop.sen_detach_frac[cabbage]))
                    {
                    throw new Exception("Invalid detachment for leaf and cabbage ratio.");
                    }

                cproc_dm_detachment1(max_part,
                                    crop.sen_detach_frac, g_dm_senesced, ref g_dlt_dm_detached,
                                    crop.dead_detach_frac, g_dm_dead, ref g_dlt_dm_dead_detached);

                cproc_n_detachment1(max_part,
                                    crop.sen_detach_frac, g_n_senesced, ref g_dlt_n_detached,
                                    crop.dead_detach_frac, g_n_dead, ref g_dlt_n_dead_detached);

                cproc_lai_detachment1(leaf,
                                    crop.sen_detach_frac, g_slai, ref g_dlt_slai_detached,
                                    crop.dead_detach_frac, g_tlai_dead, ref g_dlt_tlai_dead_detached);




                // CLEANUP AFTER PROCESSES (DO SOME HOUSEKEEPING)

                //sugar_cleanup ()
                //*       cleanup after crop processes

                sugar_update(ref g_canopy_height, ref g_cnd_photo, ref g_cswd_expansion, ref g_cswd_pheno, ref g_cswd_photo,
                            g_dlt_canopy_height, 
                            g_dlt_dm, 
                            g_dlt_dm_dead_detached, g_dlt_dm_detached, g_dlt_dm_green, g_dlt_dm_green_retrans, g_dlt_dm_senesced, g_dlt_dm_realloc,
                            g_dlt_lai, g_dlt_leaf_no, g_dlt_node_no, g_dlt_node_no_dead, 
                            g_dlt_n_dead_detached, g_dlt_n_detached, g_dlt_n_green, g_dlt_n_retrans, g_dlt_n_senesced, g_dlt_n_realloc,
                            g_dlt_plants, g_dlt_plant_wc, 
                            g_dlt_root_length, g_dlt_root_length_senesced, g_dlt_root_depth, 
                            g_dlt_slai, g_dlt_slai_detached, g_dlt_stage, g_dlt_tlai_dead_detached, 
                            ref g_dm_dead, ref g_dm_green, ref g_dm_plant_top_tot, ref g_dm_senesced, 
                            ref g_lai, ref g_leaf_area_zb, ref g_leaf_dm_zb, ref g_leaf_no_zb, 
                            ref g_node_no_zb, ref g_node_no_dead_zb,
                            g_nfact_photo, 
                            ref g_n_conc_crit, ref g_n_conc_min, ref g_n_dead, ref g_n_green, ref g_n_senesced, 
                            ref g_plants, ref g_plant_wc, 
                            g_previous_stage, 
                            ref g_root_length, ref g_root_depth, 
                            ref g_slai, 
                            g_swdef_expansion, g_swdef_pheno, g_swdef_photo, 
                            ref g_tlai_dead, 
                            crop.n_conc_crit_root, crop.n_conc_min_root, 
                            crop.x_stage_code, crop.y_n_conc_crit_cabbage, crop.y_n_conc_crit_cane, crop.y_n_conc_crit_leaf, crop.y_n_conc_min_cabbage, crop.y_n_conc_min_cane, crop.y_n_conc_min_leaf, 
                            g_current_stage, crop.stage_code_list, g_phase_tt, g_tt_tot,
                            ref g_node_no_detached_ob,
                            crop.leaf_no_at_emerg
                         );


                sugar_totals(g_current_stage, g_days_tot, g_day_of_year, dlayer, g_dlt_sw_dep, g_dm_green, ref g_isdate,
                            g_lai, ref g_lai_max, ref g_n_conc_act_stover_tot, g_n_demand, ref g_n_demand_tot, g_n_green, g_root_depth, ref g_transpiration_tot);


                sugar_event(crop.stage_code_list, crop.stage_names, g_current_stage, g_days_tot, g_day_of_year,
                            dlayer, g_dm_dead, g_dm_green, g_dm_senesced, g_lai, g_n_green, g_root_depth, sw_dep, g_year, g_ll_dep);




                //SEND EVENTS TO CHANGE OTHER MODULES

                //! send changes to owner-modules
                sugar_set_other_variables(g_dlt_no3gsm, g_dlt_nh4gsm, g_dlt_sw_dep);


                }
            else
                {
                //! crop not in
                sugar_zero_variables();
                }


            }


    #endregion

    

    #region Manager Event Handlers


        #region Functions used in Manager Event Handlers



            void sugar_start_crop(SowType i_SowData)
                {

                //*+  Purpose
                //*       Start crop using parameters specified in passed record

                //*+  Mission Statement
                //*     Start the crop based on passed parameters

                //*+  Changes
                //*     041095 nih changed start of ratton crop from emergence to sprouting



                string  l_cultivar;           //! name of cultivar
                string  l_cultivar_ratoon;    //! name of cultivar ratoon section

      
                if (g_crop_status == crop_out) 
                    {

                    if ( !g_plant_status_out_today)
                        {

                        //! request and receive variables from owner-modules
                        //call sugar_get_met_variables ()
                        sugar_get_soil_variables(ref g_no3gsm, ref g_no3gsm_min, ref g_nh4gsm, ref g_nh4gsm_min);

                        Console.WriteLine("Sowing initiate");
                        //call Publish_null (id%sowing)

                        g_plants = i_SowData.plants;
                        g_initial_plant_density = g_plants;
                        g_ratoon_no = i_SowData.Ratoon;
                        g_sowing_depth = i_SowData.sowing_depth;
                        l_cultivar = i_SowData.Cultivar;

                            //! report

                        Console.WriteLine(Environment.NewLine + Environment.NewLine);

                        Console.WriteLine("                 Crop Sowing Data");

                        Console.WriteLine("    ------------------------------------------------");

                        Console.WriteLine("    Sowing  Depth Plants Cultivar");

                        Console.WriteLine("    Day no   mm     m^2    Name   ");

                        Console.WriteLine("    ------------------------------------------------");

                        //http://msdn.microsoft.com/en-us/library/dwhawy9k.aspx

                        Console.WriteLine("{0}{1,7:D}{2,7:F1}{3,7:F1}{4}{5,-10}", "   ", g_day_of_year, g_sowing_depth, g_plants, " ", l_cultivar);  //parameters are zero based.
                    //    write (string, '(3x, i7, 2f7.1, 1x, a10)')
                    //:                   g%day_of_year, g%sowing_depth
                    //:                 , g%Population, cultivar


                        Console.WriteLine("    ------------------------------------------------");


                                //! get cultivar parameters

                        if (g_ratoon_no == 0) 
                            {
                            crop = sugar_read_crop_constants("plant_crop");
                            cult = sugar_read_cultivar_params (l_cultivar);
                            }
                        else
                            {
                            crop = sugar_read_crop_constants("ratoon_crop");

                            l_cultivar_ratoon = l_cultivar + "_ratoon";
                            cult = sugar_read_cultivar_params(l_cultivar_ratoon);
                            }

                                //! get root profile parameters

                        sugar_read_root_params();   //different if using (SoilWat or SWIM) or Eo modules in the simulation

                        if (g_ratoon_no == 0)
                            g_current_stage = (double)sowing;
                        else
                            g_current_stage = (double)sowing;
                        

                        g_crop_status = crop_alive;
                        g_crop_cultivar = l_cultivar;

                        }
                    else
                        {
                        throw new Exception("\"SugarCane\" was taken out today by \"end_crop\" action -" 
                                + "\n" 
                                + " Unable to accept sow action until the next day.");
                        }

                   }
                else
                    {
                    throw new Exception("\"SugarCane\"  is still in the ground - unable to sow until it is taken out by \"end_crop\" action.");
                    }
    

                }


            CropConstants sugar_read_crop_constants(string CropType)
                {
                CropConstants l_crop;

                if (CropType == "plant_crop")
                    {
                    Console.WriteLine("\n" + "    - Reading constants from " + "plant_crop");        
                    l_crop = plant;
                    }
                else
                    {
                    Console.WriteLine("\n" + "    - Reading constants from " + "ratoon_crop");
                    l_crop = ratoon;
                    }

                return l_crop;
                }



            CultivarConstants sugar_read_cultivar_params(string Name)
                {
                Console.WriteLine("\n" + "    - Reading constants from " + Name);

                foreach (CultivarConstants c in cultivars)
                    {   
                    if (c.cultivar_name.ToLower() == Name.ToLower())
                        {
                        return c;
                        }
                    }

                throw new Exception("Could not find in the SugarCane ini file a cultivar called: " + Name);                
                }



            void sugar_read_root_params()
                {


                //!       cproc_sw_demand_bound

                if (!Double.IsNaN(swim3))
                    {
                    uptake_source = "swim3";
                    }



                //!       sugar_sw_supply

                //work out what ll_dep is

                //if ll for sugar was specified then use it.
                if (ll.Length < max_layer)
                    {
                    for (int layer = 0; layer < num_layers; layer++)
                        {
                        g_ll_dep[layer] = ll[layer] * dlayer[layer];
                        }
                    Array.Resize(ref g_ll_dep, num_layers);
                    }
                else
                    {
                    //else if ll15 was specified then use that.
                    if (ll15.Length < max_layer)
                        {
                        for (int layer = 0; layer < num_layers; layer++)
                            {
                            g_ll_dep[layer] = ll15[layer] * dlayer[layer];
                            }
                        Array.Resize(ref g_ll_dep, num_layers);
                        Console.WriteLine("Using externally supplied Lower Limit (ll15)");
                        }
                    else
                        {
                        //if neither ll or ll15 were specified then throw an error
                        throw new Exception("No Crop Lower Limit found");
                        }
                    }



                //caluculate the root length
                ZeroArray(ref g_root_length);
                for (int layer = 0; layer < rlv_init.Length; layer++)      //rlv.Length may be less than num_layers
                    {
                    g_root_length[layer] = rlv_init[layer] * dlayer[layer];
                    }
                Array.Resize(ref g_root_length, num_layers);  //g_root_length has max_layer number of elements, so now shorten this to num_layer





                //write out to the summary file what optional values you have read in.
                //--------------------------------------------------------------------

                Console.WriteLine("\n" + "   - Reading root profile parameters");


                if (uptake_source == "calc")
                    {
                    //! report
                    Console.WriteLine("\n" + "\n");
                    Console.Write("Sugar module is calculating its own soil uptakes");
                    Console.WriteLine("\n" + "\n");
                    }
                else if (uptake_source == "apsim")
                    {
                    //! report
                    Console.WriteLine("\n" + "\n");
                    Console.Write("Sugar module is using uptakes" + " provided from another module");
                    Console.WriteLine("\n" + "\n");
                    }
                else if (uptake_source == "swim3")
                    {
                    //! report
                    Console.WriteLine("\n" + "\n");
                    Console.Write("Sugar module is using water uptake" + " provided from Swim3");
                    Console.WriteLine("\n" + "\n");
                    }
                else
                    {
                    //! the user has not specified 'calc' or 'apsim'
                    //! so give out an error message
                    throw new Exception("Bad value for uptake_source");
                    }


                string line;

                line = "                    Root Profile";
                Console.WriteLine(line);

                line = "  -----------------------------------------------------------------------";
                Console.WriteLine(line);

                line = "    Layer depth   rlv_init  root_length Lower limit       KL         XF    ";
                Console.WriteLine(line);

                line = "         (mm)      (mm/mm)     (mm)       (mm/mm)         ()       (0-1)";
                Console.WriteLine(line);

                line = "  -----------------------------------------------------------------------";
                Console.WriteLine(line);

                for (int layer = 0; layer < num_layers; layer++)
                    {
                    //create an rlv_int with a value for every layer in the soil rather than just the layers the user entered.
                    //do this so the loop below here does not crash when it gets past rlv_int[more layers then were user entered]
                    double[] l_rlv_init = new double[num_layers];
                    Array.Copy(rlv_init, l_rlv_init, rlv_init.Length);

                    Console.WriteLine(" {0,12:F0}{1,12:0.000}{2,12:0.000}{3,12:0.000}{4,12:0.000}{5,12:0.000}", 
                        dlayer[layer], l_rlv_init[layer], g_root_length[layer], ll[layer], kl[layer], xf[layer]);
                    }

                line = "   ----------------------------------------------------------------------";
                Console.WriteLine(line);
                Console.WriteLine("           nb. Initial root_length = rlv_init x Layer depth");
                Console.WriteLine("\n");


                Console.WriteLine("  {0}{1,5:0.0}{2}", "  Crop factor for bounding water use is set to ", eo_crop_factor, " times Eo");
                Console.WriteLine("\n" + "\n");

                }





            void sugar_harvest ()
                {

                //*+  Purpose
                //*       Report occurence of harvest and the current status of specific
                //*       variables.

                //*+  Mission Statement
                //*     Occurance of harvest

                //*+  Changes
                //*     070495 nih taken from template
                //*     191099 jngh changed to sugar_Send_Crop_Chopped_Event
                //*     101100 dph  added eventInterface parameter to crop_root_incorp


                double      l_biomass_dead;          //! above ground dead plant wt (kg/ha)
                double      l_biomass_green;         //! above ground green plant wt (kg/ha)
                double      l_biomass_senesced;      //! above ground senesced plant wt (kg/ha)
                string      l_cultivar_ratoon;    
                double      l_dm;                    //! above ground total dry matter (kg/ha)
                double      l_leaf_no;               //! total leaf number
                double      l_N_dead;                //! above ground dead plant N (kg/ha)
                double      l_N_green;               //! above ground green plant N (kg/ha)
                double      l_N_senesced;            //! above ground senesced plant N (kg/ha)
                double      l_N_total;               //! total gross nitrogen content (kg/ha)
                int         l_phase;                 //! phenological phase number
                double      l_si1;                   //! mean water stress type 1
                double      l_si2;                   //! mean water stress type 2
                double      l_si4;                   //! mean nitrogen stress type 1
                double      l_dm_root;               //! (g/m^2)
                double      l_N_root;                //! (g/m^2)
                double      l_dm_residue;            //! (g/m^2)
                double      l_N_residue;             //! (g/m^2)
                double[]    l_dlt_dm_crop = new double[max_part]; //! change in crop dry matter (kg/ha)
                double[]    l_dlt_dm_N = new double[max_part];   //! N content of dry matter change (kg/ha)
                double[]    l_fraction_to_Residue = new double[max_part];   //! fraction sent to residue (0-1)

                int         l_layer;
                int         l_hold_ratoon_no;
                double      l_hold_dm_root;
                double      l_hold_n_root;
                int         l_hold_num_layers;
                double      l_hold_root_depth;
                double[]    l_hold_root_length = new double[max_layer];




                    //! crop harvested. Report status
                    //*******************************

                //call Publish_null (id%harvesting)

                l_biomass_green = (SumArray(g_dm_green, max_part) - g_dm_green[root]) * gm2kg / sm2ha;

                l_biomass_senesced = (SumArray(g_dm_senesced, max_part) - g_dm_senesced[root]) * gm2kg / sm2ha;

                l_biomass_dead = (SumArray(g_dm_dead, max_part) - g_dm_dead[root]) * gm2kg / sm2ha;

                l_dm = (l_biomass_green + l_biomass_senesced + l_biomass_dead);

                l_leaf_no = sum_between_zb(zb(emerg), zb(now), g_leaf_no_zb);
                
                l_N_green = (SumArray(g_n_green, max_part) - g_n_green[root]) * gm2kg / sm2ha;

                l_N_senesced = (SumArray(g_n_senesced, max_part) - g_n_senesced[root]) * gm2kg / sm2ha;

                l_N_dead = (SumArray(g_n_dead, max_part) - g_n_dead[root]) * gm2kg / sm2ha;

                l_N_total = l_N_green + l_N_senesced + l_N_dead;

                Console.WriteLine();
                Console.WriteLine();

                Console.WriteLine("{0}{1,4}"," flowering day  = ", g_isdate);
                //write (string, '(a,i4)')

                Console.WriteLine("{0}{1,6:F3}", " maximum lai =", g_lai_max);
                //write (string, '(a,f6.3)')

                Console.WriteLine("{0}{1,10:F1}", " total above ground biomass (kg/ha) =", l_dm);
                //write (string, '(a,f10.1)')

                Console.WriteLine("{0}{1,10:F1}", " (green + senesced) above ground biomass (kg/ha) =", l_biomass_green + l_biomass_senesced);
                //write (string, '(a,f10.1)')

                Console.WriteLine("{0}{1,10:F1}", " green above ground biomass (kg/ha) =", l_biomass_green);
                //write (string, '(a,f10.1)')

                Console.WriteLine("{0}{1,10:F1}", " senesced above ground biomass (kg/ha) =", l_biomass_senesced);
                //write (string, '(a,f10.1)')

                Console.WriteLine("{0}{1,10:F1}", " dead above ground biomass (kg/ha) =", l_biomass_dead);
                //write (string, '(a,f10.1)')

                Console.WriteLine("{0}{1,6:F1}", " number of leaves =", l_leaf_no);
                //write (string, '(a,f6.1)')

                Console.WriteLine("{0}{1,10:F2}{2}{3}{4,10:F2}", " total N content (kg/ha) =", l_N_total, "    ", " senesced N content (kg/ha) =", l_N_senesced);
                //write (string, '(a,f10.2,t40,a,f10.2)')
                //sv- to compensate for t40 (tab to column 40) I just added four spaces. (26 + 10 = 36) + 4 = 40

       
                Console.WriteLine("{0}{1,10:F2}{2}{3}{4,10:F2}", " green N content (kg/ha) =", l_N_green, "    ", " dead N content (kg/ha) =", l_N_dead);
                //write (string, '(a,f10.2,t40,a,f10.2)')
                //sv- to compensate for t40 (tab to column 40) I just added two spaces. (26 + 10 = 36) + 4 = 40



                for (l_phase = zb(emerg_to_begcane); l_phase <= zb(flowering_to_crop_end); l_phase++)
                    {
                    l_si1 = MathUtility.Divide(g_cswd_photo[l_phase], g_days_tot[l_phase], 0.0);
                    l_si2 = MathUtility.Divide(g_cswd_expansion[l_phase], g_days_tot[l_phase], 0.0);
                    l_si4 = MathUtility.Divide(g_cnd_photo[l_phase], g_days_tot[l_phase], 0.0);

                    Console.WriteLine();
                    Console.WriteLine();

                    Console.WriteLine(" stress indices for " + crop.stage_names[l_phase]);

                    Console.WriteLine("{0}{1,16:E2}{2}{3,16:E2}", " water stress 1 =", l_si1, "   nitrogen stress 1 =", l_si4);
                    //write (string,'(2(a, g16.7e2))')

                    Console.WriteLine("{0}{1,16:E2}", " water stress 2 =", l_si2);
                    //write (string,'(a, g16.7e2)')
                    }




                //! the following is a copy/adaption of sugar_end_crop
                //****************************************************

                if (g_crop_status != crop_out)
                    {
                        //! report

                        //! now do post harvest processes

                    l_dm_root = g_dm_green[root] * crop.root_die_back_fr + g_dm_dead[root] + g_dm_senesced[root];

                    l_N_root  = g_n_green[root] * crop.root_die_back_fr + g_n_dead[root] + g_n_senesced[root];


                        //! put stover into surface residue

                                //Dry Matter

                    l_dm_residue =  ( SumArray(g_dm_green, max_part)    - g_dm_green[root] - g_dm_green[sstem] - g_dm_green[sucrose]) //stem and surose goes to the mill not to residue
                                +   ( SumArray(g_dm_senesced, max_part) - g_dm_senesced[root] )   
                                +   ( SumArray(g_dm_dead, max_part)     - g_dm_dead[root] );

                    for (int p = 0; p < max_part; p++)
                        {
                        l_dlt_dm_crop[p] =  (g_dm_green[p] + g_dm_senesced[p] + g_dm_dead[p]) * gm2kg/sm2ha;
                        }
                    l_dlt_dm_crop[root] = l_dm_root * gm2kg/sm2ha;

                                 //Nitrogen

                    l_N_residue =   ( SumArray(g_n_green, max_part)     - g_n_green[root] - g_n_green[sstem] - g_n_green[sucrose] )  //stem and surose goes to the mill not to residue
                                +   ( SumArray(g_n_senesced, max_part)  - g_n_senesced[root] )
                                +   ( SumArray(g_n_dead, max_part)      - g_n_dead[root] );

                    for (int p = 0; p < max_part; p++)
                        {
                        l_dlt_dm_N[p] =  (g_n_green[p] + g_n_senesced[p] + g_n_dead[p]) * gm2kg/sm2ha;
                        }
                    l_dlt_dm_N[root] = l_N_root * gm2kg/sm2ha;

                    //TODO: put this back in. Better then below where you refer to "straw".
                    //Console.WriteLine(                         "          Organic matter from crop:-      Leaves&Cabbage to surface residue      Roots to soil FOM");
                    //Console.WriteLine("{0}{1,22:F1}{2,44:F3}", "                          DM (kg/ha) =", (l_dm_residue * gm2kg / sm2ha), (l_dm_root * gm2kg / sm2ha));
                    //Console.WriteLine("{0}{1,22:F1}{2,44:F3}", "                          N  (kg/ha) =", (l_N_residue * gm2kg / sm2ha), (l_N_root * gm2kg / sm2ha));

                    //Console.WriteLine(                         "Organic matter removed from system:-            Stem&Sucrose to mill");
                    //Console.WriteLine("{0}{1,22:F1}",          "                         DM (kg/ha) =", (g_dm_green[sstem] + g_dm_green[sucrose]) * gm2kg / sm2ha );
                    //Console.WriteLine("{0}{1,22:F1}",          "                         N  (kg/ha) =", (g_n_green[sstem] + g_n_green[sucrose]) * gm2kg / sm2ha);


                    string l_40spaces = "                                        ";

                    Console.WriteLine("{0}{1}{2,7:F1}{3}", l_40spaces, "  straw residue =", l_dm_residue * gm2kg / sm2ha, " kg/ha");
                    Console.WriteLine("{0}{1}{2,6:F1}{3}", l_40spaces, "  straw N = ", l_N_residue * gm2kg / sm2ha, " kg/ha");
                    Console.WriteLine("{0}{1}{2,6:F1}{3}", l_40spaces, "  root residue = ", l_dm_root * gm2kg / sm2ha, " kg/ha");
                    Console.WriteLine("{0}{1}{2,6:F1}{3}", l_40spaces, "  root N = ", l_N_root * gm2kg / sm2ha, " kg/ha"); 

                //    write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
                //:                  '  straw residue ='
                //:                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
                //:                  , new_line
                //:                  , '  straw N = '
                //:                  , N_residue * gm2kg /sm2ha, ' kg/ha'

                //:                  , new_line
                //:                  , '  root residue = '
                //:                  , dm_root * gm2kg /sm2ha, ' kg/ha'
                //:                  , new_line
                //:                  , '  root N = '
                //:                  , N_root * gm2kg /sm2ha, ' kg/ha'



                    crop_root_incorp (l_dm_root, l_N_root, dlayer, g_root_length, g_root_depth, crop_type, max_layer);

                    for (int p = 0; p < max_part; p++)
                        {
                        l_fraction_to_Residue[p] = 1.0;
                        }  
                    l_fraction_to_Residue[root] = 0.0;
                    l_fraction_to_Residue[sstem] = 0.0;
                    l_fraction_to_Residue[sucrose] = 0.0;




                    if (SumArray(l_dlt_dm_crop, max_part) > 0.0) 
                        {
                        sugar_Send_Crop_Chopped_Event(crop_type, part_name, l_dlt_dm_crop, l_dlt_dm_N, l_fraction_to_Residue);
                        }
                    else
                        {
                        //! no surface residue
                        }
                   

                    //sv- temporarily store some values
                    l_hold_ratoon_no = g_ratoon_no;
                    l_hold_dm_root   = g_dm_green [root]*(1.0 - crop.root_die_back_fr);
                    l_hold_n_root    = g_n_green[root]*(1.0 - crop.root_die_back_fr);
                    l_hold_num_layers= num_layers;
                    l_hold_root_depth= g_root_depth;
                    //for (int layer=0; layer < max_layer; layer++)
                    for (int layer=0; layer < g_root_length.Length; layer++)
                        {
                        l_hold_root_length[layer] = g_root_length[layer] * (1.0 - crop.root_die_back_fr);
                        }

                    //sv - zero everything
                    sugar_zero_globals ();
                    sugar_zero_daily_variables();

                    //sv- use stored values to set them back
                    g_current_stage   = (double)sprouting;
                    g_ratoon_no       = l_hold_ratoon_no + 1;
                    g_dm_green[root] = l_hold_dm_root;
                    g_n_green [root]  = l_hold_n_root;
                    //num_layers      = l_hold_num_layers;   //sv- num_layers is not settable.
                    g_root_depth      = l_hold_root_depth;
                    g_plants      = g_initial_plant_density;
                    //for (int layer=0; layer < max_layer; layer++)
                    for (int layer=0; layer < g_root_length.Length; layer++)
                        {
                        g_root_length[layer] = l_hold_root_length[layer];
                        }

 

                    //! now update constants if need be  (after havest a plant crop change it to a ratoon crop)

                    if (g_ratoon_no == 1) 
                        {
                        crop = sugar_read_crop_constants("ratoon_crop");

                        l_cultivar_ratoon = g_crop_cultivar + "_ratoon";
                        cult = sugar_read_cultivar_params(l_cultivar_ratoon);
                        }
                    else
                        {
                        //! only need to update constants when we move from a plant crop to a ratoon crop.
                        }

    
                    }
 


                }






            void sugar_kill_crop(ref string i_crop_status, double[] i_dm_dead, double[] i_dm_green, double[] i_dm_senesced)
                {

                //*+  Sub-Program Arguments
                //      CHARACTER  G_crop_status   *(*)  ! (INPUT)                      //sv- this should be an OUTPUT
                //      INTEGER    G_day_of_year         ! (INPUT)  day of year
                //      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
                //      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
                //      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
                //      INTEGER    G_year                ! (INPUT)  year

                //*+  Purpose
                //*       Kill crop

                //*+  Mission Statement
                //*     Crop death due to killing


                double       l_biomass;               //! above ground dm (kg/ha)


                //c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero


                if (i_crop_status == crop_alive) 
                    {
                    i_crop_status = crop_dead;

                    l_biomass =   (SumArray(i_dm_green, max_part)  - i_dm_green[root]) * gm2kg /sm2ha
                             +  (SumArray(i_dm_senesced, max_part) - i_dm_senesced[root]) * gm2kg /sm2ha
                             +  (SumArray(i_dm_dead, max_part)   - i_dm_dead[root]) * gm2kg /sm2ha ;


                        //! report

                    Console.WriteLine(" crop_kill. Standing above-ground dm = " + l_biomass + " (kg/ha)");
   
                    }

                }





            void sugar_end_crop ()

                {

                //*+  Purpose
                //*       End crop

                //*+  Mission Statement
                //*     End the crop

                //*+  Changes
                //*       070495 nih taken from template
                //*       191099 jngh changed to sugar_Send_Crop_Chopped_Event
                //*     101100 dph  added eventInterface parameter to crop_root_incorp


                double      l_dm_residue;            //! dry matter added to residue (g/m^2)
                double      l_N_residue;             //! nitrogen added to residue (g/m^2)
                double      l_dm_root;               //! dry matter added to soil (g/m^2)
                double      l_N_root;                //! nitrogen added to soil (g/m^2)
                double[]    l_fraction_to_Residue = new double[max_part];   //! fraction sent to residue (0-1)
                double[]    l_dlt_dm_crop = new double[max_part]; //! change in crop dry matter (kg/ha)
                double[]    l_dlt_dm_N = new double[max_part];    //! N content of dry matter change (kg/ha)



                if (g_crop_status != crop_out) 
                    {
                    g_crop_status = crop_out;
                    g_current_stage = (double)crop_end;
                    g_plant_status_out_today = true;

                        //! report

                        //! now do post harvest processes

                    l_dm_root = g_dm_green[root] + g_dm_dead[root] + g_dm_senesced[root];

                    l_N_root  = g_n_green[root] + g_n_dead[root] + g_n_senesced[root];

                    crop_root_incorp(l_dm_root, l_N_root, dlayer, g_root_length, g_root_depth, crop_type, max_layer);


                        //! put stover into surface residue

                                //Dry Matter

                    l_dm_residue =  (SumArray(g_dm_green, max_part) - g_dm_green[root]) 
                                  + (SumArray(g_dm_senesced, max_part) - g_dm_senesced[root])
                                  + (SumArray(g_dm_dead, max_part) - g_dm_dead[root]);

                    for (int p = 0; p < max_part; p++)
                        {
                        l_dlt_dm_crop[p] = (g_dm_green[p] + g_dm_senesced[p] + g_dm_dead[p]) * gm2kg/sm2ha;
                        }


                                //Nitrogen

                    l_N_residue = (SumArray(g_n_green, max_part) - g_n_green[root])
                                + (SumArray(g_n_senesced, max_part) - g_n_senesced[root])
                                + (SumArray(g_n_dead, max_part) - g_n_dead[root]);

                    for (int p = 0; p < max_part; p++)
                        {
                        l_dlt_dm_N[p] = (g_n_green[p] + g_n_senesced[p] + g_n_dead[p]) * gm2kg/sm2ha;
                        }



                    for (int p = 0; p < max_part; p++)
                        {
                        l_fraction_to_Residue[p] = 1.0;
                        }
                    l_fraction_to_Residue[root] = 0.0;


                    if (SumArray(l_dlt_dm_crop, max_part) > 0.0)
                        {
                        sugar_Send_Crop_Chopped_Event(crop_type, part_name, l_dlt_dm_crop, l_dlt_dm_N, l_fraction_to_Residue);
                        }
                    else
                        {
                        //! no surface residue
                        }


                    string l_40spaces = "                                        ";

                    Console.WriteLine("{0}{1}{2,7:F1}{3}", l_40spaces, "  straw residue =", l_dm_residue * gm2kg / sm2ha, " kg/ha");
                    Console.WriteLine("{0}{1}{2,6:F1}{3}", l_40spaces, "  straw N = ", l_N_residue * gm2kg / sm2ha, " kg/ha");
                    Console.WriteLine("{0}{1}{2,6:F1}{3}", l_40spaces, "  root residue = ", l_dm_root * gm2kg / sm2ha, " kg/ha");
                    Console.WriteLine("{0}{1}{2,6:F1}{3}", l_40spaces, "  root N = ", l_N_root * gm2kg / sm2ha, " kg/ha"); 


                    //    write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
                    //:                  '  straw residue ='
                    //:                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
                    //:                  , new_line
                    //:                  , '  straw N = '
                    //:                  , N_residue * gm2kg /sm2ha, ' kg/ha'

                    //:                  , new_line
                    //:                  , '  root residue = '
                    //:                  , dm_root * gm2kg /sm2ha, ' kg/ha'
                    //:                  , new_line
                    //:                  , '  root N = '
                    //:                  , N_root * gm2kg /sm2ha, ' kg/ha'

                    }

                }


        #endregion



        [EventHandler]
        void Onsow(SowType SowData)
            {
            sugar_start_crop(SowData);
            }


        [EventHandler]
        void Onharvest(HarvestType HarvestData)
            {
            //sv- Harvest is the same as end_crop (in that it gets rid of the biomass) 
            //      only unlike end_crop it can still ratoon again (crop_status is not set to "crop_out")

             sugar_harvest();
            }

        
        [EventHandler]
        void Onkill_crop(KillCropType KillCropData)
            {            
            //sv- Killing kills just this plant or ratoon (crop_status is set to "crop_dead") but the biomass is left there standing. 
            //      You need to do a tillage to get rid of the above ground biomass. It will not grow or ratoon again. It just sits there dead with above ground biomass.

            //      Kill crop and End Crop is used in other crop modules as part of the Crop Rotations. 
            //      You want to kill the crop and set its status to dead but you want to leave it there until someone does a tillage event to plow it in just before they
            //      plant another crop.
            //      Killing also prevents another crop from being planted by the Crop Rotations manager. If you want the next crop in the rotation to be planted you then
            //      have to use the End Crop command. End Crop specifies the status as Crop Out which means that there is nothing in the ground and you can plant another crop.
            //      It will also get rid of the biomass.
            //      Perhaps we need to create a new Event for SugarCane called "KillButRegrowCrop" which will kill the crop but let is start growing again.
            //      I guess it will have to detach the dead biomass over time rather than tilling it back into the soil.  

            sugar_kill_crop(ref g_crop_status, g_dm_dead, g_dm_green, g_dm_senesced);
            }


        //
        [EventHandler]
        void Onend_crop()
            {
            //sv- End gets rid of the biomass and requires a replant to start growing again.  (crop_status is set to "crop_out")

            sugar_end_crop();
            }


        [EventHandler]
        void Ongraze()
            {
            throw new Exception("Grazing action not currently supported by this module");
            }



        [EventHandler]
        void Onlodge()
            {
            g_lodge_flag = true;
            Console.WriteLine(Clock.Today.ToString("d MMM yyyy") + " - " + "SugarCane crop is lodging");
            }



        [EventHandler]
      
        void Onhill_up(HillUpType HillUpData)
            {
            Console.WriteLine(Clock.Today.ToString("d MMM yyyy") + " - " + "SugarCane module is doing Hill Up");
            Console.WriteLine("canefr = " + HillUpData.canefr + " , topsfr = " + HillUpData.topsfr);
            sugar_hill_up(HillUpData);    //see "Events sent to Change Other Modules" section because hill_up manager event sends an IncorpFOM event to surfaceOM module.
            }


    #endregion



    #region Erosion Event Handler



        ////sv- taken from sugar_get_soil_variables ()

        //void sugar_get_soil_variables()
        //    {

        //    //! dlayer may be changed from its last setting due to erosion
        //    double l_profile_depth;
        //    double l_root_depth_new;

        //    l_profile_depth = SumArray(dlayer, dlayer.Length);

        //    if (root_depth > l_profile_depth)
        //        {
        //        l_root_depth_new = l_profile_depth;
        //        crop_root_redistribute(g_root_length,
        //                            root_depth, dlayer, num_layers,
        //                            l_root_depth_new, dlayer, dlayer.Length);

        //        g_root_depth = l_root_depth_new;
        //        }
        //    else
        //        {
        //        //! roots are ok.
        //        }

        //    //         do 1000 layer = 1, numvals
        //    //            //! What happens if crop not in ground and p%ll_dep is empty
        //    //            p%ll_dep(layer) = divide (p%ll_dep(layer)
        //    //     :                              , g%dlayer(layer), 0.0)
        //    //     :                      * dlayer(layer)

        //    //            g%dlayer(layer) = dlayer(layer)
        //    //1000     continue
        //    //         g%num_layers = numvals

        //    }



        //void crop_root_redistribute(double[] root_length,
        //                         double root_depth_old, double[] dlayer_old, int nlayr_old,
        //                         double root_depth_new, double[] dlayer_new, int nlayr_new)
        //    {


        //    //!+  Sub-Program Arguments
        //    //      real   root_length(*) ! root length (mm/mm^2)
        //    //      real   root_depth_old ! old root depth (mm)
        //    //      real   dlayer_old(*)  ! old soil profile layers (mm)
        //    //      integer nlayr_old     ! number of old soil profile layers
        //    //      real   root_depth_new ! new root depth (mm)
        //    //      real   dlayer_new(*)  ! new soil profile layers (mm)
        //    //      integer nlayr_new     ! number of new soil profile layers

        //    //!+  Purpose
        //    //!      Map root length density distribution into a new layer structure
        //    //!      after reduction is profile depth due to erosion.

        //    //!+  Mission Statement
        //    //!   Redistribute root profile according to changes in layer structure.

        //    //!+  Assumptions
        //    //!      That the new profile is shallower and the roots are at the
        //    //!      bottom of the old profile.

        //    //!+  Notes
        //    //!      Remapping is achieved by first constructing a map of
        //    //!      cumulative root length vs depth that is 'squashed'
        //    //!      to the new profile depth.
        //    //!      The new values of root length per layer can be linearly
        //    //!      interpolated back from this shape taking into account
        //    //!      the rescaling of the profile.


        //    int l_nlayr_rt_old;       //! No. of layers in old root profile
        //    int l_nlayr_rt_new;       //! No. of layers in old root profile
        //    double[] l_cum_root_length = new double[crop_max_layer]; //!Cum Root length with depth (mm/mm2)
        //    double[] l_cum_root_depth = new double[crop_max_layer]; //!Cum Root depth (mm)
        //    double l_pro_red_fr;           //! profile reduction fraction
        //    double l_cum_root_bottom;      //! cum root at bottom of layer (mm/mm2)
        //    double l_cum_root_top;         //! cum root at top of layer (mm/mm2)
        //    double l_layer_top_depth;            //! depth to top of layer (mm)
        //    double l_layer_bottom_depth;         //! depth to bottom of layer (mm)


        //    //! Identify key layers
        //    //! -------------------
        //    l_nlayr_rt_old = find_layer_no(root_depth_old, dlayer_old, nlayr_old);
        //    l_nlayr_rt_new = find_layer_no(root_depth_new, dlayer_new, nlayr_new);

        //    //! calculate the fractional reduction in total profile depth
        //    //! ---------------------------------------------------------
        //    l_pro_red_fr = MathUtility.Divide(root_depth_new, root_depth_old, 0.0);

        //    //! build interpolation pairs based on 'squashed' original root profile
        //    //! -------------------------------------------------------------------
        //    l_cum_root_depth[0] = 0.0;
        //    l_cum_root_length[0] = 0.0;

        //    for (int layer = 0; layer < l_nlayr_rt_old; layer++)
        //        {
        //        if (layer == zb(l_nlayr_rt_old))
        //            {
        //            l_cum_root_depth[layer + 1] = root_depth_old * l_pro_red_fr;
        //            }
        //        else
        //            {
        //            l_cum_root_depth[layer + 1] = l_cum_root_depth[layer] + dlayer_old[layer] * l_pro_red_fr;
        //            }

        //        l_cum_root_length[layer + 1] = l_cum_root_length[layer] + root_length[layer];
        //        }

        //    fill_real_array(ref root_length, 0.0, l_nlayr_rt_old);

        //    //! look up new root length from interpolation pairs
        //    //! ------------------------------------------------
        //    for (int layer = 0; layer < l_nlayr_rt_new; layer++)
        //        {
        //        l_layer_bottom_depth = SumArray(dlayer_new, ob(layer));
        //        l_layer_top_depth = l_layer_bottom_depth - dlayer_new[layer];
        //        l_cum_root_top = MathUtility.LinearInterpReal(l_layer_top_depth, l_cum_root_depth, l_cum_root_length, l_nlayr_rt_old + 1);
        //        l_cum_root_bottom = MathUtility.LinearInterpReal(l_layer_bottom_depth, l_cum_root_depth, l_cum_root_length, l_nlayr_rt_old + 1);
        //        root_length[layer] = l_cum_root_bottom - l_cum_root_top;
        //        }

        //    }





    #endregion




        //EVENTS - SENDING


    #region Event Declarations (sent by this Module)

        [Event]
        public event NitrogenChangedDelegate NitrogenChanged;

        [Event]
        public event WaterChangedDelegate WaterChanged;

        [Event]
        public event CropChoppedDelegate CropChopped;

        [Event]
        public event FOMLayerDelegate IncorpFOM;


        //ToFloatArray is needed because some of these Events pass Float Arrays rather then Double Arrays as Parameters.
        private float[] ToFloatArray(double[] D)
            {
            float[] f = new float[D.Length];
            for (int i = 0; i < D.Length; i++)
                f[i] = (float)D[i];
            return f;
            }


    #endregion



    #region Events sent to Change Other Modules


        void sugar_set_other_variables(double[] i_dlt_no3gsm, double[] i_dlt_nh4gsm, double[] i_dlt_sw_dep)        //called at end of OnProcess() Event Handler.
            {

            //*+  Purpose
            //*      Set the value of a variable or array in other module/s.

            //*+  Mission Statement
            //*     Set value of variable or array in other module/s

            //*+  Notes
            //*      a flag is set if any of the totals is requested.  The totals are
            //*      reset during the next process phase when this happens.


            double[] l_dlt_NO3 = new double[max_layer];    //! soil NO3 change (kg/ha)
            double[] l_dlt_NH4 = new double[max_layer];    //! soil NO3 change (kg/ha)
            int num_layers;            //! number of layers


            if (uptake_source == "calc")
                {

                num_layers = count_of_real_vals(dlayer, max_layer);

                //sv- I added this resize, from max_layer to num_layer. 
                //    Done so the NitrogenChanges.DeltaNO3 etc. gets an array of a sensible size.
                Array.Resize(ref l_dlt_NO3, num_layers);        
                Array.Resize(ref l_dlt_NH4, num_layers);
                Array.Resize(ref i_dlt_sw_dep, num_layers);    

                for (int layer = 0; layer < num_layers; layer++)
                    {
                    l_dlt_NO3[layer] = i_dlt_no3gsm[layer] * gm2kg / sm2ha;
                    l_dlt_NH4[layer] = i_dlt_nh4gsm[layer] * gm2kg / sm2ha;
                    }


                NitrogenChangedType NitrogenChanges = new NitrogenChangedType();
                NitrogenChanges.Sender = "SugarCane";
                NitrogenChanges.DeltaNO3 = l_dlt_NO3;
                NitrogenChanges.DeltaNH4 = l_dlt_NH4;

                NitrogenChanged.Invoke(NitrogenChanges);  //trigger/invoke the Nitrogen Changed Event



                WaterChangedType WaterChanges = new WaterChangedType();
                WaterChanges.DeltaWater = i_dlt_sw_dep;

                WaterChanged.Invoke(WaterChanges);      //trigger/invoke the Water Changed Event

                }
            else if (uptake_source == "swim3")
                {
                num_layers = count_of_real_vals(dlayer, max_layer);

                //sv- I added this resize, from max_layer to num_layer
                //    Done so the NitrogenChanges.DeltaNO3 etc. gets an array of the sensible size.
                Array.Resize(ref l_dlt_NO3, num_layers);
                Array.Resize(ref l_dlt_NH4, num_layers);

                for (int layer = 0; layer < num_layers; layer++)
                    {
                    l_dlt_NO3[layer] = i_dlt_no3gsm[layer] * gm2kg / sm2ha;
                    l_dlt_NH4[layer] = i_dlt_nh4gsm[layer] * gm2kg / sm2ha;
                    }


                NitrogenChangedType NitrogenChanges = new NitrogenChangedType();
                NitrogenChanges.Sender = "SugarCane";
                NitrogenChanges.DeltaNO3 = l_dlt_NO3;
                NitrogenChanges.DeltaNH4 = l_dlt_NH4;

                NitrogenChanged.Invoke(NitrogenChanges);  //trigger/invoke the Nitrogen Changed Event
                }
            else
                {
                //! assume that the module that calculated uptake has also updated these pools.
                }

            }


        void sugar_update_other_variables(double[] i_dlt_dm_detached, double[] i_dlt_dm_dead_detached,     //called in sugar_update()
                                            double[] i_dlt_n_detached, double[] i_dlt_n_dead_detached,
                                            double[] i_root_length, double i_root_depth)
            {   

            //*+  Purpose
            //*       Update other modules states

            //*+  Mission Statement
            //*     Update other modules states

            //*+  Changes
            //*      250894 jngh specified and programmed
            //*      191099 jngh changed to sugar_Send_Crop_Chopped_Event
            //*     101100 dph  added eventInterface parameter to crop_root_incorp


            double[] l_dm_residue = new double[max_part];           //! dry matter removed (kg/ha)
            double[] l_N_residue = new double[max_part];           //! nitrogen removed (kg/ha)
            double[] l_fraction_to_Residue = new double[max_part];   //! fraction sent to residue (0-1)



            //! dispose of detached material from senesced parts in live population

            for (int p = 0; p < max_part; p++)
                {

                l_dm_residue[p] = (i_dlt_dm_detached[p] + i_dlt_dm_dead_detached[p]) * gm2kg / sm2ha;

                l_N_residue[p] = (i_dlt_n_detached[p] + i_dlt_n_dead_detached[p]) * gm2kg / sm2ha;

                l_fraction_to_Residue[p] = 1.0;
                }

            l_fraction_to_Residue[root] = 0.0;



            //!      call crop_top_residue (c%crop_type, dm_residue, N_residue)
            if (SumArray(l_dm_residue, max_part) > 0.0)
                {
                sugar_Send_Crop_Chopped_Event(crop_type, part_name, l_dm_residue, l_N_residue, l_fraction_to_Residue);
                }
            else
                {
                //! no surface residue
                }


            //! put roots into root residue

            crop_root_incorp((i_dlt_dm_detached[root] + i_dlt_dm_dead_detached[root]),
                                (i_dlt_n_detached[root] + i_dlt_n_dead_detached[root]),
                                dlayer, i_root_length, i_root_depth, crop_type, max_layer);


            }


        void sugar_Send_Crop_Chopped_Event(string i_crop_type, string[] i_dm_type, double[] i_dlt_crop_dm, double[] i_dlt_dm_n, double[] i_fraction_to_Residue)   //called above
            {
            //*+  Sub-Program Arguments
            //      character  crop_type*(*)              ! (INPUT) crop type
            //      character  dm_type(*)*(*)             ! (INPUT) residue type
            //      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
            //      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
            //      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
            //      integer max_part                      ! (INPUT) number of residue types
            //*+  Purpose
            //*     Notify other modules of crop chopped.

            //*+  Mission Statement
            //*     Notify other modules of crop chopped.

   
            CropChoppedType CropChanges = new CropChoppedType();
            CropChanges.crop_type = i_crop_type;
            CropChanges.dm_type = i_dm_type;
            CropChanges.dlt_crop_dm = ToFloatArray(i_dlt_crop_dm);
            CropChanges.dlt_dm_n = ToFloatArray(i_dlt_dm_n);
            CropChanges.dlt_dm_p = new float[i_dlt_dm_n.Length];            //sv- this should return a zeroed float array.
            CropChanges.fraction_to_residue = ToFloatArray(i_fraction_to_Residue);

            //! send message regardless of fatal error - will stop anyway
            CropChopped.Invoke(CropChanges);             //trigger/invoke the CropChopped Event

            }


        void crop_root_incorp(double i_dlt_dm_root, double i_dlt_n_root, double[] i_dlayer, double[] i_root_length, double i_root_depth,
                                string c_crop_type, int i_max_layer)     //called above
            {


            //!+  Sub-Program Arguments
            //      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
            //      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)
            //      real       g_dlayer(*)           ! (INPUT) layer thicknesses (mm)
            //      real       g_root_length(*)      ! (INPUT) layered root length (mm)
            //      real       g_root_depth          ! (INPUT) root depth (mm)
            //      character  c_crop_type*(*)       ! (INPUT) crop type
            //      integer    max_layer             ! (INPUT) maximum no of soil layers
            //      integer    incorpFOMID           ! (INPUT) ID of incorp fom event.
            //!+  Purpose
            //!       Calculate and provide root matter incorporation information
            //!       to the APSIM messaging system.

            //!+  Mission Statement
            //!   Pass root material to the soil modules (based on root length distribution)

            //!+  Changes
            //!     <insert here>
            //!     280800 jngh changed literal incorp_fom to ACTION_incorp_fom
            //!     011100 dph  added event_interface as a parameter.


               
            int         l_crop_max_layer = 100; //! maximum soil layers    //sv- taken from FortranInfrastructure\ConstantsModule.f90

            double[]    l_dlt_dm_incorp = new double[l_crop_max_layer]; //! root residue (kg/ha)
            double[]    l_dlt_N_incorp = new double[l_crop_max_layer];  //! root residue N (kg/ha)



            if (i_max_layer > l_crop_max_layer) 
                {
                throw new Exception("Too many layers for crop routines");
                }
            else
                {

                if (i_dlt_dm_root > 0.0)
                    {
                    //! send out root residue

                    crop_root_dist(i_dlayer, i_root_length, i_root_depth, ref l_dlt_dm_incorp, i_dlt_dm_root * gm2kg /sm2ha);

                    bound_check_real_array(l_dlt_dm_incorp, 0.0, i_dlt_dm_root * gm2kg/sm2ha, "dlt_dm_incorp", i_max_layer);


                    crop_root_dist(i_dlayer, i_root_length, i_root_depth, ref l_dlt_N_incorp, i_dlt_n_root * gm2kg /sm2ha);

                    bound_check_real_array(l_dlt_N_incorp, 0.0, i_dlt_n_root * gm2kg/sm2ha, "dlt_n_incorp", i_max_layer);



                    //sv- FOMLayerType and FOMLayerLayerType are confusing but what I think they mean is,
                    //    FOMLayer means FOM "IN" the soil as opposed to "ON" the surface of the SOIL
                    //    I think they already used the term FOMType for the Surface Residue so they needed a name 
                    //    for the class when they were refering to FOM "in" the soil. So they used the term FOMLayer.
                    //    This then meant when they needed a name for the class for an actual specific Layer they
                    //    then used the silly name of FOMLayerLayer. Meaning a Layer of the FOMLayer type.
                    //    This is silly naming but I think that this is what they have done.
                    //    They should have called it FOMInSoil or something, rather than FOMLayer.

                    FOMLayerLayerType[] allLayers = new FOMLayerLayerType[dlayer.Length]; 

                    for (int layer = 0; layer < dlayer.Length; layer++)
                        {
                        FOMType fom_in_layer = new FOMType();
                        fom_in_layer.amount = (float)l_dlt_dm_incorp[layer];
                        fom_in_layer.N = (float)l_dlt_N_incorp[layer];
                        fom_in_layer.P = (float)0.0;
                        fom_in_layer.C = (float)0.0;
                        fom_in_layer.AshAlk = (float)0.0;

                        FOMLayerLayerType thisLayer = new FOMLayerLayerType();
                        thisLayer.FOM = fom_in_layer;
                        thisLayer.CNR = (float)0.0;
                        thisLayer.LabileP = (float)0.0;

                        allLayers[layer] = thisLayer;
                        }

                    FOMLayerType fomInSoil = new FOMLayerType();
                    fomInSoil.Type = c_crop_type;
                    fomInSoil.Layer = allLayers;

                    IncorpFOM.Invoke(fomInSoil);   //trigger/invoke the IncorpFOM Event
                    }
                else
                    {
                    //! no roots to incorporate
                    }

                }
       
    

            }




        void sugar_hill_up(HillUpType HillUpData)
            {

            //*+  Purpose
            //*       Mound soil around base of crop and bury some plant material

            //*+  Mission Statement
            //*     Mound soil around base of crop

            double      canefr;
            double      topsfr;
            double[]    fom = new double[max_layer];
            double[]    fon = new double[max_layer];
     


            if ((int)g_current_stage == emerg) 
                {
                topsfr = HillUpData.topsfr;
                canefr = HillUpData.canefr;


                //Send Event to IncorpOM

                    //create Arrays

                fill_real_array(ref fom, 0.0, max_layer);
                fill_real_array(ref fon, 0.0, max_layer);

                fom[0] = topsfr * (g_dm_green[leaf] + g_dm_green[cabbage] + g_dm_senesced[leaf] + g_dm_senesced[cabbage] + g_dm_dead[leaf] + g_dm_dead[cabbage])
                       + canefr * (g_dm_green[sstem] + g_dm_green[sucrose]+ g_dm_senesced[sstem] + g_dm_senesced[sucrose] + g_dm_dead[sstem] + g_dm_dead[sucrose]);

                fon[0] = topsfr * (g_n_green[leaf] + g_n_green[cabbage] + g_n_senesced[leaf] + g_n_senesced[cabbage] + g_n_dead[leaf] + g_n_dead[cabbage])
                       + canefr * (g_n_green[sstem] + g_n_green[sucrose] + g_n_senesced[sstem] + g_n_senesced[sucrose] + g_n_dead[sstem] + g_n_dead[sucrose]);


                    //do dotnet event sending

                FOMLayerLayerType[] allLayers = new FOMLayerLayerType[dlayer.Length]; 

                for (int layer = 0; layer < dlayer.Length; layer++)
                    {
                    FOMType fom_in_layer = new FOMType();
                    fom_in_layer.amount = (float)fom[layer];
                    fom_in_layer.N = (float)fon[layer];
                    fom_in_layer.P = (float)0.0;
                    fom_in_layer.C = (float)0.0;
                    fom_in_layer.AshAlk = (float)0.0;

                    FOMLayerLayerType thisLayer = new FOMLayerLayerType();
                    thisLayer.FOM = fom_in_layer;
                    thisLayer.CNR = (float)0.0;
                    thisLayer.LabileP = (float)0.0;

                    allLayers[layer] = thisLayer;
                    }

                FOMLayerType fomInSoil = new FOMLayerType();
                fomInSoil.Type = crop_type;
                fomInSoil.Layer = allLayers;

                IncorpFOM.Invoke(fomInSoil);   //trigger/invoke the IncorpFOM Event



                //Change Global Variables

                    //dm variables

                g_dm_green[leaf] = g_dm_green[leaf]*(1.0-topsfr);
                g_dm_green[cabbage] = g_dm_green[cabbage]*(1.0-topsfr);
                g_dm_senesced[leaf] = g_dm_senesced[leaf]*(1.0-topsfr);
                g_dm_senesced[cabbage] = g_dm_senesced[cabbage]*(1.0-topsfr);
                g_dm_dead[leaf] = g_dm_dead[leaf]*(1.0-topsfr);
                g_dm_dead[cabbage] = g_dm_dead[cabbage]*(1.0-topsfr);

                g_dm_green[sstem] = g_dm_green[sstem]*(1.0-canefr);
                g_dm_green[sucrose] = g_dm_green[sucrose]*(1.0-canefr);
                g_dm_senesced[sstem] = g_dm_senesced[sstem]*(1.0-canefr);
                g_dm_senesced[sucrose] = g_dm_senesced[sucrose]*(1.0-canefr);
                g_dm_dead[sstem] = g_dm_dead[sstem]*(1.0-canefr);
                g_dm_dead[sucrose] = g_dm_dead[sucrose]*(1.0-canefr);

                    //nitrogen variables

                g_n_green[leaf] = g_n_green[leaf]*(1.0-topsfr);
                g_n_green[cabbage] = g_n_green[cabbage]*(1.0-topsfr);
                g_n_senesced[leaf] = g_n_senesced[leaf]*(1.0-topsfr);
                g_n_senesced[cabbage] = g_n_senesced[cabbage]*(1.0-topsfr);
                g_n_dead[leaf] = g_n_dead[leaf]*(1.0-topsfr);
                g_n_dead[cabbage] = g_n_dead[cabbage]*(1.0-topsfr);

                g_n_green[sstem] = g_n_green[sstem]*(1.0-canefr);
                g_n_green[sucrose] = g_n_green[sucrose]*(1.0-canefr);
                g_n_senesced[sstem] = g_n_senesced[sstem]*(1.0-canefr);
                g_n_senesced[sucrose] = g_n_senesced[sucrose]*(1.0-canefr);
                g_n_dead[sstem] = g_n_dead[sstem]*(1.0-canefr);
                g_n_dead[sucrose] = g_n_dead[sucrose]*(1.0-canefr);


                    //! Now we need to update the leaf tracking info

                g_lai = g_lai * (1.0 - topsfr);
                g_slai = g_slai * (1.0 - topsfr);

                for (int leaf_no=0; leaf_no<max_leaf; leaf_no++)
                    {
                    g_leaf_area_zb[leaf_no] = g_leaf_area_zb[leaf_no] * (1.0 - topsfr);
                    g_leaf_dm_zb[leaf_no] = g_leaf_dm_zb[leaf_no] * (1.0 - topsfr);
                    }

                }
            else
                {
                throw new Exception("Can only hill up during emergence phase");
                }


            }







    #endregion



    }





