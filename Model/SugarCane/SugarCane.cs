using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using ModelFramework;


///<summary>
/// .NET port of the Fortran Sugar model
/// Ported by Shaun Verrall Feb 2013
///</summary> 


public class SugarCane
    {


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

          const  int    root    = 1;    //! root
          const  int    leaf    = 2;    //! leaf
          const  int    sstem   = 3;    //! structural stem
          const  int    cabbage = 4;    //! cabbage
          const  int    sucrose = 5;    //! grain

          const  int    max_part = 5;   //! number of plant parts

          string[] part_name = new string[max_part] {"root", "leaf", "sstem", "cabbage", "sucrose"};  //TODO: this is zero based not 1 based.


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
            String             crop_type;

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



        #region Crop Constants (different for Plant versus Ratoon)

            [Param]
            CropConstants plant = new CropConstants();

            [Param]
            CropConstants ratoon = new CropConstants();

        #endregion



        #region Cultivar Constants (different for each cultivar)




        #endregion



        #region Root Constants  (different depending on if using (SoilWat or SWIM) or Eo modules in the simulation)




        //*     ===========================================================
        //      subroutine sugar_read_root_params ()        //*     Crop initialisation - get root profile parameters
        //*     ===========================================================
      

             //!       cproc_sw_demand_bound

            [Param(IsOptional =  true, MinVal = 0.0, MaxVal = 100.0)]
            double eo_crop_factor = 100.0;
                    //double      eo_crop_factor = eo_crop_factor_default;
    //TODO: Either find a way to do this or remove 'eo_crop_factor_default' from the ini file.

            [Param] 
            string      uptake_source;

            //! eXtension rate Factor (0-1)
            [Param(MaxVal = 0.0, MinVal = 1.0)]
            double[]    xf = new double[max_layer];



                                    //!       sugar_sw_supply

    //TODO: Either find a way to do this or remove 'll_ub' from the ini file.
    //[Param(IsOptional = true, MinVal = 0.0, MaxVal = ll_ub)]
            [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
            double[]    ll = new double[max_layer];

                            //! root length density factor for water
    //TODO: Either find a way to do this or remove 'kl_ub' from the ini file.
    //[Param(MinVal = 0.0, MaxVal = kl_ub)]
            [Param(MinVal = 0.0, MaxVal = 1.0)]
            double[]    kl = new double[max_layer];


            [Param(MinVal = 0.0, MaxVal = 20.0)]
            double[]    rlv = new double[max_layer];


        #endregion


    #endregion



    //INPUTS FROM OTHER MODULES

    #region Inputs only needed at OnInitialised

    //sugar_read_root_params()

        //TODO: [Input(MinVal = 0.0, MaxVal = 1.0, IsOptional = true)]
        [Input(IsOptional = true)]
        double      swim3 =  Double.NaN;

        //TODO: [Input(MinVal = 0.0, MaxVal = ll_ub, IsOptional = true)]
        [Input(IsOptional = true)]
        double[]    ll15 = new double[max_layer];
    

    #endregion


    #region Inputs needed daily


    #endregion




    //MODEL

    #region Local Variables 
            

        //Sizes of arrays read in from the INI file (Actually the SIM file).
                //(see OnInitialised event handler for where these are first initialised).
        int         num_plant_rld;
        int         num_sla_lfno;
        int         num_stem_wt;
        int         num_fasw_emerg;
        int         num_node_no_app;
        int         num_node_no_leaf;
        int         num_frost_temp;
        int         num_N_conc_stage;   //! no of values in stage table
        int         num_ave_temp;       //! size_of of critical temperature table
        int         num_ave_temp_stalk; //! size_of of critical temperature table
        int         num_temp;           //! size_of of table
        int         num_sw_demand_ratio;
        int         num_demand_ratio_stalk;
        int         num_sw_avail_ratio;
        int         num_sw_ratio;
        int         num_oxdef_photo;
        int         num_afps;
        int         num_cane_dmf;
        int         num_stress_lodge;
        int         num_leaf_size;
        int         num_stress_factor_stalk;
        int         num_tillerf_leaf_size;


        //Root parameters that are calculated from values read in from the INI/SIM file

        double[]    ll_dep = new double[max_layer];          //! lower limit of plant-extractable soil water for soil layer L (mm)
        double[]    root_length =  new double[max_layer];        




        //sv- I THINK THE FOLLOWING ARE NOT USED, BUT HAVE BEEN ACCIDENTLY LEFT IN THE SOURCE CODE WHEN THEY SHOULD HAVE BEEN REMOVED.
        //!     ================================================================
        //!     Sugar Constants
        //!     ================================================================
        double      frost_kill;             //! temperature threshold for leaf death (oC)
        int         num_dead_lfno;
        int         num_factors;            //! size_of of table
        int         num_x_swdef_cellxp;        
        double      leaf_no_min;            //! lower limit of leaf number ()
        double      leaf_no_max;            //! upper limit of leaf number ()


    #endregion


    #region Functions to Zero Variables


        private void ZeroArray(ref double[] A)
            {
                for (int i = 0; i < A.Length; i++)
                {
                    A[i] = 0.0;
                }
            }



    #endregion








    //EVENT HANDLERS

    #region Functions used in Oninitialsed Event Handler

//TODO: Put this back in again.
        //void InitializeSizeOfArraysReadIn()
        //    {
        //    num_plant_rld       = plant.y_rel_root_rate.Length;
        //    num_sla_lfno        = plant.sla_lfno.Length;
        //    num_stem_wt         = plant.y_height.Length;
        //    num_fasw_emerg      = plant.rel_emerg_rate.Length;
        //    num_node_no_app     = plant.y_node_app_rate.Length;
        //    num_node_no_leaf    = plant.y_leaves_per_node.Length;
        //    num_frost_temp      = plant.frost_temp.Length;
        //    num_N_conc_stage    = plant.y_N_conc_min_cabbage.Length;
        //    num_ave_temp        = plant.x_ave_temp.Length;
        //    num_ave_temp_stalk  = plant.x_ave_temp_stalk.Length;
        //    num_temp            = plant.y_tt.Length;
        //    num_sw_demand_ratio = plant.y_swdef_leaf.Length;
        //    num_demand_ratio_stalk = plant.y_swdef_stalk.Length;
        //    num_sw_avail_ratio  = plant.y_swdef_pheno.Length;
        //    num_sw_ratio        = plant.y_sw_fac_root.Length;
        //    num_oxdef_photo     = plant.oxdef_photo.Length;
        //    num_afps            = plant.y_afps_fac.Length;
        //    num_cane_dmf        = plant.cane_dmf_tt.Length;
        //    num_stress_lodge    = plant.death_fr_lodge.Length;
        //    num_leaf_size       = plant.leaf_size_no.Length;
        //    num_stress_factor_stalk = plant.stress_factor_stalk.Length;
        //    num_tillerf_leaf_size = plant.tillerf_leaf_size_no.Length;
        //    }


        void CheckRightOptionalsReadIn()
            {
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


        //void InitialiseRootParams()    
        //    {
        //    //************************
        //    //sugar_read_root_params()
        //    //************************

        //    //!       cproc_sw_demand_bound

        //    if (swim3 != Double.NaN)
        //        uptake_source = "swim3";


        //     //!       sugar_sw_supply

        //    //if optional ll has read in some values (not its default maximum size)
        //    if (ll.Length < max_layer)
        //            {
        //            for(int layer=1; num_layers; layer++)
        //                {
        //                 ll_dep[layer] = ll[layer] * dlayer[layer];
        //                }
        //            }
        //    else
        //            {
        //            //if optional ll15 has read in some values (not its default maximum size)
        //            if (1115.Length < max_layer)
        //                {
        //                for(int layer=1; num_layers; layer++)
        //                    {
        //                    ll_dep[layer] = ll[layer] * dlayer[layer];
        //                    }
        //                 Console.WriteLine("Using externally supplied Lower Limit (ll15)");
        //                }
        //            else
        //                {
        //                throw new Exception("No Crop Lower Limit found");
        //                }
        //            }

        //    //caluculate the root length
        //    for (int layer = 1; num_layers; layer++)
        //        {
        //        root_length[layer] = rlv[layer] * dlayer[layer];
        //        }   



        //    //write out to the summary file what optional values you have read in.
        //    ReportRootParamsReadIn();
        //    }


        void ReportRootParamsReadIn()
            {


            //************************
            //sugar_read_root_params()
            //************************

            //After reading the root profile from the ini file, write out what was read to summary file.


            Console.WriteLine("\n" + "   - Reading root profile parameters");


            if (uptake_source =="calc")
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
                throw new Exception( "Bad value for uptake_source");
                }


            string line;

             line = "                    Root Profile";
             Console.WriteLine(line);

             line = "  --------------------------------------------------";
             Console.WriteLine(line);

             line = "    Layer depth  Kl factor   Lower limit Root Factor";
             Console.WriteLine(line);

             line = "         (mm)         ()        (mm/mm)     (0-1)";
             Console.WriteLine(line);

             line = "  --------------------------------------------------";
             Console.WriteLine(line);
//TODO:PUT THIS BACK IN AGAIN,
            //for (int layer= 1; numlayers; layer++)
            //    {
            //    Console.WriteLine(" {0:f12.3} {1:f12.3} {2:f12.3} {3:f12.3}", dlayer[layer], kl[layer], ll[layer], xf[layer]);
            //    }

             line = "   -------------------------------------------------";
             Console.WriteLine(line);
             Console.WriteLine("\n");


             Console.WriteLine("  {0} {1:f5.1} {1}","  Crop factor for bounding water use is set to ", eo_crop_factor, " times Eo");
             Console.WriteLine("\n" + "\n");

            }

    #endregion


    #region Clock Event Handlers

//TODO: put these back in again.
        //[EventHandler]
        //public void OnInit2()
        //    {
        //    InitializeSizeOfArraysReadIn();
        //    CheckRightOptionalsReadIn();
        //    //InitialiseRootParams();
        //    ReportRootParamsReadIn();
        //    }


        [EventHandler]
        public void OnInitialised()
            {
        //    InitializeSizeOfArraysReadIn();
        //    CheckRightOptionalsReadIn();
        //    //InitialiseRootParams();
        //    ReportRootParamsReadIn();
            }

        //[EventHandler]
        //public void OnPrepare()
        //    {
        //    InitializeSizeOfArraysReadIn();
        //    CheckRightOptionalsReadIn();
        //    //InitialiseRootParams();
        //    ReportRootParamsReadIn();
        //    }

        //[EventHandler]
        //public void OnProcess()
        //    {
        //    InitializeSizeOfArraysReadIn();
        //    CheckRightOptionalsReadIn();
        //    //InitialiseRootParams();
        //    ReportRootParamsReadIn();
        //    }


    #endregion



    }

