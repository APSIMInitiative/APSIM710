using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using ModelFramework;


///<summary>
/// .NET port of the Fortran SoilWat model
/// Ported by Shaun Verrall Mar 2011
///</summary> 

 
public class SoilWater : Instance
   {

#region Constants

    //Constants for Array Declaration
   
   private const double  precision_sw_dep  = 1.0e-3F; //!Precision for sw dep (mm)
   private const int     max_layer = 100;       //! Maximum number of layers
   private const int     max_table = 10;        //! Used in LaterFlow. 
   private const int     max_solute = 20;       //! Maximum number of solutes in the soil
   private const int     max_coeffs  = 10;      //! Maximum number of coefficients in a table
   private const int     max_crops = 10;        //! maximum number of crops in at once
   private const int     module_name_size = 30; //! maximum length of module name
   private const int     ritchie_method = 1;
   
#endregion


//INPUTS FROM SIM FILE & OUTPUTS OF THIS MODULE

#region Module Constants (from SIM file but it gets from INI file)

    //SIM file gets these from INI file

    //Soilwat2Constants       //sv- also from soilwat2_read_constants()


   [Output]
   [Param(MinVal=0.0, MaxVal=10.0)] 
   [Units("(oC)")]
   private double    min_crit_temp;             //! temperature below which eeq decreases (oC)

   [Output]
   [Param(MinVal=0.0, MaxVal=50.0)] 
   [Units("(oC)")]
   private double    max_crit_temp;             //! temperature above which eeq increases (oC)

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)]
   [Units("(0-1)")] 
   private double    max_albedo;                //! maximum bare ground soil albedo (0-1)
   
   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")] 
   private double    A_to_evap_fact;            //! factor to convert "A" to coefficient in Adam's type residue effect on Eos

   [Output]
   [Param(MinVal=0.0, MaxVal=10.0)]
   [Units("(0-10)")]  
   private double    canopy_eos_coef;           //! coef in cover Eos reduction eqn

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")] 
   private double    sw_top_crit;               //! critical sw ratio in top layer below which stage 2 evaporation occurs

   [Output]
   [Param(MinVal=0.0, MaxVal=1000.0)]
   [Units("(mm)")]  
   private double    sumes1_max;                //! upper limit of sumes1
   
   [Output]
   [Param(MinVal=0.0, MaxVal=1000.0)]
   [Units("(mm)")]  
   private double    sumes2_max;                //! upper limit of sumes2

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")] 
   private double[]  solute_flow_eff; //= new double[max_layer];       //sv- Unsaturated Flow   //! efficiency of moving solute with flow (0-1)
   private int num_solute_flow;   //bound_check_real_array() gives this a value in soilwat2_read_constants()

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")] 
   private double[]  solute_flux_eff; //= new double[max_layer];        //sv- Drainage (Saturated Flow)   //! efficiency of moving solute with flux (0-1) 
   private int num_solute_flux; //bound_check_real_array() gives this a value in soilwat2_read_constants()

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)]
   [Units("(0-1)")]  
   private double    gravity_gradient;          //! gradient due to hydraulic differentials (0-1)

   [Output]
   [Param(MinVal=0.0, MaxVal=3.0)]
   [Units("(g/cc)")] 
   private double    specific_bd;               //! specific bulk density (g/cc)

   [Output]
   [Param(MinVal=1.0, MaxVal=1000.0)] 
   [Units("(mm)")] 
   private double    hydrol_effective_depth;    //! hydrologically effective depth for runoff (mm)

   [Output]
   [Param] 
   [Units("()")] 
   private string[]  mobile_solutes = new string[max_solute];     //! names of all possible mobile solutes

   [Output]
   [Param] 
   [Units("()")] 
   private string[]  immobile_solutes = new string[max_solute];   //! names of all possible immobile solutes

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")] 
   private double[]  canopy_fact = new double[max_coeffs];        //! canopy factors for cover runoff effect ()
   int         num_canopy_fact; //! number of canopy factors read () //gives this a value in soilwat2_read_constants()

   [Output]
   [Param(MinVal=0.0, MaxVal=100000.0)] 
   [Units("(mm)")] 
   private double[]  canopy_fact_height = new double[max_coeffs]; //! heights for canopy factors (mm)

   [Output]
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")] 
   private double    canopy_fact_default;       //! default canopy factor in absence of height ()

   [Output]
   [Param] 
   private string    act_evap_method;           //! actual soil evaporation model being used //sv- hard wired to "ritchie" in the init event handler. 
   private int       evap_method;               //sv- integer representation of act_evap_method   

    //Soilwat2Constants


   //OnTillage Event
   //***************
   [Param(IsOptional=true, Name="type")]  //! Array containing information about a certain type (from table)  //sv- "type" as in the type of tillage: "disc", "burn", etc. 
   [Units("()")]
   private double[] type_info_from_sim = null;   //sv- contains the tillage_cn_red and tillage_cn_rain specified in the sim file. Only used if manager module does not send it with the Event. 


   //Irrigation Layer

   [Param(IsOptional=true, MinVal=0, MaxVal=100)]
   [Units("()")]
   [Output]
   private int         irrigation_layer = -1;      //! number of soil layer to which irrigation water is applied


#endregion


#region Soil "Property" (NOT layered): (Constants & Starting Values from SIM file), and the Outputs 

   /*
   //SIM file gets them from .APSIM file
   //sv- Any Params that are Optional give a value of -1 or "not_read". -1 is an impossible value and is used to tell you that a value was not read in from the .sim file for this variable.
   //Soilwat2Parameters   //sv- also from soilwat2_soil_property_param()
   */
   

   //see "obsrunoff" in the Daily Inputs ie. [Input] tag.
   [Output]
   [Param(IsOptional=true, Name="observed_runoff")]
   [Units("()")]
   private string      obsrunoff_name = "not_read";    //! system name of observed runoff


   private string      _eo_source = "not_read";
   [Output]
   [Param(IsOptional=true)]
   [Units("()")]
   public string      eo_source      //! system variable name of external eo source
      {
      get {return _eo_source;}
      set 
         {
         _eo_source = value;
         Console.WriteLine("Eo source: " + _eo_source);
         }
      }



   //sv- initial sw section (5 different methods to choose from) (see soilwat2_init() and soilwat2_set_default() to see which method is used)
   
      //insoil is used for two different initial sw methods:
      //1. User Specified Soil Water Conent method is used when insoil > 1 
      //2. Fill every layer in the soil to the same specified fraction of esw (specified by insoil)  (0 <= insoil <= 1) 

   private int       numvals_insoil = 0;                    //! number of values returned for insoil
   private double    _insoil = Double.NaN; 
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10.0)]
   [Units("(0-10)")]   
   public double     insoil            //! switch describing initial soil water  //sv-specifies which option you are using.
      {
      get {return _insoil;}
      set
         {
         //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_numvals_insoil = 1;
            reset_insoil = value;
            }   
         soilwat2_zero_default_variables();  
         numvals_insoil = 1;     //used in soilwat2_set_default()
         _insoil = value;
         soilwat2_set_default();      
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }
         }
      }

      //3. Starting from the top, fill the soil until a specified fraction of the entire soils esw is reached. Fill each layer to dul.

   private int       numvals_profile_fesw = 0;              //! number of values returned for profile_fesw
   private double    _profile_fesw = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1.0)]
   [Units("(0-1)")]   
   public double    profile_fesw     //! initial fraction of esw of profile distributed from top down ()
      {
      get{return _profile_fesw;}
      set
         {
         //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_numvals_profile_fesw = 1;
            reset_profile_fesw = value;
            }   
         soilwat2_zero_default_variables();
         numvals_profile_fesw = 1;  //used in soilwat2_set_default()
         _profile_fesw = value;
         soilwat2_set_default();       
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }
         }
      }


      //4. Starting from the top, fill the soil until a specified amount of esw for the entire soil is reached. Fill each layer to dul.

   private int         numvals_profile_esw_depth = 0;         //! number of values returned for profile_esw_depth
   private double      _profile_esw_depth = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10000.0)]
   [Units("(mm)")]   
   public double      profile_esw_depth   //! initial depth of extractable soil water distributed from the top down (mm)
      {
      get{return _profile_esw_depth;}
      set
         {
         //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_numvals_profile_esw_depth = 1;
            reset_profile_esw_depth = value;
            }  
         soilwat2_zero_default_variables();
         numvals_profile_esw_depth = 1;   //used in soilwat2_set_default()
         _profile_esw_depth = value;
         soilwat2_set_default();      
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }
         }      
      }


      //5. Starting from the top fill the soil to a specified soil depth. Fill each layer to dul. 

   private int         numvals_wet_soil_depth = 0;            //! number of values returned for wet_soil_depth
   private double      _wet_soil_depth = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10000.0)]
   [Units("(mm)")]   
   private double      wet_soil_depth   //! initial depth of soil filled to drained upper limit (field capacity) (mm)
      {
      get{return _wet_soil_depth;}
      set
         {
         //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_wet_soil_depth = value;
            reset_numvals_wet_soil_depth = 1;
            } 
         soilwat2_zero_default_variables();
         numvals_wet_soil_depth = 1;      //used in soilwat2_set_default()
         _wet_soil_depth = value;
         soilwat2_set_default();      
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }
         }  
      }

   //sv- end of initial sw section



   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1000.0)]
   [Units("(0-1000)")]
   private double    diffus_const = Double.NaN;     //! diffusivity constant for soil testure

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=100.0)]
   [Units("(0-100)")]
   private double    diffus_slope = Double.NaN;     //! slope for diffusivity/soil water content relationship

   private double   _cn2_bare = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=1.0, MaxVal=100.0)]
   [Units("(1-100)")]
   public double    cn2_bare         //! curve number input used to calculate daily runoff
      {
      get {return _cn2_bare;}
      set 
         {
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_cn2_bare = value;
            } 
         _cn2_bare = value;
         }
      }

   private double   _cn_red = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=100.0)]
   [Units("(0-100)")]
   public double    cn_red           //! maximum reduction in cn2_bare due to cover
      {
      get {return _cn_red;}
      set 
         {
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_cn_red = value;
            } 
         _cn_red = value;
         }
      }

   private double    _cn_cov = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1.0)]
   [Units("(0-1)")]
   public double    cn_cov           //! cover at which cn_red occurs
      {
      get {return _cn_cov;}
      set 
         {
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_cn_cov = value;
            } 
         _cn_cov = value;
         }      
      }

   private double    _max_pond = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1000.0)]
   [Units("()")]
   public double    max_pond         //! maximum surface storage capacity of soil  //sv- used to store water from runoff on the surface.
      {
      get {return _max_pond;}
      set 
         {
         //* made settable to allow for erosion 'max_pond'
         //*** dsg 280103  Added re-settable 'max-pond' for Shaun Lisson to simulate dam-break in rice cropping
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_max_pond = value;
            } 
         _max_pond = value;
         }
      }


   [Param(MinVal=0.0001, MaxVal=1.0)]
   [Units("(>0-1)")]
   [Output]  
   private double      salb;           //! bare soil albedo (unitless)



   //Extra parameters for evaporation models (this module only has Ritchie Evaporation)  
         //(see soilwat2_init() for which u and cona is used)

         //same evap for summer and winter
   private double    _u = Double.NaN;
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=40.0)]
   [Units("(mm)")]
   public double    u            //! upper limit of stage 1 soil evaporation (mm)
      {
      get {return _u;}
      set {throw new Exception("setting U is done via GUI");}
      }

   private double    _cona = Double.NaN; 
   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10.0)]
   [Units("(>0-10)")]
   public double    cona         //! stage 2 drying coefficient
      {
      get {return _cona;}
      set {throw new Exception("setting cona is done via GUI");}
      }

         //different evap for summer and winter
            //summer
   [Output]
   [Param(IsOptional=true)] 
   [Units("(dd-mmm)")]
   private string    summerdate = "not_read";       //! Date for start of summer evaporation (dd-mmm)

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=40.0)]
   [Units("(mm)")]
   private double    summeru = Double.NaN;       

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10.0)]
   [Units("(>0-10)")] 
   private double    summercona = Double.NaN;     
            //winter
   [Output]
   [Param(IsOptional=true)] 
   [Units("(dd-mmm)")]
   private string    winterdate = "not_read";       //! Date for start of winter evaporation (dd-mmm)

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10.0)]
   [Units("(mm)")]
   private double    winteru = Double.NaN;        

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=10.0)]
   [Units("(>0-10)")] 
   private double    wintercona = Double.NaN;   

   //end of Extra parameters for evaporation models


   //sv- Lateral flow properties  //sv- also from Lateral_read_param()

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1.0)]
   [Units("(0-1)")] 
   private double    slope = Double.NaN;

   [Output]
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1.0e8F)]     //1.0e8F = 100000000
   [Units("(m)")]
   private double    discharge_width = Double.NaN;  //! basal width of discharge area (m)

   [Param(IsOptional=true, MinVal=0.0, MaxVal=1.0e8F)]     //1.0e8F = 100000000
   [Units("(m2)")] 
   private double    catchment_area = Double.NaN;   //! area over which lateral flow is occuring (m2)

   //sv- end of Lateral flow properties



   //sv- PURE OUTPUTS

   //Soilwat2Globals  //sv- also from soilwat2_send_my_variable()  
                      //(Also see the bottom of OnProcess Event Handler for where calculations for some of these variables are done before outputting.)  

                        //TODO: you have not got all the variables from soilwat2_send_my_variable() yet. There are still some missing eg. esw 

   [Output]
   private double    es;                      //! total es     //sv- local variable declared in soilwat2_send_my_variable()

   [Output]
   private double    eff_rain;                  //! daily effective rainfall (mm)   //sv- local variable declared in soilwat2_send_my_variable()

   [Output]
   private double    esw;                       //! potential extractable sw in profile   //sv- local variable declared in soilwat2_send_my_variable()

   [Output] 
   private double    cover_surface_runoff;     //! effective total cover (0-1)   //residue cover + cover from any crops (tall or short)

   [Output] 
   private double    t;                        //! time after 2nd-stage soil evaporation begins (d)

   [Output] 
   private double    eo;                       //! effective potential evapotranspiration (mm)

   [Output] 
   private double    eos;                      //! pot sevap after modification for green cover & residue wt
   
   [Output] 
   private double    cn2_new;                  //! New cn2  after modification for crop cover & residue cover

   [Output] 
   private double    drain;            //! drainage rate from bottom layer (cm/d)

   [Output] 
   private double    infiltration;     //! infiltration (mm)

   [Output] 
   private double    runoff;           //! runoff (mm)

   [Output] 
   private double    pond_evap;      //! evaporation from the surface of the pond (mm)

   [Output] 
   private double    pond;           //! surface water ponding depth


   //Soilwat2Globals





   //taken from soilwat2_set_my_variable()


   //nb. water_table is both an input and an output. 
   //It is always is an output because a water table can always build up. (See soilwat_water_table())
   //Sometimes it is an input when the user specifies a set command in a manager because they want to set the water_table at a specific height on a given day. (see SetWaterTable())

   private double _water_table = Double.NaN; 
   [Output]
   [Units("(mm)")] 
   public double  water_table     //! water table depth (depth below the ground surface of the first saturated layer)
      {
      get{ return _water_table; }
      set{ SetWaterTable(value); }
      }   


   //end of soilwat2_set_my_variable()



#endregion


#region Soil "Profile" (layered): (Constants & Starting Values from SIM file), and the Outputs 


   //Has the soilwat_init() been done? If so let the fractional soil arrays (eg. sw, sat, dul etc) now allow "sets".
   //It is the fractional soil arrays that are read in from the SIM file as [Param]'s.
   //They are then used to initialise the equivalent "_dep" arrays in soilwat_int() (more specfically by soilwat2_soil_profile_param()).
   //This causes a problem because the fractional arrays also use their _dep equivalent in their "set".
   //(because it is the _dep arrays that are actually used by the model not the fractionals. The fractional arrays are just used for input and ouput, so just changing them and not the _dep array is pointless.) 
   //If the set for the fraction array is called BEFORE the soilwat_init(), then this causes an error. 
   //Which is exactly what happens at the start of the simulation when the [Param] tags get initialised. 
   //They call the fractional arrays "set" which tries to use the _dep array which is uninitialised because soilwat_init() has not been called yet. 
   //This flag is used to prevent in fractional arrays "gets" or "sets" the calling of any code with a _dep array in it before the soilwat_init() has been called. 

   bool initDone = false;    
                            
                            
                            


   //SIM file gets them from .APSIM file

    //Soilwat2Parameters   //sv- also from soilwat2_soil_profile_param()


   private double[]    _dlayer;
   [Param(MinVal=0.0, MaxVal=10000.0)] 
   [Units("(mm)")]
   [Output]  
   public double[]    dlayer    //! thickness of soil layer (mm)
      {
      get {return _dlayer;}
      set 
         { 
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_dlayer = au.Dup(value);
            } 

         //* made settable to allow for erosion
         if (initDone)
            {
            int num_layers = au.count_of_real_vals(value, max_layer);
            for(int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               //If you change the depths of the layer then you need to modify the water "_dep" variables by the same amount. (they are in mm too)
               //If you don't do this, you will have the same amount of water that is now in a shallower layer, 
               //therefore you will have a different fraction equivalent variables, the ones without the "_dep" eg. sw, dul.  
               double fract = mu.divide(value[layer], _dlayer[layer], 0.0);
               _air_dry_dep[layer] = _air_dry_dep[layer] * fract;
               _dul_dep[layer] = _dul_dep[layer] * fract;
               _ll15_dep[layer] = _ll15_dep[layer] * fract;
               _sat_dep[layer] = _sat_dep[layer] * fract;
               _sw_dep[layer] = _sw_dep[layer] * fract;
            
               //_dlayer[layer] = value[layer];
 
               soilwat2_check_profile(layer);
               }

            //resize all the arrays if they changed the number of layers
            if (num_layers != _dlayer.Length)
               {
               Array.Resize(ref _air_dry_dep, num_layers);
               Array.Resize(ref _dul_dep, num_layers);
               Array.Resize(ref _ll15_dep, num_layers);
               Array.Resize(ref _sat_dep, num_layers);
               Array.Resize(ref _sw_dep, num_layers);
               Array.Resize(ref _dlayer, num_layers);  
               }

            soilwat2_New_Profile_Event();
            }

         _dlayer = au.Dup(value);

         }
      }


   private double[]    _sat;        
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")]
   [Output]  
   public double[]    sat       //! saturated water content for layer  
      {
      get 
         {
         if (initDone)
            {  
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for (int layer = au.si; layer <= au.ci(num_layers); layer++)
               {
               _sat[layer] = mu.divide(_sat_dep[layer], _dlayer[layer], 0.0);
               }
            }
         return _sat;
         }
      set 
         {
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_sat = au.Dup(value);
            } 

         //* made settable to allow for erosion
         _sat = au.Dup(value);       //This fractional value is used in soilwat_init() to initialise _sat_dep. That is the only place it is used. Everywhere else uses _sat_dep.
         if (initDone)
            {  
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for(int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               _sat_dep[layer] = value[layer] * _dlayer[layer];   //change sat_dep NOT sat. The sat variable is just for inputting and outputting and is immediately converted to sw_dep.
               soilwat2_check_profile(layer);
               }   
            }      
         }
      }


   private double[]    _dul;    
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")]
   [Output]  
   public double[]    dul       //! drained upper limit soil water content for each soil layer 
      {
      get 
         {
         //* made settable to allow for erosion
         if (initDone)
            {  
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for (int layer = au.si; layer <= au.ci(num_layers); layer++)
               {
               _dul[layer] = mu.divide(_dul_dep[layer], _dlayer[layer], 0.0);
               }
            }
         return _dul;
         }
      set 
         { 
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_dul = au.Dup(value);
            } 
         //* made settable to allow for erosion
         _dul = au.Dup(value);     //This fractional value is used in soilwat_init() to initialise _dul_dep. That is the only place it is used. Everywhere else uses _dul_dep. 
         if (initDone)
            {       
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for(int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               _dul_dep[layer] = value[layer] * _dlayer[layer];   //change dul_dep NOT dul. The dul variable is just for inputting and outputting and is immediately converted to sw_dep.
               soilwat2_check_profile(layer);
               }         
            }
         }
      }

   private int        numvals_sw = 0;                        //! number of values returned for sw 
   private double[]    _sw;   
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")]
   [Output]  
   public double[]    sw        //! soil water content of layer
      {
      get 
         {
         if (initDone)
            {
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for (int layer = au.si; layer <= au.ci(num_layers); layer++)
               {
               _sw[layer] = mu.divide(_sw_dep[layer], _dlayer[layer], 0.0);
               }
            }
          return _sw;
          }
      set 
         {
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_numvals_sw = value.Length;
            reset_sw = au.Dup(value);
            } 

         if (initDone)
            {
            double[] sw_dep_old;
            double sw_dep_lyr, sw_dep_delta_sum;
            sw_dep_old = _sw_dep;
            soilwat2_zero_default_variables();
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            sw_dep_delta_sum = 0.0;
            for(int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               sw_dep_lyr = value[layer] * _dlayer[layer];   //sw_dep = sw * dlayer
               sw_dep_delta_sum = sw_dep_delta_sum + (sw_dep_lyr - sw_dep_old[layer]);   //accumulate the change in the entire soil profile.
               _sw_dep[layer] = sw_dep_lyr;  //change sw_dep NOT sw. The sw variable is just for inputting and outputting and is immediately converted to sw_dep.    
               soilwat2_check_profile(layer);
               }
    
            soilwat2_ExternalMassFlow(sw_dep_delta_sum);     //tell the "System Balance" module (if there is one) that the user has changed the water by this amount.
            }
         numvals_sw = value.Length;          //used in soilwat2_set_default()
         _sw = au.Dup(value);        //This fractional value is used in soilwat_init() to initialise _sw_dep. That is the only place it is used. Everywhere else uses _sw_dep.
         }
      }


   private double[]    _ll15;    
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")]
   [Output]  
   public double[]    ll15      //! 15 bar lower limit of extractable soil water for each soil layer
      {
      get 
         {
         if (initDone)
            {  
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for (int layer = au.si; layer <= au.ci(num_layers); layer++)
               {
               _ll15[layer] = mu.divide(_ll15_dep[layer], _dlayer[layer], 0.0);
               }
            }
         return _ll15;
         }
      set 
         {
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_ll15 = au.Dup(value);
            } 
         //* made settable to allow for erosion
         _ll15 = au.Dup(value);      //This fractional value is used in soilwat_init() to initialise _ll15_dep. That is the only place it is used. Everywhere else uses _ll15_dep.
         if (initDone)
            {    
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for(int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               _ll15_dep[layer] = value[layer] * _dlayer[layer];   //change ll15_dep NOT dul. The dll15 variable is just for inputting and outputting and is immediately converted to sw_dep.
               soilwat2_check_profile(layer);
               }
            }         
         }
      }


   private double[]    _air_dry;    
   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")]
   [Output]  
   public double[]    air_dry   //! air dry soil water content
      {
      get 
         {
         if (initDone)
            {  
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for (int layer = au.si; layer <= au.ci(num_layers); layer++)
               {
               _air_dry[layer] = mu.divide(_air_dry_dep[layer], _dlayer[layer], 0.0);
               }
            }
         return _air_dry;
         }
      set 
         { 
         if (!initDone)
            {
            //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
            //therefore store a copy so if there is a Reset event we can set it back to this value.
            reset_air_dry = au.Dup(value);
            } 

         //* made settable to allow for erosion  
         _air_dry = au.Dup(value);  //This fractional value is used in soilwat_init() to initialise _air_dry_dep. That is the only place it is used. Everywhere else uses _air_dry_dep.
         if (initDone)
            {    
            int num_layers = au.count_of_real_vals(_dlayer, max_layer);
            for(int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               _air_dry_dep[layer] = value[layer] * _dlayer[layer];   //change air_dry_dep NOT dul. The air_dry variable is just for inputting and outputting and is immediately converted to sw_dep.
               soilwat2_check_profile(layer);
               }    
            }     
         }
      }

   [Param(MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1 /d)")]
   [Output] 
   private double[]    swcon;     //! soil water conductivity constant (1/d) //! ie day**-1 for each soil layer
   
   [Param(IsOptional=true, MinVal=0.0, MaxVal=1.0)] 
   [Units("(0-1)")]
   [Output]  
   private double[]    mwcon;     //! impermeable soil layer indicator

   [Output]
   private bool        using_ks;       //! flag to determine if Ks has been chosen for use. //sv- set in soilwat2_init() by checking if mwcon exists

   [Param(IsOptional=true, MinVal=0.0, MaxVal=1000.0)] 
   [Units("(mm/d)")]
   [Output]  
   private double[]    ks;        //! saturated conductivity (mm/d)

   [Param(MinVal=0.01, MaxVal=3.0)] 
   [Units("(g/cc)")]
   [Output]  
   private double[]    bd;      //! moist bulk density of soil (g/cm^3)


   //sv- Lateral Flow profile   //sv- also from Lateral_read_param()

   [Param(IsOptional=true, MinVal=0, MaxVal=1.0e3F)] //1.0e3F = 1000
   [Units("(mm/d)")] 
   [Output]
   private double[]  klat;





/*
   //sv- PURE OUTPUTS

   //Soilwat2Globals          //sv- also from soilwat2_send_my_variable() 
                              //(Also see the bottom of OnProcess Event Handler for where calculations for some of these variables are done before outputting.)  

                              //TODO: you have not done all the variable calculations from soilwat2_send_my_variable() yet. Need to recalculate sw from sw_dep before outputting it.
*/
   private double[]    _sat_dep;
   [Output]
   [Units("(mm)")]
   public double[]    sat_dep   // sat * dlayer //see soilwat2_init() for initialisation
      {
      get {return _sat_dep;}
      set 
         {
         //* made settable to allow for erosion
         _sat_dep = au.Dup(value);
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }         
         }
      }

   private double[]    _dul_dep;
   [Output]
   [Units("(mm)")]
   public double[]    dul_dep   // dul * dlayer  //see soilwat2_init() for initialisation
      {
      get {return _dul_dep;}
      set 
         {
         //* made settable to allow for erosion
         _dul_dep = au.Dup(value);
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }         
         }
      }

   private double[]    _sw_dep; 
   [Output]
   [Units("(mm)")]
   public double[]    sw_dep    // sw * dlayer //see soilwat2_init() for initialisation
      {   
      get {return _sw_dep;}
      set 
         {
         soilwat2_zero_default_variables();
         numvals_sw = value.Length;          //used in soilwat2_set_default()
         _sw_dep = au.Dup(value);
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for (int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }

         //TODO: External Mass Flow event should be triggered just like for sw. Same should go for dlt_sw and dlt_sw_dep.
         }
      }


   private double[]   _ll15_dep;
   [Output]
   [Units("(mm)")]
   public double[]    ll15_dep  // ll15 * dlayer //see soilwat2_init() for initialisation
      {
      get {return _ll15_dep;}
      set 
         {
         //* made settable to allow for erosion
         _ll15_dep = au.Dup(value);
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }         
         }
      }


   private double[]   _air_dry_dep;
   [Output]
   [Units("(mm)")]
   public double[]    air_dry_dep  // air_dry * dlayer //see soilwat2_init() for initialisation
      {
      get {return _air_dry_dep;}
      set 
         {
         //* made settable to allow for erosion
          _air_dry_dep = au.Dup(value);
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            soilwat2_check_profile(layer);
            }         
         }
      }

   [Output] private double[]  sws;       //TODO: this appears to just be an output variable and is identical to sw. I think it should be removed.   //! temporary soil water array used in water_table calculation

   [Output] private double[]  flow;        //sv- Unsaturated Flow //! depth of water moving from layer i+1 into layer i because of unsaturated flow; (positive value indicates upward movement into layer i) (negative value indicates downward movement (mm) out of layer i)
   [Output] private double[]  flux;       //sv- Drainage (Saturated Flow) //! initially, water moving downward into layer i (mm), then water moving downward out of layer i (mm)

   [Output]
   private double[] flow_water;         //flow_water[layer] = flux[layer] - flow[layer]  //see soilwat2_send_my_variable() for where it is calculated.

   private double[]    solute_conc_rain = new double[max_solute];//! concentration of solutes entering soil via rainfall (ppm)

   //Soilwat2Globals



   //soilwat2_on_new_solute event handler  (can't dynamically create output variable in .NET so only allow output variables for these solutes)
   //          I assign these values each day in the OnPrepare event handler
   [Output]
   [Units("(kg/ha)")]
   private double leach_no3;

   [Output]
   [Units("(kg/ha)")]
   private double[] flow_no3;


   [Output]
   [Units("(kg/ha)")]
   private double leach_nh4;

   [Output]
   [Units("(kg/ha)")]
   private double[] flow_nh4;


   [Output]
   [Units("(kg/ha)")]
   private double leach_urea;

   [Output]
   [Units("(kg/ha)")]
   private double[] flow_urea;


   [Output]
   [Units("(kg/ha)")]
   private double leach_cl;

   [Output]
   [Units("(kg/ha)")]
   private double[] flow_cl;


   [Output]
   [Units("(kg/ha)")]
   private double leach_br;

   [Output]
   [Units("(kg/ha)")]
   private double[] flow_br;

   //soilwat2_on_new_solute event handler


   //Lateral Flow profile     //sv- also from Lateral_Send_my_variable()

   [Output]
   [Units("(mm)")] 
   private double[]  outflow_lat;   //! outflowing lateral water   //lateral outflow
   //end


#endregion


#region Send My Variables (if need to do stuff BEFORE outputting)

   //only used in soilwat2_send_my_variable() below
   private void AssignSoluteFlowLeachOutputs()
      {

      //Assign the solute Output variables for this module 
      //       taken from soilwat2_send_my_variable()

      int num_layers = au.count_of_real_vals(_dlayer, max_layer);
      double[] solute_leach1D = new double[num_layers];
      double[] solute_flow1D = new double[num_layers];

      if (num_solutes > 0)
         {
         for (int solnum = au.si; solnum <= au.ci(num_solutes); solnum++)
            {
            //convert 2D array to 1D array
            for (int layer = au.si; layer <= au.ci(num_layers); layer++)
               {
               solute_leach1D[layer] = solute_leach[solnum, layer];
               solute_flow1D[layer] = solute_leach[solnum, layer] - solute_up[solnum, layer];
               }

            //assign our solute output variables for this module.
            switch (solute_names[solnum])
               {
               case "no3":
                  leach_no3 = solute_leach1D[au.ci(num_layers)];  //just report leaching from bottom layer
                  Array.Copy(solute_flow1D, flow_no3, num_layers);
                  break;
               case "nh4":
                  leach_nh4 = solute_leach1D[au.ci(num_layers)];  //just report leaching from bottom layer
                  Array.Copy(solute_flow1D, flow_nh4, num_layers);
                  break;
               case "urea":
                  leach_urea = solute_leach1D[au.ci(num_layers)];  //just report leaching from bottom layer
                  Array.Copy(solute_flow1D, flow_urea, num_layers);
                  break;
               case "cl":
                  leach_cl = solute_leach1D[au.ci(num_layers)];  //just report leaching from bottom layer
                  Array.Copy(solute_flow1D, flow_cl, num_layers);
                  break;
               case "br":
                  leach_br = solute_leach1D[au.ci(num_layers)];  //just report leaching from bottom layer
                  Array.Copy(solute_flow1D, flow_br, num_layers);
                  break;
               default:
                  throw new Exception("SoilWater cannot output variable: leach_" + solute_names[solnum] + " or flow_" + solute_names[solnum] + " . SoilWater can only move no3, nh4, urea, cl, and br");
               }
            }
         }
      }


   private void soilwat2_send_my_variable()
      {

       int num_layers = au.count_of_real_vals (_dlayer, max_layer);

      //taken from soilwat2_send_my_variable()
      //I had to do this here because the new .NET way of outputting variables using [Output] tag does not allow for a way to do processing (summations, etc) before sending.
      //Only variables that do processing before sending in soilwat2_send_my_variable() are included here. Need to use this code and the [Output] tag.
      //Any variables in soilwat2_send_my_variable() that just send the value without doing any processing can be handled by the [Output] tag on its own.  
      es = au.sum_real_array(es_layers, max_layer);
      eff_rain = rain + runon - runoff - drain;
      esw = 0.0;
      for (int layer = au.si; layer <= au.ci(num_layers); layer++)
         {
         esw = esw + mu.l_bound(_sw_dep[layer] - _ll15_dep[layer], 0.0);
         //sw[layer] = mu.divide(_sw_dep[layer], _dlayer[layer], 0.0);
         //ll15[layer] = mu.divide(_ll15_dep[layer], _dlayer[layer], 0.0);
         //dul[layer] = mu.divide(_dul_dep[layer], _dlayer[layer], 0.0);
         //sat[layer] = mu.divide(_sat_dep[layer], _dlayer[layer], 0.0);
         //air_dry[layer] = mu.divide(_air_dry_dep[layer], _dlayer[layer], 0.0);
         //! --- Resultant water and solute flow output variables ---
         flow_water[layer] = flux[layer] - flow[layer];
         //for flow_ and leach_ solute variables see AssignSoluteFlowLeachOutputs() below this loop.  
         }
      //once again the dynamic nature of assigning the 2D solute arrays to the "leach_" & "flow_" solute variables, means this sort of processing can't be done by [Output] tag alone.  
      AssignSoluteFlowLeachOutputs();
      }


#endregion


#region Set My Variables (Let other modules change me) (these are sort of like a [Param]'s but after the start of the simulation)

   //These are the sets for ficticious variables that actually set other variables.

   [Output]
   [Units("(mm)")]
   public double[] dlt_dlayer
      {
      set 
         {      
         int num_layers = au.count_of_real_vals(value, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            //If you change the depths of the layer then you need to modify the water "_dep" variables by the same amount. (they are in mm too)
            //If you don't do this, you will have the same amount of water that is now in a shallower layer, 
            //therefore you will have a different fraction equivalent variables, the ones without the "_dep" eg. sw, dul.  
            double fract = mu.divide((_dlayer[layer] + value[layer]), _dlayer[layer], 0.0);
            _air_dry_dep[layer] = _air_dry_dep[layer] * fract;
            _dul_dep[layer] = _dul_dep[layer] * fract;
            _ll15_dep[layer] = _ll15_dep[layer] * fract;
            _sat_dep[layer] = _sat_dep[layer] * fract;
            _sw_dep[layer] = _sw_dep[layer] * fract;
            
            _dlayer[layer] = _dlayer[layer] + value[layer];
 
            soilwat2_check_profile(layer);
            }

         //resize all the arrays if they changed the number of layers
         if (num_layers != _dlayer.Length)
            {
            Array.Resize(ref _air_dry_dep, num_layers);
            Array.Resize(ref _dul_dep, num_layers);
            Array.Resize(ref _ll15_dep, num_layers);
            Array.Resize(ref _sat_dep, num_layers);
            Array.Resize(ref _sw_dep, num_layers);
            Array.Resize(ref _dlayer, num_layers);  
            }  

         soilwat2_New_Profile_Event();
         }
      }

   [Output]
   [Units("(mm)")]
   public double[] dlt_sw
      {
      set   
         {
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            //sw_dep = sw * dlayer
            _sw_dep[layer] = _sw_dep[layer] + (value[layer] * _dlayer[layer]);      //change sw_dep NOT sw. The sw variable is just for inputting and outputting and is immediately converted to sw_dep.
            soilwat2_check_profile(layer);
            }   
         }
      }

   [Output]
   [Units("(mm)")]
   public double[] dlt_sw_dep
      {
      set   
         {
         int num_layers = au.count_of_real_vals(_dlayer, max_layer);
         for(int layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            _sw_dep[layer] = _sw_dep[layer] + value[layer];      
            soilwat2_check_profile(layer);
            }   
         }
      }


   private void soilwat2_set_my_variable()
      {
    
      }

#endregion




//DAILY INPUTS FROM OTHER MODULES

#region [INPUTS]



   //taken from soilwat2_get_residue_variables()

   [Input (IsOptional=true)]
   [Units("()")]
   private double    surfaceom_cover;        //basically this just gets renamed to the residue_cover variable below
   private double    residue_cover;          //! residue cover reduces  cn2_bare //this is the internal variable name for surfaceom_cover 

   //end of soilwat2_get_residue_variables()



   // taken from soilwat2_get_solute_variables()  
   //(these get put in solute[,] 2D array in the GetDailySolute2DArray())

   [Input (IsOptional=true)]
   [Units("(kg/ha)")]
   private double[] no3;      //from soiln module

   [Input(IsOptional=true)]
   [Units("(kg/ha)")]
   private double[] nh4;      //from soiln module

   [Input(IsOptional=true)]
   [Units("(kg/ha)")]
   private double[] urea;     //from soiln module

   [Input(IsOptional=true)]
   [Units("(kg/ha)")]
   private double[] cl;       //from solute module

   [Input(IsOptional=true)]
   [Units("(kg/ha)")]
   private double[] br;       //from solute module

   // end of soilwat2_get_solute_variables()



   //taken from soilwat2_get_environ_variables()

   //from met module
      //Runon is specified in a met file or sparse data file
   [Input(IsOptional=true)]
   [Units("(mm/d")]
   private double   runon = Double.NaN;      //! external run-on of H2O (mm/d)

   //from crop modules  
      //used in runoff(as part of TotalInterception parameter) and in infilitration
   [Output]
   [Input(IsOptional=true)]
   [Units("(mm")]
   private double   interception = Double.NaN;      //! canopy interception loss (mm)

   //from surface organic matter module
      //used in runoff(as part of TotalInterception parameter) and in infilitration
   [Output]
   [Input(IsOptional=true)]
   [Units("(mm")]
   private double   residueinterception = Double.NaN;     //residue interception loss (mm)

/*
   //TODO: how do I dynamically change the name of an INPUT?  this should be eo_source not eo_system.
   [Output]
   [Input(IsOptional=true)]
   private double   eo_system;         //! eo from somewhere else in the system

   TODO: how do I dynamically change the name of an INPUT?  this should be obsrunoff_name not obsrunoff.
   //see "obsrunoff_name" in the Soil Properties parameters ie. [Param] tag.
   [Output]
   [Input(IsOptional=true)]
   private double   obsrunoff;         //! observed runoff (mm)
*/

   //end of soilwat2_get_environ_variables()



   //taken from Lateral_process()

   //from met module
      //Inflow is specified in a met file or sparse data file
   [Input(IsOptional=true)]
   [Units("(mm)")]
   private double[] inflow_lat;       //! inflowing lateral water

   //end of Lateral_process()


#endregion


#region Get Variables from other Modules (if need to do stuff AFTER inputting)
   

   private void soilwat2_get_residue_variables()
      {

      residue_cover = surfaceom_cover;  //surfaceom_cover is an [input] 

      }
   
   private void soilwat2_get_crop_variables()
      {
      //also called in prepare event as well

      //*+  Purpose
      //*      get the value/s of a variable/array.

      //*+  Mission Statement
      //*     Get crop Variables

      Double covgreen;
      Double covtot;
      Double height;

      int i = au.si;
      foreach (Component Comp in MyPaddock.Crops)
         {

         if (au.ci(i) <= max_crops) 
            {
             MyPaddock.Get(Comp.FullName + ".cover_green", out covgreen);
             MyPaddock.Get(Comp.FullName + ".cover_tot", out covtot);
             MyPaddock.Get(Comp.FullName + ".height", out height);

            ////must have at least these three variables to be considered a "crop" component.
            //if (covgreen == null) && covtot && height.Exists() && !Comp.IsOfType("outputfile"))
            //   {
            cover_green[i] = covgreen;
            cover_tot[i] = covtot;
            canopy_height[i] = height;
            num_crops = au.cl(i);
            i++;
              
            }
         else
            {
            throw new Exception("Too many modules with green cover.");
            }

         }

      }

      
   private void GetSolute2DArrayFromModuleSolutes()
      {
      //only called in soilwat2_get_solute_variables()
      //This code used to be done in soilwat2_get_solute_variables() 

      //but had to do it in .NET with a combination of hard coded daily input variables [Input] (no3, nh4, urea, cl, br)
      //and this routine called from the OnProcess Event handler. 

      double[] tempsolute = new double[max_layer];
      au.ZeroArray(ref tempsolute);


      //if there are solutes in this simulation.
      if (num_solutes > 0)
         {
         //for the number of solutes that was read in by OnNewSolute event handler)
         for (int solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
            {
            switch (solute_names[solnum])
               {
               case "no3":
                  au.CopyTo(ref tempsolute, no3, no3.Length);
                  break;
               case "nh4":
                  au.CopyTo(ref tempsolute, nh4, nh4.Length);
                  break;
               case "urea":
                  au.CopyTo(ref tempsolute, urea, urea.Length);
                  break;
               case "cl":
                  au.CopyTo(ref tempsolute, cl, cl.Length);
                  break;
               case "br":
                  au.CopyTo(ref tempsolute, br, br.Length);
                  break;
               default:
                  throw new Exception("SoilWater cannot get daily value for solute: " + solute_names[solnum] + " SoilWater does not know what module owns this solute.");            
               }

            for (int layer=au.si; layer<=au.ci(tempsolute.Length); layer++)
               {
               solute[solnum,layer] = tempsolute[layer];     
               }
            //TODO: resize solute[,] to just the size it needs to be instead of max_solute, max_layer
            }
         }
      }

   private void soilwat2_get_solute_variables()
      {
      GetSolute2DArrayFromModuleSolutes();
      }

   
   private void soilwat2_get_environ_variables()
      {
      //also called in prepare event

      if (Double.IsNaN(runon))
         runon = 0.0;
      if (Double.IsNaN(interception))
         interception = 0.0;
      if (Double.IsNaN(residueinterception))
         residueinterception = 0.0;
         
      }


   //this is called in the On Process event handler
   //it just calls all the methods above.
   private void soilwat2_get_other_variables()
      {

      soilwat2_get_residue_variables();
      
      soilwat2_get_crop_variables();

      soilwat2_get_solute_variables();

      soilwat2_get_environ_variables();

      }

#endregion 





//CHANGE OTHER MODULES

#region Link to allow you to directly set Module variables

   //nb. you have to add a reference to DotNetProxies.dll (soon to be CSDotNetProxies.dll) for this to work.

   //used in SetModuleSolutesUsingSolute2DArray()
   [Link] Paddock MyPaddock;

#endregion


#region Set Variables in other Modules (Solute model mainly)


   private void SetModuleSolutesUsingSolute2DArray()
      {
      //taken from soilwat2_set_other_variables()

      //if there are solutes in this simulation.
      if (num_solutes > 0)
         {
         int num_layers = au.count_of_real_vals(_dlayer, max_layer); //dlt_solute.GetLength(1);
         double[] temp_dlt_solute_double = new double[num_layers];
         float[] temp_dlt_solute = new float[num_layers]; 

         SoilN SoilNModel = (SoilN)MyPaddock.LinkByType("soiln");
         Component ClModel = MyPaddock.LinkByName("Cl") as Component;
         Component BrModel = MyPaddock.LinkByName("Br") as Component;

         for (int solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
            {
            //convert 2D array to 1D array
            for (int layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               temp_dlt_solute_double[layer] = dlt_solute[solnum,layer];     
               }
            
            //convert to float array
            temp_dlt_solute = au.ToFloatArray(temp_dlt_solute_double);

            //set the change in solutes for the modules
            switch (solute_names[solnum])
               {
               case "no3":
                  SoilNModel.dlt_no3 = temp_dlt_solute;
                  break;
               case "nh4":
                  SoilNModel.dlt_nh4 = temp_dlt_solute;
                  break;
               case "urea":
                  SoilNModel.dlt_urea = temp_dlt_solute;
                  break;
               case "cl":
                  MyPaddock.Set(ClModel.FullName + ".dlt_cl", temp_dlt_solute);
                  break;
               case "br":
                  MyPaddock.Set(BrModel.FullName + ".dlt_br", temp_dlt_solute);
                  break;
               default:
                  throw new Exception("SoilWater cannot alter the change in solute (due to solute movement with water) for solute: " + solute_names[solnum] + " SoilWater does not know what module owns this solute.");            
               }
            }
         }
      }



   //this is called in the On Process event handler
   private void soilwat2_set_other_variables()
      {

    
      SetModuleSolutesUsingSolute2DArray();


      
      //! Send a runoff event to the system
      if (runoff > 0.0)
         {
         RunoffEventType r = new RunoffEventType(); //! structure holding runoff event
         r.runoff = (float)runoff;
         Runoff.Invoke(r);
         }

      }


#endregion



//LOCAL VARIABLES

#region Local Variables 

//! ====================================================================
//!     soilwat2 constants
//! ====================================================================

    //Soilwat2Globals

   
   //MET
   //sv- These met variables get assigned by the OnNewMet Event Handler
   private double      rain;         //! precipitation (mm/d)
   private double      radn;         //! solar radiation (mj/m^2/day)
   private double      mint;         //! minimum air temperature (oC)
   private double      maxt;         //! maximum air temperature (oC)

   //RUNOFF
   //r double      cover_surface_runoff;
   //r double runoff;
   //who put this in? double      eff_rain; 
   private double      runoff_pot;       //! potential runoff with no pond(mm)
   private bool        obsrunoff_found;  //! whether obserevd runoff was returned from system

   //GET CROP VARIABLES
   //private int[]       crop_module = new int[max_crops];             //! list of modules replying 
   private double[]    cover_tot = new double[max_crops];     //! total canopy cover of crops (0-1)
   private double[]    cover_green = new double[max_crops];   //! green canopy cover of crops (0-1)
   private double[]    canopy_height = new double[max_crops]; //! canopy heights of each crop (mm)
   private int         num_crops;                //! number of crops ()

   //TILLAGE EVENT
   private double      tillage_cn_red;   //! reduction in CN due to tillage ()   //can either come from the manager module or from the sim file
   private double      tillage_cn_rain;  //! cumulative rainfall below which tillage reduces CN (mm) //can either come from the manager module orh the sim file
   private double      tillage_rain_sum; //! cumulative rainfall for tillage CN reduction (mm)

   //EVAPORATION
   private int         year;         //! year
   private int         day;          //! day of year
   private double      today;        //! today's julian date      //sv- see OnTick event handler //sv- Julian day is not in fact 'day of year'.  See http://en.wikipedia.org/wiki/Julian_day and http://en.wikipedia.org/wiki/ISO_8601#Ordinal_dates
   private double      sumes1;       //! cumulative soil evaporation in stage 1 (mm)
   private double      sumes2;       //! cumulative soil evaporation in stage 2 (mm)
   //r double      t;
   private double       eo_system;         //! eo from somewhere else in the system //sv- see eo_source
   //r double eo;
   private double      real_eo;                  //! potential evapotranspiration (mm) 
   //r double eos;
   //r double[] cn2_new;
   //r double[] _air_dry_dep;
   //r double[] bd[];
   //r double[] _dul_dep;
   //r double[] _ll15_dep;
   //r double[] _sat_dep;
   //r double[] flow;
   //r double[] flux;
   //r double[] _sw_dep;
   private double[]    es_layers = new double[max_layer];     //! actual soil evaporation (mm)


   //r double drain;
   //r double infiltration;


   //SOLUTES
   //OnNewSolute
   private int         num_solutes = 0;                          //! number of solutes in APSIM ()
   private string[]    solute_names = new string[max_solute];         //! names of solutes in the soil system that will be leached by soilwat2
   private int[]       solute_owners = new int[max_solute];         //! names of owner module for each solutes in the system
   private bool[]      solute_mobility = new bool[max_solute];
   //soilwat2_move_solute_down() and up()   //! nih - I dont like these names.
   private double[,]   solute = new double[max_solute, max_layer];       //! solute in each layer (kg/ha)
   private double[,]   solute_leach = new double[max_solute,max_layer];  //! amount of solute leached from each layer (kg/ha)
   private double[,]   solute_up = new double[max_solute,max_layer];     //! amount of solute upped from each layer (kg/ha)
   private double[,]   dlt_solute = new double[max_solute, max_layer];   //! change in solute each in layer (kg n/ha)


   //IRRIGATION
   private double      irrigation;       //! irrigation (mm)                                                 
   private double[]    irrigation_solute = new double[max_solute];        //! amount of solute in irrigation water (kg/ha)


   //r double pond_evap;
   //r double pond;
   //r double water_table;
   //r double[] sws;

   private double      oldSWDep;


   //end Soilwat2Globals

   


   //The following are used for doing a Reset Event. 
   //They are used to store the original values read in by the [Param] tags.
   //They only apply to [Param] tags that are also "Settable" properties which can be changed by the user.
   //Settable [Param]'s need to have their original values stored because the user can alter them and if a reset is done we need to set them back 
   //to what was originally read in. The .NET infrastructure of APSIM does not really let you do this. You only get to read in from the .sim file once at the
   //start of the simulation. So we use these variables to compensate.

   //Soil Property
      //initial starting soil water 
   int reset_numvals_insoil, reset_numvals_profile_fesw, reset_numvals_profile_esw_depth, reset_numvals_wet_soil_depth;    //used in soilwat2_set_default()
   double reset_insoil, reset_profile_fesw, reset_profile_esw_depth, reset_wet_soil_depth;   
      //runoff
   double reset_cn2_bare, reset_cn_red, reset_cn_cov;   
      //ponding 
   double reset_max_pond;  

   //Soil Profile
   int reset_numvals_sw;         //used in soilwat2_set_default()
   double[] reset_dlayer, reset_sat, reset_dul, reset_sw, reset_ll15, reset_air_dry;


   
#endregion





//MODEL


#region Functions to Zero Variables


   private void InstantiateArrays_PureOutputs()
      {
      
      //need this for arrays below.
      int num_layers = _dlayer.Length;


      //INSTANTIATE THE ARRAYS
      //----------------------      

      //profile outputs
      _sat_dep     = new double[num_layers];
      _dul_dep     = new double[num_layers];
      _sw_dep      = new double[num_layers];
      _ll15_dep    = new double[num_layers];
      _air_dry_dep = new double[num_layers];
      sws         = new double[num_layers];
      flow        = new double[num_layers];
      flux        = new double[num_layers];
      flow_water  = new double[num_layers];
      outflow_lat = new double[num_layers];

      //solute movement output variables.
      leach_no3   = 0.0;
      flow_no3    = new double[num_layers];

      leach_nh4   = 0.0;
      flow_nh4    = new double[num_layers];

      leach_urea  = 0.0;
      flow_urea   = new double[num_layers];

      leach_cl    = 0.0;
      flow_cl     = new double[num_layers];

      leach_br    = 0.0;
      flow_br     = new double[num_layers];



      //ZERO THE ARRAYS
      //---------------

      //profile outputs
      au.ZeroArray(ref _sat_dep);
      au.ZeroArray(ref _dul_dep);
      au.ZeroArray(ref _sw_dep);
      au.ZeroArray(ref _ll15_dep);
      au.ZeroArray(ref _air_dry_dep);
      au.ZeroArray(ref sws);
      au.ZeroArray(ref flow);
      au.ZeroArray(ref flux);
      au.ZeroArray(ref solute_conc_rain);
      au.ZeroArray(ref outflow_lat);

      //solute movement output variables.
      au.ZeroArray(ref flow_no3);      
      au.ZeroArray(ref flow_nh4);       
      au.ZeroArray(ref flow_urea);     
      au.ZeroArray(ref flow_cl);         
      au.ZeroArray(ref flow_br);       

      }


   private void soilwat2_zero_variables()
      {

/*
               //*+  Purpose
               //*       zero variables & arrays


         //* ====================================================================
         //* Globals
         //! 	Met and date fields are zeroed in zero_event_data
         //c         g%cover_surface_extra = 0.0          ! extra surface cover (0-1)
                  cover_surface_runoff = 0.0;             //! effective total cover (0-1)
                  au.ZeroArray(ref cover_tot);                //! total canopy cover of crops (0-1)
                  au.ZeroArray(ref cover_green);              //! green canopy cover of crops (0-1)
                  au.ZeroArray(ref canopy_height);            //! canopy heights of each crop (mm)
                  num_crops = 0;                          //! number of crops ()
                  sumes1 = 0.0;                           //! cumulative soil evaporation in stage 1 (mm)
                  sumes2 = 0.0;                           //! cumulative soil evaporation in stage 2 (mm)
                  t = 0.0;                                //! time after 2nd-stage soil evaporation begins (d)
                  au.Zero2DArray(ref solute);                 //! solute in each layer (kg/ha)
                  au.Zero2DArray(ref dlt_solute);             //! change in solute each in layer (kg n/ha)
                  au.Zero2DArray(ref solute_leach);           //! amount of solute leached from each layer (kg/ha)
                  au.Zero2DArray(ref solute_up);              //! amount of solute upped from each layer (kg/ha)
                                                            //! nih - I dont like these names.
                  au.ZeroArray(ref irrigation_solute);        //! amount of solute in irrigation water (kg/ha)
         //!        Zeroed in zero_module_links routine
         //!         g%num_solutes = 0                      //! number of solutes in APSIM ()

                  residue_cover = 0.0;                    //! residue cover reduces  cn2_bare
                  eo = 0.0;                               //! potential evapotranspiration (mm)
                  eos = 0.0;                              //! pot sevap after modification for green cover & residue wt
                  cn2_new = 0.0;                          //! New cn2  after modification for crop cover & residue cover
                  au.ZeroArray(ref _air_dry_dep);              //! air dry soil water content (mm water)
                  //au.ZeroArray(ref bd);                       //! moist bulk density of soil (g/cm^3)
                  au.ZeroArray(ref _dul_dep);                  //! drained upper limit soil water content for each soil layer (mm water)
                  au.ZeroArray(ref _ll15_dep);                 //! 15 bar lower limit of extractable soil water for each soil layer(mm water)
                  au.ZeroArray(ref _sat_dep);                  //! saturated water content for layer l (mm water)
                  au.ZeroArray(ref flow);                     //! depth of water moving from layer l+1
                                                               //! into layer l because of unsaturated
                                                               //! flow; positive value indicates upward
                                                               //! movement into layer l, negative value
                                                               //! indicates downward movement (mm) out of layer l
                  au.ZeroArray(ref flux);                     //! initially, water moving downward into layer l (mm), 
                                                               //then water moving downward out of layer l (mm)
                  au.ZeroArray(ref _sw_dep);                   //! soil water content of layer l (mm)
                  au.ZeroArray(ref es_layers);                //! actual soil evaporation (mm)

                  drain = 0.0;                            //! drainage rate from bottom layer (cm/d)
                  infiltration = 0.0;                     //! infiltration (mm)
                  runoff = 0.0;                           //! runoff (mm)
                  runoff_pot = 0.0;                       //! potential runoff with no pond(mm)  
                  irrigation = 0.0;                       //! irrigation (mm)
                  //obsrunoff = 0.0;                        //! observed runoff (mm)
                  tillage_cn_red = 0.0;                   //! reduction in CN due to tillage ()
                  tillage_cn_rain = 0.0;                  //! cumulative rainfall below which tillage reduces CN (mm)
                  tillage_rain_sum = 0.0;                 //! cumulative rainfall for tillage CN reduction (mm)
                  obsrunoff_found = false;                //! whether obserevd runoff was returned from system
                  obsrunoff_name = "not_read";                    //! system name of observed runoff
                  numvals_profile_esw_depth = 0;          //! number of values returned for profile_esw_depth
                  numvals_insoil = 0;                     //! number of values returned for insoil
                  numvals_wet_soil_depth = 0;             //! number of values returned for wet_soil_depth
                  numvals_profile_fesw = 0;               //! number of values returned for profile_fesw
                  numvals_sw = 0;                         //! number of values returned for sw

         //c        Zeroed in zero_module_links routine
         //c         g.solute_names(:) = ' '              ! names of solutes in the
         //c                                              ! soil system that will
         //c                                              ! be leached by soilwat2
         //c         g.solute_owners(:) = ' '             ! names of owner module for each
         //c                                              ! solutes in the system
         //c         g.solute_mobility (:) = ' '
         //c
         //c         g.crop_module(:) = ' '               ! list of modules replying
                  //TODO: uncomment this later on -> num_canopy_fact = 0;                    //! number of canopy factors read ()
                  eo_system = 0.0;                        //! eo from somewhere else in the system
                  _eo_source = "not_read";                         //! system variable name of external eo source

                  pond  =  0.0;                           //! surface ponding depth (mm)
                  _water_table = 0.0;                      //! water table depth (mm)
                  au.ZeroArray(ref sws);                      //! soil water (mm/layer)
                  pond_evap = 0.0;                        //! evaporation from the pond surface (mm)
                  real_eo = 0.0;                          //! eo determined before any ponded water is evaporated (mm)
         //* ====================================================================
         //* Parameters
                  irrigation_layer = 0;                   //! trickle irrigation input layer
                  au.ZeroArray(ref _dlayer);                   //! thickness of soil layer i (mm)
                  //au.ZeroArray(ref swcon);                    //! soil water conductivity constant (1/d)
                                                               //! ie day**-1 for each soil layer
                  _cn2_bare = 0.0;                         //! curve number input used to calculate daily g_runoff
                  _cn_cov = 0.0;                           //! cover at which c_cn_red occurs
                  _cn_red = 0.0;                           //! maximum reduction in p_cn2_bare due to cover
                  //_cona = 0.0;                             //! stage 2 drying coefficient
                  //diffus_const = 0.0;                     //! diffusivity constant for soil testure
                  //diffus_slope = 0.0;                     //! slope for diffusivity/soil water content relationship
                  //salb = 0.0;                             //! bare soil albedo (unitless)
                  //_u = 0.0;                                //! upper limit of stage 1 soil evaporation (mm)
                  _insoil = 0.0;                           //! switch describing initial soil water distributed evenly
                  _profile_esw_depth = 0.0;                //! initial depth of esw in profile filled top down with soil water (mm)
                  _wet_soil_depth = 0.0;                   //! initial depth profile filled top down with soil water (mm)
                  _profile_fesw = 0.0;                     //! initial fraction of profile esw filled top down with soil water (mm)

                  _max_pond = 0.0;                         //! maximum allowable surface storage (ponding) mm
                  //au.ZeroArray(ref mwcon);                    //! layer permeability factor (zero or one)
                  //au.ZeroArray(ref solute_conc_rain);         //! solute concentrations in rainfall (optional parameter)
         //* ====================================================================
         //* Constants
                  //hydrol_effective_depth = 0.0;           //! hydrologically effective depth for runoff (mm)
                  //au.ZeroArray(ref mobile_solutes);           //! names of all possible mobile solutes
                  //au.ZeroArray(ref immobile_solutes);         //! names of all possible immobile solutes
                  //min_crit_temp = 0.0;                    //! temperature below which eeq decreases (oC)
                  //max_crit_temp = 0.0;                    //! temperature above which eeq increases (oC)
                  //max_albedo = 0.0;                       //! maximum bare ground soil albedo (0-1)
                  //A_to_evap_fact = 0.0;                   //! factor to convert "A" to coefficient in Adam's type residue effect on Eos
                  //canopy_eos_coef = 0.0;                  //! coef in cover Eos reduction eqn
                  //sw_top_crit = 0.0;                      //! critical sw ratio in top layer below which stage 2 evaporation occurs
                  //sumes1_max = 0.0;                       //! upper limit of sumes1
                  //sumes2_max = 0.0;                       //! upper limit of sumes2
                  //TODO: uncomment this later on -> solute_flux_eff = 0.0;                  //! efficiency of moving solute with flux (0-1)
                  //TODO: uncomment this later on ->solute_flow_eff = 0.0;                  //! efficiency of moving solute with flow (0-1)
                  //gravity_gradient = 0.0;                 //! gradient due to hydraulic differentials (0-1)
                  //specific_bd = 0.0;                      //! specific bulk density (g/cc)
                  //au.ZeroArray(ref canopy_fact);              //! canopy factors for cover runoff effect ()
                  //au.ZeroArray(ref canopy_fact_height);       //! heights for canopy factors (mm)
                  //canopy_fact_default = 0.0;              //! default canopy factor in absence of height ()
                  //evap_method = ritchie_method;        //! actual soil evaporation model being used


                  //sv- Zero Lateral variables too.
                  //slope = 0.0;
                  //discharge_width = 0.0;
                  //catchment_area = 0.0;
                  //au.ZeroArray(ref klat);
                  au.ZeroArray(ref outflow_lat);

*/



      //You only really want to zero, 
      // Ouputs, Local Variables, 
      // and Settable Params (which you are using reset variables to store the original value in)
      //You do not want to zero non Settable Params because there is no way to reread them back in again. 
      //Plus they don't change during the simulation so why bother.  
      //By definition you don't want to reset the module constants. ( except the ones changed in soilwat2_read_constants() )


      //Settable Params
                                                      //! ie day**-1 for each soil layer
         _cn2_bare = 0.0;                         //! curve number input used to calculate daily g_runoff
         _cn_cov = 0.0;                           //! cover at which c_cn_red occurs
         _cn_red = 0.0;                           //! maximum reduction in p_cn2_bare due to cover

         _max_pond = 0.0;                         //! maximum allowable surface storage (ponding) mm

         numvals_insoil = 0;                     //! number of values returned for insoil
         numvals_profile_fesw = 0;               //! number of values returned for profile_fesw
         numvals_profile_esw_depth = 0;          //! number of values returned for profile_esw_depth
         numvals_wet_soil_depth = 0;             //! number of values returned for wet_soil_depth
         numvals_sw = 0;                         //! number of values returned for sw

         _insoil = 0.0;                           //! switch describing initial soil water distributed evenly
         _profile_fesw = 0.0;                     //! initial fraction of profile esw filled top down with soil water (mm)
         _profile_esw_depth = 0.0;                //! initial depth of esw in profile filled top down with soil water (mm)
         _wet_soil_depth = 0.0;                   //! initial depth profile filled top down with soil water (mm)

         au.ZeroArray(ref _dlayer);                   //! thickness of soil layer i (mm)
         au.ZeroArray(ref _sat_dep);                  //! saturated water content for layer l (mm water)
         au.ZeroArray(ref _dul_dep);                  //! drained upper limit soil water content for each soil layer (mm water)
         au.ZeroArray(ref _sw_dep);                   //! soil water content of layer l (mm)
         au.ZeroArray(ref _ll15_dep);                 //! 15 bar lower limit of extractable soil water for each soil layer(mm water)
         au.ZeroArray(ref _air_dry_dep);              //! air dry soil water content (mm water)

         _water_table = 0.0;                      //! water table depth (mm)



      //Outputs
         drain = 0.0;                            //! drainage rate from bottom layer (cm/d)
         infiltration = 0.0;                     //! infiltration (mm)
         runoff = 0.0;                           //! runoff (mm)

         pond  =  0.0;                           //! surface ponding depth (mm)
         pond_evap = 0.0;                        //! evaporation from the pond surface (mm)
         eo = 0.0;                               //! potential evapotranspiration (mm)
         eos = 0.0;                              //! pot sevap after modification for green cover & residue wt
         t = 0.0;                                //! time after 2nd-stage soil evaporation begins (d)
         cn2_new = 0.0;                          //! New cn2  after modification for crop cover & residue cover
         cover_surface_runoff = 0.0;             //! effective total cover (0-1)
         au.ZeroArray(ref flow);                     //! depth of water moving from layer l+1
                                                      //! into layer l because of unsaturated
                                                      //! flow; positive value indicates upward
                                                      //! movement into layer l, negative value
                                                      //! indicates downward movement (mm) out of layer l
         au.ZeroArray(ref flux);                     //! initially, water moving downward into layer l (mm), 
                                                      //then water moving downward out of layer l (mm)
         au.ZeroArray(ref es_layers);                //! actual soil evaporation (mm)

         //es is a calculated value in send_my_variables() and is based on es_layers above. So don't need to zero it.
         //private double    es;                      //! total es     //sv- local variable declared in soilwat2_send_my_variable()

         //eff_rain is a calculated value in send_my_variables(). So don't need to zero it.
         //[Output]
         //private double    eff_rain;                  //! daily effective rainfall (mm)   //sv- local variable declared in soilwat2_send_my_variable()

         //esw = 0.0;     //this is Zeroed in send_my_variables() but zeroing here is better.
         //[Output]
         //private double    esw;                       //! potential extractable sw in profile   //sv- local variable declared in soilwat2_send_my_variable()



         au.ZeroArray(ref outflow_lat);
         au.ZeroArray(ref sws);                      //! soil water (mm/layer)



         //[Output]
         //private double[] flow_water;         //flow_water[layer] = flux[layer] - flow[layer]  //see soilwat2_send_my_variable() for where it is calculated.



         ////soilwat2_on_new_solute event handler  (can't dynamically create output variable in .NET so only allow output variables for these solutes)
         ////          I assign these values each day in the OnPrepare event handler
         //[Output]
         //[Units("(kg/ha)")]
         //private double leach_no3;

         //[Output]
         //[Units("(kg/ha)")]
         //private double[] flow_no3;


         //[Output]
         //[Units("(kg/ha)")]
         //private double leach_nh4;

         //[Output]
         //[Units("(kg/ha)")]
         //private double[] flow_nh4;


         //[Output]
         //[Units("(kg/ha)")]
         //private double leach_urea;

         //[Output]
         //[Units("(kg/ha)")]
         //private double[] flow_urea;


         //[Output]
         //[Units("(kg/ha)")]
         //private double leach_cl;

         //[Output]
         //[Units("(kg/ha)")]
         //private double[] flow_cl;


         //[Output]
         //[Units("(kg/ha)")]
         //private double leach_br;

         //[Output]
         //[Units("(kg/ha)")]
         //private double[] flow_br;




      //Inputs
         residue_cover = 0.0;                    //! residue cover reduces  cn2_bare
         au.Zero2DArray(ref solute);                 //! solute in each layer (kg/ha)


      //Local Variables

         au.ZeroArray(ref cover_tot);                //! total canopy cover of crops (0-1)
         au.ZeroArray(ref cover_green);              //! green canopy cover of crops (0-1)
         au.ZeroArray(ref canopy_height);            //! canopy heights of each crop (mm)
         num_crops = 0;                          //! number of crops ()
         sumes1 = 0.0;                           //! cumulative soil evaporation in stage 1 (mm)
         sumes2 = 0.0;                           //! cumulative soil evaporation in stage 2 (mm)
         au.Zero2DArray(ref dlt_solute);             //! change in solute each in layer (kg n/ha)
         au.Zero2DArray(ref solute_leach);           //! amount of solute leached from each layer (kg/ha)
         au.Zero2DArray(ref solute_up);              //! amount of solute upped from each layer (kg/ha)
                                                   //! nih - I dont like these names.
         au.ZeroArray(ref irrigation_solute);        //! amount of solute in irrigation water (kg/ha)


		   runoff_pot = 0.0;                       //! potential runoff with no pond(mm)  
         irrigation = 0.0;                       //! irrigation (mm)

         //obsrunoff = 0.0;                        //! observed runoff (mm)
         tillage_cn_red = 0.0;                   //! reduction in CN due to tillage ()
         tillage_cn_rain = 0.0;                  //! cumulative rainfall below which tillage reduces CN (mm)
         tillage_rain_sum = 0.0;                 //! cumulative rainfall for tillage CN reduction (mm)
         obsrunoff_found = false;                //! whether obserevd runoff was returned from system
         obsrunoff_name = "not_read";                    //! system name of observed runoff


         //TODO: uncomment this later on -> num_canopy_fact = 0;                    //! number of canopy factors read ()
         eo_system = 0.0;                        //! eo from somewhere else in the system
         _eo_source = "not_read";                         //! system variable name of external eo source

         real_eo = 0.0;                          //! eo determined before any ponded water is evaporated (mm)

         irrigation_layer = 0;                   //! trickle irrigation input layer
         //TODO: remove the irrigation_layer above.



      }


/*
//TODO: this is used by the soilwat2_set_my_variables(). This allows other modules to set soilwat's variables.
// this is implememented in .NET by declaring a Property with Gets and Sets and making it an INPUT tag. Nb. that i think you have to use a local variable as a go between as well. See SoilNitrogen [Input] tags with get and set. Or maybet it is [Output] tags.
*/
   private void soilwat2_zero_default_variables()
      {

      //*+  Mission Statement
      //*     zero default soil water initialisation parameters      

      numvals_insoil = 0;
      numvals_sw = 0;
      numvals_profile_esw_depth = 0;
      numvals_wet_soil_depth = 0;
      numvals_profile_fesw = 0;

      _insoil = 0.0;
      au.ZeroArray(ref _sw_dep);
      _profile_esw_depth = 0.0;
      _wet_soil_depth = 0.0;
      _profile_fesw = 0.0;
      }

   
   private void soilwat2_zero_daily_variables()
      {

      //sv- this is exectued in the Prepare event.

      au.ZeroArray(ref flow);
      au.ZeroArray(ref flux);
      au.ZeroArray(ref es_layers, max_layer);
      au.ZeroArray(ref cover_tot, max_crops);
      au.ZeroArray(ref cover_green, max_crops);
      //au.ZeroArray(ref crop_module, max_crops);
      au.ZeroArray(ref canopy_height, max_crops);

      residue_cover     = 0.0;
      eo                = 0.0;
      eos               = 0.0;
      cn2_new           = 0.0;
      drain             = 0.0;
      infiltration      = 0.0;
      runoff            = 0.0;
      runoff_pot        = 0.0;
      num_crops         = 0;
      //obsrunoff         = 0.0;
      obsrunoff_found   = false;
      pond_evap         = 0.0;                    //! evaporation from the pond surface (mm)
      real_eo           = 0.0;                      //! eo determined before any ponded water is evaporated (mm)


      //! initialise all solute information
      au.Zero2DArray(ref solute);
      au.Zero2DArray(ref solute_leach);
      au.Zero2DArray(ref solute_up);
      au.Zero2DArray(ref dlt_solute);
      }


   private void Lateral_zero_daily_variables()
      {

      au.ZeroArray(ref outflow_lat);

      }


#endregion


#region Functions to Set Intial SW and Error Check Soil Profile


#region Set Initial SW values



   //sv- DEAN SAYS THAT THE GUI ALWAYS SPECIFIES A SET OF SW VALUES. THEREFORE YOU DON'T NEED ANY OF THIS CODE TO SET DEFAULTS ANYMORE. ALL OF THIS IS DONE IN THE GUI NOW AND YOU JUST GET GIVE THE SW VALUES FOR EACH LAYER. SO DON'T NEED THIS ANYMORE.
   //Had to uncomment this because it is called in the "set" for the "insoil" property. I don't think any simulation actually does a set on "insoil" though
   //so perhaps I can comment it out and turn "insoil" just into a normal variable that is a [Param].
   //TODO: see if I can comment out the soilwat2_set_default() as per the comments above.

   private void soilwat2_set_default()
      {

      //*+  Purpose
      //*       set default soil water values when the user does not specify any starting water values.

      int      layer;               //! layer number in loop
      int      num_layers;          //! number of layers used in profile 
      int      num_layers_filled;   //! number of layers filled in profile
      double   esw_remaining;       //! esw left after distribution top down (mm)
      //double   depth_remaining;     //! depth left after distribution top down (mm)
      double   esw_avail;           //! esw available for distribution (mm)
      double   profile_esw_depth_local;   //! depth of esw in profie to fill (mm)
      string   line;                //! temp output record



   //sv- initial sw section (5 different methods to choose from) (see soilwat2_init() and soilwat2_set_default() to see which method is used)
   
      //sv- insoil is used for two different initial sw methods:
      //sv- 1. User Specified Soil Water Conent method is used when insoil > 1  (user has entered an sw for each layer in the soil)
      //sv- 2. FASW evenly distributed method is used when  0 <= insoil <= 1    (user has entered a fraction for entire profile and wants each layer to be this fraction) 
      //! switch describing initial soil water  //sv-specifies which option you are using.
      // -> insoil

      //sv- 3. FASW filled from top method      (user has entered a fraction for entire profile but wants you to achieve this fraction for the whole profile by completely filling up the top layers before filling the lower layers. So bottom layers are left dry)
      //! initial fraction of esw of profile distributed from top down ()
      // -> profile_fesw

      //sv- 4. depth of available sw from top of profile method (same as profile_fesw but the user has entered total amount of water in millimeters for the entire profile instead of as a fraction for the whole profile)
      // -> profile_esw_depth

      //sv- 5. depth of wet soil (filled to dul) method  (same as profile_esw_depth only don't worry about what is available to the plant(profile_esw_depth =  profile_fesw * (the sum of dlayer))
      //! initial depth of soil filled to drained upper limit (field capacity) (mm)
      // -> wet_soil_depth

   //sv- end of initial sw section



      //*****************
      //Initial SW   (5 different methods to choose from)
      //*****************

      //sv- these sw values read in from the sim file gets overriden by soilwat2_set_default() unless the user specified method (ie. insoil > 1) was choosen. 
      //sv- If the user specified method (ie. insoil > 1) is not selected then soilwat2_set_default() uses one of the 4 "properties" (depending on the method that was choosen)
      //sv- to create the new initial sw profile that then replaces these read in values for sw.

     

      //Must specify one of Profile_esw_depth, wet_soil_depth, Profile_fesw Insoil, or Sw  to specify initial soilwater distribution.

      //! check for exclusiveness
      if (numvals_profile_esw_depth > 0)
         {
         if ((numvals_insoil > 0) || (numvals_sw > 0) || (numvals_profile_fesw > 0) || (numvals_wet_soil_depth > 0))
            {
            //! others present
            throw new Exception ("Insoil, Sw, profile_fesw or wet_soil_depth cannot be specified with \"profile_esw_depth\".");
            }
         else
            {
            //! numvals_profile_esw_depth present only
            line = "Initial soilwater distributed from top down using \"profile_esw_depth\" parameter.";
            Console.WriteLine(line);
            }
         }


      else if (numvals_wet_soil_depth > 0)
         {         
         //! numvals_profile_esw_depth absent
         if ((numvals_insoil > 0) || (numvals_profile_fesw > 0) || (numvals_sw > 0))
            {
            //! others present
            throw new Exception ("Insoil, Profile_fesw or Sw cannot be specified with \"wet_soil_depth\".");
            }
         else
            {
            line = "Initial soilwater distributed from top down using \"wet_soil_depth\" parameter.";
            Console.WriteLine(line);
            }
         }



      else if (numvals_profile_fesw > 0)
         {
         //! numvals_profile_esw_depth absent
         if ((numvals_insoil > 0) || (numvals_sw > 0))
            {
            //! others present
            throw new Exception("Insoil or Sw cannot be specified with \"profile_fesw\".");
            }
         else
            {
            line = "Initial soilwater distributed from top down using \"profile_fesw\" parameter.";
            Console.WriteLine(line);
            }
         }



      else if (numvals_insoil > 0)
         {
         if (numvals_sw > 0)
            {
            //! note - this never activates because the switches are set previously
            //see soilwat2_soil_profile_param() for where the switches are set.
            throw new Exception("Sw cannot be specified with \"insoil\".");
            //Console.WriteLine(line);
            }
         else
            {
            //! only insoil present
            line = "Initial soilwater distributed evenly using \"insoil\" parameter.";
            Console.WriteLine(line);
            }
         }         



      else if (numvals_sw > 0)
         {
         //! ok - only sw present
         line = "Initial soilwater distributed using \"sw\" parameter.";
         Console.WriteLine(line);
         }



      else
         {
         //! all absent - must have one
         throw new Exception("Must specify one of Insoil, Sw, wet_soil_depth, Profile_fesw or Profile_esw_depth to specify initial soilwater distribution.");
         }








      //! initialize sw
      //! set up default soil water profile

      //! we want to calculate default


      if (numvals_insoil > 0)
         {
         //! insoil parameter set - distibute evenly
         au.ZeroArray(ref _sw_dep);
         num_layers = au.count_of_real_vals(_dlayer, max_layer);

         for (layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            //! set default according to insoil fraction of plant-
            //! available water
            _sw_dep[layer] = _ll15_dep[layer] + (_dul_dep[layer] - _ll15_dep[layer]) * _insoil;

            soilwat2_layer_check(layer);
            soilwat2_check_profile(layer);
            }
         }


      else if (numvals_wet_soil_depth > 0)
         {         
         //! wet_soil_depth parameter set - distribute top down
         au.ZeroArray(ref _sw_dep);
         num_layers = au.count_of_real_vals(_dlayer, max_layer);
         au.CopyTo(ref _sw_dep, _ll15_dep, num_layers);
      
         num_layers_filled = au.find_layer_no(_wet_soil_depth, _dlayer, num_layers);

         for(layer=au.si;layer<=au.ci(num_layers_filled); layer++)
            {
            //! set default according to wet_soil_depth of plant available water
            _sw_dep[layer] = _dul_dep[layer];
            }

         //! adjust last layer
         _sw_dep[au.ci(num_layers_filled)] = _ll15_dep[au.ci(num_layers_filled)]
                                       + (_dul_dep[au.ci(num_layers_filled)] - _ll15_dep[au.ci(num_layers_filled)])
                                       * ApsimUtil.root_proportion(num_layers_filled, _dlayer, _wet_soil_depth);

         if ((au.SumArray(_dlayer)+ precision_sw_dep) < _wet_soil_depth)
            {
            line = "Can't fit wet soil depth of " + _wet_soil_depth + " into profile depth of " + au.SumArray(_dlayer);
            throw new Exception(line);
            }
         else
            {
            //! depth fits in profile
            } 
         }  


      else if (numvals_profile_fesw > 0)
         {
         //! profile_fesw parameter set - distribute top down
         au.ZeroArray(ref _sw_dep);
         num_layers = au.count_of_real_vals(_dlayer, max_layer);
         au.CopyTo(ref _sw_dep, _ll15_dep, num_layers);
         profile_esw_depth_local = au.SumArray(au.Subtract(_dul_dep, _ll15_dep)) * _profile_fesw;
         esw_remaining = profile_esw_depth_local;

         for (layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            //! set default according to profile_esw_depth of plant available water
            esw_avail =  mu.bound(esw_remaining, 0.0, (_dul_dep[layer] - _ll15_dep[layer]));

            _sw_dep[layer] = _ll15_dep[layer] + esw_avail;
            esw_remaining = esw_remaining - esw_avail;
            }

         if (esw_remaining > precision_sw_dep)
            {
            //! we have too much water to distirbute - won't fit in profile
            line = "Can't fit profile esw of "+ (profile_esw_depth_local + esw_remaining) + " into profile esw depth of " + profile_esw_depth_local;
            throw new Exception(line);
            }
         else
            {
            //! it fits
            }
         }


      else if (numvals_profile_esw_depth > 0)
         {
         //! profile_esw_depth parameter set - distribute top down
         au.ZeroArray(ref _sw_dep);
         num_layers = au.count_of_real_vals(_dlayer, max_layer);
         au.CopyTo(ref _sw_dep, _ll15_dep, num_layers);

         esw_remaining = _profile_esw_depth;

         for(layer=au.si; layer<=au.ci(num_layers); layer++)
            {
            //! set default according to profile_esw_depth of plant available water
            esw_avail =  mu.bound(esw_remaining, 0.0, (_dul_dep[layer] - _ll15_dep[layer]));

            _sw_dep[layer] = _ll15_dep[layer] + esw_avail;
            esw_remaining = esw_remaining - esw_avail;
            }

         if (esw_remaining > precision_sw_dep)
            {
            //! we have too much water to distirbute - won't fit in profile
            profile_esw_depth_local = au.SumArray(au.Subtract(_dul_dep, _ll15_dep));
            line = "Can't fit profile esw of " + _profile_esw_depth + " into profile esw depth of " + profile_esw_depth_local;
            throw new Exception(line);
            }
         else
            {
            //! it fits
            }
         }


      else if (numvals_sw > 0)
         {
         //! do nothing
         }


      else
         {
         throw new Exception("Initial soilwater distribution method not defined.");
         }

   
      }


   //All the following function are used ONLY in soilwat2_init() no where else.

   private void soilwat2_read_constants()
      {

      //##################
      //Constants    --> soilwat2_read_constants()
      //##################


      num_solute_flow = solute_flow_eff.Length;
      num_solute_flux = solute_flux_eff.Length;


      //sv- the following test is removed from soilwat2_read_constants()
      num_canopy_fact = canopy_fact.Length;
      if (num_canopy_fact != canopy_fact_height.Length)
         {
         throw new Exception("No. of canopy_fact coeffs do not match the no. of canopy_fact_height coeffs.");
         }

      //sv- the following test is removed from soilwat2_read_constants() too
      switch (act_evap_method)
         {
         case "ritchie":
            evap_method = ritchie_method;  //ritchie_method = 1
            break;
         case "bs_a":
            evap_method = 2;
            break;
         case "bs_b":
            evap_method = 3;
            break;
         case "bs_acs_jd":
            evap_method = 4;
            break;
         case "rickert":
            evap_method = 5;
            break;
         case "rwc":
            evap_method = 6;
            break;
         default:
            evap_method = ritchie_method;
            break;
         }


      if (evap_method != ritchie_method)
         {
         evap_method = ritchie_method;
         ApsimUtil.warning_error("Your ini file is set to use an evaporation method other than ritchie(act_evap_method=1)." + Environment.NewLine
                                 + "This module: SoilWater can only use ritchie evaporation." + Environment.NewLine
                                 + "Your evaporation method has therefore been reset to ritchie(act_evap_method=1).");
         }


      //##################
      //End of Constants
      //##################

      }


   private void soilwat2_soil_property_param()
      {

      //##################
      //Soil Properties  --> soilwat2_soil_property_param()
      //##################




      //*****************
      //Initial SW   (5 different methods to choose from) (these 4 "properties" are needed to create the initial sw "profile")
      //*****************

      //sv- the following test is removed from soilwat2_soil_property_param()
      //sv- used to initialise the sw profile [used in soilwat2_set_default()] -> flags indicating if anything was read in from the sim file.

      //sv- insoil is used for two different initialisation of sw methods:
      //sv- User Specified Soil Water Conent method is used when insoil > 1 
      //sv- FASW evenly distributed method is used when  0 <= insoil <= 1 
      if (Double.IsNaN(_insoil))
         numvals_insoil = 0;

      //sv- FASW filled from top method method
      if (Double.IsNaN(_profile_fesw))
         numvals_profile_fesw = 0;

      //sv- depth of available sw from top of profile method
      if (Double.IsNaN(_profile_esw_depth))
         numvals_profile_esw_depth = 0;

      //sv- depth of wet soil (filled to dul) method
      if (Double.IsNaN(_wet_soil_depth))
         numvals_wet_soil_depth = 0;


      //*****************
      //End of Initial SW
      //*****************





      //If this function has been called by a Reset Event
      //then reset (all the variables that are "Settable" by the user) back to the original values read in by [Param]  
      if (initDone)
         {
         //Soil Property

            //initial starting soil water 

         numvals_insoil = reset_numvals_insoil;
         _insoil = reset_insoil;

         numvals_profile_fesw = reset_numvals_profile_fesw;
         _profile_fesw = reset_profile_fesw;

         numvals_profile_esw_depth = reset_numvals_profile_esw_depth;
         _profile_esw_depth = reset_profile_esw_depth;

         numvals_wet_soil_depth = reset_numvals_wet_soil_depth;
         _wet_soil_depth = reset_wet_soil_depth;
         
            //runoff
         _cn2_bare = reset_cn2_bare;
         _cn_red = reset_cn_red;
         _cn_cov = reset_cn_cov;

            //ponding
         _max_pond = reset_max_pond; 
         
         }




      //sv- the following test is removed from soilwat2_soil_property_param()
      if (_cn_red >= _cn2_bare)
         {
         _cn_red = _cn2_bare - 0.00009;
         }




      //****************
      //U and Cona (used in Ritchie Evaporation)
      //*****************

      //sv- the following test is removed from soilwat2_soil_property_param()

      //u - can either use (one value for summer and winter) or two different values.
      //    (must also take into consideration where they enter two values [one for summer and one for winter] but they make them both the same)
      if (Double.IsNaN(_u))
         {
         if ( (Double.IsNaN(summeru) || (Double.IsNaN(winteru))) )
            {
            throw new Exception("A single value for u OR BOTH values for summeru and winteru must be specified");
            }
         //if they entered two values but they made them the same
         if (summeru == winteru)
            {
            _u = summeru;      //u is now no longer null. As if the user had entered a value for u.
            }
         }
      else
         {
         summeru = _u;
         winteru = _u;
         }

      //cona - can either use (one value for summer and winter) or two different values.
      //       (must also take into consideration where they enter two values [one for summer and one for winter] but they make them both the same)
      if (Double.IsNaN(_cona))
         {
         if ((Double.IsNaN(summercona)) || (Double.IsNaN(wintercona)))
            {
            throw new Exception("A single value for cona OR BOTH values for summercona and wintercona must be specified");
            }
         //if they entered two values but they made them the same.
         if (summercona == wintercona)
            {
            _cona = summercona;   //cona is now no longer null. As if the user had entered a value for cona.
            }
         }
      else
         {
         summercona = _cona;
         wintercona = _cona;
         }

      //summer and winter default dates.
      if (summerdate == "not_read")
         {
         summerdate = "1-oct";
         }

      if (winterdate == "not_read")
         {
         winterdate = "1-apr";
         }

      //assign u and cona to either sumer or winter values
      if (ApsimUtil.date_within(winterdate, summerdate, today))
         {
         _cona = wintercona;
         _u = winteru;
         }
      else
         {
         _cona = summercona;
         _u = summeru;
         }


      //***************
      //end U and Cona
      //***************




      //##################
      //End of Soil Properties
      //##################


      }


   private void soilwat2_soil_profile_param()
      {


      //TODO: Someone needs to fix the problem below properly.
      //The following is necessay because the ini file requires you to put in a default value (an array of 3 values) so that when ApsimToSim creates a sim from the gui it has a spot to put the values in the sim file.
      // This only effects Optional parameters. Non optionals must have a value specified by the user in the gui so they get these default values (array of 3 elements from ini file) written over with a full array. 
      // Optional parameters however are allowed to not be specified. And if they don't then this model instead of reading in a null value, reads in the default value from the ini file (3 element array) 
      // So we here we do a test to see if we have a default value from the ini file read in, and if so set them to null.

      if (au.IsIniDefault(mwcon))
         {
         mwcon = null;
         }
      if (au.IsIniDefault(ks))
         {
         ks = null;
         }
      if (au.IsIniDefault(klat))
         {
         klat = null;
         }


      //##################
      //Soil Profile  -->  soilwat2_soil_profile_param()
      //##################

      Console.WriteLine("   - Reading Soil Profile Parameters");


      //Initialise the Optional Array Parameters (if not read in).

      //sv- with mwcon: 0 is impermeable and 1 is permeable.
      //sv- if mwcon is not specified then set it to 1 and don't use ks. If it is specified then use mwcon and use ks. 
      //c dsg - if there is NO impermeable layer specified, then mwcon must be set to '1' in all layers by default.
      if (mwcon == null)
         {
         mwcon = new double[_dlayer.Length];
         au.fill_real_array(ref mwcon, 1.0, _dlayer.Length);
         }
      else
         {
         ApsimUtil.warning_error("mwcon is being replaced with a saturated conductivity (ks). " + Environment.NewLine
                                 + "See documentation for details.");
         }


      if (ks == null)
         {
         using_ks = false;
         ks = new double[_dlayer.Length];
         au.ZeroArray(ref ks);
         }
      else
         {
         using_ks = true;
         }


      //for (klat == null) see Lateral_init().





      //If this function has been called by a Reset Event
      //then reset (all the variables that are "Settable" by the user) back to the original values read in by [Param]  
      if (initDone)
         {
         //soil profile
         _dlayer = au.Dup(reset_dlayer);
         _sat = au.Dup(reset_sat);
         _dul = au.Dup(reset_dul);
         numvals_sw = reset_numvals_sw;  //used in soilwat2_set_default();
         _sw = au.Dup(reset_sw);
         _ll15 = au.Dup(reset_ll15);
         _air_dry = au.Dup(reset_air_dry);
         
         }




      //recalculate ALL the _dep arrays  (I don't know if this is really necessary since they are calculated in "Set" of property) 
      
      for (int i = au.si; i <= au.ci(_dlayer.Length); i++)
         {
         _sat_dep[i] = _sat[i] * _dlayer[i];
         _dul_dep[i] = _dul[i] * _dlayer[i];
         _sw_dep[i] = _sw[i] * _dlayer[i];
         _ll15_dep[i] = _ll15[i] * _dlayer[i];
         _air_dry_dep[i] = _air_dry[i] * _dlayer[i];
         }





      // THE FOLLOWING CODE INTERACTS WITH soilwat2_set_default(). 
      //It is necessary to make sure the "Sw cannot be specified with \"insoil\"." case is never activated.
      //sv- comment out the code below because the GUI always defines SW in layers. The 5 different methods are sorted out in the GUI. The GUI then specifies the sw layers depending on the method and sets insoil is always set to be >1 and 
      //sv-start
      //sv- the following initialisation is removed from soilwat2_soil_profile_param()
      //sv- if insoil is specified then sort out which of the two methods you are using (user specified sw OR FASW evenly distributed) 
      //sv- if the user specified an insoil and they specified FASW evenly distributed method (ie. 0>=insoil<=1) not the user specified sw method.
      if ((numvals_insoil > 0) && ((_insoil >= 0.0) && (_insoil <= 1.0)))
         {
         //sv- warn the user that their user specified sw is beign overridden.
         Console.WriteLine("Soil water in parameter file is being overridden by" + Environment.NewLine + "the insoil parameter which is between 0 and 1");
         numvals_sw = 0;         //sv- change the flag to pretend that no sw values were not read in.
         }
      else
         {
         numvals_insoil = 0;     //sv- change the flag to pretend that no insoil value was read in.
                                 //sv- isn't this a mistake? what if you want to use a user specifed sw method (ie. insoil > 1). I assume soilwat2_set_default() caters for this.
         }






      //sv- Since you have initialised all the _dep[] profile variables 
      //sv- AND you have got all your numvals flags indicating what initial sw method was selected sorted out
      //sv- now you can set your initial sw for the profile.
      soilwat2_set_default();
      //sv-end


      //check each layer of the init sw profile
      for (int layer = au.si; layer <= au.ci(_dlayer.Length); layer++)
         {
         soilwat2_check_profile(layer);
         }

      //*****************
      //End of Initial SW  
      //*****************



      //##################
      //End of Soil Profile
      //##################

      }


   private void soilwat2_evap_init()
      {

      //##################
      //Evap Init   --> soilwat2_evap_init (), soilwat2_ritchie_init()
      //##################   

      if (evap_method == ritchie_method)
         {
         //soilwat2_ritchie_init();
         //*+  Mission Statement
         //*       Initialise ritchie evaporation model

         double swr_top;       //! stage 2 evaporation occurs ratio available sw potentially available sw in top layer

         //! set up evaporation stage
         swr_top = mu.divide((_sw_dep[au.si] - ll15[au.si]), (_dul_dep[au.si] - _ll15_dep[au.si]), 0.0);
         swr_top = mu.bound(swr_top, 0.0, 1.0);

         //! are we in stage1 or stage2 evap?
         if (swr_top < sw_top_crit)
            {
            //! stage 2 evap
            sumes2 = sumes2_max - (sumes2_max * mu.divide(swr_top, sw_top_crit, 0.0));
            sumes1 = _u;
            t = Math.Pow((mu.divide(sumes2, _cona, 0.0)), 2);
            }
         else
            {
            //! stage 1 evap
            sumes2 = 0.0;
            sumes1 = sumes1_max - (sumes1_max * swr_top);
            t = 0.0;
            }
         }
      else
         {
         throw new Exception("Tried to initialise unknown evaporation method");
         }

      //##################
      //End of Evap Init
      //##################


      }


   private void Lateral_init()
      {

      //##################
      //Lateral Init  --> lateral_init(lateral)
      //##################


      //sv- the following test is removed from Lateral_read_param()
      //sv- Lateral variables are all optional so zero them if not entered by user.
      //These are optional parameters and so they may have a default value of NaN(double vars) or null(array vars) if they were not read in.
      //So set them to zero.

      if (Double.IsNaN(slope))
         slope = 0.0;

      if (Double.IsNaN(discharge_width))
         discharge_width = 0.0;

      if (Double.IsNaN(catchment_area))
         catchment_area = 0.0;

      if (klat == null)
         {
         klat = new double[_dlayer.Length];
         au.ZeroArray(ref klat);
         }

      //taken from Lateral_zero_variables()
      au.ZeroArray(ref outflow_lat);

      //see Lateral_process() for where daily input inflow_lat[] is initialised if not read in.

      //##################
      //End of Lateral Init  
      //##################
      }


#endregion



#region Check a given layer for Errors

   
   private void soilwat2_layer_check(int layer)
      {

      //sv- this function is only ever used in the function soilwat2_check_profile(int layer)

      //*+  Purpose
      //*       checks that layer lies in range of 1 - num_layers

      //*+  Notes
      //*             reports error if layer < min_layer
      //*             or layer > num_layers

      //*+  Mission Statement
      //*     Check Soil Water Parameters for a given layer

      int min_layer = 1;      //! lowest value for a layer number

      string error_messg;
      int    num_layers;

      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      if (layer < min_layer) 
         {
         error_messg = String.Format("{0} {1} {2} {3}", 
                                     " soil layer no. ", layer,
                                     " is below mimimum of ", min_layer);
         ApsimUtil.warning_error(error_messg);
         }
      else if (layer > num_layers) 
         {
         error_messg = String.Format("{0} {1} {2} {3}", 
                                     " soil layer no. ", layer,
                                     " is above maximum of ", num_layers);
         ApsimUtil.warning_error(error_messg);
         }
      }



   private void soilwat2_check_profile(int layer)
      {
      //*+  Purpose
      //*       checks validity of soil water parameters for a soil profile layer

      //*+  Notes
      //*           reports an error if
      //*           - g%ll15_dep, _dul_dep, and _sat_dep are not in ascending order
      //*           - ll15 is below min_sw
      //*           - sat is above max_sw
      //*           - sw > sat or sw < min_sw      

      //Constant Values
      double   min_sw_local = 0.0;
      double   max_sw_margin = 0.01;

      string   err_messg;           //! error message

      double   dul_local;                 //! drained upper limit water content of layer (mm water/mm soil)
      double   dul_errmargin;       //! rounding error margin for dulc
      double   ll15_local;                //! lower limit at 15 bars water content of layer (mm water/mm soil)
      double   ll15_errmargin;      //! rounding error margin for ll15c
      double   air_dry_local;             //! lower limit at air dry water content of layer (mm water/mm soil)
      double   air_dry_errmargin;   //! rounding error margin for air_dryc
      double   sat_local;                 //! saturated water content of layer (mm water/mm soil)
      double   sat_errmargin;       //! rounding error margin for satc
      double   sw_local;                  //! soil water content of layer l (mm water/mm soil)
      double   sw_errmargin;        //! rounding error margin for swc

      double   max_sw_local;              //! largest acceptable value for sat (mm water/mm soil)
      

      max_sw_local = 1.0 - mu.divide(bd[layer], specific_bd, 0.0);  //ie. Total Porosity

      sw_local = mu.divide(_sw_dep[layer], _dlayer[layer], 0.0);
      sat_local = mu.divide(_sat_dep[layer], _dlayer[layer], 0.0);
      dul_local = mu.divide(_dul_dep[layer], _dlayer[layer], 0.0);
      ll15_local = mu.divide(_ll15_dep[layer], _dlayer[layer], 0.0);
      air_dry_local = mu.divide(_air_dry_dep[layer], _dlayer[layer], 0.0);

      //TODO: where do these error_margins come from?
      sw_errmargin = mu.error_margin(sw_local);
      sat_errmargin = mu.error_margin(sat_local);
      dul_errmargin = mu.error_margin(dul_local);
      ll15_errmargin = mu.error_margin(ll15_local);
      air_dry_errmargin = mu.error_margin(air_dry_local);


      if ((air_dry_local + air_dry_errmargin) < min_sw_local) 
         {
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})", 
                                    " Air dry lower limit of ", 
                                    air_dry_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is below acceptable value of ", 
                                    min_sw_local);
         ApsimUtil.warning_error(err_messg);
         }


      if ((ll15_local + ll15_errmargin) < (air_dry_local - air_dry_errmargin)) 
         {
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                    " 15 bar lower limit of ", 
                                    ll15_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is below air dry value of ", 
                                    air_dry_local);
         ApsimUtil.warning_error(err_messg);
         }



      if ((dul_local + dul_errmargin) <= (ll15_local - ll15_errmargin))
         {
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                    " drained upper limit of ",
                                    dul_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is at or below lower limit of ", 
                                    ll15_local);
         ApsimUtil.warning_error(err_messg);
         }

      if ((sat_local + sat_errmargin) <= (dul_local - dul_errmargin))
         {
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                    " saturation of ", 
                                    sat_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is at or below drained upper limit of ",
                                    dul_local);
         ApsimUtil.warning_error(err_messg);
         }

      if ((sat_local - sat_errmargin) > (max_sw_local + max_sw_margin))
         {
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G} {7} {8} {9:G} {10} {11} {12:G})",
                                    " saturation of ", 
                                    sat_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is above acceptable value of ", 
                                    max_sw_local,
                                    Environment.NewLine,
                                    "You must adjust bulk density (bd) to below ",
                                    (1.0 - sat_local) * specific_bd,
                                    Environment.NewLine,
                                    "OR saturation (sat) to below ", 
                                    max_sw_local);
         ApsimUtil.warning_error(err_messg);
         }


     if (sw_local - sw_errmargin > sat_local + sat_errmargin) 
         {
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                                    " soil water of ", 
                                    sw_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is above saturation of ", 
                                    sat_local);
         ApsimUtil.warning_error(err_messg);
         }

      if (sw_local + sw_errmargin < air_dry_local - air_dry_errmargin)
         {     
         err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                                    " soil water of ", 
                                    sw_local,
                                    " in layer ", 
                                    layer,
                                    Environment.NewLine,
                                    "         is below air-dry value of ", 
                                    air_dry_local);
         ApsimUtil.warning_error(err_messg);
         }

      }

      
#endregion


#endregion


#region Soil Science Functions


#region Runoff


   private void soilwat2_runoff(double Rain, double Runon, double TotalInterception, ref double Runoff )
      {
      Runoff = 0.0;  //zero the return parameter

      if ((Rain + Runon - TotalInterception) > 0.0)
         {
         if (obsrunoff_name == "not_read") 
            {
            soilwat2_scs_runoff(Rain, Runon, TotalInterception, ref Runoff);
            }
         else
	         {
            //          write (line, '(a,i4,a,i3,a)')
            string line = String.Format("{0} {1} {2} {3} {4}",
                                       "Year = ", 
                                       year,
                                       ", day = ", 
                                       day,
                                       ", Using predicted runoff for missing observation");

            ApsimUtil.warning_error(line);
            soilwat2_scs_runoff(Rain, Runon, TotalInterception, ref Runoff);
	         }

         //The reduction in the runoff as a result of doing a tillage (tillage_cn_red) ceases after a set amount of rainfall (tillage_cn_rain).
         //this function works out the accumulated rainfall since last tillage event, and turns off the reduction if it is over the amount of rain specified.
         soilwat2_tillage_addrain(rain, runon, TotalInterception); //! Update rain since tillage accumulator. ! NB. this needs to be done _after_ cn calculation.
         }
      else
         {
         //nothing
         }
      
      }


   
   private void soilwat2_scs_runoff(double Rain, double Runon, double TotalInterception, ref double Runoff)
      {
      double   cn;                                 //! scs curve number
      double   cn1;                                //! curve no. for dry soil (antecedant) moisture
      double   cn3;                                //! curve no. for wet soil (antecedant) moisture
      double   cover_fract;                        //! proportion of maximum cover effect on runoff (0-1)
      double   cnpd;                               //! cn proportional in dry range (dul to ll15)
      int      layer;                              //! layer counter
      int      num_layers;                         //! number of layers
      double   s;                                  //! potential max retention (surface ponding + infiltration)
      double   xpb;                                //! intermedite variable for deriving runof
      double[] runoff_wf =  new double[max_layer]; //! weighting factor for depth for each la
      double   dul_fraction;                       // if between (0-1) not above dul, if (1-infinity) above dul 
      double   tillage_reduction;                  //! reduction in cn due to tillage

      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      soilwat2_runoff_depth_factor(ref runoff_wf);
      
      cnpd = 0.0;
      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         dul_fraction = mu.divide((_sw_dep[layer] - _ll15_dep[layer]), (_dul_dep[layer] - _ll15_dep[layer]), 0.0);
         cnpd = cnpd + dul_fraction * runoff_wf[layer];
         }
      cnpd = mu.bound(cnpd, 0.0,1.0);


      //reduce cn2 for the day due to the cover effect
      //nb. cover_surface_runoff should really be a parameter to this function
      cover_fract = mu.divide(cover_surface_runoff, _cn_cov, 0.0);
      cover_fract = mu.bound(cover_fract, 0.0, 1.0);
      cn2_new = _cn2_bare - (_cn_red * cover_fract);


      //tillage reduction on cn
      //nb. tillage_cn_red, tillage_cn_rain, and tillage_rain_sum, should really be parameters to this function
      if (tillage_cn_rain > 0.0)
         {
         tillage_reduction = tillage_cn_red * (mu.divide(tillage_rain_sum, tillage_cn_rain,0.0) - 1.0);
         cn2_new = cn2_new + tillage_reduction;
         }
      else
	      {
         //nothing
      	}


      //! cut off response to cover at high covers if p%cn_red < 100.
      cn2_new =  mu.bound(cn2_new, 0.0, 100.0);      

      cn1 = mu.divide(cn2_new,(2.334 - 0.01334 * cn2_new), 0.0);
      cn3 = mu.divide(cn2_new, (0.4036 + 0.005964 * cn2_new), 0.0);
      cn = cn1 + (cn3 - cn1) * cnpd;

      // ! curve number will be decided from scs curve number table ??dms
      s = 254.0 * (mu.divide(100.0, cn, 1000000.0) - 1.0);
      xpb = (Rain + Runon - TotalInterception) - 0.2 * s;
      xpb = mu.l_bound(xpb, 0.0);

      //assign the output variable
      Runoff = mu.divide(xpb*xpb, (Rain + Runon - TotalInterception + 0.8 * s), 0.0);

      //bound check the ouput variable
      ApsimUtil.bound_check_real_var(Runoff, 0.0,(Rain + Runon - TotalInterception),"runoff");
      }



  private void soilwat2_cover_surface_runoff(ref double Cover_Surface_Runoff)
      {

      //This does NOT calculate runoff. It calculates an effective cover that is used for runoff.
      //In the process event this is called before the soilwat2_runoff.
 
      //*+  Purpose
      //*       calculate the effective runoff cover

      //*+  Assumptions
      //*       Assumes that if canopy height is negative it is missing.

      //*+  Mission Statement
      //*     Calculate the Effective Runoff surface Cover
      
      double   canopyfact;             //! canopy factor (0-1)
      int      crop;                   //! crop number
      double   effective_crop_cover;   //! effective crop cover (0-1)
      double   cover_surface_crop;     //! efective total cover (0-1)

       //! cover cn response from perfect   - ML  & dms 7-7-95
       //! nb. perfect assumed crop canopy was 1/2 effect of mulch
       //! This allows the taller canopies to have less effect on runoff
       //! and the cover close to ground to have full effect (jngh)

       //! weight effectiveness of crop canopies
       //!    0 (no effect) to 1 (full effect)
       
      cover_surface_crop = 0.0;
      for (crop=au.si; crop<=au.ci(num_crops); crop++)
         {
         if (canopy_height[crop] >= 0.0)
            {
            canopyfact = au.linear_interp_real(canopy_height[crop], canopy_fact_height, canopy_fact, num_canopy_fact);
            }
         else
	         {
            canopyfact = canopy_fact_default;
	         }

         effective_crop_cover = cover_tot[crop] * canopyfact;
         cover_surface_crop = ApsimUtil.add_cover(cover_surface_crop, effective_crop_cover);   
         }

      //! add cover known to affect runoff
      //!    ie residue with canopy shading residue         
      Cover_Surface_Runoff = ApsimUtil.add_cover(cover_surface_crop, residue_cover);
      }



   private void soilwat2_runoff_depth_factor(ref double[] runoff_wf)
      {

      //runoff_wf -> ! (OUTPUT) weighting factor for runoff

      //*+  Purpose
      //*      Calculate the weighting factor hydraulic effectiveness used
      //*      to weight the effect of soil moisture on runoff.

      //*+  Mission Statement
      //*      Calculate soil moisture effect on runoff      

      double      profile_depth;             //! current depth of soil profile - for when erosion turned on     
      double      cum_depth;                 //! cumulative depth (mm)
      double      hydrol_effective_depth_local;    //! hydrologically effective depth for runoff (mm)
      int         hydrol_effective_layer;    //! layer number that the effective depth occurs in ()
      int         layer;                     //! layer counter
      int         num_layers;                //! number of layers
      double      scale_fact;                //! scaling factor for wf function to sum to 1
      double      wf_tot;                    //! total of wf ()
      double      wx;                        //! depth weighting factor for current total depth. intermediate variable for deriving wf (total wfs to current layer)
      double      xx;                        //! intermediate variable for deriving wf total wfs to previous layer


      au.fill_real_array(ref runoff_wf, 0.0, max_layer);
      xx = 0.0;
      cum_depth = 0.0;
      wf_tot = 0.0;
      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      //! check if hydro_effective_depth applies for eroded profile.
      profile_depth = au.sum_real_array(_dlayer, num_layers);
      hydrol_effective_depth_local = Math.Min(hydrol_effective_depth, profile_depth);

      scale_fact = 1.0/(1.0 - Math.Exp(-4.16));
      hydrol_effective_layer = au.find_layer_no(hydrol_effective_depth_local, _dlayer, num_layers);

      for (layer=au.si; layer<=au.ci(hydrol_effective_layer); layer++)
         {
         cum_depth = cum_depth + _dlayer[layer];
         cum_depth = mu.u_bound(cum_depth, hydrol_effective_depth_local);
         
         //! assume water content to c%hydrol_effective_depth affects runoff
         //! sum of wf should = 1 - may need to be bounded? <dms 7-7-95>
         wx = scale_fact * (1.0 - Math.Exp(-4.16 * mu.divide(cum_depth, hydrol_effective_depth_local, 0.0)));
         runoff_wf[layer] = wx - xx;
         xx = wx;

         wf_tot = wf_tot + runoff_wf[layer];
         }
      
      ApsimUtil.bound_check_real_var(wf_tot, 0.9999, 1.0001, "wf_tot");

      }



#endregion







#region Tillage

   private void soilwat2_tillage_addrain(double Rain, double Runon, double TotalInterception)
      {

     //The reduction in the runoff as a result of doing a tillage (tillage_cn_red) ceases after a set amount of rainfall (tillage_cn_rain).
     //This function works out the accumulated rainfall since last tillage event, and turns off the reduction if it is over the amount of rain specified.
     //This  soilwat2_tillage_addrain() is only called in soilwat2_runoff() 

      //sv- The Runoff is altered after a tillage event occurs.
      //sv- This code calculates how much it should be altered based on the accumulated rainfall since the last tillage event. 
      //sv- The zeroing of the tillage_rain_sum occurs in the tillage event.

      //*+  Mission Statement
      //*      Accumulate rainfall for tillage cn reduction 

      //rain         -> ! (INPUT) today's rainfall (mm)
      //runon        -> ! (INPUT) today's run on (mm)
      //interception -> ! (INPUT) todays interception loss (mm)

      string message;      //! message string

      tillage_rain_sum = tillage_rain_sum + Rain + Runon - TotalInterception;

      if ((tillage_cn_rain > 0.0) && (tillage_rain_sum > tillage_cn_rain))
         {
         //! This tillage has lost all effect on cn. CN reduction
         //!  due to tillage is off until the next tillage operation.
         tillage_cn_rain = 0.0;
         tillage_cn_red = 0.0;

         message = "Tillage CN reduction finished";
         Console.WriteLine(message);
         
         }

      }


#endregion







#region Infiltration

   private void soilwat2_infiltration(ref double Infiltration)
      {

      //TODO: I think this should be a ref for infiltration parameter.

      //infiltration -> ! (OUTPUT) infiltration into top layer (mm)
      
      //*+  Purpose
      //*     infiltration into top layer after runoff.

      //*+  Mission Statement
      //*      Calculate infiltration into top layer

      double   infiltration_1;      //! amount of infiltration from rain, irrigation - runoff
      double   infiltration_2;      //! amount of infiltration from ponding

       //! DSG 041200
       //! with the addition of the ponding feature, infiltration is now
       //! considered as consisting of two components - that from the (rain +
       //! irrigation) and that from ponding.

      infiltration_1 = rain + runon - runoff_pot - interception - residueinterception;

      //TODO: in soilwat2_zero_variables() we set irrigation_layer to 0. So this next line will never work.
      if (irrigation_layer == -1)      //sv- if the user did not enter an irrigation_layer
         {
         infiltration_1 = infiltration_1 + irrigation;
         }

      infiltration_2 = pond;
      Infiltration = infiltration_1 + infiltration_2;

      pond = 0.0;

      }


#endregion




#region Evaporation


   private void soilwat2_pot_evapotranspiration(ref double Eo)
      {
      //*+  Purpose
      //*       calculate potential evapotranspiration (eo) or get from another module

      //*+  Notes
      //*       Eventually eo will be in a separate module entirely, and
      //*       will appear to soilwat when get_other_varaibles() runs.
      //*       But, for now we use either priestly-taylor, or whatever
      //*       the user specified.
      
      if (_eo_source == "not_read")
         {
         soilwat2_priestly_taylor(ref Eo);    //! eo from priestly taylor
         }
      else
	      {
         Eo = eo_system;                     //! eo is provided by system
	      }
      }



   private void soilwat2_pot_evapotranspiration_effective(ref double eos)
      {
      //*+  Notes
      //*       Eventually eo will be in a separate module entirely, and
      //*       will appear to soilwat when get_other_varaibles() runs.
      //*       But, for now we use either priestly-taylor, or whatever
      //*       the user specified.

      //! dsg 270502  check to see if there is any ponding.  If there is, evaporate any potential (g%eos) straight out of it and transfer
      //!             any remaining potential to the soil layer 1, as per usual.  Introduce new term g%pond_evap
      //!             which is the daily evaporation from the pond.

      if (pond > 0.0)
         {
         if (pond >= eos)
            {
            pond = pond - eos;    //sv- the depth of water in the pond decreases by the amount of soil evaporation.
            pond_evap = eos;
            eos = 0.0;
            }
         else
	         {
            eos = eos - pond;
            pond_evap = pond;
            pond = 0.0;
	         }
         }     

      }

 

   private void soilwat2_priestly_taylor(ref double Eo)
      {
      double   albedo;           //! albedo taking into account plant material
      double   cover_green_sum;  //! sum of crop green covers (0-1)
      double   eeq;              //! equilibrium evaporation rate (mm)
      double   wt_ave_temp;      //! weighted mean temperature for the day (oC)

//*  ******* calculate potential evaporation from soil surface (eos) ******

//                ! find equilibrium evap rate as a
//                ! function of radiation, albedo, and temp.

      cover_green_sum = ApsimUtil.sum_cover_array(cover_green, num_crops);
      albedo = max_albedo - (max_albedo - salb) * (1.0 - cover_green_sum);

      // ! wt_ave_temp is mean temp, weighted towards max.
      wt_ave_temp = (0.60 * maxt) + (0.40 * mint);
      
      eeq = radn * 23.8846 *(0.000204 - 0.000183 * albedo) * (wt_ave_temp + 29.0);

      //! find potential evapotranspiration (eo) from equilibrium evap rate
      Eo = eeq * soilwat2_eeq_fac();
      }



   private double soilwat2_eeq_fac()      
      {
      //*+  Mission Statement
      //*     Calculate the Equilibrium Evaporation Rate

      if (maxt > max_crit_temp)
         {
         //! at very high max temps eo/eeq increases
         //! beyond its normal value of 1.1
         return ((maxt - max_crit_temp) * 0.05 + 1.1);
         }
      else
	      {
         if (maxt < min_crit_temp)
            {
            //! at very low max temperatures eo/eeq
            //! decreases below its normal value of 1.1
            //! note that there is a discontinuity at tmax = 5
            //! it would be better at tmax = 6.1, or change the
            //! .18 to .188 or change the 20 to 21.1
            return (0.01 * Math.Exp(0.18 * (maxt + 20.0)));
            }
	      }  
      
      return 1.1;  //sv- normal value of eeq fac (eo/eeq)
      }


   private void soilwat2_evaporation(ref double[] Esoil, ref double Eos)
      {
      //eos   -> ! (output) potential soil evap after modification for crop cover & residue_wt
      //esoil -> ! (output) actual soil evaporation (mm)

      double   asw1;    //! available soil water in top layer for actual soil evaporation (mm)
      
      //1. get potential soil water evaporation
      soilwat2_pot_soil_evaporation(ref Eos);

      //2. get available soil water for evaporation
      //   ! NB. ritchie + b&s evaporate from layer 1, but rickert
      //   !     can evaporate from L1 + L2.
      asw1 = _sw_dep[au.si] - _air_dry_dep[au.si];
      asw1 = mu.bound(asw1, 0.0, eo);

      //3. get actual soil water evaporation
      soilwat2_soil_evaporation(ref Esoil, Eos, asw1);
      }


   private void soilwat2_pot_soil_evaporation(ref double Eos)
      {
      //eos -> ! (output) potential soil evap after modification for crop cover & residue_w

      double cover_tot_sum;
      double eos_canopy_fract;      //! fraction of potential soil evaporation limited by crop canopy (mm)
      double eos_residue_fract;     //! fraction of potential soil evaporation limited by crop residue (mm)

      //! 1. get potential soil water evaporation

      //!---------------------------------------+
      //! reduce Eo to that under plant CANOPY                    <DMS June 95>
      //!---------------------------------------+

      //!  Based on Adams, Arkin & Ritchie (1976) Soil Sci. Soc. Am. J. 40:436-
      //!  Reduction in potential soil evaporation under a canopy is determined
      //!  the "% shade" (ie cover) of the crop canopy - this should include th
      //!  green & dead canopy ie. the total canopy cover (but NOT near/on-grou
      //!  residues).  From fig. 5 & eqn 2.                       <dms June 95>
      //!  Default value for c%canopy_eos_coef = 1.7
      //!              ...minimum reduction (at cover =0.0) is 1.0
      //!              ...maximum reduction (at cover =1.0) is 0.183.

      cover_tot_sum = ApsimUtil.sum_cover_array(cover_tot, num_crops);
      eos_canopy_fract = Math.Exp(-1* canopy_eos_coef * cover_tot_sum);

      //   !-----------------------------------------------+
      //   ! reduce Eo under canopy to that under mulch            <DMS June 95>
      //   !-----------------------------------------------+

      //   !1a. adjust potential soil evaporation to account for
      //   !    the effects of surface residue (Adams et al, 1975)
      //   !    as used in Perfect
      //   ! BUT taking into account that residue can be a mix of
      //   ! residues from various crop types <dms june 95>

      if (residue_cover >= 1.0)
         {
         //! We test for 100% to avoid log function failure.
         //! The algorithm applied here approaches 0 as cover approaches
         //! 100% and so we use zero in this case.
         eos_residue_fract = 0.0;
         }
      else
	      {
         //! Calculate coefficient of residue_wt effect on reducing first
         //! stage soil evaporation rate

         //!  estimate 1st stage soil evap reduction power of
         //!    mixed residues from the area of mixed residues.
         //!    [DM. Silburn unpublished data, June 95 ]
         //!    <temporary value - will reproduce Adams et al 75 effect>
         //!     c%A_to_evap_fact = 0.00022 / 0.0005 = 0.44
         eos_residue_fract = Math.Pow((1.0 - residue_cover), A_to_evap_fact);
	      }

      //! Reduce potential soil evap under canopy to that under residue (mulch)
      Eos = eo * eos_canopy_fract * eos_residue_fract;
      }




   private void soilwat2_soil_evaporation(ref double[] Es, double Eos, double Eos_max)
      {
      //es        -> ! (input) upper limit of soil evaporation (mm/day)
      //eos       -> ! (input) potential rate of evaporation (mm/day)
      //eos_max   -> ! (input) upper limit of soil evaporation (mm/day)

      //*+  Purpose
      //*     Wrapper for various evaporation models. Returns actual
      //*     evaporation from soil surface (es).

      //*+  Mission Statement
      //*     Soil Evaporation from Soil Surface

      //sv- Es is an array because some Evap methods do evaporation from every layer in the soil. 
      //    Most only do the surface but I think one of them does every layer so you had to make es layered to cope with this one method.
      //    That is why they created Esoil array.
      //    You will note however with Ritchie evaporation we only pass the top layer to Es[au.si] to soilwat2_ritchie_evaporation()
      //    so the ritchie method only alters the evaporation in the surface layer of this array.

      au.fill_real_array(ref Es, 0.0, max_layer);

      if (evap_method == ritchie_method)
         {
         soilwat2_ritchie_evaporation(ref Es[au.si], Eos, Eos_max);
         }
      else
	      {
         throw new Exception("Undefined evaporation method");
	      }

      }


   private void soilwat2_ritchie_evaporation(ref double Es, double Eos, double Eos_max)
      {
      //es        -> ! (output) actual evaporation (mm)
      //eos       -> ! (input) potential rate of evaporation (mm/day)
      //eos_max   -> ! (input) upper limit of soil evaporation (mm/day)

      //*+  Purpose
      //*          ****** calculate actual evaporation from soil surface (es) ******
      //*          most es takes place in two stages: the constant rate stage
      //*          and the falling rate stage (philip, 1957).  in the constant
      //*          rate stage (stage 1), the soil is sufficiently wet for water
      //*          be transported to the surface at a rate at least equal to the
      //*          evaporation potential (eos).
      //*          in the falling rate stage (stage 2), the surface soil water
      //*          content has decreased below a threshold value, so that es
      //*          depends on the flux of water through the upper layer of soil
      //*          to the evaporating site near the surface.

      //*+  Notes
      //*       This changes globals - sumes1/2 and t.


      double   esoil1;     //! actual soil evap in stage 1
      double   esoil2;     //! actual soil evap in stage 2
      double   sumes1_max; //! upper limit of sumes1
      double   w_inf;      //! infiltration into top layer (mm)



      if (ApsimUtil.date_within(winterdate, summerdate, today))
         {
         _cona = wintercona;
         _u = winteru;
         }
      else
	      {
         _cona = summercona;
         _u = summeru;
	      }

      sumes1_max = _u;
      w_inf = infiltration;





      //! if infiltration, reset sumes1
      //! reset sumes2 if infil exceeds sumes1      
      if (w_inf > 0.0)
         {
         sumes2 = Math.Max(0.0, (sumes2 - Math.Max(0.0, w_inf - sumes1)));
         sumes1 = Math.Max(0.0, sumes1 - w_inf);

         //! update t (incase sumes2 changed)
         t = Math.Pow((mu.divide(sumes2, _cona, 0.0)), 2);     
         }
      else
	      {
         //! no infiltration, no re-set.
	      }






      //! are we in stage1 ?
      if (sumes1 < sumes1_max)
         {
         //! we are in stage1
         //! set esoil1 = potential, or limited by u.
         esoil1 = Math.Min(Eos, sumes1_max - sumes1);

         if ((Eos > esoil1) && (esoil1 < Eos_max))
            {
            //*           !  eos not satisfied by 1st stage drying,
            //*           !  & there is evaporative sw excess to air_dry, allowing for esoil1.
            //*           !  need to calc. some stage 2 drying(esoil2).

            //*  if g%sumes2.gt.0.0 then esoil2 =f(sqrt(time),p%cona,g%sumes2,g%eos-esoil1).
            //*  if g%sumes2 is zero, then use ritchie's empirical transition constant (0.6).            

            if (sumes2 > 0.0)
               {
               t = t + 1.0;
               esoil2 = Math.Min((Eos - esoil1), (_cona * Math.Pow(t, 0.5) - sumes2));
               }
            else
	            {
               esoil2 = 0.6 * (Eos - esoil1);
	            }
            }
         else
            {
            //! no deficit (or esoil1.eq.eos_max,) no esoil2 on this day            
            esoil2 = 0.0;
            }

         //! check any esoil2 with lower limit of evaporative sw.
         esoil2 = Math.Min(esoil2, Eos_max - esoil1);
         

         //!  update 1st and 2nd stage soil evaporation.     
         sumes1 = sumes1 + esoil1;
         sumes2 = sumes2 + esoil2;
         t = Math.Pow((mu.divide(sumes2, _cona, 0.0)), 2);
         }
      else
	      {
         //! no 1st stage drying. calc. 2nd stage         
         esoil1 = 0.0;

         t = t + 1.0;
         esoil2 = Math.Min(Eos, (_cona * Math.Pow(t, 0.5) - sumes2)); 

         //! check with lower limit of evaporative sw.
         esoil2 = Math.Min(esoil2, Eos_max);

         //!   update 2nd stage soil evaporation.
         sumes2 = sumes2 + esoil2;
	      }

      Es = esoil1 + esoil2;

      //! make sure we are within bounds      
      Es = mu.bound(Es, 0.0, Eos);
      Es = mu.bound(Es, 0.0, Eos_max);
      }

#endregion





#region Drainage (Saturated Flow)

   
   private void soilwat2_drainage(ref double[] Flux, ref double ExtraRunoff)
      {


       //*     ===========================================================
               //subroutine soilwat2_drainage (flux,extra_runoff)
       //*     ===========================================================


       //*+  Function Arguments
             //flux              //! (output) water moving out of
             //extra_runoff      //! (output) water to add to runoff layer (mm)

       //*+  Purpose       
       //calculate flux - drainage from each layer. 
       //sv- it just calculates. It does not change anything.

       //*+  Constant Values
             //character  my_name*(*);           //! name of subroutine
             //parameter (my_name = 'soilwat2_drainage');

       //*+  Local Variables

             double       add;                   //! water to add to layer
             double       backup;                //! water to backup
             double       excess;                //! amount above saturation(overflow)(mm)
             double       excess_down;           //! amount above saturation(overflow) that moves on down (mm)
             double[]     new_sw_dep;            //! record of results of sw calculations ensure mass balance. (mm)
             int          i;                     //! counter  //sv- this was "l" (as in the leter "L") but it looks too much like the number 1, so I changed it to "i". 
             int          layer;                 //! counter for layer no.
             int          num_layers;            //! number of layers
             double       w_drain;               //! water draining by gravity (mm)
             double       w_in;                  //! water coming into layer (mm)
             double       w_out;                 //! water going out of layer (mm)
             double       w_tot;                 //! total water in layer at start (mm)

       //*- Implementation Section ----------------------------------

            //TODO: you will need to resize the flux array if using in output

             //call push_routine (my_name);

             new_sw_dep = new double[max_layer]; 

                       //! flux into layer 1 = infiltration (mm).

             w_in = 0.0;
             ExtraRunoff = 0.0;
             au.ZeroArray(ref Flux);  

                       //! calculate drainage and water
                       //! redistribution.

             num_layers = au.count_of_real_vals(_dlayer, max_layer);

             for (layer=au.si; layer<=au.ci(num_layers); layer++) 
               {
                    //! get total water concentration in layer

                w_tot = _sw_dep[layer] + w_in;

                    //! get excess water above saturation & then water left
                    //! to drain between sat and dul.  Only this water is
                    //! subject to swcon. The excess is not - treated as a
                    //! bucket model. (mm)

                if (w_tot > _sat_dep[layer])
                   {
                   excess = w_tot - _sat_dep[layer];
                   w_tot = _sat_dep[layer];
                   }
                else
                   {
                   excess = 0.0;
                   }


                if (w_tot > _dul_dep[layer])
                   {
                   w_drain = (w_tot - _dul_dep[layer]) * swcon[layer];
                   //!w_drain = min(w_drain,p%Ks(layer))
                   }
                else
                   {
                   w_drain = 0.0;
                   }

                    //! get water draining out of layer (mm)

                if (excess > 0.0)
                   {

                   //! Calculate amount of water to backup and push down
                   //! Firstly top up this layer (to saturation)
                   add = Math.Min(excess, w_drain);
                   excess = excess - add;
                   new_sw_dep[layer] = _sat_dep[layer] - w_drain + add;

                   //! partition between flow back up and flow down
                   excess_down = Math.Min(ks[layer]-w_drain, excess);
                   backup = excess - excess_down;

                   w_out = excess_down + w_drain;
                   Flux[layer] = w_out;

                   //! now back up to saturation for this layer up out of the
                   //! backup water keeping account for reduction of actual
                   //! flow rates (flux) for N movement.

                   for(i=layer-1;i>=au.si;i--)     //TODO: THis could be a problem when layer==0. May have to change to i>=0.
                      {
                      Flux[i] = Flux[i] - backup;
                      add = Math.Min(_sat_dep[i] - new_sw_dep[i],backup);
                      new_sw_dep[i] = new_sw_dep[i] + add;
                      backup = backup - add;
                      }
                   ExtraRunoff = ExtraRunoff + backup;
                   }
                 else
                   {
                   //! there is no excess so do nothing
                   w_out = w_drain;
                   Flux[layer] = w_out;
                   new_sw_dep[layer] = _sw_dep[layer] + w_in - w_out;

                   }

                //! drainage out of this layer goes into next layer down
                w_in = w_out;
              }

             //call pop_routine (my_name);

      }


   private void soilwat2_drainage_old(ref double[] Flux, ref double ExtraRunoff)
      {
      //flux         -> (output) water moving out of
      //extra_runoff -> (output) water to add to runoff layer (mm)

      //*+  Purpose
      //*       calculate flux - drainage from each layer

      //*+  Mission Statement
      //*     Calculate Drainage from each layer      

      double      add;           //! water to add to layer
      double      backup;        //! water to backup
      double      excess;        //! amount above saturation(overflow)(mm)
      double[]    new_sw_dep;    //! record of results of sw calculations ensure mass balance. (mm)
      int         i;             //! counter //sv- this was "l" (as in the leter "L") but it looks too much like the number 1, so I changed it to "i". 
      int         layer;         //! counter for layer no.
      int         num_layers;    //! number of layers
      double      w_drain;       //! water draining by gravity (mm)
      double      w_in;          //! water coming into layer (mm)
      double      w_out;         //! water going out of layer (mm)
      double      w_tot;         //! total water in layer at start (mm)

      //TODO: you will need to resize the flux array if using in output

      new_sw_dep = new double[max_layer];

      //! flux into layer 1 = infiltration (mm).
      w_in = 0.0;
      ExtraRunoff = 0.0;


      //! calculate drainage and water
      //! redistribution.
      au.fill_real_array(ref Flux, 0.0, max_layer);
      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         //! get total water concentration in layer
         w_tot = _sw_dep[layer] + w_in;

         //! get excess water above saturation & then water left
         //! to drain between sat and dul.  Only this water is
         //! subject to swcon. The excess is not - treated as a
         //! bucket model. (mm)

         if (w_tot > _sat_dep[layer])
            {
            excess = w_tot -_sat_dep[layer];
            w_tot = _sat_dep[layer];
            }
         else
	         {
            excess = 0.0;
	         }
      
         if (w_tot > _dul_dep[layer])
            {
            w_drain = (w_tot - _dul_dep[layer]) * swcon[layer];
            }
         else
	         {
            w_drain = 0.0;
	         }

         //! get water draining out of layer (mm)
         if (excess > 0.0)
            {
            if (mwcon[layer] >= 1.0)
               {
               //! all this excess goes on down so do nothing
               w_out = excess + w_drain;
               new_sw_dep[layer] = _sw_dep[layer] + w_in - w_out;
               Flux[layer] = w_out;
               }
            else
	            {
               //! Calculate amount of water to backup and push down
               //! Firstly top up this layer (to saturation)
               add = Math.Min(excess, w_drain);
               excess = excess - add;
               new_sw_dep[layer] = _sat_dep[layer] - w_drain + add;

               //! partition between flow back up and flow down
               backup = (1.0 - mwcon[layer]) * excess;
               excess = mwcon[layer] * excess;

               w_out = excess + w_drain;
               Flux[layer] = w_out;

               //! now back up to saturation for this layer up out of the
               //! backup water keeping account for reduction of actual
               //! flow rates (flux) for N movement.         
               for (i=layer-1; i>=au.si; layer--)   //TODO: THis could be a problem when layer==0. May have to change to i>=0.
                  {
                  Flux[i] = Flux[i] - backup;
                  add = Math.Min((_sat_dep[i] - new_sw_dep[i]), backup);
                  new_sw_dep[i] = new_sw_dep[i] + add;
                  backup = backup - add;
                  }

               ExtraRunoff = ExtraRunoff + backup;
	            }
            }
         else
	         {
            //! there is no excess so do nothing
            w_out = w_drain;
            Flux[layer] = w_out;
            new_sw_dep[layer] = _sw_dep[layer] + w_in - w_out;
	         }

         //! drainage out of this layer goes into next layer down
         w_in = w_out;

         }

      }



#endregion





#region Unsaturated Flow

   
   private void soilwat2_unsat_flow(ref double[] Flow)
      {

      //*+  Purpose
      //*       calculate unsaturated flow below drained upper limit

      //*+  Mission Statement
      //*     Calculate Unsaturated Solute and Water Flow


      double   esw_dep1;            //! extractable soil water in current layer (mm)
      double   esw_dep2;            //! extractable soil water in next layer below (mm)
      double   dbar;                //! average diffusivity used to calc unsaturated flow between layers
      int      layer;               //! layer counter for current layer
      int      second_last_layer;   //! last layer for flow
      int      num_layers;          //! number of layers
      int      next_layer;          //! layer counter for next lower layer
      double   flow_max;            //! maximum flow to make gradient between layers equal zero
      double   theta1;              //! sw content above ll15 for current layer (cm/cm)
      double   theta2;              //! sw content above ll15 for next lower layer (cm/cm)
      double   w_out;               //! water moving up out of this layer (mm)
                                          //! +ve = up to next layer
                                          //! -ve = down into this layer
      double   this_layer_cap;      //! capacity of this layer to accept water from layer below (mm)
      double   next_layer_cap;      //! capacity of nxt layer to accept water from layer above (mm)
      double   sw1;                 //! sw for current layer (mm/mm)
      double   sw2;                 //! sw for next lower layer (mm/mm)
      double   gradient;            //! driving force for flow
      double   sum_inverse_dlayer;
      double   dlayer1;             //! depth of current layer (mm)
      double   dlayer2;             //! depth of next lower layer (mm)
      double   ave_dlayer;          //! average depth of current and next layers (mm)
      double   sw_dep1;             //! soil water depth in current layer (mm)
      double   sw_dep2;             //! soil water depth in next layer (mm)
      double   ll15_dep1;           //! 15 bar lower limit sw depth in current layer (mm)
      double   ll15_dep2;           //! 15 bar lower limit sw depth in next layer (mm)
      double   sat_dep1;            //! saturated sw depth in current layer (mm)
      double   sat_dep2;            //! saturated sw depth in next layer (mm)
      double   dul_dep1;            //! drained upper limit in current layer (mm)
      double   dul_dep2;            //! drained upper limit in next layer (mm)
      double   swg;                 //! sw differential due to gravitational pressure head (mm)


      

      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      //! *** calculate unsaturated flow below drained upper limit (flow)***   
      au.fill_real_array(ref Flow, 0.0, max_layer);

      //TODO: may need to resize the flow array if using it for output.

      //! second_last_layer is bottom layer but 1.
      second_last_layer = num_layers - 1;


      w_out = 0.0;
      for (layer=au.si; layer<=au.ci(second_last_layer); layer++)
         {
         next_layer = layer + 1;

         dlayer1 = _dlayer[layer];
         dlayer2 = _dlayer[next_layer];
         ave_dlayer = (dlayer1 + dlayer2) * 0.5;
      
         sw_dep1 = _sw_dep[layer];
         sw_dep2 = _sw_dep[next_layer];

         ll15_dep1 = _ll15_dep[layer];
         ll15_dep2 = _ll15_dep[next_layer];

         sat_dep1 = _sat_dep[layer];
         sat_dep2 = _sat_dep[next_layer];

         dul_dep1 = _dul_dep[layer];
         dul_dep2 = _dul_dep[next_layer];

         esw_dep1 = mu.l_bound((sw_dep1 - w_out) - ll15_dep1, 0.0);
         esw_dep2 = mu.l_bound(sw_dep2 -ll15_dep2, 0.0);

         //! theta1 is excess of water content above lower limit,
         //! theta2 is the same but for next layer down.
         theta1 = mu.divide(esw_dep1, dlayer1, 0.0);
         theta2 = mu.divide(esw_dep2, dlayer2, 0.0);

         //! find diffusivity, a function of mean thet.
         dbar = diffus_const * Math.Exp(diffus_slope * (theta1 + theta2) * 0.5);
        
         //! testing found that a limit of 10000 (as used in ceres-maize)
         //! for dbar limits instability for flow direction for consecutive
         //! days in some situations.

         dbar = mu.bound(dbar, 0.0, 10000.0);

         sw1 = mu.divide((sw_dep1 - w_out), dlayer1, 0.0);
         sw1 = mu.l_bound(sw1, 0.0);

         sw2 = mu.divide(sw_dep2, dlayer2, 0.0);
         sw2 = mu.l_bound(sw2, 0.0);

         //    ! gradient is defined in terms of absolute sw content
         //cjh          subtract gravity gradient to prevent gradient being +ve when flow_max is -ve, resulting in sw > sat.
         gradient = mu.divide((sw2 - sw1), ave_dlayer, 0.0) - gravity_gradient;
      

         //!  flow (positive up) = diffusivity * gradient in water content
         Flow[layer] = dbar * gradient;

         //! flow will cease when the gradient, adjusted for gravitational
         //! effect, becomes zero.
         swg = gravity_gradient * ave_dlayer;

         //! calculate maximum flow
         sum_inverse_dlayer = mu.divide(1.0, dlayer1, 0.0) + mu.divide(1.0, dlayer2, 0.0);
         flow_max = mu.divide((sw2 - sw1 - swg), sum_inverse_dlayer, 0.0);


         //c dsg 260202
         //c dsg    this code will stop a saturated layer difusing water into a partially saturated
         //c        layer above for Water_table height calculations
         if ((_sw_dep[layer] >= _dul_dep[layer]) && (_sw_dep[next_layer] >= _dul_dep[next_layer]))
            {
            Flow[layer] = 0.0;
            }

         //c dsg 260202
         //c dsg    this code will stop unsaturated flow downwards through an impermeable layer, but will allow flow up
         if ((mwcon[layer] == 0) && (Flow[layer] < 0.0))
            {
            Flow[layer] = 0.0;
            }

    
         if (Flow[layer] < 0.0)
            {
            //! flow is down to layer below
            //! check capacity of layer below for holding water from this layer
            //! and the ability of this layer to supply the water

            //!    next_layer_cap = l_bound (sat_dep2 - sw_dep2, 0.0)
            //!    dsg 150302   limit unsaturated downflow to a max of dul in next layer
            
            next_layer_cap = mu.l_bound(dul_dep2 - sw_dep2, 0.0);
            flow_max = mu.l_bound(flow_max, -1 * next_layer_cap);
            flow_max = mu.l_bound(flow_max, -1 * esw_dep1);
            Flow[layer] = mu.l_bound(Flow[layer], flow_max);
            } 
         else
            {
            if (Flow[layer] > 0.0)
               {
               //! flow is up from layer below
               //! check capacity of this layer for holding water from layer below
               //! and the ability of the layer below to supply the water

               //!            this_layer_cap = l_bound (sat_dep1 - (sw_dep1 - w_out), 0.0)
               //!    dsg 150302   limit unsaturated upflow to a max of dul in this layer
               this_layer_cap = mu.l_bound(dul_dep1 - (sw_dep1 - w_out), 0.0);
               flow_max =  mu.u_bound(flow_max, this_layer_cap);
               flow_max =  mu.u_bound(flow_max, esw_dep2);
               Flow[layer] = mu.u_bound(Flow[layer], flow_max);          
               }
            else
	            {
               // no flow
	            }
            }


         //! For conservation of water, store amount of water moving
         //! between adjacent layers to use for next pair of layers in profile
         //! when calculating theta1 and sw1.
         w_out = Flow[layer];
         }

   }

#endregion




#region Solute



   //sv- solute movement during Drainage (Saturated Flow)

   private void soilwat2_solute_flux(ref double[] solute_out, double[] solute_kg)
      { 

      //solute_out   ->   ! (output) solute leaching out of each layer (kg/ha) 
      //solute_kg    ->   ! (input) solute in each layer (kg/ha)

      //*+  Purpose
      //*         calculate the downward movement of solute with percolating water

      //*+  Mission Statement
      //*     Calculate the Solute Movement with Saturated Water Flux

      double         in_solute;        //! solute leaching into layer from above (kg/ha)
      int            layer;            //! layer counter
      int            num_layers;       //! number of layers in profile
      double         out_max;          //! max. solute allowed to leach out of layer (kg/ha)
      double         out_solute;       //! solute leaching out of layer (kg/ha)
      double         out_w;            //! water draining out of layer (mm)
      double         solute_kg_layer;  //! quantity of solute in layer (kg/ha)
      double         water;            //! quantity of water in layer (mm)
      double         solute_flux_eff_local;

      au.fill_real_array(ref solute_out, 0.0, max_layer);
      num_layers = au.count_of_real_vals(_dlayer, max_layer);
      in_solute = 0.0;

      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         //! get water draining out of layer and n content of layer includes that leaching down         
         out_w = flux[layer];
         solute_kg_layer = solute_kg[layer] + in_solute;

         //! n leaching out of layer is proportional to the water draining out.
         if (num_solute_flux == 1)
            {
            //single value was specified in ini file (still gets put in an array with just one element)
            solute_flux_eff_local = solute_flux_eff[au.si];
            }
         else
            {
            //array was specified in ini file
            solute_flux_eff_local = solute_flux_eff[layer];
            }
         water = _sw_dep[layer] + out_w;
         out_solute = solute_kg_layer * mu.divide(out_w, water, 0.0) * solute_flux_eff_local;

         //! don't allow the n to be reduced below a minimum level
         out_max = mu.l_bound(solute_kg_layer, 0.0);
         out_solute = mu.bound(out_solute, 0.0, out_max);

         //! keep the leaching and set the input for the next layer
         solute_out[layer] = out_solute;
         in_solute = out_solute;
         }


      }

   //sv- solute movement during Unsaturated Flow

   private void soilwat2_solute_flow(ref double[] solute_up, double[] solute_kg)
      {  

      //solute_up -> ! (output) solute moving upwards into each layer (kg/ha)
      //solute_kg -> ! (input/output) solute in each layer (kg/ha)

      //*+  Purpose
      //*       movement of solute in response to differences in
      //*       water content of adjacent soil layers when the soil water
      //*       content is < the drained upper limit (unsaturated flow)

      //*+  Notes
      //*       170895 nih The variable names and comments need to be cleaned
      //*                  up.  When this is done some references to no3 or
      //*                  nitrogen need to be changed to 'solute'

      //*+  Mission Statement
      //*     Calculate the Solute Movement with Unsaturated Water Flow

      double         bottomw;             //! water movement to/from next layer (kg/ha)
      double         in_solute;           //! solute moving into layer from above (kg/ha)
      int            layer;               //! layer counter
      double[]       solute_down;         //! solute moving downwards out of each layer (kg/ha)
      int            num_layers;          //! number of layers
      double         out_solute;          //! solute moving out of layer (kg/ha)
      double         out_w;               //! water draining out of layer (mm)
      double[]       remain;              //! n remaining in each layer between movement up (kg/ha)
      double         solute_kg_layer;     //! quantity of solute in layer (kg/ha)
      double         top_w;               //! water movement to/from above layer (kg/ha)
      double         water;               //! quantity of water in layer (mm)
      double         solute_flow_eff_local;

      //sv- initialise the local arrays declared above.
      solute_down = new double[max_layer];     
      remain = new double[max_layer];


      au.fill_real_array(ref solute_up, 0.0, max_layer); 

      //! flow  up from lower layer:  + up, - down
      //******************************************
      //******************************************


      //! + ve flow : upward movement. go from bottom to top layer   
      //**********************************************************

      num_layers = au.count_of_real_vals(_dlayer, max_layer);
      in_solute = 0.0;
      for (layer=au.ci(num_layers); layer>=(au.si+1); layer--)
         {
         //! keep the nflow upwards
         solute_up[layer] = in_solute;

         //! get water moving up and out of layer to the one above
         out_w = flow[layer-1];
         if(out_w <= 0.0)
            {
            out_solute  = 0.0;
            }
         else
	         {
            //! get water movement between this and next layer
            bottomw = flow[layer];

            //! get n content of layer includes that moving from other layer
            solute_kg_layer = solute_kg[layer] + in_solute;
            water = _sw_dep[layer] + out_w - bottomw;

            //! n moving out of layer is proportional to the water moving out.
            if (num_solute_flow == 1)
               {
               solute_flow_eff_local = solute_flow_eff[0];
               }
            else
               {
               solute_flow_eff_local = solute_flow_eff[layer];
               }
            out_solute = solute_kg_layer * mu.divide(out_w, water, 0.0) * solute_flow_eff_local;
   
            //! don't allow the n to be reduced below a minimum level
            out_solute = mu.bound(out_solute, 0.0, solute_kg_layer);         
	         }
         //! set the input for the next layer
         in_solute = out_solute;
         }




      solute_up[au.si] = in_solute;
      //! now get n remaining in each layer between movements
      //! this is needed to adjust the n in each layer before calculating
      //! downwards movement.  i think we shouldn't do this within a time
      //! step. i.e. there should be no movement within a time step. jngh
      remain[au.si] = solute_up[au.si];
      for (layer=(au.si+1); layer<=au.ci(num_layers); layer++)
         {
         remain[layer] = solute_up[layer] - solute_up[layer-1];
         }




      //! -ve flow - downward movement
      //******************************

      au.fill_real_array(ref solute_down, 0.0, max_layer);
      in_solute = 0.0;
      top_w = 0.0;

      for (layer=au.si; layer<=au.ci(num_layers);layer++)
         {
         //! get water moving out of layer
         out_w = -1 * flow[layer];
         if (out_w <= 0.0)
            {
            out_solute=0.0;
            }
         else
	         {
            //! get n content of layer includes that moving from other layer
            solute_kg_layer = solute_kg[layer] + in_solute + remain[layer];
            water = _sw_dep[layer] + out_w - top_w;

            //! n moving out of layer is proportional to the water moving out.
            if (num_solute_flow == 1)
               {
               solute_flow_eff_local = solute_flow_eff[0];
               }
            else
               {
               solute_flow_eff_local = solute_flow_eff[layer];
               }

            out_solute = solute_kg_layer * mu.divide(out_w, water, 0.0) * solute_flow_eff_local;

            //! don't allow the n to be reduced below a minimum level
            out_solute = mu.round_to_zero(out_solute);
            out_solute = mu.bound(out_solute, 0.0, solute_kg_layer);
	         }
         solute_down[layer] = out_solute;
         in_solute = out_solute;
         top_w = out_w;
         }

      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         solute_up[layer] = solute_up[layer] - solute_down[layer];
         }

      }



   private void soilwat2_irrig_solute()
      {
      //*+  Mission Statement
      //*      Add solutes with irrigation
   
      int      solnum;     //! solute number counter variable     
      int      layer;      //! soil layer

         //TODO: in soilwat2_zero_variables() we set irrigation_layer to 0. So this next line will never work.
      if (irrigation_layer == -1)   //sv- if user did not enter an irrigation_layer
         {
         //!addition at surface
         layer = au.si;
         }
      else
	      {
         layer = irrigation_layer;
	      }
      
      for (solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
         {
         solute[solnum,layer]  = solute[solnum,layer] + irrigation_solute[solnum];
         dlt_solute[solnum,layer] = dlt_solute[solnum,layer] + irrigation_solute[solnum];
         }


      }

/*

   private void soilwat2_rainfall_solute()
      {
      //*+  Mission Statement
      //*      Add solutes from rainfall

      int      solnum;        //! solute number counter variable
      double   mass_rain;     //! mass of rainfall on this day (kg/ha)
      double   mass_solute;   //! mass of solute in this rainfall (kg/ha)

      //! 1mm of rain = 10000 kg/ha, therefore total mass of rainfall = g%rain * 10000 kg/ha
      mass_rain = rain * 10000.0;

      for(solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
         {
         //!assume all rainfall goes into layer 1
         //! therefore mass_solute = mass_rain * g%solute_conc_rain (in ppm) / 10^6
         mass_solute = mu.divide(mass_rain * solute_conc_rain[solnum], 1000000.0, 0.0);
         solute[solnum,au.si]   = solute[solnum,au.si] + mass_solute;
         dlt_solute[solnum,au.si] = dlt_solute[solnum,au.si] + mass_solute;
         }

      }

*/



   private void soilwat2_move_solute_down()
      {

      //*+  Mission Statement
      //*      Calculate downward movement of solutes

      int         num_layers;          
      int         layer;               //! layer number counter variable
      int         solnum;              //! solute number counter variable
      double[]    leach;               //! amount of a solute leached from each soil layer (kg/ha)
      double[]    temp_solute;         //! temp array for solute content(kg/ha)
      double[]    temp_dlt_solute;     //! temp array of changes in solute concentration (kg/ha) //diluted 

      //sv- initialise the local arrays declared above.
      leach = new double[max_layer]; 
      temp_solute = new double[max_layer];
      temp_dlt_solute = new double[max_layer];

      //! Now for each mobile solute put the global solute info into a
      //! temp solute array, pass this solute information to the solute
      //! flux routine then insert moved solute back into the global
      //! record.

      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      for (solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
         {
         if (solute_mobility[solnum] != false)     //this boolean array is created in new solute event handler.
            {
            //create temporary variables and initialise them.
            for (layer=au.si; layer<=au.ci(num_layers);layer++)
               {
               temp_solute[layer] = solute[solnum,layer];
               leach[layer] = 0.0;
               temp_dlt_solute[layer] = dlt_solute[solnum,layer];
               }
            //calculate the movement using temporary variables
            soilwat2_solute_flux(ref leach, temp_solute);               //calc leaching
            au.move_down_real(leach, ref temp_solute, num_layers);      //use leaching to set new solute values
            au.move_down_real(leach, ref temp_dlt_solute, num_layers);  //use leaching to set new delta (change in) solute values
            //set global variables to the temporary variables
            for (layer=au.si; layer<=au.ci(num_layers); layer++)
               {
               solute[solnum,layer] = temp_solute[layer];
               solute_leach[solnum,layer] = leach[layer];
               dlt_solute[solnum, layer] = temp_dlt_solute[layer];
               }
            }
         else
	         {
            //! solute was not in the mobile list - do not move it
	         }
         }


      }


   private void soilwat2_move_solute_up()
      {

      //*+  Mission Statement
      //*      Calculate upward movement of solutes

      int         layer;               //! layer number counter variable
      double[]    leach;               //! amount of a solute leached from each soil layer (kg/ha)
      int         num_layers;          //! number of layers
      int         solnum;              //! solute number counter variable
      double[]    temp_solute;         //! temp array for solute content(kg/ha)
      double[]    temp_dlt_solute;     //! temp array of changes in solute concentration (kg/ha) //doluted solute

      leach = new double[max_layer];
      temp_solute = new double[max_layer];
      temp_dlt_solute = new double[max_layer];

      //! Now for each mobile solute put the global solute info into a
      //! temp solute array, pass this solute information to the solute
      //! flux routine then insert moved solute back into the global
      //! record.

      num_layers  = au.count_of_real_vals(_dlayer, max_layer);

      for (solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
         {
         if (solute_mobility[solnum] != false)
            {
            for (layer=au.si; layer<=au.ci(max_layer); layer++)
               {
               temp_solute[layer] = solute[solnum,layer];
               leach[layer] = 0.0;
               temp_dlt_solute[layer] = dlt_solute[solnum,layer];
               }
            soilwat2_solute_flow(ref leach, temp_solute);
            au.move_up_real(leach, ref temp_solute, num_layers);
            au.move_up_real(leach, ref temp_dlt_solute, num_layers);
            for(layer=au.si; layer<=au.ci(max_layer); layer++)
               {
               solute[solnum,layer] = temp_solute[layer];
               solute_up[solnum,layer] = leach[layer];
               dlt_solute[solnum,layer] = temp_dlt_solute[layer];
               }
            }
         else
	         {
            //! solute was not in the mobile list - do not move it
	         }  
         }
      }


#endregion






#region Water Table


   private double soilwat_water_table()
      {
      //*+  Purpose
      //*     Calculate the water table
      // water table is just the depth (in mm) below the ground surface of the first layer which is above saturation.

      int      layer;
      int      num_layers;
      int      sat_layer;
      double   margin;      //! dsg 110302  allowable looseness in definition of sat
      double   saturated_fraction;
      double   saturated_fraction_above;
      double   drainable;
      double   drainable_capacity;
      double   bottom_depth;
      double   saturated;
      bool     layer_is_fully_saturated;
      bool     layer_is_saturated;
      bool     layer_above_is_saturated;


      //sv- C# has a problem with these values being initialised inside of the final else clause of an if statement. You have to give them a default value.
      sat_layer = au.ltsi;
      saturated_fraction_above = 0.0;
      layer_is_saturated = false;
     

      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      
      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         margin = mu.error_margin(_sat_dep[layer]);

         //Find the first layer that is above saturation or really close to it. 
         //nb. sat_layer is a layer number not an index. Therefore it starts at 1 and not zero. So we need to convert it to a layer number from an index. "layer" variable is really an index not a layer number.
         if ((_sat_dep[layer] - _sw_dep[layer]) <= margin)
            {
            sat_layer = au.cl(layer);     
            break;
            }
         //Or if mwcon is set to be impermeable for this layer and above sw is above dul then consider this layer as saturated.
         else if ((mwcon[layer] < 1.0) && (_sw_dep[layer] > _dul_dep[layer]))
            {
            //!  dsg 150302     also check whether impermeable layer is above dul. If so then consider it to be saturated
            sat_layer = au.cl(layer);     
            break;
            }
         else
	         {
            sat_layer= 0;   //if there is no saturated layer set it to 0
	         }
         }

      //If you found a saturated layer in the profile, 
      if (sat_layer > 0) 
         {
         //! saturated fraction of saturated layer
         //calculate the saturation_fraction of current layer incase,
         //there is no layer above
         //or incase mwcon was set to impermeable and sw was above dul (so there are layers above but no saturated layers, the impermeable layer is just above dul which is the watertable) 
         drainable = _sw_dep[au.ci(sat_layer)] - _dul_dep[au.ci(sat_layer)];
         drainable_capacity = _sat_dep[au.ci(sat_layer)] - _dul_dep[au.ci(sat_layer)];
         saturated_fraction = mu.divide(drainable, drainable_capacity, 0.0);
         //if it is not the top layer that is saturated (ie. there is a layer above the saturated layer)
         //Then see if the layer above it is above dul and if so calculate the fraction so we can add this as extra millimeters to the water_table.
         if (sat_layer > 1)
            {
            //! saturated fraction of layer above saturated layer
            drainable = _sw_dep[au.ci(sat_layer-1)] - _dul_dep[au.ci(sat_layer-1)];
            drainable_capacity = _sat_dep[au.ci(sat_layer-1)] - _dul_dep[au.ci(sat_layer-1)];
            saturated_fraction_above = mu.divide(drainable, drainable_capacity, 0.0);
            }
         else
	         {
            //! top layer fully saturated - no layer above it
            saturated_fraction_above = 0.0;
	         }
         }
      else
	      {
         //! profile not saturated
         saturated_fraction = 0.0;
	      }

      //set some boolean flags based on the saturated fraction calculated above.
      if (saturated_fraction >= 0.999999)
         {
         layer_is_fully_saturated = true;
         layer_above_is_saturated = true;
         }
      else if (saturated_fraction > 0.0)
         {
         layer_is_fully_saturated = false;
         layer_is_saturated = true;
         }
      else
         {
         layer_is_fully_saturated = false;
         layer_is_saturated = false;
         }


      if (saturated_fraction_above > 0.0)
         {
         layer_above_is_saturated = true;
         }
      else
         {
         layer_above_is_saturated = false;
         }


      //Do the calculation of the water_table      
      if (layer_is_fully_saturated && layer_above_is_saturated)
         {
         //! dsg 150302  saturated layer = layer, layer above is over dul
         bottom_depth = au.sum_real_array(_dlayer, sat_layer-1);
         saturated = saturated_fraction_above * _dlayer[au.ci(sat_layer-1)];
         return (bottom_depth - saturated);
         }
      else if (layer_is_saturated)
         {
         //! dsg 150302  saturated layer = layer, layer above not over dul
         bottom_depth = au.sum_real_array(_dlayer, sat_layer);
         saturated = saturated_fraction * _dlayer[au.ci(sat_layer)];
         return (bottom_depth - saturated);
         } 
      else
         {
         //! profile is not saturated
         bottom_depth = au.sum_real_array(_dlayer, num_layers);
         return bottom_depth;
         }
  
      }


   private void SetWaterTable(double WaterTable)
      {

      int      layer;
      int      num_layers;
      double   top;
      double   bottom;
      double   fraction;
      double   drainable_porosity;

      num_layers = au.count_of_real_vals(_dlayer, max_layer);
      top = 0.0;
      bottom = 0.0;

      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         top = bottom;
         bottom = bottom + _dlayer[layer];
         if (WaterTable >= bottom)
            {
            //do nothing;
            }
         else if (WaterTable > top)
	         {
            //! top of water table is in this layer
            fraction = (bottom - WaterTable)/(bottom - top);
            drainable_porosity = _sat_dep[layer] - _dul_dep[layer];
            _sw_dep[layer] = _dul_dep[layer] + fraction * drainable_porosity;
	         }
         else
            {
            _sw_dep[layer] = _sat_dep[layer];
            }
         }

      _water_table = WaterTable;

      }



#endregion






#region Lateral Flow


   private void Lateral_process()
      {

      int layer;
      double d;  //depth of water table in a layer (mm)
      double max_flow;
     

      int num_layers = au.count_of_real_vals(_dlayer, max_layer);


      //TODO: This initialisation section should be in soilwat2_set_my_variable() not really here. But this is how SoilWat does it, so leave it here for now.
      //inflow_lat is optional daily input so if it does not exist just create it and zero it.
      if (inflow_lat == null)
         {
         inflow_lat = new double[_dlayer.Length];
         au.ZeroArray(ref inflow_lat);
         }

      //The user does not have have specify a value for ALL the layers in the soil. Just can specify the layers from the top down to whatever layer they like.
      //Therefore we need to resize the array if they did not specify a value for every layer and then put in zero values for the layers they did not specify.
      if (inflow_lat.Length < _dlayer.Length)
         {
         int startZeroingFromHere = au.ci(inflow_lat.Length) + 1;  //seems stupid but do this incase one day change back to 1 based array again.
         Array.Resize(ref inflow_lat, _dlayer.Length);
         //This following is probably not necessary as the resize probably zeros it, but do it just incase.
         for (int i = startZeroingFromHere; i<=au.ci(_dlayer.Length); i++)
            {
            inflow_lat[i] = 0.0;
            } 
         }



      for (layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         //! dsg 150302   add the inflowing lateral water
         _sw_dep[layer] = _sw_dep[layer] + inflow_lat[layer];
         d = _dlayer[layer] * mu.divide((_sw_dep[layer]-_dul_dep[layer]),(_sat_dep[layer]-_dul_dep[layer]),0.0); 
         d = Math.Max(0.0, d);  //! water table depth in layer must be +ve
         
         double i, j;
         i = klat[layer]* d * (discharge_width / mu.mm2m) * slope;
         j = (catchment_area * mu.sm2smm) * (Math.Pow((1.0 + Math.Pow(slope, 2)), 0.5));
         outflow_lat[layer] = mu.divide(i, j, 0.0); 

         //! Cannot drop sw below dul
         max_flow = Math.Max(0.0, (_sw_dep[layer] - _dul_dep[layer]));

         outflow_lat[layer] = mu.bound(outflow_lat[layer], 0.0, max_flow);

         _sw_dep[layer] = _sw_dep[layer] - outflow_lat[layer]; 
         }

      }


#endregion





#endregion





//EVENT HANDLERS

#region Functions used in Event Handlers (mainly in Init, Reset, UserInit, and Write Summary Report Event Handlers)

   //Summary Report & Init2
   private void soilwat2_sum_report()
      {

      //*+  Mission Statement
      //*      Report SoilWat module summary details

      double   depth_layer_top;     //! depth to top of layer (mm)
      double   depth_layer_bottom;  //! depth to bottom of layer (mm)
      int      layer;               //! layer number
      int      num_layers;          //! number of soil profile layers
      string   line;                //! temp output record
      double[] runoff_wf;           //! weighting factor for runoff
      double[] usw;                 //! unavail. sw (mm)
      double[] asw;                 //! avail. sw (mm)
      double[] masw;                //! max unavail. sw (mm)
      double[] dsw;                 //! drainable sw (mm)

      runoff_wf   = new double[max_layer];
      usw         = new double[max_layer];
      asw         = new double[max_layer];
      masw        = new double[max_layer];
      dsw         = new double[max_layer];



      Console.WriteLine();    //new line

      line = "                 Soil Profile Properties";
      Console.WriteLine(line);

      line = "   ---------------------------------------------------------------------";
      Console.WriteLine(line);

      if (!using_ks)
         {

         line = "         Depth  Air_Dry  LL15   Dul    Sat     Sw     BD   Runoff  SWCON";
         Console.WriteLine(line);

         line = "           mm     mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc    wf";
         Console.WriteLine(line);
         }
      else
         {
         line = "         Depth  Air_Dry  LL15   Dul    Sat     Sw     BD   Runoff  SWCON   Ks";
         Console.WriteLine(line);

         line = "           mm     mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc    wf           mm/day";
         Console.WriteLine(line);
         }

      line = "   ---------------------------------------------------------------------";
      Console.WriteLine(line);

      num_layers = au.count_of_real_vals(_dlayer, max_layer);
      depth_layer_top = 0.0;
      soilwat2_runoff_depth_factor(ref runoff_wf);  

      for(layer=au.si;layer<=au.ci(num_layers); layer++)
         {
         depth_layer_bottom = depth_layer_top + _dlayer[layer];

         if (!using_ks)
            {
            line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000}",
                                 depth_layer_top, 
                                 "-", 
                                 depth_layer_bottom,
                                 mu.divide(_air_dry_dep[layer],_dlayer[layer], 0.0),
                                 mu.divide(_ll15_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_dul_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_sat_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_sw_dep[layer], _dlayer[layer], 0.0),
                                 bd[layer],
                                 runoff_wf[layer],
                                 swcon[layer]);
            }
         else
            {
            line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000} {11,6:0.000}",
                                 depth_layer_top, 
                                 "-", 
                                 depth_layer_bottom,
                                 mu.divide(_air_dry_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_ll15_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_dul_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_sat_dep[layer], _dlayer[layer], 0.0),
                                 mu.divide(_sw_dep[layer], _dlayer[layer], 0.0),
                                 bd[layer],
                                 runoff_wf[layer],
                                 swcon[layer],
                                 ks[layer]);
            }
         Console.WriteLine(line);
         depth_layer_top = depth_layer_bottom;
         }

      line = "   ---------------------------------------------------------------------";
      Console.WriteLine(line);

      Console.WriteLine();
      Console.WriteLine();

      line = "             Soil Water Holding Capacity";
      Console.WriteLine(line);

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);

      line = "         Depth    Unavailable Available  Max Avail.  Drainable";
      Console.WriteLine(line);
      line = "                     (LL15)   (SW-LL15)  (DUL-LL15)  (SAT-DUL)";
      Console.WriteLine(line);

      line = "                       mm        mm          mm         mm";
      Console.WriteLine(line);

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);

      num_layers = au.count_of_real_vals (_dlayer, max_layer);
      depth_layer_top = 0.0;

      for(layer=au.si;layer<=au.ci(num_layers); layer++)
         {
         depth_layer_bottom = depth_layer_top + _dlayer[layer];
         usw[layer] = _ll15_dep[layer];
         asw[layer] = mu.l_bound((_sw_dep[layer]-_ll15_dep[layer]),0.0);
         masw[layer] = _dul_dep[layer] - _ll15_dep[layer];
         dsw[layer] = _sat_dep[layer] - _dul_dep[layer];

         line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,10:0.00} {4,10:0.00} {5,10:0.00} {6,10:0.00}",   
                              depth_layer_top, 
                              "-", 
                              depth_layer_bottom,
                              usw[layer],
                              asw[layer],
                              masw[layer],
                              dsw[layer]);

         Console.WriteLine(line);
         depth_layer_top = depth_layer_bottom;
         }

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);

      line = String.Format("           Totals{0,10:0.00} {1,10:0.00} {2,10:0.00} {3,10:0.00}",
                           au.sum_real_array(usw,  num_layers),
                           au.sum_real_array(asw,  num_layers),
                           au.sum_real_array(masw, num_layers),
                           au.sum_real_array(dsw,  num_layers));

      Console.WriteLine(line);

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);


      //! echo sw parameters

      Console.WriteLine();
      Console.WriteLine();
      Console.WriteLine();
      Console.WriteLine();
      
      line = "             Initial Soil Parameters";
      Console.WriteLine(line);

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);

      line = "            Insoil        Salb     Dif_Con   Dif_Slope";
      Console.WriteLine(line);

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);

      line = String.Format("       {0,11:0.00} {1,11:0.00} {2,11:0.00} {3,11:0.00}",
                           _insoil,
                           salb,
                           diffus_const,
                           diffus_slope);

      Console.WriteLine(line);

      line = "     ---------------------------------------------------------";
      Console.WriteLine(line);
      Console.WriteLine();
      Console.WriteLine();

      if (obsrunoff_name != "not_read")
         {
         string obsrunoff_name_trunc;
         obsrunoff_name_trunc = obsrunoff_name.Trim();      //get rid of any whitespaces before and after the name.
         line = String.Format("      {0} {1} {2}",
                              "             Observed runoff data ( ",
                              obsrunoff_name_trunc,
                              " ) is used in water balance");

         Console.WriteLine(line);
         }
      else
         {
         //! no observed data
         Console.WriteLine("             Runoff is predicted using scs curve number:");
         line = "           Cn2  Cn_Red  Cn_Cov   H_Eff_Depth ";
         Console.WriteLine(line);

         line = "                                      mm     ";
         Console.WriteLine(line);

         line = "     ---------------------------------------------------------";
         Console.WriteLine(line);

         line = String.Format("      {0,8:0.00} {1,7:0.00} {2,7:0.00} {3,11:0.00}",
                              _cn2_bare, 
                              _cn_red, 
                              _cn_cov,
                              hydrol_effective_depth);

         Console.WriteLine(line);

         line = "     ---------------------------------------------------------";
         Console.WriteLine(line);
         }
      

      Console.WriteLine();
      Console.WriteLine();


      if (evap_method == ritchie_method)
         {
         line = "      Using Ritchie evaporation model";
         Console.WriteLine(line);

         if (winteru == summeru) 
            {
            line = String.Format("       {0} {1,8:0.00} {2}",
                                 "Cuml evap (U):        ",
                                 _u, 
                                 " (mm^0.5)");

            Console.WriteLine(line);
            }
         else
            {
            line = String.Format("        {0} {1,8:0.00} {2}        {3} {4,8:0.00} {5}",
                                 "Stage 1 Duration (U): Summer    ",
                                 summeru, 
                                 " (mm)" + Environment.NewLine,
                                 "                      Winter    ",
                                 winteru, 
                                 " (mm)");
            Console.WriteLine(line);
            }

         if (wintercona == summercona)
            {
            line = String.Format("       {0} {1,8:0.00} {2}",
                                 "CONA:                 ",
                                 _cona, 
                                 " ()");
            Console.WriteLine(line);
            }
         else
            {
            line = String.Format("        {0} {1,8:0.00} {2}        {3} {4,8:0.00} {5}",
                                 "Stage 2       (CONA): Summer    ",
                                 summercona, 
                                 " (mm^0.5)" + Environment.NewLine,
                                 "                      Winter    ",
                                 wintercona, 
                                 " (mm^0.5)");
            Console.WriteLine(line);
            }
   
         if ((wintercona != summercona) || (winteru != summeru))
            {
            Console.WriteLine("       Critical Dates:       Summer        " + summerdate + System.Environment.NewLine + 
            "                             Winter        "  + winterdate);
            }
         }
      else
         {
         line = "     Using unknown evaporation method!";
         Console.WriteLine(line);
         }     

      Console.WriteLine();


      if (_eo_source != "not_read")
         {
         line = String.Format("      {0} {1}",
                              "Eo source:             ",
                              _eo_source);
         Console.WriteLine(line);
         }
      else
         {
         line = String.Format("       {0}",
                              "Eo from priestly-taylor");
         Console.WriteLine(line);
         }
      
      Console.WriteLine();
   }



   //Init2, Reset, UserInit
   private void soilwat2_init()
      {
      //*+  Purpose
      //*       input initial values from soil water parameter files.

      //*+  Mission Statement
      //*       Initialise SoilWat module


      soilwat2_read_constants();

      soilwat2_soil_property_param();

      soilwat2_soil_profile_param();

      soilwat2_evap_init();

      Lateral_init();

      initDone = true;     //let the classes properties to now allow "sets"

      //publish event saying there is a new soil profile.
      soilwat2_New_Profile_Event();


      }


   private void soilwat2_save_state()
      {
      oldSWDep = soilwat2_total_sw_dep();
      }


   private void soilwat2_delta_state()
      {
      double   dltSWDep;
      double   newSWDep;

      newSWDep = soilwat2_total_sw_dep();
      dltSWDep = newSWDep - oldSWDep;
      soilwat2_ExternalMassFlow(dltSWDep);       //tell the "System Balance" module (if there is one) that the user has changed the water by this amount (by doing a Reset).
      }

   
   private double soilwat2_total_sw_dep()
      {
      //only used above in save_state and delta_state

      int    num_layers;
      num_layers = au.count_of_real_vals(_dlayer, max_layer);

      return au.SumArray(_sw_dep, num_layers);
      }


#endregion



#region Clock Event Handlers



   [EventHandler] public void OnInit2()
      {

      //sv- I had to put this in so that _sw_dep is instantiated and then soilwat2_save_state() does not throw an error. 
      InstantiateArrays_PureOutputs();

      //Save State
      soilwat2_save_state();


      soilwat2_init();

      soilwat2_sum_report();

      //Change State
      soilwat2_delta_state();
      }



   [EventHandler] void OnPrepare()
      {
      //*     ===========================================================
      //      subroutine soilwat2_prepare
      //*     ===========================================================

      //*+  Purpose
      //*       Calculate potential evapotranspiration
      //*
      //*+  Mission Statement
      //*     Perform all APSIM Timestep calculations

      //*- Implementation Section ----------------------------------

     


      //Let other modules set my variables
      //taken from Main() 
      //the set_variable event in Main occurs before the prepare event. So I figured this was the best place to put this call.
      soilwat2_set_my_variable();



      soilwat2_zero_daily_variables();
      Lateral_zero_daily_variables();     //sv- I added this from Lateral_prepare()
      soilwat2_get_crop_variables();    
      soilwat2_get_environ_variables();

      //! potential: sevap + transpiration:
      soilwat2_pot_evapotranspiration(ref eo);
      real_eo = eo;  //! store for reporting


       }



   [EventHandler] void OnProcess()
      {



      //Get variables from other modules
      //taken from Main() 
      soilwat2_get_other_variables();

      //sv- I added everything above



      //*     ===========================================================
      //      subroutine soilwat2_process
      //*     ===========================================================
      //*+  Purpose
      //*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
      //*       evaporation, solute movement, transpiration.

      //*+  Local Variables
      
      int           layer;                 //! layer number counter variable
      int           num_layers;            //! number of layers
      double        extra_runoff;          //! water backed up from flux calculations that was unable to enter profile

       //*- Implementation Section ----------------------------------


      num_layers = au.count_of_real_vals (_dlayer, max_layer);


      // LATERAL FLOW
 
      Lateral_process();




      // RUNOFF

      soilwat2_cover_surface_runoff(ref cover_surface_runoff);

      //c dsg 070302 added runon
      //! NIH Need to consider if interception losses were already considered in runoff model calibration
      soilwat2_runoff(rain,runon,(interception + residueinterception), ref runoff_pot);

      //! DSG  041200
      //! g%runoff_pot is the runoff which would have occurred without
      //! ponding.  g%runoff is the ammended runoff after taking any
      //! ponding into account

      pond = pond + runoff_pot;
      runoff = Math.Max((pond - _max_pond), 0.0);
      pond = Math.Min(pond, _max_pond);



      // INFILTRATION

      soilwat2_infiltration(ref infiltration);

      //! all infiltration and solutes(from irrigation)
      //! go into the top layer.

      _sw_dep[au.si] = _sw_dep[au.si] + infiltration;



      // IRRIGATION

      if (irrigation_layer > au.ltsi) 
         {
         //add the irrigation
         //TODO:this irrigation_layer is wrong. Should be ci.(irrigation_layer). ALso zeroing of it is wrong which is why it does not work in the fortran version. Also assigning
         _sw_dep[irrigation_layer] = _sw_dep[irrigation_layer] + irrigation;
         }

      //! save solutes from irrigation
      soilwat2_irrig_solute();
/*
      //! receive any solutes from rainfall
      soilwat2_rainfall_solute();
*/
      //! NIH 180895
      //! in order to continue capturing irrigation information we zero
      //! the value here.  If we zero the value at the beginning of the day
      //! we may zero it after irrigation has already been specified and the
      //! information would be lost.  The safest way is to hold onto the
      //! information until it is used then reset the record.

      irrigation = 0.0;
      au.fill_real_array(ref irrigation_solute, 0.0, max_solute);



      // SATURATED FLOW (flux calculation, aka Drainage) 

      //sv- I added this
      extra_runoff=0.0;

      if (using_ks)
         {
         soilwat2_drainage(ref flux, ref extra_runoff);    //sv- this returns flux[] and extra_runoff  //nb. this only calculates the flux it does not move it or change any sw values. That is done in move_down_real() 
         }
      else
         {
         soilwat2_drainage_old(ref flux, ref extra_runoff); //sv- this returns flux[] and extra_runoff //nb. this only calculates the flux it does not move it or change any sw values. That is done in move_down_real()
         }

      //"runoff" is caused by permeability of top layer(cn2Bare). This permeability is modified cover(cnCov, cnRed) and moisture content.   
      //"extra_runoff" is caused by backing up of top layer due to inability of soil to drain. See soilwat2_drainage() above.
 
      //Any extra_runoff then it becomes a pond. 
      pond = Math.Min(extra_runoff, _max_pond);
      //If there is too much for the pond handle then add the excess (ie. extra_runoff-pond) to normal runoff.
      runoff = runoff + extra_runoff - pond;
      //Deduct the extra_runoff from the infiltration because it did not infiltrate (because it backed up).
      infiltration = infiltration - extra_runoff;

      _sw_dep[au.si] = _sw_dep[au.si] - extra_runoff;   //sv- actually add the extra runoff to _sw_dep



      //! move water down     (Saturated Flow - alter _sw_dep values using flux calculation)
      au.move_down_real(flux, ref _sw_dep, num_layers);    

      //! drainage out of bottom layer
      drain = flux[au.ci(num_layers)];



      // SATURATED FLOW SOLUTE MOVEMENT

      //! now move the solutes with flux  
      //! flux -  flow > dul
      soilwat2_move_solute_down();


      // EVAPORATION

      //! actual soil evaporation:
      soilwat2_evaporation(ref es_layers, ref eos);

      //soilwat2_pot_evapotranspiration() is called in the prepare event. 
      //This "_effective calculation()" just takes ponding into account.
      //! potential: sevap + transpiration:
      soilwat2_pot_evapotranspiration_effective(ref eos);

      //! ** take away evaporation
      for(layer=au.si; layer<=au.ci(num_layers); layer++)
         {
         _sw_dep[layer] = _sw_dep[layer] - es_layers[layer];
         }


      // UNSATURATED FLOW (flow calculation)

      //! get unsaturated flow   
      soilwat2_unsat_flow(ref flow);

      //! move water up          (Unsaturated Flow - alter _sw_dep values using flow calculation)
      au.move_up_real(flow, ref _sw_dep, num_layers);

      //! now check that the soil water is not silly
      for(layer=au.si;layer<=au.ci(num_layers); layer++)
         {
         soilwat2_check_profile(layer);
         }


      // WATER TABLE
 
      _water_table = soilwat_water_table();

//TODO: this sws value appears to just be an output variable but it is identical to sw? I think it should be removed.
/*
      num_layers = au.count_of_real_vals(_dlayer, max_layer);
      for(layer=au.si;layer<=au.ci(num_layers); layer++)
         {
         sws[layer] = mu.divide(_sw_dep[layer], _dlayer[layer], 0.0);
         }
*/


      // UNSATURATED FLOW SOLUTE MOVEMENT

      //! now move the solutes with flow  
      soilwat2_move_solute_up();

 


      //sv- I added everything below.

      //Change the variables in other modules
      //taken from Main() 
      soilwat2_set_other_variables();

      //Some variables need calculations done before outputting
      //taken from Main() 
      soilwat2_send_my_variable();

      }


 
   [EventHandler] void OnTick(TimeType Tick)
      {
 
//*     ===========================================================
//      subroutine soilwat2_ONtick (variant)
//*     ===========================================================

//*+  Purpose
//*     Update internal time record and reset daily state variables.

//*+  Mission Statement
//*     Update internal time record and reset daily state variables.

//*- Implementation Section ----------------------------------

      //sv- Julian Day is NOT Day of Year. It is a common misonception that Julian Date is DOY but it is incorrect.
      //    Julian Day is days since 1 January 4713 BC Greenwich noon, Julian proleptic calendar. Expressed as a double -> days_since_4713BC.fraction_of_day
      //    The correct term is Ordinal Date or just plain old 'Day of Year'. 
      //    See http://en.wikipedia.org/wiki/Julian_day and http://en.wikipedia.org/wiki/ISO_8601#Ordinal_dates

      //sv- Tick.startday is an int because it is only the day part of the julian day. It does not have the decimal for sub day unit of time (fraction_of_day).
      //    When you convert it to double therefore there is no faction_of_day , it just puts .0 on the end.

      //sv- This julian calculation is used only for the evap calculation. Use to tell when summer and winter windows start and finish so can use different u and cona values.

      double julianDay = (double)Tick.startday;      
      ApsimUtil.jday_to_day_of_year(julianDay, out day, out year);  //sv- initialise the global variables -> 'day' and 'year' (g%day, g%year)
      today = julianDay;              //sv- initialise global variable -> 'today'

      }


#endregion



#region Met, Irrig, Solute, Plants Event Handlers



   [EventHandler] void OnNewMet(NewMetType NewMet)
      {
//*     ===========================================================
//      subroutine soilwat2_ONnewmet (variant)
//*     ===========================================================

//*+  Purpose
//*     Get new met data

//*+  Mission Statement
//*     Get new met data

//*- Implementation Section ----------------------------------
  
      radn = NewMet.radn;
      maxt = NewMet.maxt;
      mint = NewMet.mint;
      rain = NewMet.rain;

      ApsimUtil.bound_check_real_var(radn, 0.0, 60.0,"radn");
      ApsimUtil.bound_check_real_var(maxt, -50.0, 60.0,"maxt");
      ApsimUtil.bound_check_real_var(mint, -50.0, 50.0,"mint");
      ApsimUtil.bound_check_real_var(rain, 0.0, 5000.0,"rain");

      }



   [EventHandler] void Onnew_solute(NewSoluteType  newsolute)
      {
   
//*     ===========================================================
//      subroutine soilwat2_on_new_solute ()
//*     ===========================================================

      //"On New Solute" simply tells modules the name of a new solute, what module owns the new solute, and whether it is mobile or immobile.
      //       It alerts you at any given point in a simulation when a new solute is added. 
      //       It does NOT tell you the amount of the new solute in each of the layers. You have to ask the module owner for this separately.
      //creates 3 solute information arrays from the NewSoluteType
      //    solute_names[]
      //    solute_owners[] (module id's not names) (This is really useless in .NET now)
      //    TODO: change solute_owners from int to string. Should change sender_id in DataTypes.xml from int array to string and record module name not id, so you can use a GetComponentType() with a [Link].
      //    solute_mobility[]
   

      string[] names = new string[max_solute];
      int      sender;
      int      counter;
      int      mobile_no;
      int      immobile_no;
      int      numvals;             //! number of values returned
      string   name;

//*- Implementation Section ----------------------------------

       sender = newsolute.sender_id;
       numvals = newsolute.solutes.Length;
      	  
      if ((num_solutes + numvals) > max_solute)
         {
         throw new Exception("Too many solutes for SoilWater module");
         }
      else
         {
         for (counter=au.si; counter<=au.ci(numvals); counter++)
            {
            name = newsolute.solutes[counter];  //just use name variable to save space on next line.
            if ((name == "no3") || (name == "nh4") || (name == "urea") || (name == "cl") || (name == "br"))
               {
               num_solutes = num_solutes + 1;
               solute_names[au.ci(num_solutes)] = newsolute.solutes[counter]; //save the name of the solute
               solute_owners[au.ci(num_solutes)] = sender;                    //save the module id of the solute

               //sv- Find out if this solute is mobile or immobile.
               //       mobile_solutes and immobile_solutes are specified in the ini file.
               mobile_no = au.position_in_char_array(solute_names[au.ci(num_solutes)], mobile_solutes, max_solute);
               immobile_no = au.position_in_char_array(solute_names[au.ci(num_solutes)], immobile_solutes, max_solute);

               //sv- put mobile_solutes and immobile_solutes into the one boolean array that is used in your soilwat2_move_solute_up() and soilwat2_move_solute_down() 
               if (mobile_no != -1) 
                  {
                  solute_mobility[au.ci(num_solutes)] = true;
                  }

               else if (immobile_no != -1) 
                  {
                  solute_mobility[au.ci(num_solutes)] = false;
                  }
               else
                  {
                  throw new Exception("No solute mobility information for " + solute_names[au.ci(num_solutes)] + " , please specify as mobile or immobile in the SoilWater ini file.");
                  }
               }
            }
         }

      }



   [EventHandler] void OnIrrigated(IrrigationApplicationType Irrigated)
      {

       //* ====================================================================
       //subroutine soilwat2_ONirrigated ()
       //* ====================================================================

//*+  Mission Statement
//*     Add Water

//*+  Local Variables
      int         solnum;           //! solute no. counter variable               
      double      solute_amount;

//*- Implementation Section ----------------------------------


      //see OnProcess event handler for where this irrigation is added to the soil water 
      irrigation = irrigation + Irrigated.Amount;  //! amount of irrigation (mm)    

      if (num_solutes > 0)
         {

         for (solnum=au.si; solnum<=au.ci(num_solutes); solnum++)
            {

            switch (solute_names[solnum])
               {
               case "no3":
                  solute_amount = Irrigated.NO3;
                  break;
               case "nh4":
                  solute_amount = Irrigated.NH4;
                  break;
               case "cl":
                   solute_amount = Irrigated.CL;
                  break;
               default:
                  solute_amount = 0.0;
                  break;
               }

            //this irrigation_solute is added to the the soil solutes (solute 2D array) when soilwat2_irrig_solute() is called from OnProcess event handler.
            irrigation_solute[solnum] = irrigation_solute[solnum] + solute_amount;

            }

         }


      }



   [EventHandler] void OnWaterChanged(WaterChangedType WaterChanged)
      {

      //This event is Only used by Plant2 and AgPasture.
      //This event was added so that the Plant2 module could extract water via its roots from the SoilWater module.
      //At the time Plant2 was not advanced enough to be able to do a "Set" on another modules variables.
      //Plant2 still uses this method to extract water using its roots.
  
      //*+  Purpose
      //*     Another module wants to change our water


      int layer; 

      for(layer=au.si; layer<=au.ci(WaterChanged.DeltaWater.Length); layer++)
         {
         _sw_dep[layer] = _sw_dep[layer] + WaterChanged.DeltaWater[layer];
         soilwat2_check_profile(layer);
         }
      
      }



#endregion



#region Manager Event Handlers


   [EventHandler] void Onsum_report()
      { 
      //Manager module can request that each module write its variables out to the summary file. This handles that event. 
      soilwat2_sum_report();
      }



   [EventHandler] public void OnReset()
      {
      //nb. this is the same as OnUserInit Event

      //Save State
      soilwat2_save_state();
      soilwat2_zero_variables();     
      soilwat2_get_other_variables();
      soilwat2_init();

      //Change State
      soilwat2_delta_state();
      }


 //OnUserInit is no longer supported. It has been replaced by the OnReset() above.


   [EventHandler] void OnTillage(TillageType Tillage)
      {
      //*     ===========================================================
      //      subroutine soilwat2_tillage ()
      //*     ===========================================================
      //*+  Purpose
      //*     Set up for CN reduction after tillage operation

      //*+  Notes
      //*       This code is borrowed from residue module.

      //*+  Mission Statement
      //*       Calculate tillage effects

      //*+  Local Variables
      string         message;             //! message string
      string         tillage_type;             //! name of implement used for tillage//! 1. Find which implement was used. eg. disc, burn, etc.


      //*- Implementation Section ----------------------------------
    
      // cn_red is the reduction in the cn value, and cn_rain is the amount of rainfall after the tillage event that the reduction ceases to occur.

      //the event always gives us at least the type of tillage. Even if it does not give the cn_red and cn_rain.
      //if the event does not give us cn_red and cn_rain then use the type name to look up the values in the sim file (ini file).

       tillage_type = Tillage.type;       //sv - the event always gives us at least this.

      //TODO: finish writing the code to get the entire tillage table from the ini file (sim file) and look through it to find the values for our particular tillage type.

      //sv- if the Tilliage information did not come with the event.
      if ((Tillage.cn_red == 0) || (Tillage.cn_rain == 0)) 
         {  
         Console.WriteLine(Environment.NewLine + "    - Reading tillage CN info");

         if (type_info_from_sim.Length != 2)
            {
            //sv- Event did not give us the tillage information and the sim file does not have the tillage information.
            //! We have an unspecified tillage type
            tillage_cn_red = 0.0;
            tillage_cn_rain = 0.0;

            message = "Cannot find info for tillage:- " + Tillage.type; 
            throw new Exception (message);
            }
         else
            {
            //sv- Get the values from the sim file.
            tillage_type = "tillage specified in ini file.";
            if (Tillage.cn_red == 0)
               {
               tillage_cn_red = type_info_from_sim[0];
               }

            if (Tillage.cn_rain == 0)
               {
               tillage_cn_rain = type_info_from_sim[1];
               }
            }
         }
      else
         {
         tillage_cn_red = Tillage.cn_red;
         tillage_cn_rain = Tillage.cn_rain;
         }         


 
      //! Ensure cn equation won't go silly
      tillage_cn_red = mu.bound(tillage_cn_red, 0.0, _cn2_bare);


      //sv- write what we are doing to the summary file.
      string line;
      line = String.Format("{0} {1} {2}                                        {3} {4:F} {5}                                        {6} {7:F}",
                           "Soil tilled using ", tillage_type, Environment.NewLine, "CN reduction = ", tillage_cn_red, Environment.NewLine, "Acc rain     = ", tillage_cn_rain);
      Console.WriteLine(line);

      
      //! 3. Reset the accumulator
      tillage_rain_sum = 0.0;

      }


#endregion




//EVENTS - SENDING

#region Functions used to Publish Events sent by this module


   private void soilwat2_New_Profile_Event()
      {
      //*+  Mission Statement
      //*     Advise other modules of new profile specification

      NewProfileType newProfile =  new NewProfileType();
      newProfile.air_dry_dep =   au.ToFloatArray(_air_dry_dep);
      newProfile.bd =            au.ToFloatArray(bd);
      newProfile.dlayer =        au.ToFloatArray(_dlayer);
      newProfile.dul_dep =       au.ToFloatArray(_dul_dep);
      newProfile.ll15_dep =      au.ToFloatArray(_ll15_dep);
      newProfile.sat_dep =       au.ToFloatArray(_sat_dep);
      newProfile.sw_dep =        au.ToFloatArray(_sw_dep);
      New_profile.Invoke(newProfile);
      }


   private void soilwat2_ExternalMassFlow (double sw_dep_delta_sum)
      {

      //*+  Mission Statement
      //*     Update internal time record and reset daily state variables.

      //External Mass Flow event is used for a model called "System Balance" which just keeps track of all the water, solutes etc in the model. 
      //To make sure it all balances out and no water is being lost from the system. It is used for debugging purposes.
      //Some times however the user will do something that will diliberately upset this, such as forcibly reseting a water content by doing a
      //Reset command in a manager or by Setting a variable in the manager manually. When this happens the "System Balance" module's balance 
      //no longer adds up. So when you do a Reset or Set a variable you must send an External Mass Flow Type event that alerts the "System Balance"
      //module that the user has forced a change and the amount by which they have changed it, so that the "System Balance" module can add this
      //amount to its balance so it's balance will work out correctly again. 

      ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();


      if (sw_dep_delta_sum >= 0.0)
         {
         massBalanceChange.FlowType = "gain";
         }
      else
         {
         massBalanceChange.FlowType = "loss";
         }
      
         massBalanceChange.PoolClass = "soil";
         massBalanceChange.DM = 0.0F;
         massBalanceChange.C  = 0.0F;
         massBalanceChange.N  = 0.0F;
         massBalanceChange.P  = 0.0F;
         massBalanceChange.SW = Math.Abs((float)sw_dep_delta_sum);

         ExternalMassFlow.Invoke(massBalanceChange);         

      }

 

#endregion


#region Events sent by this Module

   //Events
   [Event] 
   public event NewProfileDelegate New_profile;

   [Event] 
   public event ExternalMassFlowDelegate ExternalMassFlow;

   [Event] 
   public event RunoffEventDelegate Runoff;

#endregion

   }



