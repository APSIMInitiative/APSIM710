using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using ModelFramework;
using CSGeneral;
#if (APSIMX == false)
using CMPServices;
#endif

///<summary>
/// .NET port of the Fortran SoilWat model
/// Ported by Shaun Verrall Mar 2011
/// Extended by Eric Zurcher Mar 2012
///</summary> 


public class SoilWater
{

    #region Constants

    private const double precision_sw_dep = 1.0e-3; //!Precision for sw dep (mm)
    private const int ritchie_method = 1;
    private const double mm2m = 1.0 / 1000.0;      //! conversion of mm to m
    private const double sm2smm = 1000000.0;       //! conversion of square metres to square mm
    private const double error_margin = 0.0001;
    private const double psi_ll15 = -150.0;        //! water potential (m) at -15 bar (not quite exactly right, is it? Shouldn't it be -1500/9.8 ?)

    [Output]
    private int steps_per_day = 500;

    #endregion


    //INPUTS FROM SIM FILE & OUTPUTS OF THIS MODULE

    #region Module Constants (from SIM file but it gets from XML file)

    private double time_step = 1.0;          // (d) Currently always using a one-day time step 
    //SIM file gets these from XML file

    //Soilwat2Constants       //sv- also from soilwat2_read_constants()

    [Output]
    [Param(MinVal = 0.0, MaxVal = 10.0)]
    [Units("oC")]
    [Description("Temperature below which eeq decreases")]
    private double min_crit_temp;             //! temperature below which eeq decreases (oC)

    [Output]
    [Param(MinVal = 0.0, MaxVal = 50.0)]
    [Units("oC")]
    [Description("Temperature above which eeq increases")]
    private double max_crit_temp;             //! temperature above which eeq increases (oC)

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Maximum bare ground soil albedo")]
    private double max_albedo;                //! maximum bare ground soil albedo (0-1)

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Factor to convert \"A\" to coefficient in Adam's type residue effect on Eos")]
    private double A_to_evap_fact;            //! factor to convert "A" to coefficient in Adam's type residue effect on Eos

    [Output]
    [Param(MinVal = 0.0, MaxVal = 10.0)]
    [Units("0-10")]
    [Description("Coefficient in cover Eos reduction equation")]
    private double canopy_eos_coef;           //! coef in cover Eos reduction eqn

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Shape parameter for the relationship between soil evaporation and standing cover")]
    private double k_standing = 0.59;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Exponent in the relationship between soil evaporation and residue cover")]
    private double k_residue = 0.44;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Critical sw ratio in top layer below which stage 2 evaporation occurs")]
    private double sw_top_crit;               //! critical sw ratio in top layer below which stage 2 evaporation occurs

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Upper limit of sumes1")]
    private double sumes1_max;                //! upper limit of sumes1

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Upper limit of sumes2")]
    private double sumes2_max;                //! upper limit of sumes2

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Efficiency of moving solute with unsaturated flow")]
    private double[] solute_flow_eff;          //sv- Unsaturated Flow   //! efficiency of moving solute with flow (0-1)
    private int num_solute_flow;   //bound_check_real_array() gives this a value in soilwat2_read_constants()

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Efficiency of moving solute with flux (saturated flow)")]
    private double[] solute_flux_eff;         //sv- Drainage (Saturated Flow)   //! efficiency of moving solute with flux (0-1) 
    private int num_solute_flux; //bound_check_real_array() gives this a value in soilwat2_read_constants()

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Gradient due to hydraulic differentials")]
    private double gravity_gradient;          //! gradient due to hydraulic differentials (0-1)

    [Output]
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("g/cm^3")]
    [Description("Specific bulk density")]
    private double specific_bd = 2.65;               //! specific bulk density (g/cc)

    [Output]
    [Param(MinVal = 1.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Hydrologically effective depth for runoff")]
    private double hydrol_effective_depth = 450.0;    //! hydrologically effective depth for runoff (mm)

    [Output]
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 10000.0)]
    [Units("mm")]
    [Description("Depth down to which soil evaporation takes place")]
    private double soil_evap_max_depth = 100.0; //  Double.NaN; 

    [Output]
    [Param]
    [Description("Names of all possible mobile solutes")]
    private string[] mobile_solutes;     //! names of all possible mobile solutes

    [Output]
    [Param]
    [Description("Names of all possible immobile solutes")]
    private string[] immobile_solutes;   //! names of all possible immobile solutes

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Canopy factors for cover runoff effect")]
    private double[] canopy_fact;        //! canopy factors for cover runoff effect ()

    [Output]
    [Param(MinVal = 0.0, MaxVal = 100000.0)]
    [Units("mm")]
    [Description("Heights for canopy factors")]
    private double[] canopy_fact_height; //! heights for canopy factors (mm)

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Default canopy factor in absence of height")]
    private double canopy_fact_default;       //! default canopy factor in absence of height ()

    [Output]
    [Param]
    [Description("Actual soil evaporation model being used")]
    private string act_evap_method;           //! actual soil evaporation model being used //sv- hard wired to "ritchie" in the init event handler. 
    private int evap_method;               //sv- integer representation of act_evap_method   

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm/oC")]
    [Description("Rate of snow melt")]
    private double k_melt = 4.57;       //! Snow melt rate factor

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Proportion of hydraulic conductivity due to the high-water-content component at the drained upper limit")]
    private double prop_cond_dul = 0.01;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm/d")]
    [Description("Vertical hydraulic conductivity at the drained upper limit")]
    private double k_dul = 0.125; // 1.0;
    // I think we ultimately want the ability to set this on a layer-by-layer basis
    private double[] k_dul_dep;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 0.1)]
    [Units("mm/d")]
    [Description("Vertical hydraulic conductivity at a water potential of -15 bars")]
    private double k_ll15 = 0.00005;
    // I think we ultimately want the ability to set this on a layer-by-layer basis
    private double[] k_ll15_dep;

    [Param(MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm/d")]
    [Description("Vertical hydraulic conductivity at saturation")]
    private double[] k_sat_dep;

    [Param(IsOptional = true, MinVal = -30.0, MaxVal = -0.1)]
    [Units("m")]
    [Description("Water potential at the drained upper limit")]
    private double psi_dul = -3.3;
    // I think we ultimately want the ability to set this on a layer-by-layer basis
    private double[] psi_dul_dep;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm/d")]
    [Description("Threshold drainage rate, used in the computation of the local water table depth")]
    private double thdr_wt = 0.1;

    [Param(IsOptional = true)]
    [Units("mm/m/d")]
    [Description("Rate parameter for flow due to water tables")]
    private double coeff_water_table = 0.0;
    //Soilwat2Constants

    //OnTillage Event
    //***************
    // TODO: This still isn't quite what's needed...
    [Param(IsOptional = true, Name = "type")]  //! Array containing information about a certain type (from table)  //sv- "type" as in the type of tillage: "disc", "burn", etc. 
    private double[] type_info_from_sim = null;   //sv- contains the tillage_cn_red and tillage_cn_rain specified in the sim file. Only used if manager module does not send it with the Event. 


    //Irrigation Layer

    [Param(IsOptional = true, MinVal = 0, MaxVal = 100)]
    [Output]
    [Description("Number of soil layer to which irrigation water is applied (where surface == 0, and top layer == 1)")]
    private int irrigation_layer = 0;      //! number of soil layer to which irrigation water is applied


    #endregion


    #region Soil "Property" (NOT layered): (Constants & Starting Values from SIM file), and the Outputs

    /*
   //SIM file gets them from .APSIM file
   //sv- Any Params that are Optional give a value of -1 or "not_read". -1 is an impossible value and is used to tell you that a value was not read in from the .sim file for this variable.
   //Soilwat2Parameters   //sv- also from soilwat2_soil_property_param()
   */

    //see "obsrunoff" in the Daily Inputs ie. [Input] tag.
    [Output]
    [Param(IsOptional = true, Name = "observed_runoff")]
    [Description("System variable name of external observed runoff source")]
    private string obsrunoff_name = "";    //! system name of observed runoff


    private string _eo_source = "";
    [Output]
    [Param(IsOptional = true)]
    [Description("System variable name of external eo source")]
    public string eo_source      //! system variable name of external eo source
    {
        get { return _eo_source; }
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

    private int numvals_insoil = 0;                    //! number of values returned for insoil
    private double _insoil = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Switch describing how initial soil water is specified")]
    public double insoil            //! switch describing initial soil water  //sv-specifies which option you are using.
    {
        get { return _insoil; }
        set
        {
            //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
            if (!initDone)
            {
                reset_numvals_insoil = 1;
                reset_insoil = value;
            }
            soilwat2_zero_default_variables();
            numvals_insoil = 1;     //used in soilwat2_set_default()
            _insoil = value;
            soilwat2_set_default();
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    //3. Starting from the top, fill the soil until a specified fraction of the entire soils esw is reached. Fill each layer to dul.

    private int numvals_profile_fesw = 0;              //! number of values returned for profile_fesw
    private double _profile_fesw = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Initial fraction of esw of profile distributed from top down")]
    public double profile_fesw     //! initial fraction of esw of profile distributed from top down ()
    {
        get { return _profile_fesw; }
        set
        {
            //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
            if (!initDone)
            {
                reset_numvals_profile_fesw = 1;
                reset_profile_fesw = value;
            }
            soilwat2_zero_default_variables();
            numvals_profile_fesw = 1;  //used in soilwat2_set_default()
            _profile_fesw = value;
            soilwat2_set_default();
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }


    //4. Starting from the top, fill the soil until a specified amount of esw for the entire soil is reached. Fill each layer to dul.

    private int numvals_profile_esw_depth = 0;         //! number of values returned for profile_esw_depth
    private double _profile_esw_depth = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Units("mm")]
    [Description("Initial depth of extractable soil water distributed from the top down")]
    public double profile_esw_depth   //! initial depth of extractable soil water distributed from the top down (mm)
    {
        get { return _profile_esw_depth; }
        set
        {
            //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
            if (!initDone)
            {
                reset_numvals_profile_esw_depth = 1;
                reset_profile_esw_depth = value;
            }
            soilwat2_zero_default_variables();
            numvals_profile_esw_depth = 1;   //used in soilwat2_set_default()
            _profile_esw_depth = value;
            soilwat2_set_default();
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }


    //5. Starting from the top fill the soil to a specified soil depth. Fill each layer to dul. 

    private int numvals_wet_soil_depth = 0;            //! number of values returned for wet_soil_depth
    private double _wet_soil_depth = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Units("mm")]
    [Description("Initial depth of soil filled to drained upper limit (field capacity)")]
    private double wet_soil_depth   //! initial depth of soil filled to drained upper limit (field capacity) (mm)
    {
        get { return _wet_soil_depth; }
        set
        {
            //sv- setting this automatically changes the sw values. Due to the soilwat2_set_default() call.
            if (!initDone)
            {
                reset_wet_soil_depth = value;
                reset_numvals_wet_soil_depth = 1;
            }
            soilwat2_zero_default_variables();
            numvals_wet_soil_depth = 1;      //used in soilwat2_set_default()
            _wet_soil_depth = value;
            soilwat2_set_default();
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    //sv- end of initial sw section

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
    [Description("Diffusivity constant for soil testure")]
    private double diffus_const = Double.NaN;     //! diffusivity constant for soil testure

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Slope for diffusivity/soil water content relationship")]
    private double diffus_slope = Double.NaN;     //! slope for diffusivity/soil water content relationship

    private double _cn2_bare = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 100.0)]
    [Description("Curve number input used to calculate daily runoff")]
    public double cn2_bare         //! curve number input used to calculate daily runoff
    {
        get { return _cn2_bare; }
        set
        {
            if (!initDone)
                reset_cn2_bare = value;
            _cn2_bare = value;
        }
    }

    private double _cn_red = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Maximum reduction in cn2_bare due to cover")]
    public double cn_red           //! maximum reduction in cn2_bare due to cover
    {
        get { return _cn_red; }
        set
        {
            if (!initDone)
                reset_cn_red = value;
            _cn_red = value;
        }
    }

    private double _cn_cov = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Cover at which cn_red occurs")]
    public double cn_cov           //! cover at which cn_red occurs
    {
        get { return _cn_cov; }
        set
        {
            if (!initDone)
                reset_cn_cov = value;
            _cn_cov = value;
        }
    }

    private double _max_pond = 0.0;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Maximum surface storage capacity of soil")]
    public double max_pond         //! maximum surface storage capacity of soil  //sv- used to store water from runoff on the surface.
    {
        get { return _max_pond; }
        set
        {
            //* made settable to allow for erosion 'max_pond'
            //*** dsg 280103  Added re-settable 'max-pond' for Shaun Lisson to simulate dam-break in rice cropping
            if (!initDone)
                reset_max_pond = value;
            _max_pond = value;
        }
    }

    [Param(MinVal = 0.0001, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Bare soil albedo")]
    private double salb;           //! bare soil albedo (unitless)

    //Extra parameters for evaporation models (this module only has Ritchie Evaporation)  
    //(see soilwat2_init() for which u and cona is used)

    //same evap for summer and winter
    private double _u = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 40.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation")]
    public double u            //! upper limit of stage 1 soil evaporation (mm)
    {
        get { return _u; }
        set { throw new Exception("setting U is done via GUI"); }
    }

    private double _cona = Double.NaN;
    [Output]
    [Units("mm/d^0.5")]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient")]
    public double cona         //! stage 2 drying coefficient
    {
        get { return _cona; }
        set { throw new Exception("setting cona is done via GUI"); }
    }

    //different evap for summer and winter
    //summer
    [Output]
    [Param(IsOptional = true)]
    [Description("Date for start of summer evaporation (dd-mmm)")]
    private string summerdate = "not_read";       //! Date for start of summer evaporation (dd-mmm)

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 40.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation during summer")]
    private double summeru = Double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm/d^0.5")]
    [Description("Stage 2 drying coefficient during summer")]
    private double summercona = Double.NaN;

    //winter
    [Output]
    [Param(IsOptional = true)]
    [Description("Date for start of winter evaporation (dd-mmm)")]
    private string winterdate = "not_read";       //! Date for start of winter evaporation (dd-mmm)

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation during winter")]
    private double winteru = Double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm/d^0.5")]
    [Description("Stage 2 drying coefficient during winter")]
    private double wintercona = Double.NaN;

    //end of Extra parameters for evaporation models


    //sv- Lateral flow properties  //sv- also from Lateral_read_param()

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Slope")]
    private double slope = Double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0e8F)]     //1.0e8F = 100000000
    [Units("m")]
    [Description("Basal width of discharge area")]
    private double discharge_width = Double.NaN;  //! basal width of discharge area (m)

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0e8F)]     //1.0e8F = 100000000
    [Units("m^2")]
    [Description("Area over which lateral flow is occuring")]
    private double catchment_area = Double.NaN;   //! area over which lateral flow is occuring (m2)

    //sv- end of Lateral flow properties



    //sv- PURE OUTPUTS

    [Output]
    [Units("mm")]
    [Description("Total es")]
    private double es                      //! total es
    { get { return MathUtility.Sum(es_layers); } }

    [Output]
    [Units("mm")]
    [Description("Daily effective rainfall")]
    private double eff_rain                  //! daily effective rainfall (mm)
    { get { return rain + runon - runoff - drain; } }

    [Output]
    [Units("mm")]
    [Description("Potential extractable sw in profile")]
    private double esw                       //! potential extractable sw in profile  
    {
        get
        {
           int num_layers = _dlayer.Length;
           double result = 0.0;
           for (int layer = 0; layer < num_layers; layer++)
              result += Math.Max(_sw_dep[layer] - _ll15_dep[layer], 0.0);
           return result;
        }
    }

    [Output]
    [Description("Effective total cover")]
    private double cover_surface_runoff;     //! effective total cover (0-1)   //residue cover + cover from any crops (tall or short)

    [Output]
    [Units("d")]
    [Description("time after which 2nd-stage soil evaporation begins")]
    private double t;                        //! time after 2nd-stage soil evaporation begins (d)

    [Output]
    [Units("mm")]
    [Description("Effective potential evapotranspiration")]
    private double eo;                       //! effective potential evapotranspiration (mm)

    [Output]
    [Units("mm")]
    [Description("Pot sevap after modification for green cover & residue wt")]
    private double eos;                      //! pot sevap after modification for green cover & residue wt

    [Output]
    [Description("New cn2 after modification for crop cover & residue cover")]
    private double cn2_new;                  //! New cn2  after modification for crop cover & residue cover

    [Output]
    [Units("mm")]
    [Description("Drainage rate from bottom layer")]
    private double drain            //! drainage rate from bottom layer (cm/d) // I think this is in mm, not cm....
    { get { return flux[flux.Length - 1]; } }

    [Output]
    [Units("mm")]
    [Description("Infiltration")]
    private double infiltration;     //! infiltration (mm)
    private double ts_infiltration;

    [Output]
    [Units("mm")]
    [Description("Runoff")]
    private double runoff;           //! runoff (mm)
    private double ts_runoff;        // runoff for a sub-day timestep

    [Output]
    [Units("mm")]
    [Description("Evaporation from the surface of the pond")]
    private double pond_evap;      //! evaporation from the surface of the pond (mm)
    private double ts_pond_evap;

    [Output]
    [Units("mm")]
    [Description("Surface water ponding depth")]
    private double pond;           //! surface water ponding depth

    [Output]
    [Units("mm")]
    [Description("Amount of water present as snow on the soil surface")]
    private double snowpack;         //! Amount of water present as snow on the soil surface

    [Output]
    [Units("mm")]
    [Description("Amount of water melting from snow on the soil surface")]
    private double snowmelt;         //! Amount of water melting from snow on the soil surface

    //Soilwat2Globals

    //taken from soilwat2_set_my_variable()

    //nb. water_table is both an input and an output. 
    //It is always is an output because a water table can always build up. (See soilwat_water_table())
    //Sometimes it is an input when the user specifies a set command in a manager because they want to set the water_table at a specific height on a given day. (see SetWaterTable())

    private double _water_table = Double.NaN;
    [Output]
    [Units("mm")]
    [Description("Water table depth (depth below the ground surface of the first saturated layer)")]
    public double water_table     //! water table depth (depth below the ground surface of the first saturated layer)
    {
        get { return waterTable / mm2m; }
        set { SetWaterTable(value); }
    }
    private double waterTable = 0.0; // Water table is in meters in the new spec...

    //end of soilwat2_set_my_variable()

    #endregion


    #region Soil "Profile" (layered): (Constants & Starting Values from SIM file), and the Outputs

    //Has the soilwat_init() been done? If so, let the fractional soil arrays (eg. sw, sat, dul etc) check the profile
    //layers when a "set" occurs. If not, save reset values so they can be applied if a reset event is sent.
    bool initDone = false;
    //If doing a reset, we don't want to check profile layer data until ALL the various values have been reset. This flag
    //tells us whether we're doing a reset; if so, we can skip checking.
    bool inReset = false;

    //SIM file gets them from .APSIM file

    //Soilwat2Parameters   //sv- also from soilwat2_soil_profile_param()

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Volumetric fraction of coarse soil fragments (>2 mm)")]
    private double[] coarse_fragments = null;

    private double[] _dlayer = null;
    [Param(MinVal = 0.0, MaxVal = 10000.0)]
    [Units("mm")]
    [Output]
    [Description("Thickness of soil layer")]
    public double[] dlayer    //! thickness of soil layer (mm)
    {
        get { return _dlayer; }
        set
        {
            if (!initDone)
            {
                reset_dlayer = new double[value.Length];
                Array.Copy(value, reset_dlayer, value.Length);
            }

            int num_layers = value.Length;
            //resize all the arrays if they changed the number of layers
            if (_dlayer == null || num_layers != _dlayer.Length)
            {
                Array.Resize(ref _air_dry_dep, num_layers);
                Array.Resize(ref _dul_dep, num_layers);
                Array.Resize(ref _ll15_dep, num_layers);
                Array.Resize(ref _sat_dep, num_layers);
                Array.Resize(ref _sw_dep, num_layers);
                Array.Resize(ref root_water_uptake, num_layers);
                Array.Resize(ref _dlayer, num_layers);
                Array.Resize(ref bd, num_layers);
                Array.Resize(ref es_layers, num_layers);
                Array.Resize(ref flow, num_layers);
                Array.Resize(ref flux, num_layers);
                Array.Resize(ref outflow_lat, num_layers);
                Array.Resize(ref coarse_fragments, num_layers);
                Array.Resize(ref k_dul_dep, num_layers);
                Array.Resize(ref k_ll15_dep, num_layers);
                Array.Resize(ref ks, num_layers);
                Array.Resize(ref psi_dul_dep, num_layers);
                //also resize for all solutes in this simulation.
                for (int solnum = 0; solnum < num_solutes; solnum++)
                {
                    Array.Resize(ref solutes[solnum].amount, num_layers);
                    Array.Resize(ref solutes[solnum].retained, num_layers);
                    Array.Resize(ref solutes[solnum].leach, num_layers);
                    Array.Resize(ref solutes[solnum].ts_leach, num_layers);
                    Array.Resize(ref solutes[solnum].up, num_layers);
                    Array.Resize(ref solutes[solnum].delta, num_layers);
                    Array.Resize(ref solutes[solnum].ts_delta, num_layers);
                }
            }

            for (int layer = 0; layer < _dlayer.Length; layer++)
            {
                //If you change the depths of the layer then you need to modify the water "_dep" variables by the same amount. (they are in mm too)
                //If you don't do this, you will have the same amount of water that is now in a shallower layer, 
                //therefore you will have a different fraction equivalent variables, the ones without the "_dep" eg. sw, dul.  
                double fract = MathUtility.Divide(value[layer], _dlayer[layer], 0.0);
                _air_dry_dep[layer] = _air_dry_dep[layer] * fract;
                _dul_dep[layer] = _dul_dep[layer] * fract;
                _ll15_dep[layer] = _ll15_dep[layer] * fract;
                _sat_dep[layer] = _sat_dep[layer] * fract;
                _sw_dep[layer] = _sw_dep[layer] * fract;

                k_dul_dep[layer] = k_dul;  // FOR NOW - use a constant value for all layers
                k_ll15_dep[layer] = k_ll15;  // FOR NOW - use a constant value for all layers
                psi_dul_dep[layer] = psi_dul; // FOR NOW - use a constant value for all layers

                //_dlayer[layer] = value[layer];

                soilwat2_check_profile(layer);
            }

            Array.Copy(value, _dlayer, num_layers);

            if (initDone)
            {
                PublishNew_Profile();
                CalcRunoffDepthFactor(out runoff_wf);
            }
        }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Saturated water content for layer")]
    public double[] sat       //! saturated water content for layer (bulk)
    {
        get
        {
            int num_layers = _dlayer.Length;
            Array.Resize(ref coarse_fragments, num_layers);  // TEMPORARY - until parameter inputs are set a bit better
            double[] _sat = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                _sat[layer] = MathUtility.Divide(_sat_dep[layer], _dlayer[layer], 0.0) * (1.0 - coarse_fragments[layer]);
            return _sat;
        }
        set
        {
            if (!initDone)
            {
                reset_sat = new double[value.Length];
                Array.Copy(value, reset_sat, value.Length);
            }
            else
            {
                //* made settable to allow for erosion
                int num_layers = _dlayer.Length;
                for (int layer = 0; layer < num_layers; layer++)
                {
                    _sat_dep[layer] = MathUtility.Divide(value[layer], 1.0 - coarse_fragments[layer], 0.0) * _dlayer[layer];   //change sat_dep NOT sat. The sat variable is just for inputting and outputting and is immediately converted to sw_dep.
                    soilwat2_check_profile(layer);
                }
            }
        }
    }


    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Drained upper limit soil water content for each soil layer")]
    public double[] dul       //! drained upper limit soil water content for each soil layer (bulk)
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] _dul = new double[num_layers];
            //* made settable to allow for erosion
            for (int layer = 0; layer < num_layers; layer++)
                _dul[layer] = MathUtility.Divide(_dul_dep[layer], _dlayer[layer], 0.0) * (1.0 - coarse_fragments[layer]);
            return _dul;
        }
        set
        {
            if (!initDone)
            {
                reset_dul = new double[value.Length];
                Array.Copy(value, reset_dul, value.Length);
            }
            else
            {
                //* made settable to allow for erosion
                int num_layers = _dlayer.Length;
                for (int layer = 0; layer < num_layers; layer++)
                {
                    _dul_dep[layer] = MathUtility.Divide(value[layer], 1.0 - coarse_fragments[layer], 0.0) * _dlayer[layer];   //change dul_dep NOT dul. The dul variable is just for inputting and outputting and is immediately converted to sw_dep.
                    soilwat2_check_profile(layer);
                }
            }
        }
    }

    private int numvals_sw = 0;                        //! number of values returned for sw 
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Soil water content of layer")]
    public double[] sw        //! soil water content of layer (bulk)
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] _sw = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                _sw[layer] = MathUtility.Divide(_sw_dep[layer], _dlayer[layer], 0.0);
            return _sw;
        }
        set
        {
            if (!initDone)
            {
                reset_numvals_sw = value.Length;
                reset_sw = new double[value.Length];
                Array.Copy(value, reset_sw, value.Length);
            }

            else
            {
                double[] sw_dep_old;
                double sw_dep_lyr, sw_dep_delta_sum;
                sw_dep_old = _sw_dep;
                soilwat2_zero_default_variables();
                int num_layers = _dlayer.Length;
                sw_dep_delta_sum = 0.0;
                for (int layer = 0; layer < num_layers; layer++)
                {
                    sw_dep_lyr = value[layer] * _dlayer[layer];   //sw_dep = sw * dlayer
                    sw_dep_delta_sum = sw_dep_delta_sum + (sw_dep_lyr - sw_dep_old[layer]);   //accumulate the change in the entire soil profile.
                    _sw_dep[layer] = sw_dep_lyr;  //change sw_dep NOT sw. The sw variable is just for inputting and outputting and is immediately converted to sw_dep.    
                    soilwat2_check_profile(layer);
                }
                PublishExternalMassFlow(sw_dep_delta_sum);     //tell the "System Balance" module (if there is one) that the user has changed the water by this amount.
                numvals_sw = value.Length;          //used in soilwat2_set_default()
            }
        }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("15 bar lower limit of extractable soil water for each soil layer")]
    public double[] ll15      //! 15 bar lower limit of extractable soil water for each soil layer (bulk)
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] _ll15 = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                _ll15[layer] = MathUtility.Divide(_ll15_dep[layer], _dlayer[layer], 0.0) * (1.0 - coarse_fragments[layer]);
            return _ll15;
        }
        set
        {
            if (!initDone)
            {
                reset_ll15 = new double[value.Length];
                Array.Copy(value, reset_ll15, value.Length);
            }
            else
            {
                //* made settable to allow for erosion
                int num_layers = _dlayer.Length;
                for (int layer = 0; layer < num_layers; layer++)
                {
                    _ll15_dep[layer] = MathUtility.Divide(value[layer], 1.0 - coarse_fragments[layer], 0.0) * _dlayer[layer];   //change ll15_dep NOT dul. The dll15 variable is just for inputting and outputting and is immediately converted to sw_dep.
                    soilwat2_check_profile(layer);
                }
            }
        }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Air dry soil water content")]
    public double[] air_dry   //! air dry soil water content (bulk)
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] _air_dry = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                _air_dry[layer] = MathUtility.Divide(_air_dry_dep[layer], _dlayer[layer], 0.0) * (1.0 - coarse_fragments[layer]);
            return _air_dry;
        }
        set
        {
            if (!initDone)
            {
                reset_air_dry = new double[value.Length];
                Array.Copy(value, reset_air_dry, value.Length);
            }
            else
            {
                //* made settable to allow for erosion  
                int num_layers = _dlayer.Length;
                for (int layer = 0; layer < num_layers; layer++)
                {
                    _air_dry_dep[layer] = MathUtility.Divide(value[layer], 1.0 - coarse_fragments[layer], 0.0) * _dlayer[layer];   //change air_dry_dep NOT dul. The air_dry variable is just for inputting and outputting and is immediately converted to sw_dep.
                    soilwat2_check_profile(layer);
                }
            }
        }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("/d")]
    [Output]
    [Description("Soil water conductivity constant")]
    private double[] swcon;     //! soil water conductivity constant (1/d) //! ie day**-1 for each soil layer

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Impermeable soil layer indicator")]
    private double[] mwcon = null;     //! impermeable soil layer indicator

    [Output]
    [Description("Flag to determine if Ks has been chosen for use")]
    private bool using_ks;       //! flag to determine if Ks has been chosen for use. //sv- set in soilwat2_init() by checking if mwcon exists

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm/d")]
    [Output]
    [Description("Saturated conductivity")]
    private double[] ks = null;        //! saturated conductivity (mm/d)

    [Param(MinVal = 0.01, MaxVal = 3.0)]
    [Units("g/cm^3")]
    [Output]
    [Description("Bulk density of soil")]
    private double[] bd;   //! dry bulk density of soil (g/cm^3) // ??? Is this "moist" or "dry"; how moist?

    //sv- Lateral Flow profile   //sv- also from Lateral_read_param()

    [Param(IsOptional = true, MinVal = 0, MaxVal = 1.0e3F)] //1.0e3F = 1000
    [Units("mm/d")]
    [Output]
    private double[] klat = null;


    private double[] _sat_dep;
    [Output]
    [Units("mm")]
    [Description("Sat * dlayer")]
    public double[] sat_dep   // soil water at saturation in fine earth
    {
        get { return _sat_dep; }
        set
        {
            //* made settable to allow for erosion
            _sat_dep = new double[value.Length];
            Array.Copy(value, _sat_dep, value.Length);
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    private double[] _dul_dep;
    [Output]
    [Units("mm")]
    [Description("dul * dlayer")]
    public double[] dul_dep   // soil water at drained upper limit in fine earth
    {
        get { return _dul_dep; }
        set
        {
            //* made settable to allow for erosion
            _dul_dep = new double[value.Length];
            Array.Copy(value, _dul_dep, value.Length);
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    private double[] starting_sw_dep;
    private double[] _sw_dep;
    [Output]
    [Units("mm")]
    [Description("sw * dlayer")]
    public double[] sw_dep    // current soil water in bulk soil
    {
        get { return _sw_dep; }
        set
        {
            soilwat2_zero_default_variables();
            numvals_sw = value.Length;          //used in soilwat2_set_default()
            _sw_dep = new double[value.Length];
            Array.Copy(value, _sw_dep, value.Length);
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }

            //TODO: External Mass Flow event should be triggered just like for sw. Same should go for dlt_sw and dlt_sw_dep.
        }
    }


    private double[] _ll15_dep;
    [Output]
    [Units("mm")]
    [Description("ll15 * dlayer")]
    public double[] ll15_dep  // soil water at -15 bars in fine earth
    {
        get { return _ll15_dep; }
        set
        {
            //* made settable to allow for erosion
            _ll15_dep = new double[value.Length];
            Array.Copy(value, _ll15_dep, value.Length);
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }


    private double[] _air_dry_dep;
    [Output]
    [Units("mm")]
    [Description("air_dry * dlayer")]
    public double[] air_dry_dep  // soil water when air dry, in fine earth
    {
        get { return _air_dry_dep; }
        set
        {
            //* made settable to allow for erosion
            _air_dry_dep = new double[value.Length];
            Array.Copy(value, _air_dry_dep, value.Length);
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    [Output]
    [Units("g/cm^3")]
    [Description("Bulk density of \"fine earth\" fraction")]
    private double[] bd_fine
    {
        get
        {
            double[] result = new double[bd.Length];
            for (int layer = 0; layer < bd.Length; layer++)
                result[layer] = MathUtility.Divide(bd[layer] - specific_bd * coarse_fragments[layer],
                                               1.0 - coarse_fragments[layer], bd[layer]);
            return result;
        }
    }

    [Output]
    [Units("mm/mm")]
    [Description("Soil water content, expressed on a \"fine earth\" volume basis")]
    private double[] sw_fine
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] result = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                result [layer] = MathUtility.Divide(_sw_dep[layer], _dlayer[layer] * (1.0 - coarse_fragments[layer]), 0.0);
            return result;
        }
    }

    [Output]
    [Units("mm/mm")]
    [Description("Saturated water content of \"fine earth\" fraction")]
    private double[] sat_fine
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] result = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                result[layer] = MathUtility.Divide(_sat_dep[layer], _dlayer[layer], 0.0);
            return result;
        }
    }

    [Output]
    [Units("mm/mm")]
    [Description("Drained upper limit water content of \"fine earth\" fraction")]
    private double[] dul_fine
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] result = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                result[layer] = MathUtility.Divide(_dul_dep[layer], _dlayer[layer], 0.0);
            return result;
        }
    }

    [Output]
    [Units("mm/mm")]
    [Description("15 bar lower limit water content of \"fine earth\" fraction")]
    private double[] ll15_fine
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] result = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                result[layer] = MathUtility.Divide(_ll15_dep[layer], _dlayer[layer], 0.0);
            return result;
        }
    }

    [Output]
    [Units("mm/mm")]
    [Description("Air dry soil water content of \"fine earth\" fraction")]
    private double[] air_dry_fine
    {
        get
        {
            int num_layers = _dlayer.Length;
            double[] result = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                result[layer] = MathUtility.Divide(_air_dry_dep[layer], _dlayer[layer], 0.0);
            return result;
        }
    }

    [Output]
    [Units("mm")]
    [Description("Depth of water moving from layer i+1 into layer i because of unsaturated flow; (positive value indicates upward movement into layer i) (negative value indicates downward movement (mm) out of layer i)")]
    private double[] flow;        //sv- Unsaturated Flow //! depth of water moving from layer i+1 into layer i because of unsaturated flow; (positive value indicates upward movement into layer i) (negative value indicates downward movement (mm) out of layer i)
    private double[] ts_flow;     // Flow within a sub-day timestep

    [Output]
    [Units("mm")]
    [Description("Initially, water moving downward into layer i (mm), then water moving downward out of layer i (saturated flow)")]
    private double[] flux;       //sv- Drainage (Saturated Flow) //! initially, water moving downward into layer i (mm), then water moving downward out of layer i (mm)\
    private double[] ts_flux;    // Flux within a sub-day timestep

    [Output]
    [Units("mm")]
    [Description("flow_water[layer] = flux[layer] - flow[layer]")]
    private double[] flow_water         //flow_water[layer] = flux[layer] - flow[layer] 
    {
        get
        {
            int num_layers = flow.Length;
            double[] water_flow = new double[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
                water_flow[layer] = flux[layer] - flow[layer];
            return water_flow;
        }
    }

    [Output]
    [Units("mm")]
    [Description("Drainage rate into water table")]
    private double drain_wt;          

    //Soilwat2Globals

    //soilwat2_on_new_solute event handler

    //Lateral Flow profile     //sv- also from Lateral_Send_my_variable()

    [Output]
    [Units("mm")]
    [Description("Lateral outflow")]
    private double[] outflow_lat;   //! outflowing lateral water   //lateral outflow
    private double[] ts_outflow_lat;
    //end


    #endregion


    #region Set My Variables (Let other modules change me) (these are sort of like a [Param]'s but after the start of the simulation)

    //These are the sets for ficticious variables that actually set other variables.

    [Output]
    [Units("mm")]
    public double[] dlt_dlayer
    {
        set
        {
            int num_layers = value.Length;
            for (int layer = 0; layer < num_layers; layer++)
            {
                //If you change the depths of the layer then you need to modify the water "_dep" variables by the same amount. (they are in mm too)
                //If you don't do this, you will have the same amount of water that is now in a shallower layer, 
                //therefore you will have a different fraction equivalent variables, the ones without the "_dep" eg. sw, dul.  
                double fract = MathUtility.Divide((_dlayer[layer] + value[layer]), _dlayer[layer], 0.0);
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
                Array.Resize(ref root_water_uptake, num_layers);
                Array.Resize(ref _dlayer, num_layers);
                Array.Resize(ref bd, num_layers);
                Array.Resize(ref es_layers, num_layers);
                Array.Resize(ref flow, num_layers);
                Array.Resize(ref flux, num_layers);
                Array.Resize(ref outflow_lat, num_layers);
                Array.Resize(ref coarse_fragments, num_layers);
                Array.Resize(ref k_dul_dep, num_layers);
                Array.Resize(ref k_ll15_dep, num_layers);
                Array.Resize(ref ks, num_layers);
                Array.Resize(ref psi_dul_dep, num_layers);
                //also resize for all solutes in this simulation.
                for (int solnum = 0; solnum < num_solutes; solnum++)
                {
                    Array.Resize(ref solutes[solnum].amount, num_layers);
                    Array.Resize(ref solutes[solnum].retained, num_layers);
                    Array.Resize(ref solutes[solnum].leach, num_layers);
                    Array.Resize(ref solutes[solnum].ts_leach, num_layers);
                    Array.Resize(ref solutes[solnum].up, num_layers);
                    Array.Resize(ref solutes[solnum].delta, num_layers);
                    Array.Resize(ref solutes[solnum].ts_delta, num_layers);
                }

                for (int layer = num_layers; layer < _dlayer.Length; layer++)
                {
                    k_dul_dep[layer] = k_dul;  // FOR NOW - use a constant value for all layers
                    k_ll15_dep[layer] = k_ll15;  // FOR NOW - use a constant value for all layers
                    psi_dul_dep[layer] = psi_dul; // FOR NOW - use a constant value for all layers
                }
            }

            CalcRunoffDepthFactor(out runoff_wf);
            PublishNew_Profile();
        }
    }

    [Output]
    [Units("0-1")]
    public double[] dlt_sw
    {
        set
        {
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
                root_water_uptake[layer] -= value[layer] * _dlayer[layer];
        }
    }

    [Output]
    [Units("mm")]
    public double[] dlt_sw_dep
    {
        set
        {
            int num_layers = _dlayer.Length;
            for (int layer = 0; layer < num_layers; layer++)
                root_water_uptake[layer] -= value[layer];
        }
    }


    [Output]
    [Units("mm")]
    public double[] root_water_uptake;
    public double[] rwu;
    #endregion


    //DAILY INPUTS FROM OTHER MODULES

    #region [INPUTS]

    //taken from soilwat2_get_residue_variables()

    [Input(IsOptional = true)]
    [Units("0-1")]
    private double surfaceom_cover = 0.0;

    //end of soilwat2_get_residue_variables()

    //taken from soilwat2_get_environ_variables()

    //from met module
    //Runon is specified in a met file or sparse data file
    [Input(IsOptional = true)]
    [Units("mm/d")]
    private double runon = 0.0;      //! external run-on of H2O (mm/d)

    [Input(IsOptional = true)]
    [Units("mm")]
    private double snow = 0.0;      //! water content of snow falling during the day

    //from crop modules  
    //used in runoff(as part of TotalInterception parameter) and in infilitration
    [Output]
    [Input(IsOptional = true)]
    [Units("mm")]
    private double interception = 0.0;      //! canopy interception loss (mm)

    //from surface organic matter module
    //used in runoff(as part of TotalInterception parameter) and in infilitration
    [Output]
    [Input(IsOptional = true)]
    [Units("mm")]
    private double residueinterception = 0.0;     //residue interception loss (mm)

    //end of soilwat2_get_environ_variables()


    //taken from Lateral_process()

    //from met module
    //Inflow is specified in a met file or sparse data file
    [Input(IsOptional = true)]
    [Units("mm")]
    private double[] inflow_lat;       //! inflowing lateral water

    //end of Lateral_process()

    [Input(IsOptional = true)]
    [Units("m")]
    [Param(MinVal= 0.0)]
    private double piezometric_head;

    [Input]
    DateTime Today;

    #endregion


    #region Get Variables from other Modules (if need to do stuff AFTER inputting)



    private void GetCropVariables()
    {
        //also called in prepare event as well

        //*     Get crop Variables

        if (num_crops != MyPaddock.Crops.Count)
        {
            num_crops = MyPaddock.Crops.Count;
            Array.Resize(ref cover_green, num_crops);
            Array.Resize(ref cover_tot, num_crops);
            Array.Resize(ref canopy_height, num_crops);
        }

        for (int i = 0; i < num_crops; i++)
        {
            // Set to zero, in case the Get fails...
            cover_green[i] = 0.0;
            cover_tot[i] = 0.0;
            canopy_height[i] = 0.0;
            MyPaddock.Get(MyPaddock.Crops[i].FullName + ".cover_green", out cover_green[i]);
            MyPaddock.Get(MyPaddock.Crops[i].FullName + ".cover_tot", out cover_tot[i]);
            MyPaddock.Get(MyPaddock.Crops[i].FullName + ".height", out canopy_height[i]);
        }
    }


    private void GetSoluteVariables()
    {
        //for the number of solutes that was read in by OnNewSolute event handler)
        for (int solnum = 0; solnum < num_solutes; solnum++)
        {
            double[] Value;
            string propName;
            if (solutes[solnum].ownerName != "")
                propName = solutes[solnum].ownerName + "." + solutes[solnum].name;
            else
                propName = solutes[solnum].name;
            if (MyPaddock.Get(propName, out Value))
            {
                // Should check array size here to be sure it matches...
                Array.Copy(Value, solutes[solnum].amount, Math.Min(Value.Length, solutes[solnum].amount.Length));


                // We need to somehow initialize the amount of "retained" solute. 
                // As a quick and dirty hack, we'll initially assume all of the solute is retained.
                if (MathUtility.Sum(solutes[solnum].retained) == 0.0)
                {
                    for (int layer = 0; layer < solutes[solnum].amount.Length; layer++ )
                        solutes[solnum].retained[layer] = solutes[solnum].amount[layer];
                }

            }
        }
    }

    //this is called in the On Process event handler
    //it just calls all the methods above.
    private void GetOtherVariables()
    {
        GetCropVariables();
        GetSoluteVariables();
    }

    #endregion

    //CHANGE OTHER MODULES

    #region Link to allow you to directly set Module variables

    //nb. you have to add a reference to DotNetProxies.dll (soon to be CSDotNetProxies.dll) for this to work.

    //used in SetModuleSolutes()
    [Link]
    Paddock MyPaddock = null;

    #endregion


    #region Set Variables in other Modules (Solute model mainly)

    private void SetModuleSolutes()
    {
        //taken from soilwat2_set_other_variables()

        //for all solutes in this simulation.
        for (int solnum = 0; solnum < num_solutes; solnum++)
        {
            //convert to float array
            int num_layers = solutes[solnum].leach.Length;
            float[] temp_dlt_solute = new float[num_layers];
            for (int layer = 0; layer < num_layers; layer++)
            {
                temp_dlt_solute[layer] = (float)((layer > 0 ? solutes[solnum].leach[layer - 1] : 0) - solutes[solnum].leach[layer]);
                if (irrigation > 0.0 && layer == irrigation_layer)
                    temp_dlt_solute[layer] += (float)solutes[solnum].irrigation;
            }

            //set the change in solutes for the modules
            string propName;
            if (solutes[solnum].ownerName != "")
                propName = solutes[solnum].ownerName + ".dlt_" + solutes[solnum].name;
            else
                propName = "dlt_" + solutes[solnum].name;
            MyPaddock.Set(propName, temp_dlt_solute);
        }
    }

    //this is called in the On Process event handler
    private void SetOtherVariables()
    {

       SetModuleSolutes();

        //! Send a runoff event to the system
        if (runoff > 0.0)
        {
            RunoffEventType r = new RunoffEventType(); //! structure holding runoff event
            r.runoff = (float)runoff;
            if (Runoff != null)
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
    private double rain;         //! precipitation (mm/d)
    private double radn;         //! solar radiation (mj/m^2/day)
    private double mint;         //! minimum air temperature (oC)
    private double maxt;         //! maximum air temperature (oC)

    //RUNOFF
    //r double      cover_surface_runoff;
    //r double runoff;
    //who put this in? double      eff_rain; 
    private double runoff_pot;       //! potential runoff with no pond(mm)
    private double obsrunoff;         //! observed runoff (mm)

    //GET CROP VARIABLES
    //private int[]       crop_module = new int[max_crops];             //! list of modules replying 
    private double[] cover_tot = null;     //! total canopy cover of crops (0-1)
    private double[] cover_green = null;   //! green canopy cover of crops (0-1)
    private double[] canopy_height = null; //! canopy heights of each crop (mm)
    private int num_crops = 0;                //! number of crops ()

    //TILLAGE EVENT
    private double tillage_cn_red;   //! reduction in CN due to tillage ()   //can either come from the manager module or from the sim file
    private double tillage_cn_rain;  //! cumulative rainfall below which tillage reduces CN (mm) //can either come from the manager module orh the sim file
    private double tillage_rain_sum; //! cumulative rainfall for tillage CN reduction (mm)

    //EVAPORATION
    private int year;         //! year
    private int day;          //! day of year
    private double sumes1;       //! cumulative soil evaporation in stage 1 (mm)
    private double sumes2;       //! cumulative soil evaporation in stage 2 (mm)
    //r double      t;
    private double eo_system;         //! eo from somewhere else in the system //sv- see eo_source
    //r double eo;
    private double real_eo;                  //! potential evapotranspiration (mm) 
    //r double eos;
    private double[] es_layers = null;     //! actual soil evaporation (mm)
    private double[] ts_es_layers;


    //r double drain;
    //r double infiltration;


    //SOLUTES
    //OnNewSolute
    private struct Solute
    {
        public string name;        // Name of the solute
        public string ownerName;    // FQN of the component handling this solute
        public double[] amount;    // total amount of solute in each layer (kg/ha) - a state variable, but officially held in another component
        public double[] retained;  // amount of solute in "retained" water in each layer (kg/ha) - STATE VARIABLE
        public double[] leach;     // amount leached from each layer (kg/ha) over the full day
        public double[] ts_leach;  // amount leached from each layer (kg/ha) in the current time step
        public double[] up;        // amount "upped" from each layer (kg/ha)
        public double[] delta;     // change in solute in each layer (kg/ha) over the full day
        public double[] ts_delta;  // change in solute in each layer (kg/ha) in the current time step
        public double rain_conc;   // concentration entering via rainfall (ppm)
        public double irrigation;  // amount of solute in irrigation water (kg/ha)
        public int get_flow_id;    // registration ID for getting flow values
        public int get_leach_id;    // registration ID for getting leach value
    };

    private Solute[] solutes;
    int num_solutes = 0;


    //IRRIGATION
    private double irrigation;       //! irrigation (mm)                                                 

    //r double pond_evap;
    //r double pond;
    //r double water_table;
    //r double[] sws;

    private double oldSWDep;


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

    private double[] b;  // Exponent for diffusivity calculations
    double[] beta_micro = null;
    double[] beta_macro = null;
    double[] k_sat_macro = null;
    double[] runoff_wf;


    #endregion


    //MODEL

    #region Functions to Zero Variables


    private void ZeroArray(ref double[] A)
    {
        for (int i = 0; i < A.Length; i++)
        {
            A[i] = 0.0;
        }
    }

    private void soilwat2_zero_variables()
    {

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

        ZeroArray(ref _dlayer);                   //! thickness of soil layer i (mm)
        ZeroArray(ref _sat_dep);                  //! saturated water content for layer l (mm water)
        ZeroArray(ref _dul_dep);                  //! drained upper limit soil water content for each soil layer (mm water)
        ZeroArray(ref _sw_dep);                   //! soil water content of layer l (mm)
        ZeroArray(ref _ll15_dep);                 //! 15 bar lower limit of extractable soil water for each soil layer(mm water)
        ZeroArray(ref _air_dry_dep);              //! air dry soil water content (mm water)

        _water_table = 0.0;                      //! water table depth (mm)

        //Outputs
        drain_wt = 0.0;                            //! drainage rate from bottom layer (cm/d)
        infiltration = 0.0;                     //! infiltration (mm)
        runoff = 0.0;                           //! runoff (mm)

        pond = 0.0;                             //! surface ponding depth (mm)
        pond_evap = 0.0;                        //! evaporation from the pond surface (mm)
        eo = 0.0;                               //! potential evapotranspiration (mm)
        eos = 0.0;                              //! pot sevap after modification for green cover & residue wt
        t = 0.0;                                //! time after 2nd-stage soil evaporation begins (d)
        cn2_new = 0.0;                          //! New cn2  after modification for crop cover & residue cover
        cover_surface_runoff = 0.0;             //! effective total cover (0-1)
        ZeroArray(ref flow);                     //! depth of water moving from layer l+1
        //! into layer l because of unsaturated
        //! flow; positive value indicates upward
        //! movement into layer l, negative value
        //! indicates downward movement (mm) out of layer l
        ZeroArray(ref flux);                     //! initially, water moving downward into layer l (mm), 
        //then water moving downward out of layer l (mm)
        ZeroArray(ref es_layers);                //! actual soil evaporation (mm)

        ZeroArray(ref outflow_lat);

        //Local Variables

        cover_tot = null;                //! total canopy cover of crops (0-1)
        cover_green = null;              //! green canopy cover of crops (0-1)
        canopy_height = null;            //! canopy heights of each crop (mm)
        num_crops = 0;                          //! number of crops ()
        sumes1 = 0.0;                           //! cumulative soil evaporation in stage 1 (mm)
        sumes2 = 0.0;                           //! cumulative soil evaporation in stage 2 (mm)

        for (int sol = 0; sol < num_solutes; sol++)
        {
            ZeroArray(ref solutes[sol].amount);
            ZeroArray(ref solutes[sol].delta);
            ZeroArray(ref solutes[sol].leach);  //! amount of solute leached from each layer (kg/ha)
            ZeroArray(ref solutes[sol].up);     //! amount of solute upped from each layer (kg/ha)
            solutes[sol].rain_conc = 0.0;
            solutes[sol].irrigation = 0.0;
        }

        runoff_pot = 0.0;                       //! potential runoff with no pond(mm)  
        irrigation = 0.0;                       //! irrigation (mm)

        obsrunoff = 0.0;                        //! observed runoff (mm)
        tillage_cn_red = 0.0;                   //! reduction in CN due to tillage ()
        tillage_cn_rain = 0.0;                  //! cumulative rainfall below which tillage reduces CN (mm)
        tillage_rain_sum = 0.0;                 //! cumulative rainfall for tillage CN reduction (mm)
        obsrunoff_name = "";                    //! system name of observed runoff

        eo_system = 0.0;                        //! eo from somewhere else in the system
        _eo_source = "";                        //! system variable name of external eo source

        real_eo = 0.0;                          //! eo determined before any ponded water is evaporated (mm)

        irrigation_layer = 0;                   //! trickle irrigation input layer
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
        ZeroArray(ref _sw_dep);
        _profile_esw_depth = 0.0;
        _wet_soil_depth = 0.0;
        _profile_fesw = 0.0;
    }


    private void ZeroDailyVariables()
    {

        //sv- this is exectued in the Prepare event.

        ZeroArray(ref flow);
        ZeroArray(ref flux);
        ZeroArray(ref es_layers);
        ZeroArray(ref root_water_uptake);
        ZeroArray(ref outflow_lat);
        cover_tot = null;
        cover_green = null;
        canopy_height = null;

        eo           = 0.0;
        eos          = 0.0;
        cn2_new      = 0.0;
        drain_wt     = 0.0;
        infiltration = 0.0;
        runoff       = 0.0;
        runoff_pot   = 0.0;
        num_crops    = 0;
        obsrunoff    = 0.0;
        pond_evap    = 0.0;                    //! evaporation from the pond surface (mm)
        real_eo      = 0.0;                      //! eo determined before any ponded water is evaporated (mm)


        //! initialise all solute information
        for (int solnum = 0; solnum < num_solutes; solnum++)
        {
            ZeroArray(ref solutes[solnum].amount);
            ZeroArray(ref solutes[solnum].leach);
            ZeroArray(ref solutes[solnum].up);
            ZeroArray(ref solutes[solnum].delta);
            solutes[solnum].rain_conc = 0.0;
        }

    }


    #endregion


    #region Bounds checking and warning functions
    [Link]
    private Component My = null;  // Get access to "Warning" function

    private void IssueWarning(string warningText)
    {
#if (APSIMX == true)
        Console.WriteLine(warningText);
#else
        My.Warning(warningText);
#endif
    }

    private double bound(double A, double MinVal, double MaxVal)
    {
        //force A to stay between the MinVal and the MaxVal. Set A to the MaxVal or MinVal if it exceeds them.
        if (MinVal > MaxVal)
        {
            IssueWarning("Lower bound " + MinVal + " is > upper bound " + MaxVal + "\n"
                               + "        Variable is not constrained");
            return A;
        }
        else
          return Math.Max(Math.Min(A, MaxVal), MinVal);
    }


    // Unlike u_bound and l_bound, this does not force the variable to be between the bounds. It just warns the user in the summary file.
    protected void bound_check_real_var(double Variable, double LowerBound, double UpperBound, string VariableName)
    {
        string warningMsg;
        if (Variable > UpperBound)
        {
            warningMsg = "The variable: /'" + VariableName + "/' is above the expected upper bound of: " + UpperBound;
            IssueWarning(warningMsg);
        }
        if (Variable < LowerBound)
        {
            warningMsg = "The variable: /'" + VariableName + "/' is below the expected lower bound of: " + LowerBound;
            IssueWarning(warningMsg);
        }
    }

    protected void bound_check_real_array(double[] A, double LowerBound, double UpperBound, string ArrayName, int ElementToStopChecking)
    {
        for (int i = 0; i < ElementToStopChecking; i++)
        {
            bound_check_real_var(A[i], LowerBound, UpperBound, ArrayName + "(" + i + 1 + ")");
        }
    }

    #endregion

    #region Functions to Set Intial SW and Error Check Soil Profile


    #region Set Initial SW values



    //sv- DEAN SAYS THAT THE GUI ALWAYS SPECIFIES A SET OF SW VALUES. THEREFORE YOU DON'T NEED ANY OF THIS CODE TO SET DEFAULTS ANYMORE. ALL OF THIS IS DONE IN THE GUI NOW AND YOU JUST GET GIVE THE SW VALUES FOR EACH LAYER. SO DON'T NEED THIS ANYMORE.
    //Had to uncomment this because it is called in the "set" for the "insoil" property. I don't think any simulation actually does a set on "insoil" though
    //so perhaps I can comment it out and turn "insoil" just into a normal variable that is a [Param].
    //TODO: see if I can comment out the soilwat2_set_default() as per the comments above.

    private double root_proportion(int Layer, double RootDepth)
    {

        //integer    layer                 ! (INPUT) layer to look at
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

        depth_to_layer_bottom = MathUtility.Sum(_dlayer, 0, Layer + 1, 0.0);
        depth_to_layer_top = depth_to_layer_bottom - _dlayer[Layer - 1];
        depth_to_root = Math.Min(depth_to_layer_bottom, RootDepth);

        depth_of_root_in_layer = Math.Max(depth_to_root - depth_to_layer_top, 0.0);
        return MathUtility.Divide(depth_of_root_in_layer, _dlayer[Layer - 1], 0.0);

    }

    private void soilwat2_set_default()
    {

        //*+  Purpose
        //*       set default soil water values when the user does not specify any starting water values.

        int layer;               //! layer number in loop
        int num_layers;          //! number of layers used in profile 
        int last_layer_filled;   //! number of layers filled in profile
        double esw_remaining;       //! esw left after distribution top down (mm)
        //double   depth_remaining;     //! depth left after distribution top down (mm)
        double esw_avail;           //! esw available for distribution (mm)
        double profile_esw_depth_local;   //! depth of esw in profie to fill (mm)
        string line;                //! temp output record



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
                throw new Exception("Insoil, Sw, profile_fesw or wet_soil_depth cannot be specified with \"profile_esw_depth\".");
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
                throw new Exception("Insoil, Profile_fesw or Sw cannot be specified with \"wet_soil_depth\".");
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
            ZeroArray(ref _sw_dep);
            num_layers = _dlayer.Length;

            for (layer = 0; layer < num_layers; layer++)
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
            ZeroArray(ref _sw_dep);
            num_layers = _dlayer.Length;
            Array.Copy(_ll15_dep, _sw_dep, num_layers);

            last_layer_filled = FindLayerNo(_wet_soil_depth);

            for (layer = 0; layer <= last_layer_filled; layer++)
            {
                //! set default according to wet_soil_depth of plant available water
                _sw_dep[layer] = _dul_dep[layer];
            }

            //! adjust last layer
            _sw_dep[last_layer_filled] = _ll15_dep[last_layer_filled]
                                          + (_dul_dep[last_layer_filled] - _ll15_dep[last_layer_filled])
                                          * root_proportion(last_layer_filled + 1, _wet_soil_depth);

            if ((MathUtility.Sum(_dlayer) + precision_sw_dep) < _wet_soil_depth)
            {
                line = "Can't fit wet soil depth of " + _wet_soil_depth + " into profile depth of " + MathUtility.Sum(_dlayer);
                throw new Exception(line);
            }
        }


        else if (numvals_profile_fesw > 0)
        {
            //! profile_fesw parameter set - distribute top down
            ZeroArray(ref _sw_dep);
            num_layers = _dlayer.Length;
            Array.Copy(_ll15_dep, _sw_dep, num_layers);
            profile_esw_depth_local = 0.0;
            for (layer = 0; layer < num_layers; layer++)
                profile_esw_depth_local += _dul_dep[layer] - _ll15_dep[layer];
            profile_esw_depth_local *= _profile_fesw;
            esw_remaining = profile_esw_depth_local;

            for (layer = 0; layer < num_layers; layer++)
            {
                //! set default according to profile_esw_depth of plant available water
                esw_avail = bound(esw_remaining, 0.0, (_dul_dep[layer] - _ll15_dep[layer]));

                _sw_dep[layer] = _ll15_dep[layer] + esw_avail;
                esw_remaining = esw_remaining - esw_avail;
            }

            if (esw_remaining > precision_sw_dep)
            {
                //! we have too much water to distirbute - won't fit in profile
                line = "Can't fit profile esw of " + (profile_esw_depth_local + esw_remaining) + " into profile esw depth of " + profile_esw_depth_local;
                throw new Exception(line);
            }
        }


        else if (numvals_profile_esw_depth > 0)
        {
            //! profile_esw_depth parameter set - distribute top down
            ZeroArray(ref _sw_dep);
            num_layers = _dlayer.Length;
            Array.Copy(_ll15_dep, _sw_dep, num_layers);

            esw_remaining = _profile_esw_depth;

            for (layer = 0; layer < num_layers; layer++)
            {
                //! set default according to profile_esw_depth of plant available water
                esw_avail = bound(esw_remaining, 0.0, (_dul_dep[layer] - _ll15_dep[layer]));

                _sw_dep[layer] = _ll15_dep[layer] + esw_avail;
                esw_remaining = esw_remaining - esw_avail;
            }

            if (esw_remaining > precision_sw_dep)
            {
                //! we have too much water to distirbute - won't fit in profile
                profile_esw_depth_local = 0.0;
                for (layer = 0; layer < num_layers; layer++)
                    profile_esw_depth_local += _dul_dep[layer] - _ll15_dep[layer];
                line = "Can't fit profile esw of " + _profile_esw_depth + " into profile esw depth of " + profile_esw_depth_local;
                throw new Exception(line);
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


        if (canopy_fact.Length != canopy_fact_height.Length)
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
            IssueWarning("Your ini file is set to use an evaporation method other than ritchie(act_evap_method=1)." + "\n"
                                    + "This module: SoilWater can only use ritchie evaporation." + "\n"
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
        if (inReset)
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
            if ((Double.IsNaN(summeru) || (Double.IsNaN(winteru))))
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
        // Need to add 12 hours to move from "midnight" to "noon", or this won't work as expected
        if (DateUtility.WithinDates(winterdate, Today, summerdate))
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
            for (int i = 0; i < mwcon.Length; i++)
                mwcon[i] = 1.0;
        }
        else
        {
            IssueWarning("mwcon is being replaced with a saturated conductivity (ks). " + "\n"
                                    + "See documentation for details.");
        }


        if (ks == null)
        {
            using_ks = false;
            ks = new double[_dlayer.Length];
            ZeroArray(ref ks);
        }
        else
        {
            using_ks = true;
        }


        //for (klat == null) see Lateral_init().


        //If this function has been called by a Reset Event
        //then reset (all the variables that are "Settable" by the user) back to the original values read in by [Param]  
        //soil profile
        bool wasInReset = inReset;
        inReset = true;  // Set "inReset" flag temporarily to suppress profile checking until everything has been set
        if (initDone)
        {
            _dlayer = new double[reset_dlayer.Length];
            Array.Copy(reset_dlayer, _dlayer, reset_dlayer.Length);
        }
        sat = reset_sat;
        dul = reset_dul;
        numvals_sw = reset_numvals_sw;  //used in soilwat2_set_default();
        sw = reset_sw;
        ll15 = reset_ll15;
        air_dry = reset_air_dry;
        inReset = wasInReset; // Restore "inReset" flag

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
            Console.WriteLine("Soil water in parameter file is being overridden by");
            Console.WriteLine("the insoil parameter which is between 0 and 1");
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
            swr_top = MathUtility.Divide((_sw_dep[0] - ll15[0]), (_dul_dep[0] - _ll15_dep[0]), 0.0);
            swr_top = bound(swr_top, 0.0, 1.0);

            //! are we in stage1 or stage2 evap?
            if (swr_top < sw_top_crit)
            {
                //! stage 2 evap
                sumes2 = sumes2_max - (sumes2_max * MathUtility.Divide(swr_top, sw_top_crit, 0.0));
                sumes1 = _u;
                t = Math.Pow((MathUtility.Divide(sumes2, _cona, 0.0)), 2);
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
            klat = new double[_dlayer.Length];

        //taken from Lateral_zero_variables()
        ZeroArray(ref outflow_lat);

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
        int num_layers;

        num_layers = _dlayer.Length;

        if (layer < min_layer)
        {
            error_messg = String.Format("{0} {1} {2} {3}",
                                        " soil layer no. ", layer,
                                        " is below mimimum of ", min_layer);
            IssueWarning(error_messg);
        }
        else if (layer > num_layers)
        {
            error_messg = String.Format("{0} {1} {2} {3}",
                                        " soil layer no. ", layer,
                                        " is above maximum of ", num_layers);
            IssueWarning(error_messg);
        }
    }

    private void soilwat2_check_profile(int layer)
    {
        // TODO: This may need some readjustment in light of the new distinction between "fine earth" fraction and 
        // bulk soil properties

        //*+  Purpose
        //*       checks validity of soil water parameters for a soil profile layer

        //*+  Notes
        //*           reports an error if
        //*           - g%ll15_dep, _dul_dep, and _sat_dep are not in ascending order
        //*           - ll15 is below min_sw
        //*           - sat is above max_sw
        //*           - sw > sat or sw < min_sw      

        if (inReset || !initDone)
            return;

        //Constant Values
        double min_sw_local = 0.0;
        double max_sw_margin = 0.01;

        string err_messg;           //! error message

        double dul_local;                 //! drained upper limit water content of layer (mm water/mm soil)
        double dul_errmargin;       //! rounding error margin for dulc
        double ll15_local;                //! lower limit at 15 bars water content of layer (mm water/mm soil)
        double ll15_errmargin;      //! rounding error margin for ll15c
        double air_dry_local;             //! lower limit at air dry water content of layer (mm water/mm soil)
        double air_dry_errmargin;   //! rounding error margin for air_dryc
        double sat_local;                 //! saturated water content of layer (mm water/mm soil)
        double sat_errmargin;       //! rounding error margin for satc
        double sw_local;                  //! soil water content of layer l (mm water/mm soil)
        double sw_errmargin;        //! rounding error margin for swc

        double max_sw_local;              //! largest acceptable value for sat (mm water/mm soil)

        max_sw_local = 1.0 - MathUtility.Divide(bd_fine[layer], specific_bd, 0.0);  //ie. Total Porosity (of "fine earth" fraction)

        sw_local = MathUtility.Divide(_sw_dep[layer], _dlayer[layer], 0.0);
        sat_local = MathUtility.Divide(_sat_dep[layer], _dlayer[layer], 0.0);
        dul_local = MathUtility.Divide(_dul_dep[layer], _dlayer[layer], 0.0);
        ll15_local = MathUtility.Divide(_ll15_dep[layer], _dlayer[layer], 0.0);
        air_dry_local = MathUtility.Divide(_air_dry_dep[layer], _dlayer[layer], 0.0);

        //TODO: where do these error_margins come from?
        sw_errmargin = error_margin;
        sat_errmargin = error_margin;
        dul_errmargin = error_margin;
        ll15_errmargin = error_margin;
        air_dry_errmargin = error_margin;


        if ((air_dry_local + air_dry_errmargin) < min_sw_local)
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                       " Air dry lower limit of ",
                                       air_dry_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is below acceptable value of ",
                                       min_sw_local);
            IssueWarning(err_messg);
        }


        if ((ll15_local + ll15_errmargin) < (air_dry_local - air_dry_errmargin))
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                       " 15 bar lower limit of ",
                                       ll15_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is below air dry value of ",
                                       air_dry_local);
            IssueWarning(err_messg);
        }



        if ((dul_local + dul_errmargin) <= (ll15_local - ll15_errmargin))
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                       " drained upper limit of ",
                                       dul_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is at or below lower limit of ",
                                       ll15_local);
            IssueWarning(err_messg);
        }

        if ((sat_local + sat_errmargin) <= (dul_local - dul_errmargin))
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                                       " saturation of ",
                                       sat_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is at or below drained upper limit of ",
                                       dul_local);
            IssueWarning(err_messg);
        }

        if ((sat_local - sat_errmargin) > (max_sw_local + max_sw_margin))
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G} {7} {8} {9:G} {10} {11} {12:G})",
                                       " saturation of ",
                                       sat_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is above acceptable value of ",
                                       max_sw_local,
                                       "\n",
                                       "You must adjust bulk density (bd) to below ",
                                       (1.0 - sat_local) * specific_bd,
                                       "\n",
                                       "OR saturation (sat) to below ",
                                       max_sw_local);
            IssueWarning(err_messg);
        }


        if (sw_local - sw_errmargin > sat_local + sat_errmargin)
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                                       " soil water of ",
                                       sw_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is above saturation of ",
                                       sat_local);
            IssueWarning(err_messg);
        }

        if (sw_local + sw_errmargin < air_dry_local - air_dry_errmargin)
        {
            err_messg = String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                                       " soil water of ",
                                       sw_local,
                                       " in layer ",
                                       layer,
                                       "\n",
                                       "         is below air-dry value of ",
                                       air_dry_local);
            IssueWarning(err_messg);
        }

    }


    #endregion


    #endregion


    #region Soil Science Functions

    private void UpdateStateVariables()
    {
        snowpack += time_step * (snow - snowmelt);

        pond += time_step * (rain + snowmelt + (irrigation_layer == 0 ? irrigation : 0.0) - interception +
                runon - ts_runoff - ts_pond_evap - ts_infiltration);

        sumes1 += time_step * Math.Max(-ts_infiltration + MathUtility.Sum(ts_es_layers), -sumes1);

        int num_layers = _dlayer.Length;
        for (int layer = 0; layer < num_layers; layer++)
        {
            _sw_dep[layer] += time_step * ((irrigation_layer == layer ? irrigation : 0.0) + inflow_lat[layer] - ts_outflow_lat[layer]
                + (layer == 0 ? ts_infiltration : ts_flux[layer - 1] + ts_flow[layer - 1]) - ts_flux[layer] - ts_flow[layer] - ts_es_layers[layer]
                - rwu[layer]);
            if (Double.IsNaN(_sw_dep[layer]) || _sw_dep[layer] < 0.0)
                throw new Exception("something is wrong...");
        }
    }

    private int FindLayerNo(double Depth)
    {
        // Find the soil layer in which the indicated depth is located
        // NOTE: The returned layer number is 0-based
        // If the depth is not reached, the last element is used
        double depth_cum = 0.0;
        for (int i = 0; i < _dlayer.Length; i++)
        {
            depth_cum = depth_cum + _dlayer[i];
            if (depth_cum >= Depth)
                return i;
        }
        return _dlayer.Length - 1;
    }


    private void MeltSnow()
    {
        // Process equations, section 4.1 of specification
        double potentialMelt = k_melt * Math.Max(maxt, 0.0);
        snowmelt = Math.Min(snowpack + snow * time_step, potentialMelt);
    }

    #region Runoff

    private void CalcRunoffDepthFactor(out double[] runoff_weight)
    {
        // Calculate WEIGHT[runoff, i] for process equations, section 4.2 of specification
        int num_layers = _dlayer.Length;
        runoff_weight = new double[num_layers];
        double hydrol_depth = Math.Min(hydrol_effective_depth, MathUtility.Sum(_dlayer));
        double ext_param = 0.01; // Andrew' spec uses 0.01, but CREAMS, PERFECT, etc. use the equivalent of 0.0156

        double cum_depth = 0.0;
        for (int layer = 0; layer < num_layers; layer++)
        {
            double exp1 = Math.Min(1.0, MathUtility.Divide(cum_depth, hydrol_depth, 0.0));
            cum_depth = cum_depth + _dlayer[layer];
            double exp2 = Math.Min(1.0, MathUtility.Divide(cum_depth, hydrol_depth, 0.0));

            runoff_weight[layer] = (Math.Pow(ext_param, exp1) - Math.Pow(ext_param, exp2)) / (1.0 - ext_param);
        }
    }

    private void CalcRunoff()
    {
        // Process equations, section 4.2 of specification

        int num_layers = _dlayer.Length;
        double[] backup = new double[num_layers + 1];

        double cover_fract = Math.Min(1.0, MathUtility.Divide(cover_surface_runoff, _cn_cov, 0.0));
        cn2_new = _cn2_bare - (_cn_red * cover_fract);

        double cn1 = MathUtility.Divide(cn2_new, 1.0 + MathUtility.Divide(100.0 - cn2_new, 75.0, 0.0), 0.0);
        double cn3 = MathUtility.Divide(cn2_new, 1.0 - MathUtility.Divide(100.0 - cn2_new, 167.7, 0.0), 0.0);

        double swf_sum = 0.0; //! cn proportional in dry range (dul to ll15)
        for (int layer = 0; layer < num_layers; layer++)
            swf_sum += runoff_wf[layer] * MathUtility.Divide(_sw_dep[layer] - _ll15_dep[layer], _dul_dep[layer] - ll15_dep[layer], 0.0);
        double swf = 1.0 - Math.Max(0.0, Math.Min(swf_sum, 1.0));

        double retain = 254.0 * (MathUtility.Divide(100.0, cn1 + swf * (cn3 - cn1), 1000000.0) - 1.0);
        double ppt = rain + runon + (irrigation_layer == 0 ? irrigation : 0.0) - interception;
        double runoff_base = MathUtility.Divide(MathUtility.Sqr(Math.Max(0.0, ppt - 0.2 * retain)),
                                                ppt + 0.8 * retain, 0.0);

        // We need to find drainage to do the "Backup" calculations
        CalcDrainage();
        backup[num_layers] = 0.0;

        for (int layer = num_layers - 1; layer > 0; layer--)
        {
            backup[layer] = Math.Max(0.0, drain_star[layer - 1] - drain_star[layer] + backup[layer + 1] -
                            MathUtility.Divide(_sat_dep[layer] - _sw_dep[layer], time_step, 0.0));
        }
        backup[0] = Math.Max(0.0, (ppt - runoff_base) - drain_star[0] + (num_layers > 1 ? backup[1] : 0.0) -
                            MathUtility.Divide(_sat_dep[0] - _sw_dep[0], time_step, 0.0));

        ts_infiltration = Math.Max(0.0, ppt - runoff_base - backup[0]);

        ts_runoff = runoff_base + Math.Max(0.0, ppt - runoff_base - ts_infiltration - MathUtility.Divide(max_pond - pond, time_step, 0.0));

        if (_cn2_bare == 0.0) // TEMPORARY HACK to turn off runoff
        {
            ts_infiltration = ppt;
            ts_runoff = 0.0;
        }
    }


    #endregion


    #region Tillage

    private void soilwat2_tillage_addrain(double TotalInterception)
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

        tillage_rain_sum = tillage_rain_sum + rain + runon - TotalInterception;

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


    #region Evaporation

    private void CalcEvaporation()
    {
        if (_cona == 0.0 && _u == 0.0) // Temporary hack to turn off evaporation and enable comparisons with earlier models
            return;
        double cover_tot_sum = 0.0;
        for (int i = 0; i < num_crops; i++)
            cover_tot_sum = 1.0 - (1.0 - cover_tot_sum) * (1.0 - cover_tot[i]);

        ts_pond_evap = Math.Min(pond, eo * Math.Exp(MathUtility.Divide(-1 * cover_tot_sum, k_standing, 0.0)));

        eos = (eo - ts_pond_evap) * Math.Exp(MathUtility.Divide(-1.0 * cover_tot_sum, k_standing, 0.0)) *
            Math.Pow(1.0 - surfaceom_cover, k_residue);

        double ess;

        sumes2 = sumes1 - _u - ts_infiltration;

        if (sumes2 + eos <= 0.0)
            ess = eos;
        else if (sumes2 < 0.0)
            ess = Math.Abs(sumes2) + _cona * Math.Sqrt(1.0 - MathUtility.Divide(Math.Abs(sumes2), eos, 0.0));
        else
        {
            ess = Math.Sqrt(MathUtility.Sqr(sumes2) + MathUtility.Sqr(_cona)) - sumes2;
            t = MathUtility.Sqr(MathUtility.Divide(sumes2, _cona, 0.0));
        }

        int num_layers = _dlayer.Length;
        double depth_sum = 0.0;

        double maxDepth;
        // If the user fails to provide a maximum depth, use the total profile depth
        if (Double.IsNaN(soil_evap_max_depth)) 
            maxDepth = MathUtility.Sum(_dlayer);
        else
            maxDepth = soil_evap_max_depth;

        for (int layer = 0; layer < num_layers; layer++)
        {
            double pow1 = Math.Min(1.0, MathUtility.Divide(depth_sum, maxDepth, 0.0));
            depth_sum += _dlayer[layer];
            double pow2 = Math.Min(1.0, MathUtility.Divide(depth_sum, maxDepth, 0.0));
            double layer_factor = MathUtility.Divide(Math.Pow(0.01, pow1) - Math.Pow(0.01, pow2), 1.0 - 0.01, 0.0);
            ts_es_layers[layer] = Math.Min(layer_factor * ess, Math.Max(0.0, _sw_dep[layer] - _air_dry_dep[layer]));
        }
    }

    private void CalcPotEvapotranspiration()
    {
        //*+  Purpose
        //*       calculate potential evapotranspiration (eo) or get from another module

        //*+  Notes
        //*       Eventually eo will be in a separate module entirely, and
        //*       will appear to soilwat when get_other_varaibles() runs.
        //*       But, for now we use either priestly-taylor, or whatever
        //*       the user specified.

        // We can't really rely on eo being provided from an external source
        // so we need to check whether it is. If it isn't, we can do our own
        // Priestly-Taylor calculation
        eo_system = Double.NaN;
        if (_eo_source != "" &&  My.Get(_eo_source, out eo_system) && !Double.IsNaN(eo_system))
            eo = eo_system;                     //! eo is provided by system

        //else if (My.Get("eo", out eo_system) && !Double.IsNaN(eo_system))
        //    eo = eo_system;

        else
            eo = Priestly_taylor();    //! eo from priestly taylor
    }

    private double Priestly_taylor()
    {
        double albedo;           //! albedo taking into account plant material
        double cover_green_sum;  //! sum of crop green covers (0-1)
        double eeq;              //! equilibrium evaporation rate (mm)
        double wt_ave_temp;      //! weighted mean temperature for the day (oC)

        //*  ******* calculate potential evaporation from soil surface (eos) ******

        //                ! find equilibrium evap rate as a
        //                ! function of radiation, albedo, and temp.

        cover_green_sum = 0.0; //  ApsimUtil.sum_cover_array(cover_green, num_crops); // This is a fallow water balance
        albedo = max_albedo - (max_albedo - salb) * (1.0 - cover_green_sum);

        // ! wt_ave_temp is mean temp, weighted towards max.
        wt_ave_temp = (0.60 * maxt) + (0.40 * mint);

        eeq = radn * 23.8846 * (0.000204 - 0.000183 * albedo) * (wt_ave_temp + 29.0);

        //! find potential evapotranspiration (eo) from equilibrium evap rate
        return (eeq * soilwat2_eeq_fac());
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


    #endregion

    #region Drainage (Saturated Flow)

    double[] drain_star;
    double[] cond;   // hydraulic conductiviy

    private void CalcWaterTable()
    {
        // From Section 4.4 of specification
        int num_layers = _dlayer.Length;

        waterTable = mm2m * MathUtility.Sum(_dlayer);  // Initially set water table to maximum depth of the profile

        // Is the rate of drainage out of the profile neglible?
        if (drain_star[num_layers - 1] <= thdr_wt || coeff_water_table == 0.0)
        {
            drain_wt = 0.0;
        }
        else
        {
            // NEED TO TEST THIS LOGIC!
            int layer;
            for (layer = num_layers - 1; layer >= 0; layer--)
            {
                waterTable = mm2m * (MathUtility.Sum(_dlayer, 0, layer, 0.0) + 
                                     _dlayer[layer] * Math.Min(1.0, MathUtility.Divide(_sat_dep[layer] - _sw_dep[layer], _sat_dep[layer] - _dul_dep[layer], 0.0)));

                drain_wt = coeff_water_table * (piezometric_head - waterTable);
                if (drain_star[layer] < drain_star[num_layers - 1] - drain_wt)
                    break;
            } 
        }
    }

    private void CalcDrainageConstants()
    {
        int num_layers = _dlayer.Length;
        beta_micro = new double[num_layers];
        beta_macro = new double[num_layers];
        k_sat_macro = new double[num_layers];
        b = new double[num_layers];
        Array.Resize(ref beta_micro, num_layers);
        Array.Resize(ref beta_macro, num_layers);
        Array.Resize(ref k_sat_macro, num_layers);
        for (int layer = num_layers - 1; layer >= 0; layer--) // Work from the bottom up....
        {
            beta_micro[layer] = MathUtility.Divide(Math.Log((1.0 - prop_cond_dul) * k_dul_dep[layer]) - Math.Log(k_ll15_dep[layer]),
                          Math.Log(_dul_dep[layer]) - Math.Log(_ll15_dep[layer]),
                          0.0);
            k_sat_macro[layer] = Math.Max(1e-10, ks[layer] - k_ll15_dep[layer] *
                          Math.Pow(MathUtility.Divide(_ll15_dep[layer], _sat_dep[layer], 0.0), -beta_micro[layer]));
            beta_macro[layer] = MathUtility.Divide(Math.Log(k_sat_macro[layer]) - Math.Log(prop_cond_dul * k_dul_dep[layer]),
                          Math.Log(_sat_dep[layer]) - Math.Log(_dul_dep[layer]),
                          0.0);
            b[layer] = MathUtility.Divide(Math.Log(Math.Abs(psi_ll15)) - Math.Log(Math.Abs(psi_dul_dep[layer])),
                          Math.Log(_ll15_dep[layer]) - Math.Log(_dul_dep[layer]), 0.0);
        }
    }

    private void CalcDrainage()
    {
        // From Section 4.3 of specification
        int num_layers = _dlayer.Length;
        double[] sat_fines = sat_fine;
        double[] sw_fines = sw_fine;
        drain_star = new double[num_layers];
        cond = new double[num_layers];
        if (ks.Length < num_layers)   // TEMPORARY - until parameter setting is handled better
        {
            int oldLeng = ks.Length;
            Array.Resize(ref ks, num_layers);
            for (int layer = oldLeng; layer < num_layers; layer++)
                ks[layer] = ks[oldLeng - 1];
        }

        for (int layer = num_layers - 1; layer >= 0; layer--) // Work from the bottom up....
        {
            double rel_sat = Math.Min(1.0, MathUtility.Divide(sw_fines[layer], sat_fines[layer], 0.0));
            if (rel_sat <= 0.0) // Handle case where sw hasn't been initialised yet, and is still 0
                cond[layer] = k_ll15_dep[layer];
            else if (k_sat_macro[layer] == 1e-10)  // We've got some ugly parameters - use field capacity value
                cond[layer] = k_dul_dep[layer];
            else
                cond[layer] = k_sat_macro[layer] * Math.Pow(rel_sat, beta_macro[layer]) +
                             (ks[layer] - k_sat_macro[layer]) * Math.Pow(rel_sat, beta_micro[layer]);
            if (_sw_dep[layer] <= _dul_dep[layer])
                drain_star[layer] = 0.0;
            else
                drain_star[layer] = MathUtility.Divide(_sw_dep[layer] - _dul_dep[layer], time_step, 0.0) *
                                    (1.0 - Math.Exp(-(cond[layer] * time_step) / (_sw_dep[layer] - _dul_dep[layer])));
        }

        // Drainage and water table are closely linked
        // We need drain_star to find the water table, 
        // and we may need to know the water table to calculate
        // drainage from the bottom of the profile.
        // Calculate water table and drainage out the bottom of the profile
        CalcWaterTable();
        for (int layer = num_layers - 1; layer >= 0; layer--) // Work from the bottom up....
        {
            if (layer == num_layers - 1)
                ts_flux[layer] = drain_star[layer] + drain_wt;
            else
            {
                ts_flux[layer] = Math.Min(drain_star[layer], drain_star[layer + 1] + 
                                       MathUtility.Divide(_sat_dep[layer + 1] - _sw_dep[layer + 1], time_step, 0.0));
            }
        }
    }

    private void CalcDiffusion()
    {
        // From Section 4.6 of specification
        int num_layers = _dlayer.Length;
        double[] diffusivity = new double[num_layers];
        double[] sw_fines = sw_fine;
        double[] ll15_fines = ll15_fine;
        double[] dul_fines = dul_fine;
        for (int layer = num_layers - 1; layer >= 0; layer--) // Work from the bottom up....
        {
            if (sw_fines[layer] == 0.0)  // Avoid problems if sw hasn't been initialised yet
                diffusivity[layer] = 0.0;
            else
            {
                diffusivity[layer] = 1e3 * cond[layer] * b[layer] *
                                 MathUtility.Divide(psi_ll15, sw_fines[layer], 0.0) *
                                 Math.Pow(MathUtility.Divide(sw_fines[layer], ll15_fines[layer], 0.0), b[layer]);
            }
            if (layer == num_layers - 1 || (_sw_dep[layer] >= _dul_dep[layer] && _sw_dep[layer + 1] >= _dul_dep[layer + 1]))
                ts_flow[layer] = 0.0;
            else
            {
                double geo_mean_diffus = Math.Sqrt(diffusivity[layer] * diffusivity[layer + 1]);
                double water = MathUtility.Divide(sw_fines[layer] - sw_fines[layer + 1],
                                                  (_dlayer[layer] + _dlayer[layer + 1]) / 2.0, 0.0);

                double grav = 0.001 * sw_fines[layer] / (b[layer] * psi_ll15) * Math.Pow(sw_fines[layer] / ll15_fines[layer], -b[layer]);
                water += grav;
                ts_flow[layer] = geo_mean_diffus * water;

                // Not in the spec., but we shouldn't be pulling water from the layer below beyond it's air-dry content
                if (ts_flow[layer] < 0.0) // && _sw_dep[layer + 1] < _air_dry_dep[layer + 1])
                    ts_flow[layer] = Math.Min(0.0, Math.Max(ts_flow[layer],
                                    MathUtility.Divide(_air_dry_dep[layer + 1] - _sw_dep[layer + 1], time_step, 0.0)));
                // Nor should we provide water to the layer below if it takes us beyond air-dry
                if (ts_flow[layer] > 0.0) // && _sw_dep[layer] < _air_dry_dep[layer])
                    ts_flow[layer] = Math.Max(0.0, Math.Min(ts_flow[layer],
                                    MathUtility.Divide(_sw_dep[layer] - _air_dry_dep[layer], time_step, 0.0)));
            }
        }
    }

    #endregion

    #region Solute
                  
    private void CalcSoluteMovement()
    {
        int num_layers = _dlayer.Length;
        double[] leach_1_from_above = new double[num_solutes];
        double[] leach_2_from_above = new double[num_solutes];
        if (irrigation > 0.0)
          IrrigSolute(); // Add any solute coming from irrigation

        for (int layer = 0; layer < num_layers; layer++)
        {
            double prop_conv_1 = 0.0;
            double prop_conv_2 = 0.0;
            double prop_leach_1 = 0.0;
            double prop_leach_2 = 0.0;
            double ratio_mobile = 0.0;
            if (flux[layer] != 0.0)
            {
                //double psi_excl = -310;  // Need some values here. What are likely values? -310 is air-dry, which should be close to what we need
                //double excl = _ll15_dep[layer] * Math.Pow(MathUtility.Divide(psi_excl, psi_ll15, 0.0), MathUtility.Divide(1.0, b[layer], 0.0));
                //double excl = _air_dry_dep[layer]; // Why not take the direct approach?
                double excl = 0.5 * _ll15_dep[layer]; // This follows Addicott and Whitmore - one half of ll15.
                double flux_in;
                if (layer == 0)
                    flux_in = infiltration;
                else
                    flux_in = flux[layer - 1];
                double net_flux = flux_in - flux[layer];
                prop_conv_1 = MathUtility.Divide(Math.Max(0.0, Math.Min(0.5 * flux_in, _dul_dep[layer] - starting_sw_dep[layer])),
                                         Math.Max(0.0, starting_sw_dep[layer] + 0.5 * net_flux - _dul_dep[layer]),
                                         0.0);
                prop_conv_1 = Math.Min(1.0, Math.Max(0.0, prop_conv_1)); // Make sure we're in the range 0-1
                prop_conv_2 = MathUtility.Divide(Math.Max(0.0, Math.Min(0.5 * flux_in, _dul_dep[layer] - (starting_sw_dep[layer] + 0.5 * net_flux))),
                                         Math.Max(0.0, starting_sw_dep[layer] + net_flux - _dul_dep[layer]),
                                         0.0);
                prop_conv_2 = Math.Min(1.0, Math.Max(0.0, prop_conv_2)); // Make sure we're in the range 0-1
                ratio_mobile = MathUtility.Divide(Math.Max(0.0, starting_sw_dep[layer] + 0.5 * net_flux - _dul_dep[layer]),
                                         Math.Max(0.0, starting_sw_dep[layer] + 0.5 * net_flux - excl),
                                         0.0);
                ratio_mobile = Math.Min(1.0, Math.Max(0.0, ratio_mobile)); // Make sure we're in the range 0-1
                prop_leach_1 = MathUtility.Divide(0.5 * flux[layer], 
                                         Math.Max(0.0, starting_sw_dep[layer] - _dul_dep[layer]), 
                                         0.0);
                prop_leach_1 = Math.Min(1.0, Math.Max(0.0, prop_leach_1)); // Make sure we're in the range 0-1
                prop_leach_2 = MathUtility.Divide(0.5 * flux[layer],
                                         Math.Max(0.0, starting_sw_dep[layer] + 0.5 * net_flux - _dul_dep[layer]),
                                         0.0);
                prop_leach_2 = Math.Min(1.0, Math.Max(0.0, prop_leach_2)); // Make sure we're in the range 0-1

                for (int solnum = 0; solnum < num_solutes; solnum++)
                {
                    double beta = 0.1 ; /// NEED TO GET VALUE(S) AS PARAMETERS
                    double mass_retained_0 = Math.Max(0.0, Math.Min(solutes[solnum].amount[layer], solutes[solnum].retained[layer]));
                    double mass_mobile_0 = Math.Max(0.0, solutes[solnum].amount[layer] - solutes[solnum].retained[layer]);
                    double leach_1 = prop_leach_1 * mass_mobile_0;
                    double diffusion = (1.0 - beta) *
                        ((1.0 - ratio_mobile) * (mass_mobile_0 + leach_1_from_above[solnum] - leach_1) - (ratio_mobile * mass_retained_0));
                    double mass_retained_2 = Math.Max(0.0, mass_retained_0 + prop_conv_1 * (mass_mobile_0 + leach_1_from_above[solnum] - leach_1) + diffusion);
                    double mass_mobile_2 = Math.Max(0.0, (1 - prop_conv_1) * (mass_mobile_0 + leach_1_from_above[solnum] - leach_1) - diffusion);
                    double leach_2 = prop_leach_2 * mass_mobile_2;
                    solutes[solnum].delta[layer] = mass_retained_2 + prop_conv_2 * (mass_mobile_2 + leach_2_from_above[solnum] - leach_2) - solutes[solnum].retained[layer];
                    solutes[solnum].leach[layer] = leach_1 + leach_2;
                    leach_1_from_above[solnum] = leach_1;  // Store these values for the next layer's calculations
                    leach_2_from_above[solnum] = leach_2;
                }
            }
        }
    }


    private void IrrigSolute()
    {
        //*+  Mission Statement
        //*      Add solutes with irrigation

        int solnum;     //! solute number counter variable     
        int layer;      //! soil layer

        if (irrigation_layer == 0)   //sv- if user did not enter an irrigation_layer
            layer = 0;             //!addition at surface
        else
            layer = irrigation_layer - 1;

        for (solnum = 0; solnum < num_solutes; solnum++)
        {
            solutes[solnum].amount[layer] += solutes[solnum].irrigation;
        }

    }

    #endregion



    #region Water Table

    private void SetWaterTable(double WaterTable)
    {

        int layer;
        int num_layers;
        double top;
        double bottom;
        double fraction;
        double drainable_porosity;

        num_layers = _dlayer.Length;
        top = 0.0;
        bottom = 0.0;

        for (layer = 0; layer < num_layers; layer++)
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
                fraction = (bottom - WaterTable) / (bottom - top);
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


    private void CalcLateralFlow()
    {
        int num_layers = _dlayer.Length;

        //The user does not have have specify a value for ALL the layers in the soil. Just can specify the layers from the top down to whatever layer they like.
        //Therefore we need to resize the array if they did not specify a value for every layer and then put in zero values for the layers they did not specify.
        if (inflow_lat == null || inflow_lat.Length != _dlayer.Length)
            Array.Resize(ref inflow_lat, _dlayer.Length);

        for (int layer = 0; layer < num_layers; layer++)
        {
            double term1 = klat[layer] * mm2m * _dlayer[layer];
            double term2 = MathUtility.Divide((_sw_dep[layer] + inflow_lat[layer] - _dul_dep[layer]), (_sat_dep[layer] - _dul_dep[layer]), 0.0);
            term2 = Math.Max(0.0, Math.Min(1.0, term2));
            double term3 = MathUtility.Divide(discharge_width, catchment_area, 0.0);
            double term4 = MathUtility.Divide(slope, Math.Sqrt(1.0 + MathUtility.Sqr(slope)), 0.0);
            ts_outflow_lat[layer] = term1 * term2 * term3 * term4;
        }
    }

    #endregion



    //EVENT HANDLERS

    #region Functions used in Event Handlers (mainly in Init, Reset, UserInit, and Write Summary Report Event Handlers)

    //Summary Report & Init2
    private void soilwat2_sum_report()
    {

        //*+  Mission Statement
        //*      Report SoilWat module summary details

        double depth_layer_top;     //! depth to top of layer (mm)
        double depth_layer_bottom;  //! depth to bottom of layer (mm)
        int layer;               //! layer number
        int num_layers;          //! number of soil profile layers
        string line;                //! temp output record
        //double[] runoff_wf;           //! weighting factor for runoff
        double[] usw;                 //! unavail. sw (mm)
        double[] asw;                 //! avail. sw (mm)
        double[] masw;                //! max unavail. sw (mm)
        double[] dsw;                 //! drainable sw (mm)

        num_layers = _dlayer.Length;
        runoff_wf = new double[num_layers];
        usw = new double[num_layers];
        asw = new double[num_layers];
        masw = new double[num_layers];
        dsw = new double[num_layers];

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

        depth_layer_top = 0.0;
        CalcRunoffDepthFactor(out runoff_wf);

        for (layer = 0; layer < num_layers; layer++)
        {
            depth_layer_bottom = depth_layer_top + _dlayer[layer];

            if (!using_ks)
            {
                line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000}",
                                     depth_layer_top,
                                     "-",
                                     depth_layer_bottom,
                                     MathUtility.Divide(_air_dry_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_ll15_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_dul_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_sat_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_sw_dep[layer], _dlayer[layer], 0.0),
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
                                     MathUtility.Divide(_air_dry_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_ll15_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_dul_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_sat_dep[layer], _dlayer[layer], 0.0),
                                     MathUtility.Divide(_sw_dep[layer], _dlayer[layer], 0.0),
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

        num_layers = _dlayer.Length;
        depth_layer_top = 0.0;

        for (layer = 0; layer < num_layers; layer++)
        {
            depth_layer_bottom = depth_layer_top + _dlayer[layer];
            usw[layer] = _ll15_dep[layer];
            asw[layer] = Math.Max((_sw_dep[layer] - _ll15_dep[layer]), 0.0);
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
                             MathUtility.Sum(usw),
                             MathUtility.Sum(asw),
                             MathUtility.Sum(masw),
                             MathUtility.Sum(dsw));

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

        if (obsrunoff_name != "")
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
                Console.WriteLine("       Critical Dates:       Summer        " + summerdate + Environment.NewLine +
                "                             Winter        " + winterdate);
            }
        }
        else
        {
            line = "     Using unknown evaporation method!";
            Console.WriteLine(line);
        }

        Console.WriteLine();


        if (_eo_source != "")
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

        CalcDrainageConstants();

        for (int layer = 0; layer < _dlayer.Length; layer++)
            soilwat2_check_profile(layer);

        //publish event saying there is a new soil profile.
        PublishNew_Profile();

    }


    private void SaveState()
    {
        oldSWDep = Total_sw_dep();
    }


    private void ChangeState()
    {
        double dltSWDep;
        double newSWDep;

        newSWDep = Total_sw_dep();
        dltSWDep = newSWDep - oldSWDep;
        PublishExternalMassFlow(dltSWDep);       //tell the "System Balance" module (if there is one) that the user has changed the water by this amount (by doing a Reset).
    }


    private double Total_sw_dep()
    {
        //only used above in SaveState and ChangeState
        return MathUtility.Sum(_sw_dep);
    }


    #endregion



    #region Clock Event Handlers



    [EventHandler]
    public void OnInitialised()
    {

        initDone = true;     //let the classes properties to now allow "sets"

        day = Today.DayOfYear;
        year = Today.Year;

        //Save State
        SaveState();

        soilwat2_init();

        soilwat2_sum_report();

        //Change State
        ChangeState();
    }



    [EventHandler]
    public void OnPrepare()
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

        day = Today.DayOfYear;
        year = Today.Year;

        Array.Resize(ref rwu, root_water_uptake.Length);
        Array.Copy(root_water_uptake, rwu, root_water_uptake.Length);
        ZeroDailyVariables();

        // We may be the component providing evaporation..
        // In case we are, we do the calculations now
        GetCropVariables();

        //! potential: sevap + transpiration:
        CalcPotEvapotranspiration();
        real_eo = eo;  //! store for reporting
    }


    [EventHandler]
    public void OnProcess()
    {

        // Get variables from other modules
        // Most variables are now set automatically, but there are a few that we will get manually
        // GetOtherVariables();
        // Crop variables (cover) already obtained in OnPrepare
        GetSoluteVariables();

        //*     ===========================================================
        //      subroutine soilwat2_process
        //*     ===========================================================
        //*+  Purpose
        //*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
        //*       evaporation, solute movement, transpiration.

        int num_layers = _dlayer.Length;

        ts_flux = new double[num_layers];
        ts_flow = new double[num_layers];
        ts_es_layers = new double[num_layers];
        ts_outflow_lat = new double[num_layers];

        if (DateUtility.WithinDates(winterdate, Today, summerdate))
        {
            _cona = wintercona;
            _u = winteru;
        }
        else
        {
            _cona = summercona;
            _u = summeru;
        }

        // Store the staring values for _sw_dep
        starting_sw_dep = new double[_sw_dep.Length];
        Array.Copy(_sw_dep, starting_sw_dep, _sw_dep.Length);

        // CalcPotEvapotranspiration();  // Done in OnPrepare

        // For efficiency, we want to use the largest timestep we can get away with, 
        // but a timestep that is too large can result in oscillations or in clearly
        // erroneous outcomes like negative soil water contents. What we need is an
        // "adaptive" timestep - but how can we determine what timestep is appropriate?
        // The key state variable with which we're likely to have problems is the
        // water content of the soil layers. We can begin with a 1 day timestep, see
        // what deltas for soil water arise from that choice, and compare the deltas with
        // the current values. If the deltas are "large" relative to the initial values,
        // or (especially) if they would send values negative (or even much below the air-dry
        // values), we probably need a smaller timestep. A few questions arise: how
        // large a delta is "too large", and how much should the timestep be adjusted with
        // each iteration?

        double maxFactor = 0.01;
        steps_per_day = 1;

        double day_fract = 0.0;
        int nSteps = 0;
        bool newTimeStep;
        //for (int step = 1; step <= steps_per_day; step++)
        while (day_fract < 1.0)
        {
            newTimeStep = false;
            time_step = 1.0 / steps_per_day;

            // LATERAL FLOW
            // 4.5 of specification
            CalcLateralFlow();

            // SNOWMELT
            // 4.1 of specification
            MeltSnow();

            // INFILTRATION AND RUNOFF
            // 4.2, 4.3, and 4.4 of specification
            CalcRunoff(); // This will in turn call CalcDrainage and CalcWaterTable

            // DIFFUSIVE MOVEMENT
            // 4.6 of specification
            CalcDiffusion();

            // EVAPORATION
            // 4.7 of specification
            CalcEvaporation();

            if (steps_per_day < 200)
            {
                for (int layer = 0; layer < num_layers; layer++)
                {
                    double delta = time_step * ((irrigation_layer == layer ? irrigation : 0.0) + inflow_lat[layer] - ts_outflow_lat[layer]
                        + (layer == 0 ? ts_infiltration : ts_flux[layer - 1] + ts_flow[layer - 1]) - ts_flux[layer] - ts_flow[layer] - ts_es_layers[layer]
                        - rwu[layer]);
                    // If we go beyond either air-dry or saturated in any layer, we probably need a smaller timestep.
                    if ((_sw_dep[layer] + delta < _air_dry_dep[layer] - precision_sw_dep) ||
                        (_sw_dep[layer] + delta > _sat_dep[layer] + precision_sw_dep) ||
                        (Math.Abs(delta) > (_sat_dep[layer] - _air_dry_dep[layer]) * maxFactor))
                    {
                        steps_per_day *= 2;
                        newTimeStep = true;
                        break;
                    }
                }
            }

            if (!newTimeStep)
            {
                day_fract += time_step;
                nSteps++;

                for (int layer = 0; layer < num_layers; layer++)
                {
                    flux[layer] += time_step * ts_flux[layer];
                    flow[layer] += time_step * -ts_flow[layer];  // Reverse sign for compatibilty with previous APSIM convention 
                    es_layers[layer] += time_step * ts_es_layers[layer];
                    outflow_lat[layer] += time_step * ts_outflow_lat[layer];
                }

                runoff += time_step * ts_runoff;
                infiltration += time_step * ts_infiltration;
                pond_evap += time_step * ts_pond_evap;
                UpdateStateVariables();
            }

        }

        // SOLUTE MOVEMENT
        // 4.8 of specification
        // Hhmmm. Perhaps this part doesn't require the sub-day timestepping - should be OK if placed outside the timestep loop
        // and then apply daily values. But some caution is needed - I think it needs to work with water contents as they were
        // at the start of the day, rather than what they've been set to now...
        CalcSoluteMovement();

        for (int solnum = 0; solnum < num_solutes; solnum++)
        {
            for (int layer = 0; layer < num_layers; layer++)
            {
                solutes[solnum].retained[layer] = Math.Max(0.0, solutes[solnum].retained[layer] + solutes[solnum].delta[layer]);
            }
        }


        //! now check that the soil water is not silly
        for (int layer = 0; layer < num_layers; layer++)
        {
            soilwat2_check_profile(layer);
        }

        SetOtherVariables();

    }

    [EventHandler]
    public void OnPost()
    {
        //! NIH 180895
        //! in order to continue capturing irrigation information we zero
        //! the value here.  If we zero the value at the beginning of the day
        //! we may zero it after irrigation has already been specified and the
        //! information would be lost.  The safest way is to hold onto the
        //! information until it is used then reset the record.

        irrigation = 0.0;
        for (int solnum = 0; solnum < num_solutes; solnum++)
            solutes[solnum].irrigation = 0.0;
    }

    #endregion


    #region Met, Irrig, Solute, Plants Event Handlers

    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
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

        bound_check_real_var(radn, 0.0, 60.0, "radn");
        bound_check_real_var(maxt, -50.0, 60.0, "maxt");
        bound_check_real_var(mint, -50.0, 50.0, "mint");
        bound_check_real_var(rain, 0.0, 5000.0, "rain");

    }

#if (APSIMX == false)
    public bool getPropertyValue(int propID, ref TPropertyInfo value, bool isReqSet)
    {
        if (isReqSet)  // currently only handling read requests, so fail if this is not.
            return false; 
        for (int solnum = 0; solnum < num_solutes; solnum++)
        {
            if (solutes[solnum].get_leach_id == propID)
            {
                value.setValue(solutes[solnum].leach[solutes[solnum].leach.Length - 1]);
                return true;
            }
            if (solutes[solnum].get_flow_id == propID)
            {
                int num_layers = solutes[solnum].up.Length;
                double[] result = new double[num_layers];
                for (int layer = 0; layer < num_layers; layer++)
                   result[layer] = solutes[solnum].leach[layer] - solutes[solnum].up[layer];
                value.setValue(result);
                return true;
            }
        }
        return false;
    }
#endif

    [EventHandler(EventName="new_solute")]
    public void OnNew_solute(NewSoluteType newsolute)
    {

        //*     ===========================================================
        //      subroutine soilwat2_on_new_solute ()
        //*     ===========================================================

        //"On New Solute" simply tells modules the name of a new solute, what module owns the new solute, and whether it is mobile or immobile.
        //       It alerts you at any given point in a simulation when a new solute is added. 
        //       It does NOT tell you the amount of the new solute in each of the layers. You have to ask the module owner for this separately.


        int sender;
        int counter;
        int numvals;             //! number of values returned
        string name;

        //*- Implementation Section ----------------------------------
#if (APSIMX == true)
        string compName = "";
#else
        string compName = MyPaddock.SiblingNameFromId(newsolute.sender_id);
#endif

        sender = newsolute.sender_id;
        numvals = newsolute.solutes.Length;

        Array.Resize(ref solutes, num_solutes + numvals);

        for (counter = 0; counter < numvals; counter++)
        {
            name = newsolute.solutes[counter].ToLower();
            solutes[num_solutes].name = name;
            solutes[num_solutes].ownerName = compName;
            int nLayers = _dlayer.Length;
            // Create layer arrays for the new solute
            solutes[num_solutes].amount = new double[nLayers];
            solutes[num_solutes].retained = new double[nLayers];
            solutes[num_solutes].delta = new double[nLayers];
            solutes[num_solutes].ts_delta = new double[nLayers];
            solutes[num_solutes].leach = new double[nLayers];
            solutes[num_solutes].ts_leach = new double[nLayers];
            solutes[num_solutes].up = new double[nLayers];
            // Register new "flow" and "leach" outputs for these solutes
            // See "getPropertyValue" function for the callback used to actually retrieve the values
#if (APSIMX == false)
            solutes[num_solutes].get_flow_id = My.RegisterProperty("flow_" + name, "<type kind=\"double\" array=\"T\" unit=\"kg/ha\"/>", true, false, false, "flow of " + name, "", getPropertyValue);
            solutes[num_solutes].get_leach_id = My.RegisterProperty("leach_" + name, "<type kind=\"double\" unit=\"kg/ha\"/>", true, false, false, "leaching of " + name, "", getPropertyValue);
#endif
            num_solutes = num_solutes + 1;
        }
    }

    [EventHandler]
    public void OnIrrigated(IrrigationApplicationType Irrigated)
    {
        //*+  Mission Statement
        //*     Add Water

        double solute_amount;

        //*- Implementation Section ----------------------------------


        //see OnProcess event handler for where this irrigation is added to the soil water 
        irrigation += Irrigated.Amount;  //! amount of irrigation (mm)    

        for (int solnum = 0; solnum < num_solutes; solnum++)
        {
            switch (solutes[solnum].name)
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

            solutes[solnum].irrigation += solute_amount;
        }
    }


    [EventHandler]
    public void OnWaterChanged(WaterChangedType WaterChanged)
    {

        //This event is Only used by Plant2 and AgPasture.
        //This event was added so that the Plant2 module could extract water via its roots from the SoilWater module.
        //At the time Plant2 was not advanced enough to be able to do a "Set" on another modules variables.
        //Plant2 still uses this method to extract water using its roots.

        //*+  Purpose
        //*     Another module wants to change our water


        int layer;

        for (layer = 0; layer < WaterChanged.DeltaWater.Length; layer++)
        {
            root_water_uptake[layer] += WaterChanged.DeltaWater[layer];
            //_sw_dep[layer] = _sw_dep[layer] + WaterChanged.DeltaWater[layer];
            //soilwat2_check_profile(layer);
        }

    }


    #endregion

    #region Manager Event Handlers


    [EventHandler]
    public void Onsum_report()
    {
        //Manager module can request that each module write its variables out to the summary file. This handles that event. 
        soilwat2_sum_report();
    }



    [EventHandler]
    public void OnReset()
    {
        //nb. this is (almost) the same as OnUserInit Event

        inReset = true; // Temporarily turn of profile checking, until we've reset all values
        //Save State
        SaveState();
        soilwat2_zero_variables();
        GetOtherVariables();
        soilwat2_init();

        //Change State
        ChangeState();
        inReset = false;
    }


    //OnUserInit is no longer supported. It has been replaced by the OnReset() above.


    [EventHandler]
    public void OnTillage(TillageType Tillage)
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
        string message;             //! message string
        string tillage_type;             //! name of implement used for tillage//! 1. Find which implement was used. eg. disc, burn, etc.


        //*- Implementation Section ----------------------------------

        // cn_red is the reduction in the cn value, and cn_rain is the amount of rainfall after the tillage event that the reduction ceases to occur.

        //the event always gives us at least the type of tillage. Even if it does not give the cn_red and cn_rain.
        //if the event does not give us cn_red and cn_rain then use the type name to look up the values in the sim file (ini file).

        tillage_type = Tillage.type;       //sv - the event always gives us at least this.

        //TODO: finish writing the code to get the entire tillage table from the ini file (sim file) and look through it to find the values for our particular tillage type.

        //sv- if the Tilliage information did not come with the event.
        if ((Tillage.cn_red == 0) || (Tillage.cn_rain == 0))
        {
            Console.WriteLine();
            Console.WriteLine("    - Reading tillage CN info");

            if (type_info_from_sim.Length != 2)
            {
                //sv- Event did not give us the tillage information and the sim file does not have the tillage information.
                //! We have an unspecified tillage type
                tillage_cn_red = 0.0;
                tillage_cn_rain = 0.0;

                message = "Cannot find info for tillage:- " + Tillage.type;
                throw new Exception(message);
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
        tillage_cn_red = bound(tillage_cn_red, 0.0, _cn2_bare);

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


    private float[] ToFloatArray(double[] D)
    {
        float[] f = new float[D.Length];
        for (int i = 0; i < D.Length; i++)
            f[i] = (float)D[i];
        return f;
    }


    private void PublishNew_Profile()
    {
        //*+  Mission Statement
        //*     Advise other modules of new profile specification

        NewProfileType newProfile = new NewProfileType();
        int nLayers = _dlayer.Length;
        // Convert array values from doubles to floats
        newProfile.air_dry_dep = ToFloatArray(_air_dry_dep);
        newProfile.bd = ToFloatArray(bd);
        newProfile.dlayer = ToFloatArray(_dlayer);
        newProfile.dul_dep = ToFloatArray(_dul_dep);
        newProfile.ll15_dep = ToFloatArray(_ll15_dep);
        newProfile.sat_dep = ToFloatArray(_sat_dep);
        newProfile.sw_dep = ToFloatArray(_sw_dep);
        if (newProfile != null)
            New_profile.Invoke(newProfile);
    }


    private void PublishExternalMassFlow(double sw_dep_delta_sum)
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
        massBalanceChange.C = 0.0F;
        massBalanceChange.N = 0.0F;
        massBalanceChange.P = 0.0F;
        massBalanceChange.SW = Math.Abs((float)sw_dep_delta_sum);

        if (ExternalMassFlow != null)
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
