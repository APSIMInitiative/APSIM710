using System;
using System.Collections.Generic;
using ModelFramework;
using CSGeneral;
using System.Xml.Serialization;
using CMPServices;

///<summary>
/// .NET port of the Fortran SoilWat model
/// Ported by Shaun Verrall Mar 2011
/// Extended by Eric Zurcher Mar 2012
/// Modified by Rogerio Cichota, Mar 2016
///</summary>

[XmlType("SoilWater.NET")]
public class SoilWater
{
    #region Links to allow you to directly access other module's variables

    [Link]
    public Clock Clock;

    [Link]
    public MetFile MetFile;

    [Link]
    Paddock MyPaddock = null;

    [Link]
    private Component My = null;

    #endregion

    #region Events sent by this Module

    //Events
    [Event]
    public event NewProfileDelegate New_profile;

    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;

    [Event]
    public event RunoffEventDelegate Runoff;

    [Event]
    public event NitrogenChangedDelegate NitrogenChanged;

    #endregion

    #region Module parameters

    [Output]
    [Param(MinVal = 0.0, MaxVal = 10.0)]
    [Units("oC")]
    [Description("Temperature below which eeq decreases")]
    public double min_crit_temp = 5.0;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 50.0)]
    [Units("oC")]
    [Description("Temperature above which eeq increases")]
    public double max_crit_temp = 35.0;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Maximum bare soil albedo, 100% green crop cover")]
    public double max_albedo = 0.23;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Coefficient for the reduction on evaporation due to residue cover")]
    public double A_to_evap_fact = 0.44;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 10.0)]
    [Units("0-10")]
    [Description("Coefficientfor the reduction on evaporation due to canopy")]
    public double canopy_eos_coef = 1.7;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Critical sw ratio in top layer below which stage 2 evaporation occurs")]
    public double sw_top_crit = 0.9;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Upper limit of sumes1")]
    public double sumes1_max = 100;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Upper limit of sumes2")]
    public double sumes2_max = 25;

    [Output] 
    [Param(MinVal = 0.0, MaxVal = 1.0)] 
    [Units("0-1")] 
    [Description("Efficiency of moving solute with flow below DUL")] 
    public double[] solute_flow_eff = {1.0};

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Efficiency of moving solute with flow above DUL")]
    public double[] solute_flux_eff =  { 1.0 };

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Gradient due to hydraulic differentials")]
    public double gravity_gradient = 0.00002;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("g/cm^3")]
    [Description("Reference bulk density")]
    public double specific_bd = 2.65;

    [Output]
    [Param(MinVal = 1.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Hydrologically effective depth for runoff")]
    public double hydrol_effective_depth = 450;

    [Output]
    [Param]
    [Description("Names of all possible mobile solutes")]
    public string[] mobile_solutes;

    [Output]
    [Param]
    [Description("Names of all possible immobile solutes")]
    public string[] immobile_solutes;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Canopy factors for cover runoff effect")]
    public double[] canopy_fact = {1, 1, 0, 0};

    [Output]
    [Param(MinVal = 0.0, MaxVal = 100000.0)]
    [Units("mm")]
    [Description("Heights for canopy factors")]
    public double[] canopy_fact_height = {0, 600, 1800, 30000};

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Default canopy factor in absence of height")]
    public double canopy_fact_default = 0.5;

    [Output]
    [Param]
    [Description("Actual soil evaporation model being used")]
    public string act_evap_method = "ritchie";


    [Link]
    public SoilWatTillageType SoilWatTillageType;

    [Param(IsOptional = true, MinVal = 0, MaxVal = 100)]
    [Output]
    [Units ("yes/no")]
    [Description("Irrigation will runoff like rain")]
    private string irrigation_will_runoff = "no";

    //Irrigation layer
    [Param(IsOptional = true, MinVal = 0, MaxVal = 100)]
    [Output]
    [Description("Number of soil layer to which irrigation water is applied (where top layer == 1)")]
    private int irrigation_layer;

    #endregion

    #region Model input parameters

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
    [Description("Diffusivity constant for water movement below DUL")]
    public double diffus_const = Double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Slope for relationshipe between water diffusivity and soil water content")]
    public double diffus_slope = Double.NaN;

    #region Runoff paramters

    [Output]
    [Param(IsOptional = true, Name = "observed_runoff")]
    [Description("System variable name of external observed runoff source")]
    private string obsrunoff_name = "";

    private double _cn2_bare = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 100.0)]
    [Description("Curve number input used to calculate daily runoff")]
    public double cn2_bare
    {
        get { return _cn2_bare; }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                reset_cn2_bare = value;
            }
            _cn2_bare = value;
        }
    }

    private double _cn_red = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Maximum reduction in cn2_bare due to cover")]
    public double cn_red
    {
        get { return _cn_red; }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                reset_cn_red = value;
            }
            _cn_red = value;
        }
    }

    private double _cn_cov = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Cover at which cn_red occurs")]
    public double cn_cov
    {
        get { return _cn_cov; }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                reset_cn_cov = value;
            }
            _cn_cov = value;
        }
    }

    private double _max_pond = 0.0;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Maximum surface storage capacity of soil")]
    public double max_pond
    {
        get { return _max_pond; }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                reset_max_pond = value;
            }
            _max_pond = value;
        }
    }

    #endregion

    #region Evaporation parameters

    private string _eo_source = "";
    [Output]
    [Param(IsOptional = true)]
    [Description("System variable name of external eo source")]
    public string eo_source
    {
        get { return _eo_source; }
        set
        {
            _eo_source = value;
            Console.WriteLine("Eo source set to: " + _eo_source);
        }
    }

    [Param(MinVal = 0.0001, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Bare soil albedo")]
    public double salb;

    private double _u = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 40.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation")]
    public double u
    {
        get { return _u; }
        set { _u = value; }
    }

    private double _cona = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient")]
    public double cona
    {
        get { return _cona; }
        set { _cona = value; }
    }

    [Output]
    [Param(IsOptional = true)]
    [Description("Date for start of summer evaporation (dd-mmm)")]
    public string summerdate = "not set";

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 40.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation during summer")]
    public double summeru = Double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient during summer")]
    public double summercona = Double.NaN;

    //winter
    [Output]
    [Param(IsOptional = true)]
    [Description("Date for start of winter evaporation (dd-mmm)")]
    public string winterdate = "not set";

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation during winter")]
    public double winteru = Double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient during winter")]
    public double wintercona = Double.NaN;

    #endregion

    #region Lateral Flow parameters

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Slope of terrain for lateral flow calculations")]
    public double slope = Double.NaN;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0e8F)]
    [Units("m")]
    [Description("Basal width of discharge area")]
    public double discharge_width = Double.NaN;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0e8F)]
    [Units("m^2")]
    [Description("Area over which lateral flow is occuring")]
    public double catchment_area = Double.NaN;

    #endregion

    #region Basic soil description

    private double[] dLayer;
    private double[] resetDLayer;
    private int nLayers;
    [Param(MinVal = 0.0, MaxVal = 10000.0)]
    [Units("mm")]
    [Output(Immutable = true)]
    [Description("Thickness of soil layers")]
    public double[] dlayer
    {
        get { return dLayer; }
        set
        {
            nLayers = value.Length;
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetDLayer = new double[nLayers];
                Array.Copy(value, resetDLayer, nLayers);
            }

            //resize all the arrays if they changed the number of layers
            if (dLayer == null || nLayers != dLayer.Length)
            {
                ResizeProfileArrays();
            }

            Array.Copy(value, dLayer, nLayers);
            for (int layer = 0; layer < nLayers; layer++)
            {
                //If you change the depths of the layer then you need to modify the water "_dep" variables by the same amount. (they are in mm too)
                //If you don't do this, you will have the same amount of water that is now in a shallower layer, 
                //therefore you will have a different fraction equivalent variables, the ones without the "_dep" eg. sw, dul.  
                double fract = MathUtility.Divide(value[layer], dLayer[layer], 0.0);
                swSAT[layer] = swSAT[layer] * fract;
                swDUL[layer] = swDUL[layer] * fract;
                swLL15[layer] = swLL15[layer] * fract;
                swAirDry[layer] = swAirDry[layer] * fract;
                swAmount[layer] = swAmount[layer] * fract;

                soilwat2_check_profile(layer);
            }

            if (initDone)
                soilwat2_New_Profile_Event();
        }
    }

    private double[] resetSAT;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("mm/mm")]
    [Output(Immutable = true)]
    [Description("Saturated water content for layer")]
    public double[] sat
    {
        get
        {
            double[] _sat = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _sat[layer] = MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0);
            return _sat;
        }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetSAT = new double[value.Length];
                Array.Copy(value, resetSAT, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swSAT[layer] = value[layer] * dLayer[layer];
                soilwat2_check_profile(layer);
            }
        }
    }

    private double[] resetDUL;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("mm/mm")]
    [Output(Immutable = true)]
    [Description("Drained upper limit soil water content for each soil layer")]
    public double[] dul
    {
        get
        {
            double[] _dul = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _dul[layer] = MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0);
            return _dul;
        }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetDUL = new double[value.Length];
                Array.Copy(value, resetDUL, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swDUL[layer] = value[layer] * dLayer[layer];
                soilwat2_check_profile(layer);
            }
        }
    }

    private double[] resetLL15;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("mm/mm")]
    [Output(Immutable = true)]
    [Description("15 bar lower limit of extractable soil water for each soil layer")]
    public double[] ll15
    {
        get
        {
            double[] _ll15 = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _ll15[layer] = MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0);
            return _ll15;
        }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetLL15 = new double[value.Length];
                Array.Copy(value, resetLL15, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swLL15[layer] = value[layer] * dLayer[layer];
                soilwat2_check_profile(layer);
            }
        }
    }

    private double[] resetAirDry;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("mm/mm")]
    [Output(Immutable = true)]
    [Description("Air dry soil water content")]
    public double[] air_dry
    {
        get
        {
            double[] _air_dry = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _air_dry[layer] = MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0);
            return _air_dry;
        }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetAirDry = new double[value.Length];
                Array.Copy(value, resetAirDry, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swAirDry[layer] = value[layer] * dLayer[layer];
                soilwat2_check_profile(layer);
            }
        }
    }

    private int numvals_sw = 0;   //! number of values returned for sw
    private double[] resetSoilWater;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("mm/mm")]
    [Output]
    [Description("Soil water content of layer")]
    public double[] sw
    {
        get
        {
            double[] _sw = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _sw[layer] = MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0);
            return _sw;
        }
        set
        {
            if (!initDone)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                reset_numvals_sw = value.Length;
                resetSoilWater = new double[value.Length];
                Array.Copy(value, resetSoilWater, value.Length);
            }

            double[] sw_dep_old;
            double sw_dep_lyr, sw_dep_delta_sum;
            sw_dep_old = swAmount;
            soilwat2_zero_default_variables();
            sw_dep_delta_sum = 0.0;
            for (int layer = 0; layer < nLayers; layer++)
            {
                sw_dep_lyr = value[layer] * dLayer[layer];   //sw_dep = sw * DLayer
                sw_dep_delta_sum = sw_dep_delta_sum + (sw_dep_lyr - sw_dep_old[layer]);   //accumulate the change in the entire soil profile.
                swAmount[layer] = sw_dep_lyr;  //change sw_dep NOT sw. The sw variable is just for inputting and outputting and is immediately converted to sw_dep.    
                soilwat2_check_profile(layer);
            }
            if (initDone)
                SendExternalMassFlow(sw_dep_delta_sum);     //tell the "System Balance" module (if there is one) that the user has changed the water by this amount.
            numvals_sw = value.Length;          //used in soilwat2_set_default()
        }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("/d")]
    [Output(Immutable = true)]
    [Description("Soil water conductivity constant")]
    public double[] swcon;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Output(Immutable = true)]
    [Description("Impermeable soil layer indicator")]
    public double[] mwcon;

    [Output]
    [Description("Flag to determine if Ks has been chosen for use")]
    private bool using_ks;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Units("mm/d")]
    [Output(Immutable = true)]
    [Description("Saturated conductivity")]
    public double[] ks;

    [Param(MinVal = 0.01, MaxVal = 3.0)]
    [Units("g/cm^3")]
    [Output(Immutable = true)]
    [Description("Bulk density of soil")]
    public double[] bd;

    [Param(IsOptional = true, MinVal = 0, MaxVal = 10000.0)]
    [Units("mm/d")]
    [Output(Immutable = true)]
    public double[] klat;

    #endregion

    #region Initial water content


    //sv- initial sw section (5 different methods to choose from) (see soilwat2_init() and soilwat2_set_default() to see which method is used)
    //insoil is used for two different initial sw methods:
    //1. User Specified Soil Water Content method is used when insoil > 1 
    //2. Fill every layer in the soil to the same specified fraction of esw (specified by insoil)  (0 <= insoil <= 1) 
    private int numvals_insoil = 0;                    //! number of values returned for insoil
    private double _insoil = Double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Switch describing how initial soil water is specified")]
    public double insoil
    {
        get { return _insoil; }
        set
        {
            if (!initDone)
            {
                //if we are reading in the [Param], because this variable is "settable" it can be changed from this value, 
                //therefore store a copy so if there is a Reset event we can set it back to this value.
                reset_numvals_insoil = 1;
                reset_insoil = value;
            }
            else
            {
                soilwat2_zero_default_variables();
                numvals_insoil = 1;
                _insoil = value;
                soilwat2_set_default();
                for (int layer = 0; layer < nLayers; layer++)
                {
                    soilwat2_check_profile(layer);
                }
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
    public double profile_fesw
    {
        get { return _profile_fesw; }
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
            for (int layer = 0; layer < nLayers; layer++)
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
    public double profile_esw_depth
    {
        get { return _profile_esw_depth; }
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
            for (int layer = 0; layer < nLayers; layer++)
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
    public double wet_soil_depth   //! initial depth of soil filled to drained upper limit (field capacity) (mm)
    {
        get { return _wet_soil_depth; }
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
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    #endregion

    #endregion

    #region Model outputs, some settable

    [Output]
    [Units("mm")]
    [Description("Total es")]
    private double es
    { get { return MathUtility.Sum(evaporation); } }

    [Output]
    [Units("mm")]
    [Description("Daily effective rainfall")]
    private double eff_rain
    { get { return MetFile.Rain + runon - runoff - drain; } }

    [Output(Immutable=true)]
    [Units("mm")]
    [Description("Potential extractable sw in profile")]
    private double esw
    {
        get
        {
            if (dLayer == null)
                return 0;
           double result = 0.0;
           for (int layer = 0; layer < nLayers; layer++)
              result += Math.Max(swAmount[layer] - swLL15[layer], 0.0);
           return result;
        }
    }

    [Output]
    [Description("Effective total cover")]
    private double cover_surface_runoff;

    private double timeStage2;

    [Output]
    [Units("d")]
    [Description("time after which 2nd-stage soil evaporation begins")]
    private double t1
    {
        get { return timeStage2; }
    }

    [Output]
    [Units("mm")]
    [Description("Effective potential evapotranspiration")]
    private double eo;

    [Output]
    [Units("mm")]
    [Description("Potential sol evaporation after modification for green cover & residue wt")]
    private double eos;

    [Output]
    [Description("New cn2 after modification for crop cover & residue cover")]
    private double cn2_new;

    [Output]
    [Units("mm")]
    [Description("Drainage rate from bottom layer")]
    private double drain;

    [Output]
    [Units("mm")]
    [Description("Infiltration")]
    private double infiltration;

    [Output]
    [Units("mm")]
    [Description("Runoff")]
    private double runoff;

    [Output]
    [Units("mm")]
    [Description("Evaporation from the surface of the pond")]
    private double pond_evap;

    [Output]
    [Units("mm")]
    [Description("Surface water ponding depth")]
    private double pond;

    private double waterTableDepth = Double.NaN;
    [Output]
    [Units("mm")]
    [Description("Water table depth (depth below the ground surface of the first saturated layer)")]
    public double water_table
    {
        get { return waterTableDepth; }
        set { waterTableDepth = SetWaterTable(value); }
    }

    /// <summary>Water amount at saturation</summary>
    private double[] swSAT;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at saturation")]
    public double[] sat_dep
    {
        get { return swSAT; }
        set
        {
            //* made settable to allow for erosion
            swSAT = new double[value.Length];
            Array.Copy(value, swSAT, value.Length);
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    /// <summary>Water amount at drainage upper limit</summary>
    private double[] swDUL;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at drainage upper limit")]
    public double[] dul_dep
    {
        get { return swDUL; }
        set
        {
            swDUL = new double[value.Length];
            Array.Copy(value, swDUL, value.Length);
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    /// <summary>Water amount at drainage lower limit</summary>
    private double[] swLL15;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at drainage lower limit")]
    public double[] ll15_dep
    {
        get { return swLL15; }
        set
        {
            swLL15 = new double[value.Length];
            Array.Copy(value, swLL15, value.Length);
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    /// <summary>Water amount at air dry conditions</summary>
    private double[] swAirDry;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at air dry conditions")]
    public double[] air_dry_dep
    {
        get { return swAirDry; }
        set
        {
            swAirDry = new double[value.Length];
            Array.Copy(value, swAirDry, value.Length);
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilwat2_check_profile(layer);
            }
        }
    }

    /// <summary>Current water amount in the soil</summary>
    private double[] swAmount;
    [Output]
    [Units("mm")]
    [Description("Water amount")]
    public double[] sw_dep
    {
        get { return swAmount; }
        set
        {
            soilwat2_zero_default_variables();
            numvals_sw = value.Length;
            swAmount = new double[value.Length];
            Array.Copy(value, swAmount, value.Length);
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilwat2_check_profile(layer);
            }

            //TODO: External Mass Flow event should be triggered just like for sw. Same should go for dlt_sw and dlt_sw_dep.
        }
    }

    [Output]
    [Units("mm")]
    [Description( "Depth of water moving between layers because of unsaturated flow")]
    private double[] flow
    {
        //positive value indicates upward movement, negative value indicates downward movement
        get { return waterFlowBelowDUL; }
    }

    [Output]
    [Units("mm")]
    [Description("Initially, water moving downward into layer i (mm), then water moving downward out of layer i (saturated flow)")]
    private double[] flux;

    /// <summary>Water amount by passing each layer</summary>
    private double[] waterBypass;
    [Output]
    [Units("mm")]
    [Description("Water moving downward during saturated flow, bypass")]
    private double[] WaterFlowBypass
    {
        get { return waterBypass; }
    }

    /// <summary>Water amount flowing above DUL</summary>
    private double[] waterFlowAboveDUL;
    [Output]
    [Units("mm")]
    [Description("Water moving downward during flow above DUL")]
    private double[] WaterFlowAboveDUL
    {
        get { return waterFlowAboveDUL; }
    }

    /// <summary>Water amount flowing below DUL</summary>
    private double[] waterFlowBelowDUL;
    [Output]
    [Units("mm")]
    [Description("Water moving during flow below DUL (upwards or downwards")]
    private double[] WaterFlowBelowDUL
    {
        get { return waterFlowBelowDUL; }
    }

    [Output]
    [Units("mm")]
    [Description("Total water flow")]
    private double[] flow_water
    {
        get
        {
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = waterBypass[layer] +
                                waterFlowAboveDUL[layer] +
                                waterFlowBelowDUL[layer];
            return result;
        }
    }

    [Output]
    [Units("mm")]
    [Description("Lateral outflow")]
    private double[] outflow_lat;

    #endregion

    #region Settable variables

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
                double fract = MathUtility.Divide((dLayer[layer] + value[layer]), dLayer[layer], 0.0);
                swAirDry[layer] = swAirDry[layer] * fract;
                swDUL[layer] = swDUL[layer] * fract;
                swLL15[layer] = swLL15[layer] * fract;
                swSAT[layer] = swSAT[layer] * fract;
                swAmount[layer] = swAmount[layer] * fract;

                dLayer[layer] = dLayer[layer] + value[layer];

                soilwat2_check_profile(layer);
            }

            // TODO: I don't think this is needed (or even be allowed), if is, what values are used in new layers?
            //resize all the arrays if the number of layers changed
            if (num_layers != nLayers)
            {
                nLayers = num_layers;
                ResizeProfileArrays();
            }

            soilwat2_New_Profile_Event();
        }
    }

    [Output]
    [Units("mm")]
    public double[] dlt_sw
    {
        set
        {
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAmount[layer] = swAmount[layer] + (value[layer] * dLayer[layer]);
                soilwat2_check_profile(layer);
            }
        }
    }

    [Output]
    [Units("mm")]
    public double[] dlt_sw_dep
    {
        set
        {
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAmount[layer] = swAmount[layer] + value[layer];
                soilwat2_check_profile(layer);
            }
        }
    }

    #endregion

    #region Daily inputs from other models

    [Input(IsOptional = true)]
    [Units("0-1")]
    [Description("Residue cover")]
    private double surfaceom_cover = 0.0;

    //runOn is specified in a met file or sparse data file
    [Input(IsOptional = true)]
    [Units("mm/d")]
    [Description("External Run on amount of water")]
    private double runon = 0.0;

    [Input(IsOptional = true)]
    [Units("mm")]
    [Description("water amount as snow falling")]
    private double snow = 0.0;

    [Output]
    [Input(IsOptional = true)]
    [Units("mm")]
    [Description("Water interception by plant canopy")]
    private double interception = 0.0;

    [Output]
    [Input(IsOptional = true)]
    [Units("mm")]
    [Description("Water interception by residues")]
    private double residueinterception = 0.0;

    //Inflow is specified in a met file or sparse data file
    private double[] inflowLateral;
    [Input(IsOptional = true)]
    [Units("mm")]
    [Description("Inflowing lateral water")]
    private double[] inflow_lat
    {
        set
        {
            if (value.Length < nLayers)
            {
                Array.Resize(ref value, nLayers);
            }

            Array.Copy(value, inflowLateral, nLayers);
        }
    }

    #endregion

    #region Get Variables from other modules

    /// <summary>
    /// Gets the value for cover and height from crops
    /// </summary>
    private void GetCropVariables()
    {
        double coverLive;
        double coverTotal;
        double height;
        bool foundCL;
        bool foundCT;
        bool foundH;

        int i = 0;
        foreach (Component Comp in MyPaddock.Crops)
        {
            foundCL = MyPaddock.Get(Comp.FullName + ".canopyGreenCover", out coverLive);
            foundCT = MyPaddock.Get(Comp.FullName + ".canopyTotalCover", out coverTotal);
            foundH = MyPaddock.Get(Comp.FullName + ".Height", out height);

            ////must have at least these three variables to be considered a "crop" component.
            if (foundCL && foundCT && foundH)
                {
                nCrops = i + 1;
                Array.Resize(ref canopyGreenCover, nCrops);
                Array.Resize(ref canopyTotalCover, nCrops);
                Array.Resize(ref canopyHeight, nCrops);
                canopyGreenCover[i] = coverLive;
                canopyTotalCover[i] = coverTotal;
                canopyHeight[i] = height;
                i++;
                }
            else
                {
                throw new Exception("Crop Module: " +  Comp.FullName  + 
                        " is missing one/or more of the following 3 variables (canopyGreenCover, canopyTotalCover, height) " +
                        Environment.NewLine +
                        "These 3 output variables are needed by the SoilWater module (for evaporation, runoff etc.");
                }
        }
    }

    /// <summary>
    /// Gets the amount of existing solutes
    /// </summary>
    private void GetSoluteVariables()
    {
        //for the number of solutes that was read in by OnNewSolute event handler)
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            double[] Value;
            string propName;
            if (solutes[solnum].OwnerName != "")
                propName = solutes[solnum].OwnerName + "." + solutes[solnum].Name;
            else
                propName = solutes[solnum].Name;
            if (MyPaddock.Get(propName, out Value))
              Array.Copy(Value, solutes[solnum].Amount, Math.Min(Value.Length, solutes[solnum].Amount.Length));
        }
    }

    #endregion

    #region Local Variables

    //INITIALIZATION-RESET
    bool initDone = false;
    bool inReset = false;

    //RUNOFF
    private double potentialRunoff;       //! potential runoff with no pond(mm)
    private double observedRunoff;         //! observed runoff (mm)
    private double ExtraRunoff;            // water backup, could not infiltrate

    //GET CROP VARIABLES
    private double[] canopyTotalCover;     //! total canopy cover of crops (0-1)
    private double[] canopyGreenCover;   //! green canopy cover of crops (0-1)
    private double[] canopyHeight; //! canopy heights of each crop (mm)
    private int nCrops;                //! number of crops ()

    //TILLAGE EVENT
    private double tillage_cn_red;   //! reduction in CN due to tillage ()   //can either come from the manager module or from the sim file
    private double tillage_cn_rain;  //! cumulative rainfall below which tillage reduces CN (mm) //can either come from the manager module orh the sim file
    private double tillage_rain_sum; //! cumulative rainfall for tillage CN reduction (mm)

    //EVAPORATION
    private int evapMethod;           // integer representation of act_evap_method   
    private double eoApsim;           // eo from somewhere else in the system //sv- see eo_source
    private double sumes1;            // cumulative soil evaporation in stage 1 (mm)
    private double sumes2;            // cumulative soil evaporation in stage 2 (mm)
    private double[] evaporation;     // actual soil evaporation (mm)

    //SOLUTES
    private List<SoluteInfo> solutes = new List<SoluteInfo>();
    //private SoluteInfo[] solutes;
    int nSolutes;

    //IRRIGATION
    private double irrigation;       //! irrigation (mm)                                                 

    //The following are used to store the original values, and can be used when doing a Reset Event
    //initial starting soil water 
    private int reset_numvals_insoil;
    private int reset_numvals_profile_fesw;
    private int reset_numvals_profile_esw_depth;
    int reset_numvals_wet_soil_depth;
    private double reset_insoil;
    private double reset_profile_fesw;
    private double reset_profile_esw_depth;
    double reset_wet_soil_depth;
    //runoff
    private double reset_cn2_bare;
    private double reset_cn_red; 
    double reset_cn_cov;
    //ponding 
    double reset_max_pond;

    //Soil Profile
    int reset_numvals_sw;

    #endregion

    #region Local constants

    private const int defaultEvapMethod = 1; // that is the Ritchie method
    private const double myEpsilon = 0.000001; // Precision level for sw dep (mm)
    private const double precisionForProfileSW = 0.00001; // Precision for sw dep over all profile(mm)

    #endregion

    //MODEL

    #region Set Initial SW values

    //sv- DEAN SAYS THAT THE GUI ALWAYS SPECIFIES theValue SET OF SW VALUES. THEREFORE YOU DON'T NEED ANY OF THIS CODE TO SET DEFAULTS ANYMORE. ALL OF THIS IS DONE IN THE GUI NOW AND YOU JUST GET GIVE THE SW VALUES FOR EACH LAYER. SO DON'T NEED THIS ANYMORE.
    //Had to uncomment this because it is called in the "set" for the "insoil" property. I don't think any simulation actually does a set on "insoil" though
    //so perhaps I can comment it out and turn "insoil" just into a normal variable that is a [Param].
    //TODO: see if I can comment out the soilwat2_set_default() as per the comments above.

    /// <summary>
    /// Computes the proportion of layer that is between the soil surface and the depth given (0-1)
    /// </summary>
    /// <param name="layer">Layer to be analysed</param>
    /// <param name="theDepth">Depth being tested</param>
    /// <returns>The proportion of layer that is between the soil surface and the given depth</returns>
    private double LayerProportion(int layer, double theDepth)
    {
        double depthAtTopLayer;     //! depth to top of layer (mm)

        depthAtTopLayer = MathUtility.Sum(dLayer, 0, layer, 0.0) - dLayer[layer - 1];
        double result = MathUtility.Divide(theDepth - depthAtTopLayer, dLayer[layer - 1], 0.0);
        return Math.Max(0.0, Math.Min(result, 1.0));
    }

    private void soilwat2_set_default()
    {
        //*       set default soil water values when the user does not specify any starting water values.

        int layer;               //! layer number in loop
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

        //sv- 5. depth of wet soil (filled to dul) method  (same as profile_esw_depth only don't worry about what is available to the plant(profile_esw_depth =  profile_fesw * (the sum of DLayer))
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
            ZeroArray(ref swAmount);

            for (layer = 0; layer < nLayers; layer++)
            {
                //! set default according to insoil fraction of plant-
                //! available water
                swAmount[layer] = swLL15[layer] + (swDUL[layer] - swLL15[layer]) * _insoil;

                soilwat2_layer_check(layer);
                soilwat2_check_profile(layer);
            }
        }


        else if (numvals_wet_soil_depth > 0)
        {
            //! wet_soil_depth parameter set - distribute top down
            ZeroArray(ref swAmount);
            Array.Copy(swLL15, swAmount, nLayers);

            last_layer_filled = FindLayerNo(_wet_soil_depth);

            for (layer = 0; layer <= last_layer_filled; layer++)
            {
                //! set default according to wet_soil_depth of plant available water
                swAmount[layer] = swDUL[layer];
            }

            //! adjust last layer
            swAmount[last_layer_filled] = swLL15[last_layer_filled]
                                          + (swDUL[last_layer_filled] - swLL15[last_layer_filled])
                                          * LayerProportion(last_layer_filled + 1, _wet_soil_depth);

            if ((MathUtility.Sum(dLayer) + precisionForProfileSW) < _wet_soil_depth)
            {
                line = "Can't fit wet soil depth of " + _wet_soil_depth + " into profile depth of " + MathUtility.Sum(dLayer);
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
            ZeroArray(ref swAmount);
            Array.Copy(swLL15, swAmount, nLayers);
            profile_esw_depth_local = 0.0;
            for (layer = 0; layer < nLayers; layer++)
                profile_esw_depth_local += swDUL[layer] - swLL15[layer];
            profile_esw_depth_local *= _profile_fesw;
            esw_remaining = profile_esw_depth_local;

            for (layer = 0; layer < nLayers; layer++)
            {
                //! set default according to profile_esw_depth of plant available water
                esw_avail = bound(esw_remaining, 0.0, (swDUL[layer] - swLL15[layer]));

                swAmount[layer] = swLL15[layer] + esw_avail;
                esw_remaining = esw_remaining - esw_avail;
            }

            if (esw_remaining > precisionForProfileSW)
            {
                //! we have too much water to distirbute - won't fit in profile
                line = "Can't fit profile esw of " + (profile_esw_depth_local + esw_remaining) + " into profile esw depth of " + profile_esw_depth_local;
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
            ZeroArray(ref swAmount);
            Array.Copy(swLL15, swAmount, nLayers);

            esw_remaining = _profile_esw_depth;

            for (layer = 0; layer < nLayers; layer++)
            {
                //! set default according to profile_esw_depth of plant available water
                esw_avail = bound(esw_remaining, 0.0, (swDUL[layer] - swLL15[layer]));

                swAmount[layer] = swLL15[layer] + esw_avail;
                esw_remaining = esw_remaining - esw_avail;
            }

            if (esw_remaining > precisionForProfileSW)
            {
                //! we have too much water to distirbute - won't fit in profile
                profile_esw_depth_local = 0.0;
                for (layer = 0; layer < nLayers; layer++)
                    profile_esw_depth_local += swDUL[layer] - swLL15[layer];
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
    private void CheckParameters()
    {
        // Check that the brokenStick funtion for canopy effects has equal number of X and Y
        if (canopy_fact.Length != canopy_fact_height.Length)
        {
            throw new Exception("No. of canopy_fact coeffs do not match the no. of canopy_fact_height coeffs.");
        }

        // Check that we are using a valid evaporation method
        switch (act_evap_method)
        {
            case "ritchie":
                evapMethod = defaultEvapMethod; //default method = 1, Ritchie
                break;
            case "bs_a":
                evapMethod = 2;
                break;
            case "bs_b":
                evapMethod = 3;
                break;
            case "bs_acs_jd":
                evapMethod = 4;
                break;
            case "rickert":
                evapMethod = 5;
                break;
            case "rwc":
                evapMethod = 6;
                break;
            default:
                evapMethod = defaultEvapMethod;
                break;
        }

        if (evapMethod != defaultEvapMethod)
        {
            evapMethod = defaultEvapMethod;
            My.Warning("The evaporation method is set to other than ritchie (act_evap_method=1)." + "\n" +
                       "SoilWater can only use ritchie evaporation for now, therefore this method has been changed.");
        }

        // Check that we have one value for each layer
        if (solute_flux_eff.Length == 1)
        {
            // we have only one value, use this for all layers
            double theValue = solute_flux_eff[0];
            solute_flux_eff = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                solute_flux_eff[layer] = theValue;
        }
        else if (solute_flux_eff.Length < nLayers)
        {
            // we have some data but it is not complete
            throw new Exception("The number of values give to \'solute_flux_eff\' does not match the number of layers");
        }

        if (solute_flow_eff.Length == 1)
        {
            // we have only one value, use this for all layers
            double theValue = solute_flow_eff[0];
            solute_flow_eff = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                solute_flow_eff[layer] = theValue;
        }
        else if (solute_flow_eff.Length < nLayers)
        {
            // we have some data but it is not complete
            throw new Exception("The number of values give to \'solute_flow_eff\' does not match the number of layers");
        }

        if (inflowLateral == null)
        {
            inflow_lat = new double[nLayers];
        }

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

        if (_cn_red >= _cn2_bare)
        {
            _cn_red = _cn2_bare - 0.00009;
        }



        // The paramters u and cona can either use a single value or two different (for summer and winter).
        if (!initDone)
        {
            if (Double.IsNaN(_u))
            {
                if ((Double.IsNaN(summeru) || (Double.IsNaN(winteru))))
                {
                    throw new Exception(
                        "theValue single value for u OR BOTH values for summeru and winteru must be specified");
                }
                //if they entered two values but they made them the same
                if (summeru == winteru)
                {
                    _u = summeru;
                }
            }
            else
            {
                summeru = _u;
                winteru = _u;
            }

            if (Double.IsNaN(_cona))
            {
                if ((Double.IsNaN(summercona)) || (Double.IsNaN(wintercona)))
                {
                    throw new Exception(
                        "theValue single value for cona OR BOTH values for summercona and wintercona must be specified");
                }
                //if they entered two values but they made them the same.
                if (summercona == wintercona)
                {
                    _cona = summercona; //cona is now no longer null. As if the user had entered a value for cona.
                }
            }
            else
            {
                summercona = _cona;
                wintercona = _cona;
            }

            //summer and winter default dates.
            if (summerdate == "not set")
            {
                summerdate = "1-oct";
            }

            if (winterdate == "not set")
            {
                winterdate = "1-apr";
            }
        }

        //assign u and cona to either sumer or winter values
        // Need to add 12 hours to move from "midnight" to "noon", or this won't work as expected
        if (DateUtility.WithinDates(winterdate, Clock.Today, summerdate))
        {
            _cona = wintercona;
            _u = winteru;
        }
        else
        {
            _cona = summercona;
            _u = summeru;
        }
    }

    private void soilwat2_soil_profile_param()
    {
         Console.WriteLine("   - Reading Soil Profile Parameters");

        //Initialise the Optional Array Parameters (if not read in).
        if (!initDone)
        {
            // This is actual initialisation, establish whether we will use ks
            //  Note: mwcon: 0 is impermeable and 1 is permeable.  if mwcon is not specified then set it to 1
            //   and don't use ks. If it is specified then use mwcon and ks. If there is NO impermeable layer specified,
            //   then mwcon must be set to '1' in all layers by default.
            if (mwcon == null)
            {
                mwcon = new double[nLayers];
                for (int i = 0; i < mwcon.Length; i++)
                    mwcon[i] = 1.0;
            }
            else
            {
                My.Warning("mwcon is being replaced with saturated conductivity (ks). " + "\n"
                                    + "See documentation for details.");
            }

            if (ks == null)
            {
                using_ks = false;
                ks = new double[nLayers];
                ZeroArray(ref ks);
            }
            else
            {
                using_ks = true;
            }
        }
        else
        {
            //If this function has been called by a Reset Event
            //then reset (all the variables that are "Settable" by the user) back to the original values read in by [Param]  
            inReset = true;

            dLayer = new double[resetDLayer.Length];
            Array.Copy(resetDLayer, dLayer, resetDLayer.Length);
            Array.Copy(resetSAT, sat, resetDLayer.Length);
            Array.Copy(resetDUL, dul, resetDLayer.Length);
            Array.Copy(resetLL15, ll15, resetDLayer.Length);
            Array.Copy(resetAirDry, air_dry, resetDLayer.Length);
            Array.Copy(resetSoilWater, sw, resetDLayer.Length);
            numvals_sw = reset_numvals_sw;
            sw = resetSoilWater;
            inReset = false;
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
            //warn the user that their user specified sw is beign overridden.
            Console.WriteLine("Soil water in parameter file is being overridden by");
            Console.WriteLine("the insoil parameter which is between 0 and 1");
            numvals_sw = 0; //change the flag to pretend that no sw values were not read in.
        }
        else
        {
            numvals_insoil = 0;     //change the flag to pretend that no insoil value was read in.
            //sv- isn't this a mistake? what if you want to use a user specifed sw method (ie. insoil > 1). I assume soilwat2_set_default() caters for this.
        }

        //sv- Since you have initialised all the _dep[] profile variables 
        //sv- AND you have got all your numvals flags indicating what initial sw method was selected sorted out
        //sv- now you can set your initial sw for the profile.
        soilwat2_set_default();
     }

    private void soilwat2_evap_init()
    {
        if (evapMethod == defaultEvapMethod)
        {
            // Initialise ritchie evaporation model

            double swr_top; //! stage 2 evaporation occurs ratio available sw potentially available sw in top layer

            //! set up evaporation stage
            swr_top = MathUtility.Divide((swAmount[0] - ll15_dep[0]), (swDUL[0] - swLL15[0]), 0.0);
            swr_top = bound(swr_top, 0.0, 1.0);

            //! are we in stage1 or stage2 evap?
            if (swr_top >= sw_top_crit)
            {
                // stage 1 evap
                sumes2 = 0.0;
                sumes1 = sumes1_max - (sumes1_max * swr_top);
                timeStage2 = 0.0;
            }
            else
            {
                // stage 2 evap
                sumes2 = sumes2_max - (sumes2_max * MathUtility.Divide(swr_top, sw_top_crit, 0.0));
                sumes1 = _u;
                timeStage2 = MathUtility.Sqr(MathUtility.Divide(sumes2, _cona, 0.0));
            }
        }
    }

    private void Lateral_init()
    {
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
            klat = new double[nLayers];

        //taken from Lateral_zero_variables()
        ZeroArray(ref outflow_lat);

        //see CalcLateralFlow() for where daily input inflow_lat[] is initialised if not read in.
    }

    #endregion

    #region Functions to Zero Variables

    private void soilwat2_zero_variables()
    {

        //You only really want to zero, 
        // Ouputs, Local Variables, 
        // and Settable Params (which you are using reset variables to store the original value in)
        //You do not want to zero non Settable Params because there is no way to reread them back in again. 
        //Plus they don't change during the simulation so why bother.  
        //By definition you don't want to reset the module constants. ( except the ones changed in CheckParameters() )


        //Settable Params
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

        ZeroArray(ref dLayer);                   //! thickness of soil layer i (mm)
        ZeroArray(ref swSAT);                  //! saturated water content for layer l (mm water)
        ZeroArray(ref swDUL);                  //! drained upper limit soil water content for each soil layer (mm water)
        ZeroArray(ref swAmount);                   //! soil water content of layer l (mm)
        ZeroArray(ref swLL15);                 //! 15 bar lower limit of extractable soil water for each soil layer(mm water)
        ZeroArray(ref swAirDry);              //! air dry soil water content (mm water)

        waterTableDepth = 0.0;                      //! water table depth (mm)

        //Outputs
        drain = 0.0;                            //! drainage rate from bottom layer (cm/d)
        infiltration = 0.0;                     //! infiltration (mm)
        runoff = 0.0;                           //! runoff (mm)

        pond = 0.0;                             //! surface ponding depth (mm)
        pond_evap = 0.0;                        //! evaporation from the pond surface (mm)
        eo = 0.0;                               //! potential evapotranspiration (mm)
        eos = 0.0;                              //! pot sevap after modification for green cover & residue wt
        timeStage2 = 0.0;                                //! time after 2nd-stage soil evaporation begins (d)
        cn2_new = 0.0;                          //! New cn2  after modification for crop cover & residue cover
        cover_surface_runoff = 0.0;             //! effective total cover (0-1)
        ZeroArray(ref flux);                     //! initially, water moving downward into layer l (mm), 
        ZeroArray(ref waterBypass);
        ZeroArray(ref waterFlowAboveDUL);
        ZeroArray(ref waterFlowBelowDUL);
        ZeroArray(ref evaporation);                //! actual soil evaporation (mm)

        ZeroArray(ref outflow_lat);

        //Local Variables
        canopyTotalCover = null;                //! total canopy cover of crops (0-1)
        canopyGreenCover = null;              //! green canopy cover of crops (0-1)
        canopyHeight = null;            //! canopy heights of each crop (mm)
        nCrops = 0;                          //! number of crops ()
        sumes1 = 0.0;                           //! cumulative soil evaporation in stage 1 (mm)
        sumes2 = 0.0;                           //! cumulative soil evaporation in stage 2 (mm)

        for (int sol = 0; sol < nSolutes; sol++)
        {
            ZeroArray(ref solutes[sol].Amount);
            ZeroArray(ref solutes[sol].Delta);
            ZeroArray(ref solutes[sol].FlowSaturated);
            ZeroArray(ref solutes[sol].FlowAboveDUL);
            ZeroArray(ref solutes[sol].FlowBelowDUL);
            solutes[sol].ConcentrationInRain = 0.0;
            solutes[sol].AmountInIrrigation = 0.0;
        }

        potentialRunoff = 0.0;                       //! potential runoff with no pond(mm)  
        irrigation = 0.0;                       //! irrigation (mm)

        observedRunoff = 0.0;                        //! observed runoff (mm)
        tillage_cn_red = 0.0;                   //! reduction in CN due to tillage ()
        tillage_cn_rain = 0.0;                  //! cumulative rainfall below which tillage reduces CN (mm)
        tillage_rain_sum = 0.0;                 //! cumulative rainfall for tillage CN reduction (mm)
        obsrunoff_name = "";                    //! system name of observed runoff

        eoApsim = 0.0;                        //! eo from somewhere else in the system
        _eo_source = "";                        //! system variable name of external eo source

        irrigation_layer = 0;                   // irrigation applied onto soil surface
    }

    //TODO: this is used by the soilwat2_set_my_variables(). This allows other modules to set soilwat's variables.
    // this is implememented in .NET by declaring a Property with Gets and Sets and making it an INPUT tag. 
    // Nb. that i think you have to use a local variable as a go between as well. See SoilNitrogen [Input] tags with get and set. 
    // Or maybet it is [Output] tags.
    private void soilwat2_zero_default_variables()
    {
        //*     zero default soil water initialisation parameters      
        numvals_insoil = 0;
        numvals_sw = 0;
        numvals_profile_esw_depth = 0;
        numvals_wet_soil_depth = 0;
        numvals_profile_fesw = 0;
        _insoil = 0.0;
        ZeroArray(ref swAmount);
        _profile_esw_depth = 0.0;
        _wet_soil_depth = 0.0;
        _profile_fesw = 0.0;
    }

    private void soilwat2_zero_daily_variables()
    {
        ZeroArray(ref outflow_lat);
        ZeroArray(ref waterBypass);
        ZeroArray(ref waterFlowAboveDUL);
        ZeroArray(ref waterFlowBelowDUL);
        ZeroArray(ref flux);
        ZeroArray(ref evaporation);
        canopyTotalCover = null;
        canopyGreenCover = null;
        canopyHeight = null;
        eo = 0.0;
        eos = 0.0;
        cn2_new = 0.0;
        drain = 0.0;
        infiltration = 0.0;
        runoff = 0.0;
        potentialRunoff = 0.0;
        nCrops = 0;
        observedRunoff = 0.0;
        pond_evap = 0.0;

        //! initialise all solute information
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            ZeroArray(ref solutes[solnum].Amount);
            ZeroArray(ref solutes[solnum].FlowSaturated);
            ZeroArray(ref solutes[solnum].FlowAboveDUL);
            ZeroArray(ref solutes[solnum].FlowBelowDUL);
            ZeroArray(ref solutes[solnum].Delta);
            solutes[solnum].ConcentrationInRain = 0.0;
        }
    }

    #endregion

    #region Bounds checking and warning functions

    #region Check a given layer for Errors

    /// <summary>
    /// Checks that layer lies within the parameterised soil profile
    /// </summary>
    /// <param name="layer">the layer being tested</param>
    private void soilwat2_layer_check(int layer)
    {
        if (layer < 1)
        {
            My.Warning(String.Format("{0} {1} {2} {3}", " soil layer no. ", layer, 
                       " is below mimimum of ", 1));
        }
        else if (layer > nLayers)
        {
            My.Warning(String.Format("{0} {1} {2} {3}", " soil layer no. ", layer,
                       " is above maximum of ", nLayers));
        }
    }

    /// <summary>
    /// Checks validity of soil water parameters for a soil profile layer
    /// </summary>
    /// <param name="layer">the layer being checked</param>
    private void soilwat2_check_profile(int layer)
    {
        //*+  Notes
        //*           reports an error if
        //*           - g%ll15_dep, swDUL, and swSAT are not in ascending order
        //*           - ll15 is below min_sw
        //*           - sat is above max_sw
        //*           - sw > sat or sw < min_sw      

        if (inReset || !initDone)
            return;

        //Constant Values
        double min_sw_local = 0.0;
        double max_sw_margin = 0.01;
        double dul_local;                 //! drained upper limit water content of layer (mm water/mm soil)
        double ll15_local;                //! lower limit at 15 bars water content of layer (mm water/mm soil)
        double air_dry_local;             //! lower limit at air dry water content of layer (mm water/mm soil)
        double sat_local;                 //! saturated water content of layer (mm water/mm soil)
        double sw_local;                  //! soil water content of layer l (mm water/mm soil)
 
        double max_sw_local;              //! largest acceptable value for sat (mm water/mm soil)

        max_sw_local = 1.0 - MathUtility.Divide(bd[layer], specific_bd, 0.0);  //ie. Total Porosity

        sw_local = MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0);
        sat_local = MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0);
        dul_local = MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0);
        ll15_local = MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0);
        air_dry_local = MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0);

        if ((air_dry_local + myEpsilon) < min_sw_local)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " Air dry lower limit of ", air_dry_local,
                       " in layer ", layer, "\n",
                       "         is below acceptable value of ", min_sw_local));
        }

        if ((ll15_local + myEpsilon) < (air_dry_local - myEpsilon))
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " 15 bar lower limit of ", ll15_local,
                       " in layer ", layer, "\n",
                       "         is below air dry value of ", air_dry_local));
        }

        if ((dul_local + myEpsilon) <= (ll15_local - myEpsilon))
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " drained upper limit of ", dul_local,
                       " in layer ", layer, "\n",
                       "         is at or below lower limit of ", ll15_local));
        }
        if ((sat_local + myEpsilon) <= (dul_local - myEpsilon))
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " saturation of ", sat_local,
                       " in layer ", layer, "\n",
                       "         is at or below drained upper limit of ", dul_local));
        }
        if ((sat_local - myEpsilon) > (max_sw_local + max_sw_margin))
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G} {7} {8} {9:G} {10} {11} {12:G})",
                       " saturation of ", sat_local,
                       " in layer ", layer, "\n",
                       "         is above acceptable value of ", max_sw_local, "\n",
                       "You must adjust bulk density (bd) to below ", (1.0 - sat_local) * specific_bd, "\n",
                       "OR saturation (sat) to below ", max_sw_local));
        }

        if (sw_local - myEpsilon > sat_local + myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                       " soil water of ", sw_local,
                       " in layer ", layer, "\n",
                       "         is above saturation of ", sat_local));
        }

        if (sw_local + myEpsilon < air_dry_local - myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                       " soil water of ", sw_local,
                       " in layer ", layer, "\n",
                       "         is below air-dry value of ", air_dry_local));
        }
    }

    #endregion

    /// <summary>
    /// Forces the value to be constrained between to bounds
    /// </summary>
    /// <param name="theValue">The value being tested</param>
    /// <param name="minVal">The lower bound</param>
    /// <param name="maxVal">The upper bound</param>
    /// <returns>The value within bounds</returns>
    private double bound(double theValue, double minVal, double maxVal)
    {
        if (minVal > maxVal)
        {
            My.Warning("Lower bound (" + minVal + ") is greater than the upper bound (" + maxVal + ")\n" + 
                       "        theValue is not constrained");
            return theValue;
        }

        return Math.Max(Math.Min(theValue, maxVal), minVal);
    }

     /// <summary>
    /// Checks whether a values is within bounds, raises a warning if not
    /// </summary>
    /// <param name="theValue">The value being tested</param>
    /// <param name="minVal">The lower bound</param>
    /// <param name="maxVal">The upper bound</param>
    /// <param name="variableName">The name of the variable</param>
    protected void bound_check_real_var(double theValue, double minVal, double maxVal, string variableName)
    {
        string warningMsg;
        if (theValue > maxVal)
        {
            warningMsg = "The variable /'" + variableName + "/' is set to a value (" + theValue +
                         ") above the expected upper bound (" + maxVal +")";
            My.Warning(warningMsg);
        }
        if (theValue < minVal)
        {
            warningMsg = "The variable /'" + variableName + "/' is set to a value (" + theValue +
                         ") below the expected lower bound (" + minVal + ")";
            My.Warning(warningMsg);
        }
    }

    /// <summary>
    /// Checks whether a set of values is within bounds, raises a warning if not
    /// </summary>
    /// <param name="theArray">The array of values being tested</param>
    /// <param name="minVal">The lower bound</param>
    /// <param name="maxVal">The upper bound</param>
    /// <param name="variableName">The name of the variable</param>
    /// <param name="elementToStopChecking">the index of the last element to be checked</param>
    protected void bound_check_real_array(double[] theArray, double minVal, double maxVal, string variableName, int elementToStopChecking)
    {
        for (int i = 0; i < theArray.Length ; i++)
        {
            bound_check_real_var(theArray[i], minVal, maxVal, variableName + "(" + i + 1 + ")");
            if (i == elementToStopChecking)
                i = theArray.Length;
        }
    }

    #endregion

    #region Soil Science Functions

    #region Runoff

    /// <summary>
    /// Gets the potential runoff
    /// </summary>
    /// <param name="precipitation"></param>
    /// <param name="runOn"></param>
    /// <param name="totalInterception"></param>
    /// <returns>Potential runoff amount</returns>
    private double PotentialRunoff(double precipitation, double runOn, double totalInterception)
    {
        double result = 0.0;

        if (precipitation + runOn - totalInterception > 0.0)
        {
            if (obsrunoff_name == "")
            {
                result = CalcSCSRunoff(precipitation, runOn, totalInterception);
            }
            else
            {
                if (MyPaddock.Get(obsrunoff_name, out observedRunoff))
                   result = observedRunoff;
                else
                {
                    observedRunoff = Double.NaN;
                    My.Warning(String.Format("{0} {1} {2} {3} {4}",
                                             "Year = ", Clock.Today.Year,
                                             ", day = ", Clock.Today.Day,
                                             ", Using predicted runoff for missing observation"));
                    result = CalcSCSRunoff(precipitation, runOn, totalInterception);
                }
            }
        }

        return result;
    }

    /// <summary>
    /// Calculates the potential runoff
    /// </summary>
    /// <remarks>
    /// This code uses the SCS curve number approach
    /// </remarks>
    /// <param name="precipitation">Amount of precipitation (rain + irrigation)</param>
    /// <param name="runOn">Amount of water rununing in</param>
    /// <param name="totalInterception">Amount of water intercepted by canopy and residues</param>
    /// <returns>Runoff amount</returns>
    private double CalcSCSRunoff(double precipitation, double runOn, double totalInterception)
    {
        double cn;                                 //! scs curve number
        double cn1;                                //! curve no. for dry soil (antecedent) moisture
        double cn3;                                //! curve no. for wet soil (antecedent) moisture
        double coverFractor;                       //! proportion of maximum cover effect on runoff (0-1)
        double cnpd;                               //! cn proportional in dry range (dul to ll15)
        double maxPotentialStorage;                //! potential max retention (surface ponding + infiltration)
        double xpb;                                //! intermediate variable for deriving runof
        double[] runoff_wf;                        //! weighting factor for depth for each layer
        double dul_fraction;                       // if between 0 and 1 sw is below DUL, if >1 then is above DUL
        double tillageEffect;                      //! reduction in cn due to tillage

        runoff_wf = CalcWeightFactorRunoff();

        cnpd = 0.0;
        for (int layer = 0; layer < nLayers; layer++)
        {
            dul_fraction = MathUtility.Divide(swAmount[layer] - swLL15[layer], swDUL[layer] - swLL15[layer], 0.0);
            cnpd = cnpd + dul_fraction * runoff_wf[layer];
        }
        cnpd = bound(cnpd, 0.0, 1.0);

        //reduce cn2 for the day due to the cover effect
        cover_surface_runoff = CalcSurfaceCoverRunoff();
        coverFractor = MathUtility.Divide(cover_surface_runoff, _cn_cov, 0.0);
        coverFractor = bound(coverFractor, 0.0, 1.0);
        cn2_new = _cn2_bare - (_cn_red * coverFractor);

        //tillage reduction on cn
        if (tillage_cn_rain > 0.0)
        {
            tillageEffect = tillage_cn_red * (MathUtility.Divide(tillage_rain_sum, tillage_cn_rain, 0.0) - 1.0);
            cn2_new = cn2_new + tillageEffect;
        }

        //! cut off response to cover at high covers if p%cn_red < 100.
        cn2_new = bound(cn2_new, 0.0, 100.0);

        cn1 = MathUtility.Divide(cn2_new, (2.334 - 0.01334 * cn2_new), 0.0);
        cn3 = MathUtility.Divide(cn2_new, (0.4036 + 0.005964 * cn2_new), 0.0);
        cn = cn1 + (cn3 - cn1) * cnpd;

        // ! curve number will be decided from scs curve number table ??dms
        maxPotentialStorage = 254.0 * (MathUtility.Divide(100.0, cn, 1000000.0) - 1.0);
        xpb = (precipitation + runOn - totalInterception) - 0.2 * maxPotentialStorage;
        xpb = Math.Max(xpb, 0.0);

        //assign the output variable
        double result = MathUtility.Divide(xpb * xpb,
                        precipitation + runOn - totalInterception + 0.8 * maxPotentialStorage, 0.0);

        //bound check the ouput variable
        bound_check_real_var(result, 0.0, (precipitation + runOn - totalInterception), "runoff");

        //Consider the reduction in the curve number as a result of a tillage
        EvaluateTillageEffect(precipitation, runOn, totalInterception);

        return result;
    }

    /// <summary>
    /// Calculates an effective cover that is used for runoff
    /// </summary>
    /// <remarks>
    /// This code considers cover cn response to cover from 'perfect' model
    /// It is assumed that crop canopy was 1/2 effect of mulch, thus allowing the taller canopies to 
    ///  have less effect on runoff and the cover close to ground to have full effect.
    /// </remarks>
    /// <returns>surcafe cover</returns>
    private double CalcSurfaceCoverRunoff()
    {
        double canopyfact; //! canopy factor (0-1)
        double effectiveCropCover; //! effective crop cover (0-1)
        double coverSurfaceCrop; //! efective total cover (0-1)

        //! weight effectiveness of crop canopies
        //!    0 (no effect) to 1 (full effect)

        coverSurfaceCrop = 0.0;
        for (int crop = 0; crop < nCrops; crop++)
        {
            if (canopyHeight[crop] >= 0.0)
            {
                bool bDidInterpolate;
                canopyfact = MathUtility.LinearInterpReal(canopyHeight[crop], canopy_fact_height, canopy_fact,
                    out bDidInterpolate);
            }
            else
            {
                canopyfact = canopy_fact_default;
            }

            effectiveCropCover = canopyTotalCover[crop] * canopyfact;
            coverSurfaceCrop = 1.0 - (1.0 - coverSurfaceCrop) * (1.0 - effectiveCropCover);

            //TODO: there seem to be a problem here, the values above are no accumulated or saved anywhere,
            //  so only the value for the last crop is actually used. This would only be an issue for multicrop
        }

        //! add canopy cover with that of residues
        double result = 1.0 - (1.0 - coverSurfaceCrop) * (1.0 - surfaceom_cover);
        return result;
    }

    /// <summary>
    /// Calculate the weighting factor for hydraulic effectiveness of each soil layer
    /// </summary>
    /// <returns>weighting factor</returns>
    private double[] CalcWeightFactorRunoff()
    {
        double profileDepth;                      //! current depth of soil profile - for when erosion turned on     
        double depthFromSurface;                  //! cumulative depth (mm)
        double actualEffectiveHydrologicalDepth;  //! hydrologically effective depth for runoff (mm)
        int layerOfEffectiveDepth;                //! layer number that the effective depth occurs in ()
        double scale_fact;                //! scaling factor for wf function to sum to 1
        double wf_tot;                    //! total of wf ()
        double wx;                        //! depth weighting factor for current total depth. intermediate variable for deriving wf (total wfs to current layer)
        double xx;                        //! intermediate variable for deriving wf total wfs to previous layer

        xx = 0.0;
        depthFromSurface = 0.0;
        wf_tot = 0.0;
        double[] result = new double[nLayers];

        //! check if hydro_effective_depth applies for eroded profile.
        profileDepth = MathUtility.Sum(dLayer);
        actualEffectiveHydrologicalDepth = Math.Min(hydrol_effective_depth, profileDepth);

        scale_fact = 1.0 / (1.0 - Math.Exp(-4.16));
        layerOfEffectiveDepth = FindLayerNo(actualEffectiveHydrologicalDepth);

        for (int layer = 0; layer <= layerOfEffectiveDepth; layer++)
        {
            depthFromSurface = depthFromSurface + dLayer[layer];
            depthFromSurface = Math.Min(depthFromSurface, actualEffectiveHydrologicalDepth);

            //! assume water content to c%hydrol_effective_depth affects runoff
            //! sum of wf should = 1 - may need to be bounded? <dms 7-7-95>
            wx = scale_fact * (1.0 - Math.Exp(-4.16 * MathUtility.Divide(depthFromSurface, actualEffectiveHydrologicalDepth, 0.0)));
            result[layer] = wx - xx;
            xx = wx;
            wf_tot = wf_tot + result[layer];
        }

        bound_check_real_var(wf_tot, 0.9999, 1.0001, "wf_tot");

        return result;
    }

    /// <summary>
    /// Evaluates the reduction in cn after tillage
    /// </summary>
    /// <remarks>
    /// The cn for runoff can be altered after when tillage event occurs (tillage_cn_red)
    /// This effect ceases after a set given amount of rainfall is accumulated (tillage_cn_rain)
    /// </remarks>
    /// <param name="precipitation">Amount of precipitation (rain + irrigation)</param>
    /// <param name="runOn">Amount of water rununing in</param>
    /// <param name="totalInterception">Amount of water intercepted by canopy and residues</param>
    private void EvaluateTillageEffect(double precipitation, double runOn, double totalInterception)
    {
        // TODO: the reduction in cn should be fading away as rainfall acumulates, here it is only as switch, has or has not effect

        if (tillage_cn_rain > 0.0)
        {
            tillage_rain_sum = tillage_rain_sum + precipitation + runOn - totalInterception;
        }

        if (tillage_rain_sum > tillage_cn_rain)
        {
            tillage_cn_rain = 0.0;
            tillage_cn_red = 0.0;

            Console.WriteLine("Reduction of CN due to tillage is finished");
        }
    }

    #endregion
            
    #region Infiltration

    /// <summary>
    /// Computes the infiltration into top layer after runoff
    /// </summary>
    private double CalcInfiltration()
    {
        double infiltrationFromPrecipitation;
        double infiltrationFromPonding;

        infiltrationFromPrecipitation = MetFile.Rain + runon - potentialRunoff - interception - residueinterception;
        if (irrigation_layer <= 0)
        {
            //There is surface irrigation
            infiltrationFromPrecipitation = MetFile.Rain + irrigation + runon - potentialRunoff - interception -
                                            residueinterception;
        }

        infiltrationFromPonding = pond;
        pond = 0.0;

        return infiltrationFromPrecipitation + infiltrationFromPonding;
    }

    #endregion

    #region Evaporation

    /// <summary>
    /// Gets the potential evapotranspiration (eo)
    /// </summary>
    private double PotentialEvapotranspiration()
    {
        //+  Notes
        //    Eventually eo will be in a separate module entirely, and soilwater will get as an input.
        //    But, for now we either retrieve it "manually", or use priestly-taylor.

        double result = 0.0;
        eoApsim = Double.NaN;
        if (_eo_source != "" && My.Get(_eo_source, out eoApsim) && !Double.IsNaN(eoApsim))
        {
            // eo is provided by system
            result = eoApsim;
        }
        else
        {
            //eo from priestly taylor
            result= CalcPriestlyTaylor();
        }

        return result;
    }

    /// <summary>
    /// Compute the effective potential evapotranspiration
    /// </summary>
    private void CalcEffectivePotentialEvaporation()
    {
        // Check to see if there is any ponding.  If there is, evaporate any potential (eos) straight out of it and transfer
        //  any remaining potential to the soil layer 1, as per usual.  Introduce new term pond_evap, which is the daily evaporation from the pond.

        if (pond > 0.0)
        {
            if (pond >= eos)
            {
                pond = pond - eos;
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

    /// <summary>
    /// Calculate the potential evapotranspiration
    /// </summary>
    /// <remarks>
    /// This code uses the Priestly and Taylor method
    /// </remarks>
    /// <returns>potential evapotranspiration</returns>
    private double CalcPriestlyTaylor()
    {
        double albedo;           //! albedo taking into account plant material
        double cover_green_sum;  //! sum of crop green covers (0-1)
        double eeq;              //! equilibrium evaporation rate (mm)
        double wt_ave_temp;      //! weighted mean temperature for the day (oC)

        cover_green_sum = 0.0;
        for (int crop = 0; crop < nCrops; ++crop)
            cover_green_sum = 1.0 - (1.0 - cover_green_sum) * (1.0 - canopyGreenCover[crop]);

        albedo = max_albedo - (max_albedo - salb) * (1.0 - cover_green_sum);

        // ! wt_ave_temp is mean temp, weighted towards max.
        wt_ave_temp = (0.60 * MetFile.MaxT) + (0.40 * MetFile.MinT);

        eeq = MetFile.Radn * 23.8846 * (0.000204 - 0.000183 * albedo) * (wt_ave_temp + 29.0);

        //! find potential evapotranspiration (eo) from equilibrium evap rate
        return eeq * EquilibriumEvaporationRate();
    }

    /// <summary>
    /// Calculate the Equilibrium Evaporation Rate
    /// </summary>
    /// <returns></returns>
    private double EquilibriumEvaporationRate()
    {
        if (MetFile.MaxT > max_crit_temp)
        {
            //! at very high max temps eo/eeq increases
            //! beyond its normal value of 1.1
            return ((MetFile.MaxT - max_crit_temp) * 0.05 + 1.1);
        }

        if (MetFile.MaxT < min_crit_temp)
        {
            //! at very low max temperatures eo/eeq
            //! decreases below its normal value of 1.1
            //! note that there is a discontinuity at tmax = 5
            //! it would be better at tmax = 6.1, or change the
            //! .18 to .188 or change the 20 to 21.1
            return (0.01 * Math.Exp(0.18 * (MetFile.MaxT + 20.0)));
        }

        return 1.1;  //sv- normal value of eeq fac (eo/eeq)
    }

    private void CalcEvaporation()
    {
        //eos   -> ! (output) potential soil evap after modification for crop cover & residue_wt
        //esoil -> ! (output) actual soil evaporation (mm)

        double asw1;    //! available soil water in top layer for actual soil evaporation (mm)

        //1. get potential soil water evaporation
        eos = PotentialEvaporation();

        //2. get available soil water for evaporation
        //   NB. ritchie + b&s evaporate from layer 1, but rickert can evaporate from L1 + L2.
        asw1 = Math.Max(0.0, swAmount[0] - swAirDry[0]);

        //3. get actual soil water evaporation
        CalcActualEvaporation(asw1);
    }

    /// <summary>
    /// Gets the potential soil water evaporation
    /// </summary>
    /// <returns>potential soil evaporation</returns>
    private double PotentialEvaporation()
    {
        //!---------------------------------------+
        //! reduce Eo to that under plant CANOPY
        //!---------------------------------------+

        //!  Based on Adams, Arkin & Ritchie (1976) Soil Sci. Soc. Am. J. 40:436-
        //!  Reduction in potential soil evaporation under a canopy is determined
        //!  the "% shade" (ie cover) of the crop canopy - this should include th
        //!  green & dead canopy ie. the total canopy cover (but NOT near/on-grou
        //!  residues).  From fig. 5 & eqn 2.                       <dms June 95>
        //!  Default value for c%canopy_eos_coef = 1.7
        //!              ...minimum reduction (at cover =0.0) is 1.0
        //!              ...maximum reduction (at cover =1.0) is 0.183.
        //   !1a. adjust potential soil evaporation to account for the effects of surface 
        //   !    residue (Adams et al, 1975) as used in Perfect

        double cover_tot_sum = 0.0;
        for (int i = 0; i < nCrops; i++)
          cover_tot_sum = 1.0 - (1.0 - cover_tot_sum) * (1.0 - canopyTotalCover[i]);
        
        // fraction of potential soil evaporation limited by crop canopy (mm)
        double  eos_canopy_fract = Math.Exp(-1 * canopy_eos_coef * cover_tot_sum);

            // fraction of potential soil evaporation limited by crop residue (mm)
        double eos_residue_fract = 0.0;
        if (surfaceom_cover < 1.0)
       {
            //!  estimate 1st stage soil evap reduction power of mixed residues based on the cover by mixed residues.
            //!    [DM. Silburn unpublished data, June 95 ]
            //!    <temporary value - will reproduce Adams et al 75 effect>
            //!     c%A_to_evap_fact = 0.00022 / 0.0005 = 0.44
            eos_residue_fract = Math.Pow((1.0 - surfaceom_cover), A_to_evap_fact);
        }

        //! Reduce potential soil evaporation under canopy and residues (mulch)
        double result = eo * eos_canopy_fract * eos_residue_fract;
        return result;
    }


    /// <summary>
    /// Gets the actual evaporation from soil surface
    /// </summary>
    /// <param name="maxEOS">maximum evaporation</param>
    private void CalcActualEvaporation(double maxEOS)
    {
        // Wrapper for various evaporation models.
        // Returns actual evaporation from soil surface (es).
        // 'es' is an array because some methods do evaporation from every layer in the soil, although most only do the surface.

        if (evapMethod == defaultEvapMethod)
        {
            evaporation = EvaporationRitchie(maxEOS);
        }
        else
        {
            throw new Exception("Undefined evaporation method");
        }

    }

    /// <summary>
    /// calculate actual evaporation from soil surface
    /// </summary>
    /// <param name="Eos_max">maximum evaporation</param>
    /// <returns>actual evaporation from each soil layer</returns>
    private double[] EvaporationRitchie(double Eos_max)
    {
        // Evaporation (es) takes place in two stages: the constant rate stage (stage 1) and the
        //  falling rate stage (Stage2), after Philip (1957). In stage1, the soil is sufficiently
        //  wet for water be transported to the surface at a rate at least equal to the evaporation
        //  potential (eos), in the falling rate stage (stage 2), the surface soil water content
        //  has decreased below a threshold value, so that es depends on the flux of water through
        //  the upper layer of soil to the evaporating site near the surface.

        double esoil1;     //! actual soil evap in stage 1
        double esoil2;     //! actual soil evap in stage 2
        double sumes1Max; //! upper limit of sumes1
        double w_inf;      //! infiltration into top layer (mm)
        double[] result = new double[nLayers];

        // Need to add 12 hours to move from "midnight" to "noon", or this won't work as expected
        if (DateUtility.WithinDates(winterdate, Clock.Today, summerdate))
        {
            _cona = wintercona;
            _u = winteru;
        }
        else
        {
            _cona = summercona;
            _u = summeru;
        }

        sumes1Max = _u;
        w_inf = infiltration;

        //! if infiltration, reset sumes1, reset sumes2 if infiltration exceeds sumes1      
        if (w_inf > 0.0)
        {
            sumes2 = Math.Max(0.0, (sumes2 - Math.Max(0.0, w_inf - sumes1)));
            sumes1 = Math.Max(0.0, sumes1 - w_inf);

            //! update t (incase sumes2 changed)
            timeStage2 = MathUtility.Sqr(MathUtility.Divide(sumes2, _cona, 0.0));
        }

        //! are we in stage1 ?
        if (sumes1 < sumes1Max)
        {
            //! we are in stage1, set esoil1 = potential or limited by u.
            esoil1 = Math.Min(eos, sumes1Max - sumes1);

            if ((eos > esoil1) && (esoil1 < Eos_max))
            {
                // eos was not satisfied by 1st stage drying & there is evaporative sw in excess
                //  to air_dry, allowing for esoil1. Need to calculate some stage 2 drying (esoil2).

                //*  if sumes2>0.0 then esoil2 =f(sqrt(time),cona,sumes2,eos-esoil1).
                //*  if sumes2 is zero, then use ritchie's empirical transition constant (0.6).            

                if (sumes2 > 0.0)
                {
                    timeStage2 = timeStage2 + 1.0;
                    esoil2 = Math.Min(eos - esoil1, _cona * Math.Pow(timeStage2, 0.5) - sumes2);
                }
                else
                {
                    esoil2 = 0.6 * (eos - esoil1);
                }
            }
            else
            {
                //! no deficit (or esoil1 = eos_max,) no esoil2 on this day            
                esoil2 = 0.0;
            }

            //! check any esoil2 with lower limit of evaporative sw.
            esoil2 = Math.Min(esoil2, Eos_max - esoil1);

            //!  update 1st and 2nd stage soil evaporation.     
            sumes1 = sumes1 + esoil1;
            sumes2 = sumes2 + esoil2;
            timeStage2 = MathUtility.Sqr(MathUtility.Divide(sumes2, _cona, 0.0));
        }
        else
        {
            //! no 1st stage drying. calc. 2nd stage         
            esoil1 = 0.0;

            timeStage2 = timeStage2 + 1.0;
            esoil2 = Math.Min(eos, _cona * Math.Pow(timeStage2, 0.5) - sumes2);

            //! check with lower limit of evaporative sw.
            esoil2 = Math.Min(esoil2, Eos_max);

            //!   update 2nd stage soil evaporation.
            sumes2 = sumes2 + esoil2;
        }

        result[0] = esoil1 + esoil2;

        //! make sure we are within bounds      
        result[0] = bound(result[0], 0.0, eos);
        result[0] = bound(result[0], 0.0, Eos_max);

        return result;
    }

    #endregion

    #region Drainage

    /// <summary>
    /// Calculate water flow above DUL for each layer
    /// </summary>
    private void soilwat2_drainage()
    {
        //flux              //! (output) water moving out of
        //extra_runoff      //! (output) water to add to runoff (mm)
        //sv- it just calculates. It does not change anything.

        // Local Variables
        double waterToAdd; //! water to add to layer
        double waterToBackup; //! water to backup
        double waterExcess; //! amount above saturation(overflow)(mm)
        double waterByPassing = 0.0; //! amount above saturation(overflow) that moves on down (mm)
        double[] newWaterAmount; //! record of results of sw calculations ensure mass balance. (mm)
        double waterDraining; //! water draining by gravity (mm)
        double incomingWater=0.0; //! water coming into layer (mm)
        double outgoingWater; //! water going out of layer (mm)
        double newWaterAmountLayer; //! total water in layer at start (mm)

        //! flux into layer 1 = infiltration (mm).
        ExtraRunoff = 0.0;

        //! calculate drainage and water redistribution.
        newWaterAmount = new double[nLayers];

        for (int layer = 0; layer < nLayers; layer++)
        {
            // get 1st estimate for water amount in this layer
            newWaterAmountLayer = swAmount[layer] + incomingWater;

            //! get excess water above saturation & then water left
            //! to drain between sat and dul.  Only this water is
            //! subject to swcon. The excess is not - treated as a
            //! bucket model. (mm)
            if (newWaterAmountLayer > swSAT[layer])
            {
                waterExcess = newWaterAmountLayer - swSAT[layer];
                newWaterAmountLayer = swSAT[layer];
            }
            else
            {
                waterExcess = 0.0;
            }

            if (newWaterAmountLayer > swDUL[layer])
            {
                waterDraining = (newWaterAmountLayer - swDUL[layer]) * swcon[layer];
            }
            else
            {
                waterDraining = 0.0;
            }

            //! get water draining out of layer (mm)
            if (waterExcess > 0.0)
            {
                //! Calculate amount of water to backup and push down
                //! Firstly top up this layer (to saturation)
                waterToAdd = Math.Min(waterExcess, waterDraining);
                waterExcess = waterExcess - waterToAdd;
                newWaterAmount[layer] = swSAT[layer] - waterDraining + waterToAdd;

                //! partition between flow back up and flow down
                waterByPassing = Math.Min(ks[layer] - waterDraining, waterExcess);
                waterToBackup = waterExcess - waterByPassing;
                
                waterBypass[layer] = waterByPassing;
                waterFlowAboveDUL[layer] = waterDraining;

                outgoingWater = waterByPassing + waterDraining;
                flux[layer] = outgoingWater;

                //! now back up to saturation for this layer up out of the
                //! backup water keeping account for reduction of actual
                //! flow rates (flux) for N movement.
                double bkUpFraction = 0.0;
                for (int z = layer - 1; z >= 0; z--)
                {
                    bkUpFraction = MathUtility.Divide(waterBypass[z], flux[z], 0.0);
                    waterBypass[z] -= waterToBackup * bkUpFraction;
                    waterFlowAboveDUL[z] -= waterToBackup * (1.0 - bkUpFraction);
                    flux[z] -= waterToBackup;
                    waterToAdd = Math.Min(swSAT[z] - newWaterAmount[z], waterToBackup);
                    newWaterAmount[z] = newWaterAmount[z] + waterToAdd;
                    waterToBackup = waterToBackup - waterToAdd;
                }

                ExtraRunoff = ExtraRunoff + waterToBackup;
            }
            else
            {
                //! there is no excess so do nothing
                outgoingWater = waterDraining;
                flux[layer] = outgoingWater;
                newWaterAmount[layer] = swAmount[layer] + incomingWater - outgoingWater;
            }

            //! drainage out of this layer goes into next layer down
            incomingWater = outgoingWater;
        }
    }

    /// <summary>
    /// Calculate water flow above DUL from each layer
    /// </summary>
    private void soilwat2_drainage_old()
    {
        //flux         -> (output) water moving out of
        //extra_runoff -> (output) water to add to runoff layer (mm)
        //sv- it just calculates. It does not change anything.

        // Local Variables
        double waterToAdd; //! water to add to layer
        double waterToBackup; //! water to backup
        double waterExcess; //! amount above saturation(overflow)(mm)
        double waterByPassing = 0.0; //! amount above saturation(overflow) that moves on down (mm)
        double[] newWaterAmount; //! record of results of sw calculations ensure mass balance. (mm)
        double waterDraining; //! water draining by gravity (mm)
        double incomingWater; //! water coming into layer (mm)
        double outgoingWater; //! water going out of layer (mm)
        double newWaterAmountLayer; //! total water in layer at start (mm)

        //! flux into layer 1 = infiltration (mm).
        incomingWater = 0.0;
        ExtraRunoff = 0.0;

        //! calculate drainage and water redistribution.
        flux = new double[nLayers];
        newWaterAmount = new double[nLayers];

        for (int layer = 0; layer < nLayers; layer++)
        {
            //! get total water concentration in layer
            newWaterAmountLayer = swAmount[layer] + incomingWater;

            //! get excess water above saturation & then water left
            //! to drain between sat and dul.  Only this water is
            //! subject to swcon. The excess is not - treated as a
            //! bucket model. (mm)
            if (newWaterAmountLayer > swSAT[layer])
            {
                waterExcess = newWaterAmountLayer - swSAT[layer];
                newWaterAmountLayer = swSAT[layer];
            }
            else
            {
                waterExcess = 0.0;
            }

            if (newWaterAmountLayer > swDUL[layer])
            {
                waterDraining = (newWaterAmountLayer - swDUL[layer]) * swcon[layer];
            }
            else
            {
                waterDraining = 0.0;
            }

            //! get water draining out of layer (mm)
            if (waterExcess > 0.0)
            {
                if (mwcon == null || mwcon[layer] >= 1.0)
                {
                    //! all this excess goes on down so do nothing
                    outgoingWater = waterExcess + waterDraining;
                    newWaterAmount[layer] = swAmount[layer] + incomingWater - outgoingWater;
                    flux[layer] = outgoingWater;
                }
                else
                {
                    //! Calculate amount of water to backup and push down
                    //! Firstly top up this layer (to saturation)
                    waterToAdd = Math.Min(waterExcess, waterDraining);
                    waterExcess = waterExcess - waterToAdd;
                    newWaterAmount[layer] = swSAT[layer] - waterDraining + waterToAdd;

                    //! partition between flow back up and flow down
                    waterToBackup = (1.0 - mwcon[layer]) * waterExcess;
                    waterByPassing = mwcon[layer] * waterExcess;

                    waterBypass[layer] = waterByPassing;
                    waterFlowAboveDUL[layer] = waterDraining;

                    outgoingWater = waterByPassing + waterDraining;
                    flux[layer] = outgoingWater;

                    //! now back up to saturation for this layer up out of the
                    //! backup water keeping account for reduction of actual
                    //! flow rates (flux) for N movement.
                    double bkUpFraction = 0.0;
                    for (int z = layer - 1; z >= 0; z--)
                    {
                        bkUpFraction = MathUtility.Divide(waterBypass[z], flux[z], 0.0);
                        waterBypass[z] -= waterToBackup * bkUpFraction;
                        waterFlowAboveDUL[z] -= waterToBackup * (1.0 - bkUpFraction);
                        flux[z] = flux[z] - waterToBackup;
                        waterToAdd = Math.Min(swSAT[z] - newWaterAmount[z], waterToBackup);
                        newWaterAmount[z] = newWaterAmount[z] + waterToAdd;
                        waterToBackup = waterToBackup - waterToAdd;
                    }

                    ExtraRunoff = ExtraRunoff + waterToBackup;
                }
            }
            else
            {
                //! there is no excess so do nothing
                outgoingWater = waterDraining;
                flux[layer] = outgoingWater;
                newWaterAmount[layer] = swAmount[layer] + incomingWater - outgoingWater;
            }

            //! drainage out of this layer goes into next layer down
            incomingWater = outgoingWater;
        }
    }

    /// <summary>
    /// Calculate water flow below DUL for each layer
    /// </summary>
    private void soilwat2_unsat_flow()
    {
        double esw_dep1;            //! extractable soil water in current layer (mm)
        double esw_dep2;            //! extractable soil water in next layer below (mm)
        double dbar;                //! average diffusivity used to calc unsaturated flow between layers
        int second_last_layer;      //! last layer for flow
        int next_layer;             //! layer counter for next lower layer
        double flow_max;            //! maximum flow to make gradient between layers equal zero
        double theta1;              //! sw content above ll15 for current layer (cm/cm)
        double theta2;              //! sw content above ll15 for next lower layer (cm/cm)
        double w_out;               //! water moving up out of this layer (mm)
        //! +ve = up to next layer
        //! -ve = down into this layer
        double this_layer_cap;      //! capacity of this layer to accept water from layer below (mm)
        double next_layer_cap;      //! capacity of nxt layer to accept water from layer above (mm)
        double sw1;                 //! sw for current layer (mm/mm)
        double sw2;                 //! sw for next lower layer (mm/mm)
        double gradient;            //! driving force for flow
        double sum_inverse_dlayer;
        double dlayer1;             //! depth of current layer (mm)
        double dlayer2;             //! depth of next lower layer (mm)
        double ave_dlayer;          //! average depth of current and next layers (mm)
        double sw_dep1;             //! soil water depth in current layer (mm)
        double sw_dep2;             //! soil water depth in next layer (mm)
        double ll15_dep1;           //! 15 bar lower limit sw depth in current layer (mm)
        double ll15_dep2;           //! 15 bar lower limit sw depth in next layer (mm)
        double sat_dep1;            //! saturated sw depth in current layer (mm)
        double sat_dep2;            //! saturated sw depth in next layer (mm)
        double dul_dep1;            //! drained upper limit in current layer (mm)
        double dul_dep2;            //! drained upper limit in next layer (mm)
        double swg;                 //! sw differential due to gravitational pressure head (mm)

        second_last_layer = nLayers - 1;
        w_out = 0.0;

        for (int layer = 0; layer < second_last_layer; layer++)
        {
            next_layer = layer + 1;

            dlayer1 = dLayer[layer];
            dlayer2 = dLayer[next_layer];
            ave_dlayer = (dlayer1 + dlayer2) * 0.5;

            sw_dep1 = swAmount[layer];
            sw_dep2 = swAmount[next_layer];

            ll15_dep1 = swLL15[layer];
            ll15_dep2 = swLL15[next_layer];

            sat_dep1 = swSAT[layer];
            sat_dep2 = swSAT[next_layer];

            dul_dep1 = swDUL[layer];
            dul_dep2 = swDUL[next_layer];

            esw_dep1 = Math.Max((sw_dep1 - w_out) - ll15_dep1, 0.0);
            esw_dep2 = Math.Max(sw_dep2 - ll15_dep2, 0.0);

            //! theta1 is excess of water content above lower limit,
            //! theta2 is the same but for next layer down.
            theta1 = MathUtility.Divide(esw_dep1, dlayer1, 0.0);
            theta2 = MathUtility.Divide(esw_dep2, dlayer2, 0.0);

            //! find diffusivity, a function of mean thet.
            dbar = diffus_const * Math.Exp(diffus_slope * (theta1 + theta2) * 0.5);

            //! testing found that a limit of 10000 (as used in ceres-maize) for dbar limits
            //! instability for flow direction for consecutive days in some situations.
            dbar = bound(dbar, 0.0, 10000.0);

            sw1 = MathUtility.Divide((sw_dep1 - w_out), dlayer1, 0.0);
            sw1 = Math.Max(sw1, 0.0);

            sw2 = MathUtility.Divide(sw_dep2, dlayer2, 0.0);
            sw2 = Math.Max(sw2, 0.0);

            //    ! gradient is defined in terms of absolute sw content
            //cjh          subtract gravity gradient to prevent gradient being +ve when flow_max is -ve, resulting in sw > sat.
            gradient = MathUtility.Divide((sw2 - sw1), ave_dlayer, 0.0) - gravity_gradient;

            //!  flow (positive up) = diffusivity * gradient in water content
            waterFlowBelowDUL[layer] = dbar * gradient;

            //! flow will cease when the gradient, adjusted for gravitational effect, becomes zero.
            swg = gravity_gradient * ave_dlayer;

            //! calculate maximum flow
            sum_inverse_dlayer = MathUtility.Divide(1.0, dlayer1, 0.0) + MathUtility.Divide(1.0, dlayer2, 0.0);
            flow_max = MathUtility.Divide((sw2 - sw1 - swg), sum_inverse_dlayer, 0.0);

            //c dsg 260202
            //c dsg    this code will stop a saturated layer difusing water into a partially saturated
            //c        layer above for Water_table height calculations
            if ((swAmount[layer] >= swDUL[layer]) && (swAmount[next_layer] >= swDUL[next_layer]))
            {
                waterFlowBelowDUL[layer] = 0.0;
            }

            //c dsg 260202
            //c dsg    this code will stop unsaturated flow downwards through an impermeable layer, but will allow flow up
            if ((mwcon != null) && (mwcon[layer] < 0.000001) && (waterFlowBelowDUL[layer] < 0.0))
            {
                waterFlowBelowDUL[layer] = 0.0;
            }

            if (waterFlowBelowDUL[layer] < 0.0)
            {
                //! flow is down to layer below
                //! check capacity of layer below for holding water from this layer
                //! and the ability of this layer to supply the water

                //!    next_layer_cap = l_bound (sat_dep2 - sw_dep2, 0.0)
                //!    dsg 150302   limit unsaturated downflow to a max of dul in next layer

                next_layer_cap = Math.Max(dul_dep2 - sw_dep2, 0.0);
                flow_max = Math.Max(flow_max, -1 * next_layer_cap);
                flow_max = Math.Max(flow_max, -1 * esw_dep1);
                waterFlowBelowDUL[layer] = Math.Max(waterFlowBelowDUL[layer], flow_max);
            }
            else
            {
                if (waterFlowBelowDUL[layer] > 0.0)
                {
                    //! flow is up from layer below
                    //! check capacity of this layer for holding water from layer below
                    //! and the ability of the layer below to supply the water

                    //!            this_layer_cap = l_bound (sat_dep1 - (sw_dep1 - w_out), 0.0)
                    //!    dsg 150302   limit unsaturated upflow to a max of dul in this layer
                    this_layer_cap = Math.Max(dul_dep1 - (sw_dep1 - w_out), 0.0);
                    flow_max = Math.Min(flow_max, this_layer_cap);
                    flow_max = Math.Min(flow_max, esw_dep2);
                    waterFlowBelowDUL[layer] = Math.Min(waterFlowBelowDUL[layer], flow_max);
                }
                // else { no flow }
            }

            //! For conservation of water, store amount of water moving
            //! between adjacent layers to use for next pair of layers in profile
            //! when calculating theta1 and sw1.
            w_out = waterFlowBelowDUL[layer];
        }
    }

    #endregion

    #region Solute Flow

    /// <summary>
    /// Calculates solute transport during flow above DUL
    /// </summary>
    /// <param name="existingSolute"></param>
    private double[] SoluteFlowAboveDUL(double[] existingSolute)
    {
        //solute_out   ->   ! (output) solute leaching out of each layer (kg/ha) 
        //existingSolute    ->   ! (input) solute in each layer (kg/ha)

        double incomingWater; //! water draining into this layer (mm)
        double outgoingWater; //! water draining out of layer (mm)
        double incomingSolute; //! solute leaching into layer from above (kg/ha)
        double outgoingSolute; //! solute leaching out of layer (kg/ha)
        double newSoluteAmountLayer; //! quantity of solute in layer (kg/ha)
        double waterAmount; //! quantity of water in layer (mm)

        double[] soluteLeaching = new double[nLayers];
        incomingSolute = 0.0;
        incomingWater = infiltration;
        // TODO: check the order of water changes

        for (int layer = 0; layer < nLayers; layer++)
        {
            //! get water draining out of layer and n content of layer includes that leaching down         
            outgoingWater = waterFlowAboveDUL[layer];
            newSoluteAmountLayer = existingSolute[layer] + incomingSolute;

            waterAmount = swAmount[layer] + outgoingWater;

            double fractionDrainIncoming = MathUtility.Divide(0.5 * incomingWater,
                0.5 * incomingWater + Math.Max(0.0, swAmount[layer] - swDUL[layer]), 0.0);

            double fractionDrained = MathUtility.Divide(outgoingWater * fractionDrainIncoming,
                incomingWater, 0.0);

            double outgoingIncomingSolute = incomingSolute * fractionDrained;
            //TODO: need to chck these

            fractionDrained = MathUtility.Divide(outgoingWater * (1.0 - fractionDrainIncoming),
                swAmount[layer], 0.0);

            double outgoingExistingSolute = existingSolute[layer] * fractionDrained * solute_flux_eff[layer];
            //TODO: need to chck these

            outgoingSolute = outgoingIncomingSolute + outgoingExistingSolute;
                //newSoluteAmountLayer * fractionDrained * solute_flux_eff[layer];

            // check that leaching is within bounds
            outgoingSolute = bound(outgoingSolute, 0.0, newSoluteAmountLayer);

            //! keep the leaching and set the input for the next layer
            soluteLeaching[layer] = outgoingSolute;
            incomingSolute = outgoingSolute;
        }

        return soluteLeaching;
    }

    /// <summary>
    /// Calculates solute transport during flow above DUL
    /// </summary>
    /// <param name="existingSolute"></param>
    private double[] SoluteFlowAboveDUL_old(double[] existingSolute)
    {
        //solute_out   ->   ! (output) solute leaching out of each layer (kg/ha) 
        //existingSolute    ->   ! (input) solute in each layer (kg/ha)

        double outgoingWater; //! water draining out of layer (mm)
        double incomingSolute; //! solute leaching into layer from above (kg/ha)
        double outgoingSolute; //! solute leaching out of layer (kg/ha)
        double newSoluteAmountLayer; //! quantity of solute in layer (kg/ha)
        double waterAmount; //! quantity of water in layer (mm)

        double[] soluteLeaching = new double[nLayers];
        incomingSolute = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            //! get water draining out of layer and n content of layer includes that leaching down         
            outgoingWater = flux[layer];
            newSoluteAmountLayer = existingSolute[layer] + incomingSolute;

            waterAmount = swAmount[layer] + outgoingWater;
            double fractionDrained = MathUtility.Divide(outgoingWater, waterAmount, 0.0);
            outgoingSolute = newSoluteAmountLayer * fractionDrained * solute_flux_eff[layer];

            // check that leaching is within bounds
            outgoingSolute = bound(outgoingSolute, 0.0, newSoluteAmountLayer);

            //! keep the leaching and set the input for the next layer
            soluteLeaching[layer] = outgoingSolute;
            incomingSolute = outgoingSolute;
        }

        return soluteLeaching;
    }

    /// <summary>
    /// Calculates solute transport during flow below DUL
    /// </summary>
    /// <param name="solute_up"></param>
    /// <param name="solute_kg"></param>
    private void soilwat2_solute_flow(ref double[] solute_up, double[] solute_kg)
    {
        //solute_up -> ! (output) solute moving upwards into each layer (kg/ha)
        //existingSolute -> ! (input/output) solute in each layer (kg/ha)

        double bottomw; //! water movement to/from next layer (kg/ha)
        double incomingSolute; //! solute moving into layer from above (kg/ha)
        double[] solute_down; //! solute moving downwards out of each layer (kg/ha)
        double outgoingSolute; //! solute moving out of layer (kg/ha)
        double outgoingWater; //! water draining out of layer (mm)
        double[] remain; //! n remaining in each layer between movement up (kg/ha)
        double soluteAmountLayer; //! quantity of solute in layer (kg/ha)
        double top_w; //! water movement to/from above layer (kg/ha)
        double waterAmountLayer; //! quantity of water in layer (mm)

        //! flow  up from lower layer:  + up, - down
        //! + ve flow : upward movement. go from bottom to top layer   

        solute_up = new double[nLayers];
        solute_down = new double[nLayers];
        remain = new double[nLayers];

        incomingSolute = 0.0;
        for (int layer = nLayers - 1; layer > 0; layer--)
        {
            //! keep the solute flow upwards
            solute_up[layer] = incomingSolute;

            //! get water moving up and out of layer to the one above
            outgoingWater = waterFlowBelowDUL[layer - 1];
            if (outgoingWater <= 0.0)
            {
                outgoingSolute = 0.0;
            }
            else
            {
                //! get water movement between this and next layer
                bottomw = waterFlowBelowDUL[layer];

                //! get new solute content of layer includes that moving from other layer
                soluteAmountLayer = solute_kg[layer] + incomingSolute;
                waterAmountLayer = swAmount[layer] + outgoingWater - bottomw;

                //! solute moving out of layer is proportional to the water moving out.
                outgoingSolute = soluteAmountLayer * MathUtility.Divide(outgoingWater, waterAmountLayer, 0.0) *
                                 solute_flow_eff[layer];

                //! check that leaching is within bounds
                outgoingSolute = bound(outgoingSolute, 0.0, soluteAmountLayer);
            }

            //! set the input for the next layer
            incomingSolute = outgoingSolute;
        }

        solute_up[0] = incomingSolute;
        //! now get remaining solute in each layer between movements
        //! this is needed to adjust the amount in each layer before calculating
        //! downwards movement.  I think we shouldn't do this within a time
        //! step. i.e. there should be no movement within a time step. jngh
        remain[0] = solute_up[0];
        for (int layer = 1; layer < nLayers; layer++)
        {
            remain[layer] = solute_up[layer] - solute_up[layer - 1];
        }

        //! -ve flow - downward movement
        incomingSolute = 0.0;
        top_w = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            //! get water moving out of layer
            outgoingWater = -1 * waterFlowBelowDUL[layer];
            if (outgoingWater <= 0.0)
            {
                outgoingSolute = 0.0;
            }
            else
            {
                //! get solute content of layer, includes that moving from other layer
                soluteAmountLayer = solute_kg[layer] + incomingSolute + remain[layer];
                waterAmountLayer = swAmount[layer] + outgoingWater - top_w;

                //! solute moving out of layer is proportional to the water moving out.
                outgoingSolute = soluteAmountLayer * MathUtility.Divide(outgoingWater, waterAmountLayer, 0.0) *
                                 solute_flow_eff[layer];

                //! check that leaching is within bounds
                outgoingSolute = MathUtility.RoundToZero(outgoingSolute);
                outgoingSolute = bound(outgoingSolute, 0.0, soluteAmountLayer);
            }

            solute_down[layer] = outgoingSolute;
            incomingSolute = outgoingSolute;
            top_w = outgoingWater;
        }

        for (int layer = 0; layer < nLayers; layer++)
        {
            solute_up[layer] = solute_up[layer] - solute_down[layer];
        }
    }

    /// <summary>
    /// Add solutes from irrigation
    /// </summary>
    private void soilwat2_irrig_solute()
    {
        //sv- 11 Dec 2012. 
        //Since I have allowed irrigations to runoff just like rain (using argument "will_runoff = 1" in apply command)
        //I should really remove a proportion of the solutes that are lost due to some of the irrigation running off.
        //Perhaps something like (irrigation / (rain + irrigation)) * runoff 
        //to work out how much of the runoff is caused by irrigation and remove this proportion of solutes from the surface layer.
        //HOWEVER, when rain causes runoff we don't remove solutes from the surface layer of the soil. 
        //So why when irrigation causes runoff should we remove solutes.  

        int layer = Math.Max(0, irrigation_layer);

        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            solutes[solnum].Amount[layer] += solutes[solnum].AmountInIrrigation;
            solutes[solnum].Delta[layer] += solutes[solnum].AmountInIrrigation;
        }
    }

    /// <summary>
    /// Add solutes from rainfall
    /// </summary>
    private void soilwat2_rainfall_solute()
    {
        double mass_solute; //! mass of solute in this rainfall (kg/ha)

        //!assume all rainfall goes into surface layer
        // rain is given in mm (=kg/m2) while rain_conc in ppm (mg/kg),
        // so, to get amount of solute rain * 10^4 * rain_conc / 10^6

        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            mass_solute = MetFile.Rain * solutes[solnum].ConcentrationInRain * 0.01;
            solutes[solnum].Amount[0] += mass_solute;
            solutes[solnum].Delta[0] += mass_solute;
        }
    }

    private void MoveDownReal(double[] DownAmount, ref double[] A)
    {

        //!+ Sub-Program Arguments
        //   real       array (*)             ! (INPUT/OUTPUT) amounts currently in
        //                                    !   each layer
        //   real       down (*)              ! (INPUT) amounts to move into each
        //                                    !   layer from the one above

        //!+ Purpose
        //!     move amounts specified, downwards from one element to the next

        //!+  Definition
        //!     Each of the "nlayr" elements of "array" holds quantities
        //!     for a given soil layer.  "array"(1) corresponds to the
        //!     uppermost layer.   "array"(n) corresponds to the layer
        //!     (n-1) layers down from the uppermost layer.  "down"(n)
        //!     indicates a quantity to be moved from the layer
        //!     corresponding to "array"(n) down into the layer
        //!     corresponding to "array"(n+1).  This subroutine subtracts
        //!     "down"(n) from "array"(n) and adds it to "array"(n+1) for
        //!     n=1 .. ("nlayr"-1).  "down"("nlayr") is subtracted from
        //!     "array"("nlayr").

        //!+  Mission Statement
        //!      Move amounts of %1 down array %2

        //!+ Changes
        //!       031091  jngh changed variable movedn to down - cr157

        //!+ Local Variables
        int layer;  //! layer number
        double win;    //! amount moving from layer above to current layer
        double wout;   //! amount moving from current layer to the one below

        //!- Implementation Section ----------------------------------

        win = 0.0;
        for (layer = 0; layer < Math.Min(A.Length, DownAmount.Length); layer++)
        {
            wout = DownAmount[layer];
            A[layer] = A[layer] + win - wout;
            win = wout;
        }
    }

    private void soilwat2_move_solute_down()
    {

        //*+  Mission Statement
        //*      Calculate downward movement of solutes

        int solnum;              //! solute number counter variable

        for (solnum = 0; solnum < nSolutes; solnum++)
        {
            if (solutes[solnum].IsMobile)
            {
                ZeroArray(ref solutes[solnum].FlowAboveDUL);
                solutes[solnum].FlowAboveDUL = SoluteFlowAboveDUL(solutes[solnum].Amount);               //calc leaching
                MoveDownReal(solutes[solnum].FlowAboveDUL, ref solutes[solnum].Amount);      //use leaching to set new solute values
                MoveDownReal(solutes[solnum].FlowAboveDUL, ref solutes[solnum].Delta);       //use leaching to set new delta (change in) solute values
            }
        }
    }

    private void MoveUpReal(double[] UpAmount, ref double[] A)
    {
        //move_up_real(leach, temp_solute, num_layers);


        //!     ===========================================================
        //   subroutine Move_up_real (up, array, nlayr)
        //!     ===========================================================


        //!+ Sub-Program Arguments
        //eal        array (*)             //! (INPUT/OUTPUT) amounts currently in each layer
        //int         nlayr                 //! (INPUT) number of layers
        //real        up (*)                //! (INPUT) amounts to move into each layer from the one below

        //!+ Purpose
        //!       move amounts specified, upwards from one element to the next

        //!+  Definition
        //!     Each of the "nlayr" elements of "array" holds quantities
        //!     for a given soil layer.  "array"(1) corresponds to the
        //!     uppermost layer.   "array"(n) corresponds to the layer
        //!     (n-1) layers down from the uppermost layer.  "up"(n)
        //!     indicates a quantity to be moved from the layer
        //!     corresponding to "array"(n+1) up into the layer
        //!     corresponding to "array"(n).  This subroutine subtracts
        //!     "up"(n) from "array"(n+1) and adds it to "array"(n) for
        //!     n=1..("nlayr"-1).  "up"("nlayr") is added to "array"("nlayr").

        //!+  Mission Statement
        //!      Move amounts %1 up array %2

        //!+ Changes
        //!       031091  jngh renamed moveup to up - cr158
        //!                    included description of variables in parameter list
        //!                      - cr159
        //!                    corrected description - cr160

        //!+ Calls

        //!+ Local Variables
        int layer;                 //! layer number
        double win;                   //! amount moving from layer below to current layer
        double wout;                  //! amount moving from current layer to the one above

        //!- Implementation Section ----------------------------------

        wout = 0.0;
        for (layer = 0; layer < Math.Min(A.Length, UpAmount.Length); layer++)
        {
            win = UpAmount[layer];
            A[layer] = A[layer] + win - wout;
            wout = win;
        }
    }

    private void soilwat2_move_solute_up()
    {

        //*+  Mission Statement
        //*      Calculate upward movement of solutes

        int solnum;              //! solute number counter variable

        for (solnum = 0; solnum < nSolutes; solnum++)
        {
            if (solutes[solnum].IsMobile)
            {
                ZeroArray(ref solutes[solnum].FlowBelowDUL);
                soilwat2_solute_flow(ref solutes[solnum].FlowBelowDUL, solutes[solnum].Amount);
                MoveUpReal(solutes[solnum].FlowBelowDUL, ref solutes[solnum].Amount);
                MoveUpReal(solutes[solnum].FlowBelowDUL, ref solutes[solnum].Delta);
            }
        }
    }

    #endregion

    #region Water Table

    /// <summary>
    /// Calculate the water table depth
    /// </summary>
    /// <returns>The depth (in mm) below the ground surface of the first layer which is saturated</returns>
    private double DepthToWaterTable()
    {
        int saturatedLayer= -1;
        double saturatedFraction = 0.0;
        double saturatedFractionAbove = 0.0;
        double depthWaterTable;

        for (int layer = 0; layer < nLayers; layer++)
        {
            //Find the first layer that is above saturation
            if (swSAT[layer] - swAmount[layer] <= myEpsilon)
            {
                saturatedLayer = layer;
                break;
            }

            //Or if mwcon is set to be impermeable for this layer and above sw is above dul then consider this layer as saturated.
            if ((mwcon != null) && (mwcon[layer] < 1.0) && (swAmount[layer] > swDUL[layer]))
            {
                saturatedLayer = layer;
                break;
            }
        }

        if (saturatedLayer >= 0)
        {
            //! saturated fraction of saturated layer
            saturatedFraction = MathUtility.Divide(swAmount[saturatedLayer] - swDUL[saturatedLayer],
                                                    swSAT[saturatedLayer] - swDUL[saturatedLayer], 0.0);
            if (saturatedLayer >= 1)
            {
                //! saturated fraction of layer above saturated layer
                saturatedFractionAbove = MathUtility.Divide(swAmount[saturatedLayer - 1] - swDUL[saturatedLayer - 1],
                                                              swSAT[saturatedLayer - 1] - swDUL[saturatedLayer - 1], 0.0);
            }
        }

        //Do the calculation of the water table depth
        if (saturatedFraction >= 0.999999 && saturatedFractionAbove >= 0.0)
        {
            // there is saturated layer and layer above is over DUL
            depthWaterTable = MathUtility.Sum(dLayer, 0, saturatedLayer, 0.0);
            depthWaterTable -= saturatedFractionAbove * dLayer[saturatedLayer - 1];
        }
        else
        {
            if (saturatedFractionAbove >= 0.0)
            {
                // there is a saturated layer and layer above not over DUL
                depthWaterTable = MathUtility.Sum(dLayer, 0, saturatedLayer + 1, 0.0);
                depthWaterTable -= saturatedFraction * dLayer[saturatedLayer];
            }
            else
            {
                // there is no saturated layer, profile is not saturated, no water table
                depthWaterTable = MathUtility.Sum(dLayer);
            }
        }

        return depthWaterTable;
    }

    private double SetWaterTable(double depthWaterTable)
    {
        double depthTopOfLayer;
        double depthBottomOfLayer;

        depthBottomOfLayer = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            depthTopOfLayer = depthBottomOfLayer;
            depthBottomOfLayer = depthBottomOfLayer + dLayer[layer];
                if (depthWaterTable > depthTopOfLayer && depthWaterTable < depthBottomOfLayer)
            {
                //! top of water table is in this layer
                double fractionLayer = (depthBottomOfLayer - depthWaterTable) / (depthBottomOfLayer - depthTopOfLayer);
                swAmount[layer] = swDUL[layer] + fractionLayer * (swSAT[layer] - swDUL[layer]);
            }
            else
            {
                swAmount[layer] = swSAT[layer];
            }
        }

        return depthWaterTable;
    }

    #endregion

    #region Lateral Flow

    /// <summary>
    /// Computes the outflow of water moving laterally
    /// </summary>
    /// <returns>lateral outflow</returns>
    private double[] CalcLateralFlow()
    {
        double depthAboveDUL; //depth of water table in a layer (mm)
        double nominator;
        double denominator;
        double maxFlow;
        double[] result = new double[nLayers];

        for (int layer = 0; layer < nLayers; layer++)
        {
            depthAboveDUL = dLayer[layer] * Math.Max(0.0,
                            MathUtility.Divide(swAmount[layer] - swDUL[layer], swSAT[layer] - swDUL[layer], 0.0));
            nominator = klat[layer] * depthAboveDUL * (discharge_width * 1000.0) * slope;
            denominator = catchment_area * 1000000.0 * Math.Pow((1.0 + Math.Pow(slope, 2)), 0.5);
            result[layer] = MathUtility.Divide(nominator, denominator, 0.0);

            //! Cannot drop sw below dul
            maxFlow = Math.Max(0.0, swAmount[layer] - swDUL[layer]);
            result[layer] = bound(outflow_lat[layer], 0.0, maxFlow);
        }

        return result;
    }

    #endregion

    #endregion

    //EVENT HANDLERS
    #region Initialisation and reset

    [EventHandler]
    public void OnInitialised()
    {
        //Save State
        double oldSWDep = MathUtility.Sum(swAmount);

        //Change State
        soilwat2_init();

        //Do reporting
        WriteReport();

        //Send mass balance changes
        double newSWDep = MathUtility.Sum(swAmount);
        SendExternalMassFlow(newSWDep - oldSWDep);
    }

    /// <summary>
    /// Initialise SoilWater module
    /// </summary>
    private void soilwat2_init()
    {
        CheckParameters();

        soilwat2_soil_profile_param();

        soilwat2_evap_init();

        Lateral_init();

        initDone = true;

        for (int layer = 0; layer < nLayers; layer++)
            soilwat2_check_profile(layer);

        //publish event saying there is a new soil profile.
        soilwat2_New_Profile_Event();
    }

    /// <summary>
    /// Report SoilWat module summary details
    /// </summary>
    private void WriteReport()
    {
        double depthTopOfLayer;     //! depth to top of layer (mm)
        double depthBottomOfLayer;  //! depth to bottom of layer (mm)
        string line;                //! temp output record
        double[] runoff_wf;           //! weighting factor for runoff
        double[] usw;                 //! unavail. sw (mm)
        double[] asw;                 //! avail. sw (mm)
        double[] masw;                //! max unavail. sw (mm)
        double[] dsw;                 //! drainable sw (mm)

        runoff_wf = new double[nLayers];
        usw = new double[nLayers];
        asw = new double[nLayers];
        masw = new double[nLayers];
        dsw = new double[nLayers];

        Console.WriteLine();
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

        depthTopOfLayer = 0.0;
        runoff_wf = CalcWeightFactorRunoff();

        for (int layer = 0; layer < nLayers; layer++)
        {
            depthBottomOfLayer = depthTopOfLayer + dLayer[layer];

            if (!using_ks)
            {
                line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000}",
                                     depthTopOfLayer,
                                     "-",
                                     depthBottomOfLayer,
                                     MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0),
                                     bd[layer],
                                     runoff_wf[layer],
                                     swcon[layer]);
            }
            else
            {
                line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000} {11,6:0.000}",
                                     depthTopOfLayer,
                                     "-",
                                     depthBottomOfLayer,
                                     MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0),
                                     bd[layer],
                                     runoff_wf[layer],
                                     swcon[layer],
                                     ks[layer]);
            }
            Console.WriteLine(line);
            depthTopOfLayer = depthBottomOfLayer;
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
        depthTopOfLayer = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            depthBottomOfLayer = depthTopOfLayer + dLayer[layer];
            usw[layer] = swLL15[layer];
            asw[layer] = Math.Max((swAmount[layer] - swLL15[layer]), 0.0);
            masw[layer] = swDUL[layer] - swLL15[layer];
            dsw[layer] = swSAT[layer] - swDUL[layer];

            line = String.Format("   {0,6:0.#} {1} {2,4:0.#} {3,10:0.00} {4,10:0.00} {5,10:0.00} {6,10:0.00}",
                                 depthTopOfLayer,
                                 "-",
                                 depthBottomOfLayer,
                                 usw[layer],
                                 asw[layer],
                                 masw[layer],
                                 dsw[layer]);

            Console.WriteLine(line);
            depthTopOfLayer = depthBottomOfLayer;
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

        if (evapMethod == defaultEvapMethod)
        {
            line = "      Using Ritchie evaporation model";
            Console.WriteLine(line);

            if (winteru == summeru)
            {
                line = String.Format("       {0} {1,8:0.00} {2}",
                                     "Cuml evap (U):        ", _u,
                                     " (mm^0.5)");

                Console.WriteLine(line);
            }
            else
            {
                line = String.Format("        {0} {1,8:0.00} {2}        {3} {4,8:0.00} {5}",
                                     "Stage 1 Duration (U): Summer    ", summeru,
                                     " (mm)" + Environment.NewLine,
                                     "                      Winter    ", winteru,
                                     " (mm)");
                Console.WriteLine(line);
            }

            if (wintercona == summercona)
            {
                line = String.Format("       {0} {1,8:0.00} {2}",
                                     "CONA:                 ", _cona,
                                     " ()");
                Console.WriteLine(line);
            }
            else
            {
                line = String.Format("        {0} {1,8:0.00} {2}        {3} {4,8:0.00} {5}",
                                     "Stage 2       (CONA): Summer    ", summercona,
                                     " (mm^0.5)" + Environment.NewLine,
                                     "                      Winter    ", wintercona,
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

        if (_eo_source != "")
        {
            line = String.Format("      {0} {1}",
                                 "Eo source:             ", _eo_source);
            Console.WriteLine(line);
        }
        else
        {
            line = String.Format("       {0}",
                                 "Eo from priestly-taylor");
            Console.WriteLine(line);
        }
    }

    #endregion

    #region Daily processes

    /// <summary>
    /// Perform daily prepare
    /// </summary>
    [EventHandler]
    public void OnPrepare()
    {
        soilwat2_zero_daily_variables();

        //Calculate potential evapotranspiration
        eo = PotentialEvapotranspiration();
    }

    /// <summary>
    /// Perform daily processes
    /// </summary>
    [EventHandler]
    public void OnProcess()
    {
        //Get variables from other modules
        GetCropVariables();
        GetSoluteVariables();

        double extra_runoff; //! water backed up from flux calculations that was unable to enter profile

        // LATERAL FLOW
        for (int layer = 0; layer < nLayers; layer++)
            swAmount[layer] += inflowLateral[layer];

        outflow_lat = CalcLateralFlow();
        for (int layer = 0; layer < nLayers; layer++)
            swAmount[layer] -= outflow_lat[layer];

        // RUNOFF
        if (irrigation_will_runoff == "no")
        {
            potentialRunoff = PotentialRunoff(MetFile.Rain, runon, interception + residueinterception);
        }
        else
        {
            //calculate runoff but allow irrigations to runoff just like rain.
            potentialRunoff = PotentialRunoff(MetFile.Rain + irrigation, runon, interception + residueinterception);
        }

        // Note: potentialRunoff is the runoff which would have occurred without ponding.
        //  This value is then ammended after taking any ponding into account

        pond = pond + potentialRunoff;
        runoff = Math.Max(pond - _max_pond, 0.0);
        pond = Math.Min(pond, _max_pond);

        // INFILTRATION
        infiltration = CalcInfiltration();
        swAmount[0] += infiltration;

        // IRRIGATION, if subsurface
        if (irrigation_layer > 0)
        {
            swAmount[irrigation_layer] += irrigation;
        }

        // RAIN + IRRIGATION
        soilwat2_irrig_solute();
        soilwat2_rainfall_solute();

        // dispose off the data from irrigation
        irrigation = 0.0;
        for (int solnum = 0; solnum < nSolutes; solnum++)
            solutes[solnum].AmountInIrrigation = 0.0;

        // WATER FLOW (aka flux, aka Drainage) 
        extra_runoff = 0.0;
        if (using_ks)
        {
            soilwat2_drainage();
        }
        else
        {
            soilwat2_drainage_old();
        }

        if (extra_runoff > 0.0)
        {
            // The "extra_runoff" is caused by water backing up of top layer due to inability of soil to drain.
            // Any extra_runoff then is added to pond, if possible or will actually runoff
            pond = Math.Min(extra_runoff, _max_pond);
            runoff = runoff + extra_runoff - pond;
            //Deduct the extra_runoff from the infiltration because it did not infiltrate
            infiltration = infiltration - extra_runoff;
            swAmount[0] -= extra_runoff;
        }

        //! move water down     (Saturated Flow - alter swAmount values using flux calculation)
        MoveDownReal(flux, ref swAmount);

        //! drainage out of bottom layer
        drain = flux[nLayers - 1];

        // SATURATED FLOW SOLUTE MOVEMENT

        //! now move the solutes with flow above dul
        soilwat2_move_solute_down();

        // EVAPORATION
        CalcEvaporation();
        CalcEffectivePotentialEvaporation();
        // TODO: shouldn't the effective potential evaporation be computed before actual evaporation?

        //! ** take away evaporation
        for (int layer = 0; layer < nLayers; layer++)
        {
            swAmount[layer] -= evaporation[layer];
        }

        // UNSATURATED FLOW (flow calculation)

        //! get unsaturated flow
        soilwat2_unsat_flow();
        MoveUpReal(waterFlowBelowDUL, ref swAmount);

        //! now check that the soil water is not silly
        for (int layer = 0; layer < nLayers; layer++)
        {
            soilwat2_check_profile(layer);
        }

        // WATER TABLE
        waterTableDepth = DepthToWaterTable();

        // UNSATURATED FLOW SOLUTE MOVEMENT
        //! now move the solutes with flow  
        soilwat2_move_solute_up();

        //Change the variables in other modules
        SetSolutesValues();
        RaiseRunoffEvent();
    }

    #endregion

    #region Sporadic Events

    /// <summary>
    /// Gets the initial information about solutes
    /// </summary>
    /// <param name="newsolute">Solute data</param>
    [EventHandler(EventName="new_solute")]
    public void OnNew_solute(NewSoluteType newsolute)
    {
        // Gets the name of a new solute, what module owns the new solute, and whether it is mobile or immobile.
        //  It alerts you at any given point in a simulation when a new solute is added. 

        int sender = (int)My.eventSenderId;
        string compName = MyPaddock.SiblingNameFromId(newsolute.sender_id);
        int numSolutes = newsolute.solutes.Length;

        for (int counter = 0; counter < numSolutes; counter++)
        {
            SoluteInfo newSolute = new SoluteInfo(newsolute.solutes[counter].ToLower(), nLayers);
            newSolute.OwnerName = compName;
            if (PositionInArray(newSolute.Name, mobile_solutes) >= 0)
                newSolute.IsMobile = true;
            else if (PositionInArray(newSolute.Name, immobile_solutes) >= 0)
                newSolute.IsMobile = false;
            else
                throw new Exception("No information for mobility of " + newSolute.Name +
                    " was given, please specify it as mobile or immobile in the SoilWater ini file.");

            // Register new "flow" and "leach" outputs for these solutes
            // See "getPropertyValue" function for the callback used to actually retrieve the values
            newSolute.flow_id = My.RegisterProperty("flow_" + newSolute.Name,
                                            "<type kind=\"double\" array=\"T\" unit=\"kg/ha\"/>",
                                            true, false, false, "flow of " + newSolute.Name, "",
                                            getPropertyValue);
            newSolute.leach_id = My.RegisterProperty("leach_" + newSolute.Name,
                                            "<type kind=\"double\" unit=\"kg/ha\"/>",
                                            true, false, false, "leaching of " + newSolute.Name, "", getPropertyValue);
            solutes.Add(newSolute);
            nSolutes = nSolutes + 1;
        }
    }

    //public void OnNew_solute(NewSoluteType newsolute)
    //{
    //    // Gets the name of a new solute, what module owns the new solute, and whether it is mobile or immobile.
    //    //  It alerts you at any given point in a simulation when a new solute is added. 

    //    int sender = (int)My.eventSenderId;
    //    string compName = MyPaddock.SiblingNameFromId(newsolute.sender_id);
    //    int numSolutes = newsolute.solutes.Length;
    //    string name;

    //    Array.Resize(ref solutes, nSolutes + numSolutes);

    //    for (int counter = 0; counter < numSolutes; counter++)
    //    {
    //        name = newsolute.solutes[counter].ToLower();
    //        solutes[nSolutes].name = name;
    //        solutes[nSolutes].ownerName = compName;
    //        solutes[nSolutes].mobility = PositionInArray(name, mobile_solutes) >= 0;
    //        if (!solutes[nSolutes].mobility && PositionInArray(name, immobile_solutes) < 0)
    //            throw new Exception("No solute mobility information for " + name + " , please specify as mobile or immobile in the SoilWater ini file.");

    //        // Create layer arrays for the new solute
    //        solutes[nSolutes].amount = new double[nLayers];
    //        solutes[nSolutes].delta = new double[nLayers];
    //        solutes[nSolutes].leach = new double[nLayers];
    //        solutes[nSolutes].up = new double[nLayers];

    //        // Register new "flow" and "leach" outputs for these solutes
    //        // See "getPropertyValue" function for the callback used to actually retrieve the values
    //        solutes[nSolutes].get_flow_id = My.RegisterProperty("flow_" + name,
    //                                        "<type kind=\"double\" array=\"T\" unit=\"kg/ha\"/>",
    //                                        true, false, false, "flow of " + name, "",
    //                                        getPropertyValue);
    //        solutes[nSolutes].get_leach_id = My.RegisterProperty("leach_" + name,
    //                                        "<type kind=\"double\" unit=\"kg/ha\"/>",
    //                                        true, false, false, "leaching of " + name, "", getPropertyValue);
    //        nSolutes = nSolutes + 1;
    //    }
    //}

    /// <summary>
    /// Infraestructure that allows other modules to get leaching outputs
    /// </summary>
    /// <param name="propID">The id of the variable desired (flow_ or leach_)</param>
    /// <param name="value">the output container</param>
    /// <param name="isReqSet">whether is a get or a set</param>
    /// <returns></returns>
    public bool getPropertyValue(int propID, ref TPropertyInfo value, bool isReqSet)
    {
        if (isReqSet)  // currently only handling read requests (get), so fail if this is not.
            return false;
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            if (solutes[solnum].leach_id == propID)
            {
                value.setValue(solutes[solnum].FlowAboveDUL[solutes[solnum].FlowAboveDUL.Length - 1]);
                return true;
            }
            if (solutes[solnum].flow_id == propID)
            {
                int num_layers = solutes[solnum].FlowBelowDUL.Length;
                double[] result = new double[num_layers];
                for (int layer = 0; layer < num_layers; layer++)
                    result[layer] = solutes[solnum].FlowAboveDUL[layer] - solutes[solnum].FlowBelowDUL[layer];
                value.setValue(result);
                return true;
            }
        }
        return false;
    }

    /// <summary>
    /// Gets the index of first occurence of a value in an array
    /// </summary>
    /// <param name="theValue">the value being sough</param>
    /// <param name="theArray">the array being searched</param>
    /// <returns>the index number</returns>
    private int PositionInArray(string theValue, string[] theArray)
    {
        for (int i = 0; i < theArray.Length; i++)
            if (theArray[i].ToLower() == theValue.ToLower())
                return i;
        return -1;  // Not found
    }

    /// <summary>
    /// Add water from irrigation
    /// </summary>
    /// <param name="irrigationData">Irrigation data</param>
    [EventHandler]
    public void OnIrrigated(IrrigationApplicationType irrigationData)
    {
        // Collect the amount of irrigation (mm), addition will happen OnProcess
        irrigation = irrigationData.Amount;

        if (irrigationData.will_runoff == 1)
        {
            irrigation_will_runoff = "yes";
            if (irrigationData.Depth > 0.0)
            {
                // get the specific layer that the irrigation is to go into.
                irrigation_layer = FindLayerNo(irrigationData.Depth);

                // check runof option
                irrigation_will_runoff = "no";
                My.Warning("The irrigation applied was set to allow runoff, but it is not a surface irrigation \n" +
                           "Only surface irrigation can runoff, thus this irrigation event will be allowed to runoff \n" +
                           " note: runoff may happen even with subsurface irrigation when soil is saturated and ponding occurs.");
            }
        }
        else
        {
            irrigation_will_runoff = "no";
            irrigation_layer = 0;
        }

        //Solute amount in irrigation water.
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            switch (solutes[solnum].Name)
            {
                case "no3":
                    solutes[solnum].AmountInIrrigation += irrigationData.NO3;
                    break;
                case "nh4":
                    solutes[solnum].AmountInIrrigation += irrigationData.NH4;
                    break;
                case "cl":
                    solutes[solnum].AmountInIrrigation += irrigationData.CL;
                    break;
                default:
                    solutes[solnum].AmountInIrrigation += 0.0;
                    break;
            }
        }
    }

    /// <summary>
    /// Gets the water changes from another module
    /// </summary>
    /// <param name="WaterChanged">Changes in water</param>
    [EventHandler]
    public void OnWaterChanged(WaterChangedType WaterChanged)
    {
        // this method is used by Plant2 and AgPasture
        for (int layer = 0; layer < WaterChanged.DeltaWater.Length; layer++)
        {
            swAmount[layer] = swAmount[layer] + WaterChanged.DeltaWater[layer];
            soilwat2_check_profile(layer);
        }
    }

    [EventHandler]
    public void Onsum_report()
    {
        //Manager module can request that each module write its variables out to the summary file. 
        WriteReport();
    }

    [EventHandler]
    public void OnReset()
    {
        //Save State
        double oldSWDep = MathUtility.Sum(swAmount);

        //Change state
        soilwat2_zero_variables();
        GetCropVariables();
        GetSoluteVariables();
        soilwat2_init();

        //Send mass balance changes
        double newSWDep = MathUtility.Sum(swAmount);
        SendExternalMassFlow(newSWDep - oldSWDep);
    }

    /// <summary>
    /// Set up CN reduction after tillage operation
    /// </summary>
    /// <param name="TillageData">TillageData data</param>
    [EventHandler]
    public void OnTillage(TillageType TillageData)
    {
        string message;             //! message string
        string tillage_type;             //! name of implement used for tillage//! 1. Find which implement was used. eg. disc, burn, etc.


        // cn_red is the reduction in the cn value, and cn_rain is the amount of rainfall after the
        //  tillage event that the reduction ceases to occur.
        // In the data there is at least the type of tillage. Even if it does not give the cn_red and cn_rain.
        //  if the data does not contain cn_red and cn_rain (i.e. they are negatives) then use the type name to
        //  look up the values in the ini file. This departs slightly from the Fortran version, where they were 
        //  optional arguments

        tillage_type = TillageData.type;
        if ((TillageData.cn_red <= 0) || (TillageData.cn_rain <= 0))
        {
            Console.WriteLine();

            TillageType data = SoilWatTillageType.GetTillageData(tillage_type);
            if (data != null)
            {
                if (TillageData.cn_red >= 0)
                    tillage_cn_red = data.cn_red;

                if (TillageData.cn_rain >= 0)
                    tillage_cn_rain = data.cn_rain;

                Console.WriteLine(" Using default tillage info for type: " + tillage_type);
                Console.WriteLine("  CN reduction: " + tillage_cn_red.ToString("#0.0#") +
                                  ", accum.  rain: " + tillage_cn_rain.ToString("#0"));
            }
            else
            {
                //sv- Event did not give us the tillage information and the sim file does not have the tillage information.
                //! We have an unspecified tillage type
                tillage_cn_red = 0.0;
                tillage_cn_rain = 0.0;

                message = " Cannot find info for tillage type: " + TillageData.type;
                throw new Exception(message);
            }
        }
        else
        {
            tillage_cn_red = bound(tillage_cn_red, 0.0, _cn2_bare);
            tillage_cn_rain = Math.Max(0.0, TillageData.cn_rain);

            Console.WriteLine(" Using tillage info supplied for type: " + tillage_type);
            Console.WriteLine("  CN reduction: " + tillage_cn_red.ToString("#0.0#") +
                              ", accum. rain: " + tillage_cn_rain.ToString("#0"));
        }

        //! 3. Reset the rain accumulator
        tillage_rain_sum = 0.0;
    }

    #endregion

    //EVENTS - SENDING
    #region Publish Events

    private void soilwat2_New_Profile_Event()
    {
        //*+  Mission Statement
        //*     Advise other modules of new profile specification
        if (New_profile != null)
        {
            NewProfileType newProfile = new NewProfileType();
            // Convert array values from doubles to floats
            newProfile.air_dry_dep = ToFloatArray(swAirDry);
            newProfile.bd = ToFloatArray(bd);
            newProfile.dlayer = ToFloatArray(dLayer);
            newProfile.dul_dep = ToFloatArray(swDUL);
            newProfile.ll15_dep = ToFloatArray(swLL15);
            newProfile.sat_dep = ToFloatArray(swSAT);
            newProfile.sw_dep = ToFloatArray(swAmount);
            if (newProfile != null)
                New_profile.Invoke(newProfile);
        }
    }

    /// <summary>
    /// Send some info about changes in mass balance
    /// </summary>
    /// <param name="sw_dep_delta_sum"></param>
    private void SendExternalMassFlow(double sw_dep_delta_sum)
    {
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

    private void SetSolutesValues()
    {
        NitrogenChangedType NitrogenChanges = new NitrogenChangedType();
        NitrogenChanges.Sender = "SoilWater";
        NitrogenChanges.SenderType = "WateModule";
        NitrogenChanges.DeltaUrea = new double[nLayers];
        NitrogenChanges.DeltaNH4 = new double[nLayers];
        NitrogenChanges.DeltaNO3 = new double[nLayers];

        //for all solutes in this simulation.
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            float[] temp_dlt_solute = ToFloatArray(solutes[solnum].Delta);

            //set the change in solutes for the modules
            string propName;
            if (solutes[solnum].Name == "urea")
                NitrogenChanges.DeltaUrea = solutes[solnum].Delta;
            else if (solutes[solnum].Name == "nh4")
                NitrogenChanges.DeltaNH4 = solutes[solnum].Delta;
            else if (solutes[solnum].Name == "no3")
                NitrogenChanges.DeltaNO3 = solutes[solnum].Delta;
            else
            {
                if (solutes[solnum].OwnerName != "")
                    propName = solutes[solnum].OwnerName + ".dlt_" + solutes[solnum].Name;
                else
                    propName = "dlt_" + solutes[solnum].Name;
                MyPaddock.Set(propName, temp_dlt_solute);
            }
        }
        NitrogenChanged.Invoke(NitrogenChanges);
    }

    private void RaiseRunoffEvent()
    {
        if (runoff > 0.0)
        {
            RunoffEventType r = new RunoffEventType(); //! structure holding runoff event
            r.runoff = (float)runoff;
            if (Runoff != null)
                Runoff.Invoke(r);
        }
    }

    #endregion

    #region Generic functions

    private void ResizeProfileArrays()
    {
        Array.Resize(ref dLayer, nLayers);
        Array.Resize(ref swSAT, nLayers);
        Array.Resize(ref swDUL, nLayers);
        Array.Resize(ref swLL15, nLayers);
        Array.Resize(ref swAirDry, nLayers);
        Array.Resize(ref swAmount, nLayers);
        Array.Resize(ref bd, nLayers);
        Array.Resize(ref evaporation, nLayers);
        Array.Resize(ref waterBypass, nLayers);
        Array.Resize(ref waterFlowAboveDUL, nLayers);
        Array.Resize(ref waterFlowBelowDUL, nLayers);
        Array.Resize(ref flux, nLayers);
        Array.Resize(ref outflow_lat, nLayers);

        //also resize for all solutes in this simulation.
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            Array.Resize(ref solutes[solnum].Amount, nLayers);
            Array.Resize(ref solutes[solnum].FlowSaturated, nLayers);
            Array.Resize(ref solutes[solnum].FlowAboveDUL, nLayers);
            Array.Resize(ref solutes[solnum].FlowBelowDUL, nLayers);
            Array.Resize(ref solutes[solnum].Delta, nLayers);
        }
    }

    /// <summary>
    /// Find the soil layer in which the indicated depth is located
    /// </summary>
    /// <param name="Depth">the depth being tested</param>
    /// <returns>the layer number (0-based)</returns>
    private int FindLayerNo(double Depth)
    {
        // If the depth is not reached, the last element is used
        double depthFromSurface = 0.0;
        for (int i = 0; i < nLayers; i++)
        {
            depthFromSurface = depthFromSurface + dLayer[i];
            if (depthFromSurface >= Depth)
                return i;
        }
        return nLayers - 1;
    }

    private float[] ToFloatArray(double[] D)
    {
        float[] f = new float[D.Length];
        for (int i = 0; i < D.Length; i++)
            f[i] = (float)D[i];
        return f;
    }

    private void ZeroArray(ref double[] theArray)
    {
        for (int i = 0; i < theArray.Length; i++)
        {
            theArray[i] = 0.0;
        }
    }

    #endregion

    /// <summary>
    /// Defines a SoilWater solute
    /// </summary>
    private class SoluteInfo
    {
        public string Name;        // theValue of the solute
        public string OwnerName;    // FQN of the component handling this solute
        public bool IsMobile;      // Is the solute mobile?
        public double[] Amount;    // amount of solute in each layer (kg/ha)
        public double[] FlowSaturated;     // amount leached from each layer (kg/ha)
        public double[] FlowAboveDUL;        // amount "upped" from each layer (kg/ha)
        public double[] FlowBelowDUL;        // amount "upped" from each layer (kg/ha)
        public double[] Delta;     // change in solute in each layer (kg/ha)
        public double ConcentrationInRain;   // concentration in rainfall (ppm)
        public double ConcentrationInIrrigation;   // concentration in irrigation water (ppm)
        public double AmountInIrrigation;  // amount of solute in irrigation water (kg/ha)
        public int flow_id;    // registration ID for getting flow values
        public int leach_id;    // registration ID for getting leach value

        public SoluteInfo(string nameSolute, int numLayers)
        {
            Name = nameSolute;
            Amount = new double[numLayers];
            FlowSaturated = new double[numLayers];
            FlowAboveDUL = new double[numLayers];
            FlowBelowDUL = new double[numLayers];
            Delta = new double[numLayers];

            IsMobile = true;
            ConcentrationInRain = 0.0;
            ConcentrationInIrrigation = 0.0;
        }
    }

    //private struct Solute
    //{
    //    public string name;        // theValue of the solute
    //    public string ownerName;    // FQN of the component handling this solute
    //    public bool mobility;      // Is the solute mobile?
    //    public double[] amount;    // amount of solute in each layer (kg/ha)
    //    public double[] leach;     // amount leached from each layer (kg/ha)
    //    public double[] up;        // amount "upped" from each layer (kg/ha)
    //    public double[] delta;     // change in solute in each layer (kg/ha)
    //    public double rain_conc;   // concentration entering via rainfall (ppm)
    //    public double irrigation;  // amount of solute in irrigation water (kg/ha)
    //    public int get_flow_id;    // registration ID for getting flow values
    //    public int get_leach_id;    // registration ID for getting leach value
    //}
}

public class SoilWatTillageType
{
    Dictionary<string, float[]> tillage_types;

    protected float[] strToArr(string str)
    {
        string[] temp = str.Split(new char[] { ' ', '\t', ',', '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
        float[] result = new float[temp.Length];

        for (int i = 0; i < result.Length; i++)
            result[i] = float.Parse(temp[i]);

        return result;
    }

    [Param]
    System.Xml.XmlNode xe = null;

    [XmlAnyElement]
    public System.Xml.XmlElement[] Nodes = null;

    [EventHandler]
    public void OnInitialised()
    {
        tillage_types = new Dictionary<string, float[]>();

            foreach (System.Xml.XmlNode xnc in xe.ChildNodes)
                if (xnc.NodeType == System.Xml.XmlNodeType.Element)
                    tillage_types.Add(xnc.Name, strToArr(xnc.FirstChild.Value));
    }

    public TillageType GetTillageData(string name)
    {
        return tillage_types.ContainsKey(name) ? new TillageType() { type = name, cn_red = tillage_types[name][0], cn_rain = tillage_types[name][1] } : null;
    }
}
