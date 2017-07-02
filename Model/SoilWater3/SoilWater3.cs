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
    [Description("Maximum soil albedo with 100% green crop cover")]
    public double max_albedo = 0.23;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Coefficient for the reduction on evaporation due to residue cover")]
    public double A_to_evap_fact = 0.44;

    [Output]
    [Param(MinVal = 0.0, MaxVal = 10.0)]
    [Units("0-10")]
    [Description("Coefficient for the reduction on evaporation due to canopy")]
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
    public double[] solute_flow_eff = { 1.0 };

    [Output]
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Efficiency of moving solute with flow above DUL")]
    public double[] solute_flux_eff = { 1.0 };

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
    public double[] canopy_fact = { 1, 1, 0, 0 };

    [Output]
    [Param(MinVal = 0.0, MaxVal = 100000.0)]
    [Units("mm")]
    [Description("Heights for canopy factors")]
    public double[] canopy_fact_height = { 0, 600, 1800, 30000 };

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

    private bool irrigationCanRunoff = false;
    [Param(IsOptional = true, MinVal = 0, MaxVal = 100)]
    [Output]
    [Units("yes/no")]
    [Description("Irrigation will runoff like rain")]
    private string irrigation_will_runoff
    {
        get { return irrigationCanRunoff ? "yes" : "no"; }
        set { irrigationCanRunoff = value.ToLower() == "yes"; }
    }

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
    public double diffus_const = double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Slope for relationshipe between water diffusivity and soil water content")]
    public double diffus_slope = double.NaN;

    #region Runoff paramters

    [Output]
    [Param(IsOptional = true, Name = "observed_runoff")]
    [Description("System variable name of external observed runoff source")]
    private string obsrunoff_name = "";

    /// <summary>Curve number for bare soil</summary>
    private double bareSoilCN = double.NaN;
    private double resetBareSoilCN;
    [Output]
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 100.0)]
    [Description("Curve number for bare soil used to calculate daily runoff")]
    public double cn2_bare
    {
        get { return bareSoilCN; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetBareSoilCN = value;
            }
            bareSoilCN = value;
        }
    }

    /// <summary>Maximum reduction of curve number due to cover</summary>
    private double maxCNreduction = double.NaN;
    private double resetMaxCNreduction;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Maximum reduction of curve number due to cover")]
    public double cn_red
    {
        get { return maxCNreduction; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetMaxCNreduction = value;
            }
            maxCNreduction = value;
        }
    }

    /// <summary>Cover for maximum cn reduction</summary>
    private double coverAtMinimumCN = double.NaN;
    private double resetCoverMinCN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Cover at which maximum cn reduction occurs")]
    public double cn_cov
    {
        get { return coverAtMinimumCN; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetCoverMinCN = value;
            }
            coverAtMinimumCN = value;
        }
    }

    /// <summary>Maximum surface storage capacity (mm)</summary>
    private double maxPondCapacity = 0.0;
    double resetMaxPond;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Maximum surface storage capacity of soil")]
    public double max_pond
    {
        get { return maxPondCapacity; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetMaxPond = value;
            }
            maxPondCapacity = value;
        }
    }

    #endregion

    #region Evaporation parameters

    private string myPETsource = "";
    [Output]
    [Param(IsOptional = true)]
    [Description("Name of potential evaporation source")]
    public string eo_source
    {
        get { return myPETsource; }
        set { myPETsource = value.ToLower(); }
    }

    [Param(MinVal = 0.0001, MaxVal = 1.0)]
    [Units("0-1")]
    [Output]
    [Description("Bare soil albedo")]
    public double salb;

    private double myU = double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 40.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation")]
    public double u
    {
        get { return myU; }
        set { myU = value; }
    }

    private double myConA = double.NaN;
    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient")]
    public double cona
    {
        get { return myConA; }
        set { myConA = value; }
    }

    [Output]
    [Param(IsOptional = true)]
    [Description("Date for start of summer evaporation (dd-mmm)")]
    public string summerdate = "not set";

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 40.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation during summer")]
    public double summeru = double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient during summer")]
    public double summercona = double.NaN;

    //winter
    [Output]
    [Param(IsOptional = true)]
    [Description("Date for start of winter evaporation (dd-mmm)")]
    public string winterdate = "not set";

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Units("mm")]
    [Description("Upper limit of stage 1 soil evaporation during winter")]
    public double winteru = double.NaN;

    [Output]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Stage 2 drying coefficient during winter")]
    public double wintercona = double.NaN;

    #endregion

    #region Lateral Flow parameters

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Slope of terrain for lateral flow calculations")]
    public double slope = double.NaN;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0e8F)]
    [Units("m")]
    [Description("Basal width of discharge area")]
    public double discharge_width = double.NaN;

    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0e8F)]
    [Units("m^2")]
    [Description("Area over which lateral flow is occuring")]
    public double catchment_area = double.NaN;

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
            if (isInitialising)
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

            //check and correct the water amounts
            for (int layer = 0; layer < nLayers; layer++)
            {
                double fract = MathUtility.Divide(value[layer], dLayer[layer], 0.0);
                swSAT[layer] = swSAT[layer] * fract;
                swDUL[layer] = swDUL[layer] * fract;
                swLL15[layer] = swLL15[layer] * fract;
                swAirDry[layer] = swAirDry[layer] * fract;
                swAmount[layer] = swAmount[layer] * fract;
                dlayer[layer] = value[layer];
            }

            if (!isInitialising)
            {
                //initialisation has finished
                //publish event telling other modules that there is a new soil profile
                sendNewProfileEvent();
            }
        }
    }

    /// <summary>Soil bulk density (g/cm3)</summary>
    private double[] myBD;
    private double[] resetBD;
    [Param(MinVal = 0.01, MaxVal = 3.0)]
    [Units("g/cm^3")]
    [Output(Immutable = true)]
    [Description("Bulk density of soil, fines only")]
    public double[] bd
    {
        get { return myBD; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetBD = new double[value.Length];
                Array.Copy(value, resetBD, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                myBD[layer] = value[layer];
                //soilwat2_check_profile(layer);
            }
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
            double[] mySAT = new double[nLayers];

            for (int layer = 0; layer < nLayers; layer++)
                mySAT[layer] = MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0);
            return mySAT;
        }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetSAT = new double[value.Length];
                Array.Copy(value, resetSAT, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swSAT[layer] = value[layer] * dLayer[layer];
                checkSoilWaterThresholds(layer);
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
            double[] myDUL = new double[nLayers];

            for (int layer = 0; layer < nLayers; layer++)
                myDUL[layer] = MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0);
            return myDUL;
        }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetDUL = new double[value.Length];
                Array.Copy(value, resetDUL, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swDUL[layer] = value[layer] * dLayer[layer];
                checkSoilWaterThresholds(layer);
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
            double[] myLL15 = new double[nLayers];

            for (int layer = 0; layer < nLayers; layer++)
                myLL15[layer] = MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0);
            return myLL15;
        }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetLL15 = new double[value.Length];
                Array.Copy(value, resetLL15, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swLL15[layer] = value[layer] * dLayer[layer];
                checkSoilWaterThresholds(layer);
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
            double[] myAirDry = new double[nLayers];

            for (int layer = 0; layer < nLayers; layer++)
                myAirDry[layer] = MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0);
            return myAirDry;
        }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetAirDry = new double[value.Length];
                Array.Copy(value, resetAirDry, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                swAirDry[layer] = value[layer] * dLayer[layer];
                checkSoilWaterThresholds(layer);
            }
        }
    }

    private double[] resetSWC;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("mm/mm")]
    [Output]
    [Description("Soil water content, actual")]
    public double[] sw
    {
        get
        {
            double[] mySW = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                mySW[layer] = MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0);
            return mySW;
        }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetSWC = new double[value.Length];
                Array.Copy(value, resetSWC, value.Length);
            }

            double oldSWAmount = MathUtility.Sum(swAmount);
            // changes are actually done in swAmount
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAmount[layer] = value[layer] * dLayer[layer];
                checkSoilWaterContent(layer);
            }

            if (!isInitialising)
            {
                //tell the "System Balance" module (if there is one) that the user has changed the water by this amount
                double newSWAmount = MathUtility.Sum(swAmount);
                sendExternalMassFlow(newSWAmount - oldSWAmount);
            }
        }
    }

    /// <summary>Soil water conductance constant (/day)</summary>
    private double[] mySWCon;
    private double[] resetSWCon;
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("/d")]
    [Output(Immutable = true)]
    [Description("Soil water conductance constant")]
    public double[] swcon
    {
        get { return mySWCon; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetSWCon = new double[value.Length];
                Array.Copy(value, resetSWCon, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
                mySWCon[layer] = value[layer];
        }
    }

    /// <summary>Soil water conductance constant (/day)</summary>
    private double[] myMWCon;
    private double[] resetMWCon;
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("/d")]
    [Output(Immutable = true)]
    [Description("Saturated water flow indicator")]
    public double[] mwcon
    {
        get { return myMWCon; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetMWCon = new double[value.Length];
                myMWCon = new double[value.Length];
                Array.Copy(value, resetMWCon, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
                myMWCon[layer] = value[layer];
        }
    }

    [Output]
    [Description("Flag to determine if Ks has been chosen for use")]
    private bool usingKsat;

    /// <summary>Soil water conductance constant (/day)</summary>
    private double[] myKSat;
    private double[] resetKSat;
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Units("mm/d")]
    [Output(Immutable = true)]
    [Description("Saturated water conductivity")]
    public double[] ks
    {
        get { return myKSat; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetKSat = new double[value.Length];
                myKSat = new double[value.Length];
                Array.Copy(value, resetKSat, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
                myKSat[layer] = value[layer];
        }
    }

    /// <summary>Soil water conductance constant (/day)</summary>
    private double[] myKLat;
    private double[] resetKLat;
    [Param(IsOptional = true, MinVal = 0, MaxVal = 10000.0)]
    [Units("mm/d")]
    [Output(Immutable = true)]
    [Description("Water conductivity for lateral flow")]
    public double[] klat
    {
        get { return myKLat; }
        set
        {
            if (isInitialising)
            {
                //store a copy so if there is a Reset event we can set it back to this value.
                resetKLat = new double[value.Length];
                myKLat = new double[value.Length];
                Array.Copy(value, resetKLat, value.Length);
            }

            for (int layer = 0; layer < nLayers; layer++)
                myKLat[layer] = value[layer];
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

    [Output(Immutable = true)]
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
    [Description("Curve number after modification for crop cover & residue cover")]
    private double cn2_cover;

    [Output]
    [Description("Curve number after modification for antecedent soil moisture")]
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

    /// <summary>Depth of water table (mm)</summary>
    private double waterTableDepth = double.NaN;
    [Output]
    [Units("mm")]
    [Description("Water table depth (depth below the ground surface of the first saturated layer)")]
    public double water_table
    {
        get { return waterTableDepth; }
        set { waterTableDepth = SetWaterTable(value); }
    }

    /// <summary>Water amount at saturation (mm)</summary>
    private double[] swSAT;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at saturation")]
    public double[] sat_dep
    {
        get { return swSAT; }
        set
        {
            for (int layer = 0; layer < value.Length; layer++)
            {
                swSAT[layer] = value[layer];
                checkSoilWaterThresholds(layer);
            }
        }
    }

    /// <summary>Water amount at drainage upper limit (mm)</summary>
    private double[] swDUL;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at drainage upper limit")]
    public double[] dul_dep
    {
        get { return swDUL; }
        set
        {
            for (int layer = 0; layer < nLayers; layer++)
            {
                swDUL[layer] = value[layer];
                checkSoilWaterThresholds(layer);
            }
        }
    }

    /// <summary>Water amount at lower limit for water movement (mm)</summary>
    private double[] swLL15;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at drainage lower limit")]
    public double[] ll15_dep
    {
        get { return swLL15; }
        set
        {
            for (int layer = 0; layer < nLayers; layer++)
            {
                swLL15[layer] = value[layer];
                checkSoilWaterThresholds(layer);
            }
        }
    }

    /// <summary>Water amount at air dry condition (mm)</summary>
    private double[] swAirDry;
    [Output(Immutable = true)]
    [Units("mm")]
    [Description("Water amount at air dry condition")]
    public double[] air_dry_dep
    {
        get { return swAirDry; }
        set
        {
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAirDry[layer] = value[layer];
                checkSoilWaterThresholds(layer);
            }
        }
    }

    /// <summary>Current water amount in the soil (mm)</summary>
    private double[] swAmount;
    [Output]
    [Units("mm")]
    [Description("Water amount, current")]
    public double[] sw_dep
    {
        get { return swAmount; }
        set
        {
            double oldSWAmount = MathUtility.Sum(swAmount);
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAmount[layer] = value[layer];
                checkSoilWaterContent(layer);
            }

            //tell the "System Balance" module (if there is one) that the user has changed the water by this amount
            double newSWAmount = MathUtility.Sum(swAmount);
            sendExternalMassFlow(newSWAmount- oldSWAmount);
        }
    }

    [Output]
    [Units("mm")]
    [Description("Initially, water moving downward into layer i (mm), then water moving downward out of layer i (saturated flow)")]
    private double[] flux;

    /// <summary>Water amount bypassing each layer (mm)</summary>
    private double[] waterBypass;
    [Output]
    [Units("mm")]
    [Description("Water moving downward during saturated flow, bypass")]
    private double[] WaterFlowBypass
    {
        get { return waterBypass; }
    }

    /// <summary>Water amount moving during flow above DUL (mm)</summary>
    private double[] waterFlowAboveDUL;
    [Output]
    [Units("mm")]
    [Description("Water amount moving during flow above DUL")]
    private double[] WaterFlowAboveDUL
    {
        get { return waterFlowAboveDUL; }
    }

    /// <summary>Water amount moving during flow below DUL (mm)</summary>
    private double[] waterFlowBelowDUL;
    [Output]
    [Units("mm")]
    [Description("Water amount moving during flow below DUL (upwards or downwards")]
    private double[] WaterFlowBelowDUL
    {
        get { return waterFlowBelowDUL; }
    }

    [Output]
    [Units("mm")]
    [Description("Depth of water moving between layers because of unsaturated flow")]
    private double[] flow
    {
        //positive value indicates upward movement, negative value indicates downward movement
        get
        {
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = -waterFlowBelowDUL[layer];
            return result;
        }
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

    private double[] outflowLateral;
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
            //check and correct the water amounts
            for (int layer = 0; layer < value.Length; layer++)
            {
                double fract = MathUtility.Divide((dLayer[layer] + value[layer]), dLayer[layer], 0.0);
                swAirDry[layer] = swAirDry[layer] * fract;
                swDUL[layer] = swDUL[layer] * fract;
                swLL15[layer] = swLL15[layer] * fract;
                swSAT[layer] = swSAT[layer] * fract;
                swAmount[layer] = swAmount[layer] * fract;
                dLayer[layer] = dLayer[layer] + value[layer];
            }

            //publish event telling other modules that there is a new soil profile.
            sendNewProfileEvent();
        }
    }

    [Output]
    [Units("mm")]
    public double[] dlt_sw
    {
        set
        {
            double oldSWAmount = MathUtility.Sum(swAmount);
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAmount[layer] += (value[layer] * dLayer[layer]);
                checkSoilWaterContent(layer);
            }

            //tell the "System Balance" module (if there is one) that the user has changed the water by this amount
            double newSWAmount = MathUtility.Sum(swAmount);
            sendExternalMassFlow(newSWAmount - oldSWAmount);
        }
    }

    [Output]
    [Units("mm")]
    public double[] dlt_sw_dep
    {
        set
        {
            double oldSWAmount = MathUtility.Sum(swAmount);
            for (int layer = 0; layer < nLayers; layer++)
            {
                swAmount[layer] += value[layer];
                checkSoilWaterContent(layer);
            }

            //tell the "System Balance" module (if there is one) that the user has changed the water by this amount
            double newSWAmount = MathUtility.Sum(swAmount);
            sendExternalMassFlow(newSWAmount - oldSWAmount);
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
                Array.Resize(ref value, nLayers);

            if (inflowLateral.Length < nLayers)
                Array.Resize(ref inflowLateral, nLayers);

            Array.Copy(value, inflowLateral, nLayers);
        }
    }

    //eo might come from micromet, needs setting of eo_source
    [Input(IsOptional = true)]
    [Units("mm")]
    [Description("potential evaporation")]
    private double pet_total = double.NaN;

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
            foundCL = MyPaddock.Get(Comp.FullName + ".cover_green", out coverLive);
            foundCT = MyPaddock.Get(Comp.FullName + ".cover_tot", out coverTotal);
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
                throw new Exception("Crop Module: " + Comp.FullName +
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

    //Initialisation and reset
    bool isInitialising = true;
    bool isResetting = false;

    //Runoff
    private double potentialRunoff;     // potential runoff with no pond(mm)
    private double observedRunoff;      // observed runoff (mm)
    private double extraRunoff;         // water backed up, could not infiltrate

    //GET CROP VARIABLES
    private double[] canopyTotalCover;  // total canopy cover of crops (0-1)
    private double[] canopyGreenCover;  // green canopy cover of crops (0-1)
    private double[] canopyHeight;      // canopy heights of each crop (mm)
    private int nCrops;                 // number of crops ()

    //Tillage
    private double tillage_cn_red;      // reduction in CN due to tillage ()
    private double tillage_cn_rain;     // cumulative rainfall below which tillage reduces CN (mm)
    private double tillage_rain_sum;    // cumulative rainfall for tillage CN reduction (mm)

    //Evaporation
    private int evapMethod;             // indication of the method to compute actual evaporation (currently only Ritchie)
    private double suppliedPET;         // eo from somewhere else in the system (mm) - see eo_source
    private double sumes1;              // cumulative soil evaporation in stage 1 (mm)
    private double sumes2;              // cumulative soil evaporation in stage 2 (mm)
    private double[] evaporation;       // actual soil evaporation (mm)

    //Solutes
    private List<SoluteInfo> solutes = new List<SoluteInfo>();
    int nSolutes;

    //Irrigation
    private double irrigation;       // irrigation (mm)

    #endregion

    #region Local constants

    private const int defaultEvapMethod = 1; // that is the Ritchie method
    private const double myEpsilon = 0.000001; // Precision level for sw dep (mm)
    private const double precisionForProfileSW = 0.00001; // Precision for sw dep over all profile(mm)

    #endregion

    //MODEL

    #region Check initial values and reset

    /// <summary>
    /// Computes the proportion of layer that is between the soil surface and the depth given (0-1)
    /// </summary>
    /// <param name="layer">Layer to be analysed</param>
    /// <param name="theDepth">Depth being tested</param>
    /// <returns>The proportion of layer that is between the soil surface and the given depth</returns>
    private double LayerProportion(int layer, double theDepth)
    {
        double depthAtTopLayer;     // depth to top of layer (mm)

        depthAtTopLayer = MathUtility.Sum(dLayer, 0, layer, 0.0) - dLayer[layer - 1];
        double result = MathUtility.Divide(theDepth - depthAtTopLayer, dLayer[layer - 1], 0.0);
        return Math.Max(0.0, Math.Min(result, 1.0));
    }

    /// <summary>
    /// Verify that the parameters for evaporation have been initialised
    /// </summary>
    private void checkEvaporationParameters()
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

        // The paramters u and cona can either use a single value or two different (for summer and winter).
            if (Double.IsNaN(myU))
            {
                if ((Double.IsNaN(summeru) && (Double.IsNaN(winteru))))
                    throw new Exception("the value for \'u\' OR BOTH values for \'summeru\' and \'winteru\' must be specified");
            }
            else
            {
                summeru = myU;
                winteru = myU;
            }

            if (Double.IsNaN(myConA))
            {
                if ((Double.IsNaN(summercona)) && (Double.IsNaN(wintercona)))
                    throw new Exception("the value for \'cona\' OR BOTH values for \'summercona\' and \'wintercona\' must be specified");
            }
            else
            {
                summercona = myConA;
                wintercona = myConA;
            }

            //summer and winter default dates.
            if (summerdate == "not set")
                summerdate = "1-oct";

            if (winterdate == "not set")
                winterdate = "1-apr";

        //assign u and cona to either sumer or winter values
        // Need to add 12 hours to move from "midnight" to "noon", or this won't work as expected
        //if (DateUtility.WithinDates(winterdate, Clock.Today, summerdate))
        //{
        //    _cona = wintercona;
        //    _u = winteru;
        //}
        //else
        //{
        //    _cona = summercona;
        //    _u = summeru;
        //}
    }

    /// <summary>
    /// Check the values for solute transport efficiency
    /// </summary>
    /// <remarks>
    /// Values are needed for each layer, so if only one was supplied, use it for all layers.
    /// </remarks>
    private void checkSoluteTransportEfficiencies()
    {
        // For transport above DUL
        if (solute_flux_eff.Length == 1)
        {
            // only one value was supplied, use this for all layers
            double theValue = solute_flux_eff[0];
            solute_flux_eff = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                solute_flux_eff[layer] = theValue;
        }
        else if (solute_flux_eff.Length < nLayers)
        {
            // data supplied is not complete
            throw new Exception("The number of values give to \'solute_flux_eff\' does not match the number of layers");
        }

        // For transport below DUL
        if (solute_flow_eff.Length == 1)
        {
            // only one value was supplied, use this for all layers
            double theValue = solute_flow_eff[0];
            solute_flow_eff = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                solute_flow_eff[layer] = theValue;
        }
        else if (solute_flow_eff.Length < nLayers)
        {
            // data supplied is not complete
            throw new Exception("The number of values give to \'solute_flow_eff\' does not match the number of layers");
        }
    }

    /// <summary>
    /// Initialise the optional parameters controling saturated flow (if not read in)
    /// </summary>
    /// <remarks>
    /// Need to establish, during initialisation, whether mwcon or ks will be used
    /// Note: mwcon = 0.0 indicates impermeable layer, while mwcon = 1.0 is fully permeable
    /// If mwcon is not specified then set it to 1.0. If ks is specified then mwcon is not used.
    /// </remarks>
    private void checkSaturatedFlowParameters()
    {
        if (myMWCon == null)
        {
            myMWCon = new double[nLayers];
            resetMWCon = new double[nLayers];
            for (int z = 0; z < nLayers; z++)
            {
                myMWCon[z] = 1.0;
                resetMWCon[z] = 1.0;
            }
        }

        if (myKSat == null)
        {
            usingKsat = false;
            myKSat = new double[nLayers];
            resetKSat = new double[nLayers];
        }
        else
            usingKsat = true;
    }

    /// <summary>
    /// Check that all parameters for lateral flow have been initialised
    /// </summary>
    /// <remarks>
    /// Lateral flow parameters are all optional, so zero them if not entered by user.
    /// </remarks>
    private void checkLateralFlowParameters()
    {
        if (Double.IsNaN(slope))
            slope = 0.0;

        if (Double.IsNaN(discharge_width))
            discharge_width = 0.0;

        if (Double.IsNaN(catchment_area))
            catchment_area = 0.0;

        if (myKLat == null)
        {
            myKLat = new double[nLayers];
            resetKLat = new double[nLayers];
        }
    }

    /// <summary>
    /// Reset all parameters that are "Settable" back to the original values
    /// </summary>
    private void resetSoilWater()
    {
        isResetting = true;

        //soil profile
        int nLayersReset = resetDLayer.Length;
        dLayer = new double[nLayersReset];
        Array.Copy(resetDLayer, dLayer, nLayersReset);
        Array.Copy(resetBD, bd, nLayersReset);
        Array.Copy(resetSAT, sat, nLayersReset);
        Array.Copy(resetDUL, dul, nLayersReset);
        Array.Copy(resetLL15, ll15, nLayersReset);
        Array.Copy(resetAirDry, air_dry, nLayersReset);
        Array.Copy(resetSWCon, swcon, nLayersReset);
        Array.Copy(resetMWCon, mwcon, nLayersReset);
        Array.Copy(resetKSat, ks, nLayersReset);
        Array.Copy(resetKLat, klat, nLayersReset);

        Array.Copy(resetSWC, sw, nLayersReset);

        //runoff
        bareSoilCN = resetBareSoilCN;
        maxCNreduction = resetMaxCNreduction;
        coverAtMinimumCN = resetCoverMinCN;

        //ponding
        maxPondCapacity = resetMaxPond;

        isResetting = false;
    }

    #endregion


    #region Functions to Zero Variables

    private void zeroVariables()
    {
        ZeroArray(ref evaporation);
        ZeroArray(ref outflowLateral);
        ZeroArray(ref waterBypass);
        ZeroArray(ref waterFlowAboveDUL);
        ZeroArray(ref waterFlowBelowDUL);
        ZeroArray(ref flux);

        nCrops = 0;
        canopyTotalCover = null;
        canopyGreenCover = null;
        canopyHeight = null;

        irrigation = 0.0;
        infiltration = 0.0;
        eo = 0.0;
        eos = 0.0;
        cn2_cover = 0.0;
        cn2_new = 0.0;
        potentialRunoff = 0.0;
        runoff = 0.0;
        drain = 0.0;
        pond_evap = 0.0;

        irrigation_layer = 0;

        // zero all solute information
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            ZeroArray(ref solutes[solnum].Amount);
            ZeroArray(ref solutes[solnum].FlowSaturated);
            ZeroArray(ref solutes[solnum].FlowAboveDUL);
            ZeroArray(ref solutes[solnum].FlowBelowDUL);
            ZeroArray(ref solutes[solnum].Delta);
            solutes[solnum].ConcentrationInRain = 0.0;
            solutes[solnum].AmountInIrrigation = 0.0;
        }

    }

    #endregion

    #region Bounds checking and warning functions

    #region Check a given layer for errors

    /// <summary>
    /// Checks that layer lies within the parameterised soil profile
    /// </summary>
    /// <param name="layer">the layer being tested</param>
    private void checkLayerExists(int layer)
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
    /// Checks validity of soil water parameters for a give soil layer
    /// </summary>
    /// <param name="layer">the layer being checked</param>
    private void checkSoilWaterThresholds(int layer)
    {
        if (isResetting || isInitialising)
            return;

        double min_sw_local = 0.0;
        double dul_local;               // drained upper limit water content of layer (mm water/mm soil)
        double ll15_local;              // lower limit at 15 bars water content of layer (mm water/mm soil)
        double airDry_local;            // lower limit at air dry water content of layer (mm water/mm soil)
        double sat_local;               // saturated water content of layer (mm water/mm soil)
        double sw_local;                // soil water content of layer l (mm water/mm soil)
        double max_porosity;            // largest acceptable value for sat (mm water/mm soil)

        max_porosity = 1.0 - MathUtility.Divide(myBD[layer], specific_bd, 0.0);

        sw_local = MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0);
        sat_local = MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0);
        dul_local = MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0);
        ll15_local = MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0);
        airDry_local = MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0);

        if (airDry_local - min_sw_local < myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " Air dry lower limit of ", airDry_local,
                       " in layer ", layer, "\n",
                       "         is below acceptable value of ", min_sw_local));
        }

        if (ll15_local - airDry_local < -myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " 15 bar lower limit of ", ll15_local,
                       " in layer ", layer, "\n",
                       "         is below air dry value of ", airDry_local));
        }

        if (dul_local - ll15_local < myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " drained upper limit of ", dul_local,
                       " in layer ", layer, "\n",
                       "         is at or below lower limit of ", ll15_local));
        }
        if (sat_local - dul_local < myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G})",
                       " saturation of ", sat_local,
                       " in layer ", layer, "\n",
                       "         is at or below drained upper limit of ", dul_local));
        }
        if (max_porosity - sat_local < -myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G} {7} {8} {9:G} {10} {11} {12:G})",
                       " saturation of ", sat_local,
                       " in layer ", layer, "\n",
                       "         is above acceptable value of ", max_porosity, "\n",
                       "You must adjust bulk density (bd) to below ", (1.0 - sat_local) * specific_bd, "\n",
                       "OR saturation (sat) to below ", max_porosity));
        }

        checkSoilWaterContent(layer);
    }

    private void checkSoilWaterContent(int layer)
    {
        if (isResetting || isInitialising)
            return;

        double sw_local;            // soil water content of layer l (mm water/mm soil)
        double airDry_local;        // lower limit at air dry water content of layer (mm water/mm soil)
        double sat_local;           // saturated water content of layer (mm water/mm soil)

        sat_local = MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0);
        airDry_local = MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0);
        sw_local = MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0);

        if (sat_local - sw_local < -myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                       " soil water of ", sw_local,
                       " in layer ", layer, "\n",
                       "         is above saturation of ", sat_local));
        }

        if (sw_local - airDry_local < -myEpsilon)
        {
            My.Warning(String.Format("({0} {1:G}) {2} {3} {4} {5} {6:G}",
                       " soil water of ", sw_local,
                       " in layer ", layer, "\n",
                       "         is below air-dry value of ", airDry_local));
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

    /// <summary>
    /// check whether a value of a given variable is within bounds
    /// </summary>
    /// <param name="theValue">Value being tested</param>
    /// <param name="minVal">Lower bound</param>
    /// <param name="maxVal">Upper bound</param>
    /// <param name="variableName">Variable name</param>
    /// <param name="doConstrain">Wheter the value is forced within bounds</param>
    /// <returns>The value given, adjusted or not</returns>
    private double checkBounds(double theValue, double minVal, double maxVal, string variableName, bool doConstrain, bool doReport = false)
    {
        double result;
        if (minVal > maxVal)
        {
            My.Warning("Lower bound (" + minVal + ") is greater than the upper bound (" + maxVal + "), the value of"
                + variableName + " cannot be constrained");
            return theValue;
        }

        if ((theValue < minVal) || (theValue > maxVal))
        {
            if (doReport)
            {
                My.Warning("The variable /'" + variableName + "/' is set to a value (" + theValue +
                    ") outside the expected range (" + minVal + " - " + maxVal + ")");
            }

            if (doConstrain)
                result = Math.Max(Math.Min(theValue, maxVal), minVal);
            else
                result = theValue;
        }

        return Math.Max(Math.Min(theValue, maxVal), minVal);
    }

    #endregion

    #region Soil Science Functions

    #region Runoff

    /// <summary>
    /// Gets the potential runoff
    /// </summary>
    /// <remarks>
    /// There are two options, controlled by obsrunoff_name. If empty runoff is calculated using scs curve number
    /// otherwise the user or apsim must supply a value.
    /// </remarks>
    /// <param name="precipitation">Amount of precipitation (rain + irrigation)</param>
    /// <param name="runOn">Amount of water rununing in</param>
    /// <param name="totalInterception">Amount of water intercepted by canopy and residues</param>
    /// <returns>Potential runoff amount (mm)</returns>
    private double PotentialRunoff(double precipitation, double runOn, double totalInterception)
    {
        double result = 0.0;

        if (precipitation + runOn - totalInterception > 0.0)
        {
            // there is some water that could runoff
            if (obsrunoff_name == "")
            {
                // no runoff values were supplied, computing using scs curve number
                result = CalcSCSRunoff(precipitation, runOn, totalInterception);
            }
            else
            {
                // runoff values are meant to be supplied, check them
                if (MyPaddock.Get(obsrunoff_name, out observedRunoff))
                {
                    // values were given, bound to available water
                    result = checkBounds(observedRunoff, 0.0, precipitation + runOn - totalInterception, "observed runoff", true, true);
                }
                else
                {
                    // no values were actually supplied
                    observedRunoff = double.NaN;
                    throw new Exception("No values for runoff, given by \'" + obsrunoff_name
                        + "\' were found. Check setting of \'obsrunoff_name\'");
                }
            }
        }

        return result;
    }

    /// <summary>
    /// Calculates the potential runoff using the SCS curve number approach
    /// </summary>
    /// <param name="precipitation">Amount of precipitation (rain + irrigation)</param>
    /// <param name="runOn">Amount of water rununing in</param>
    /// <param name="totalInterception">Amount of water intercepted by canopy and residues</param>
    /// <returns>Runoff amount (mm)</returns>
    private double CalcSCSRunoff(double precipitation, double runOn, double totalInterception)
    {
        double cn;                      // scs curve number
        double cn_drySoil;              // curve no. for dry soil (antecedent) moisture
        double cn_wetSoil;              // curve no. for wet soil (antecedent) moisture
        double coverFractor;            // proportion of maximum cover effect on runoff (0-1)
        double moistureFactor;          // proportional in range dul to ll15, to adjust cn
        double maxPotentialRetention;     // potential max retention (surface ponding + infiltration)
        double xpb;                     // intermediate variable for deriving runof
        double[] layerFactor;           // weighting factor for depth for each layer
        double swFac;                   // if between 0 and 1 sw is below DUL, if >1 then is above DUL
        double tillageEffect;           // reduction in cn due to tillage
        double result;

        // get the moisture factor
        moistureFactor = 0.0;
        layerFactor = CalcWeightFactorRunoff();
        for (int layer = 0; layer < nLayers; layer++)
        {
            swFac = MathUtility.Divide(swAmount[layer] - swLL15[layer], swDUL[layer] - swLL15[layer], 0.0);
            moistureFactor += swFac * layerFactor[layer];
        }
        moistureFactor = checkBounds(moistureFactor, 0.0, 1.0, "runoff moistureFactor", true);

        // adjust cn for the day due to the cover effect
        cover_surface_runoff = CalcSurfaceCoverForRunoff();
        coverFractor = MathUtility.Divide(cover_surface_runoff, coverAtMinimumCN, 0.0);
        coverFractor = Math.Min(1.0, coverFractor);
        cn2_cover = bareSoilCN - (maxCNreduction * coverFractor);

        // check tillage effects on cn
        tillageEffect = EvaluateTillageEffect(precipitation, runOn, totalInterception);
        cn2_cover = cn2_cover + tillageEffect;

        // check cn bounds
        cn2_cover = checkBounds(cn2_cover, 0.0, 100.0, "runoff curve number", true);

        // adjust cn according to soil moisture
        cn_drySoil = MathUtility.Divide(cn2_cover, (2.334 - 0.01334 * cn2_cover), 0.0);
        cn_wetSoil = MathUtility.Divide(cn2_cover, (0.4036 + 0.005964 * cn2_cover), 0.0);
        cn2_new = cn_drySoil + (cn_wetSoil - cn_drySoil) * moistureFactor;

        // get potential water retention (estimate of ponding + infiltration)
        maxPotentialRetention = 254.0 * (MathUtility.Divide(100.0, cn2_new, 1000000.0) - 1.0);

        // get the potential runoff amount
        xpb = Math.Max(0.0, (precipitation + runOn - totalInterception) - 0.2 * maxPotentialRetention);
        result = MathUtility.Divide(xpb * xpb, precipitation + runOn - totalInterception + 0.8 * maxPotentialRetention, 0.0);

        //bound check the ouput variable
        checkBounds(result, 0.0, precipitation + runOn - totalInterception, "runoff", false, true);

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
    private double CalcSurfaceCoverForRunoff()
    {
        double canopyfactor;        // canopy factor (0-1)
        double effectiveCropCover;  // effective crop cover (0-1)
        double coverSurfaceCrop;    // efective total cover (0-1)

        // weight effectiveness of crop canopies
        //    0 (no effect) to 1 (full effect)

        coverSurfaceCrop = 0.0;
        for (int crop = 0; crop < nCrops; crop++)
        {
            if (canopyHeight[crop] >= 0.0)
            {
                bool didWork;
                canopyfactor = MathUtility.LinearInterpReal(canopyHeight[crop], canopy_fact_height, canopy_fact, out didWork);
            }
            else
                canopyfactor = canopy_fact_default;

            effectiveCropCover = canopyTotalCover[crop] * canopyfactor;
            coverSurfaceCrop = 1.0 - (1.0 - coverSurfaceCrop) * (1.0 - effectiveCropCover);
        }

        // add canopy cover with that of residues
        double result = 1.0 - (1.0 - coverSurfaceCrop) * (1.0 - surfaceom_cover);
        return Math.Min(1.0, Math.Max(0.0, result));
    }

    /// <summary>
    /// Calculate the weighting factor for hydraulic effectiveness for runoff of each soil layer
    /// </summary>
    /// <remarks>
    /// This represents the fraction of each layer that contributes to the moisture effect on the calculation of runoff
    /// </remarks>
    /// <returns>weighting factor</returns>
    private double[] CalcWeightFactorRunoff()
    {
        double depthFromSurface;            // cumulative depth from soil surface (mm)
        double actualEffectiveDepth;        // effective hydrologicallydepth for runoff (mm)
        int layerOfEffectiveDepth;          // layer number for the effective hydrological depth
        double scalingFactor;               // scaling factor for the weight factor function, so it sums up to 1.0
        double sumWF;                       // sum of depth weight factor over the profile
        double layerWF;                     // depth weighting factor for current layer, intermediate variable for deriving wf
        double auxWFx;                      // intermediate variable for deriving wf considering total wfs to previous layer

        auxWFx = 0.0;
        depthFromSurface = 0.0;
        sumWF = 0.0;
        double[] result = new double[nLayers];

        // check that effective hydrological depth is within the profile (need to check this in case of changes due to erosion).
        double profileDepth = MathUtility.Sum(dLayer);
        actualEffectiveDepth = Math.Min(hydrol_effective_depth, profileDepth);
        layerOfEffectiveDepth = FindLayerNo(actualEffectiveDepth);

        // get the weighting factor for each layer
        scalingFactor = 1.0 / (1.0 - Math.Exp(-4.16));
        for (int layer = 0; layer <= layerOfEffectiveDepth; layer++)
        {
            depthFromSurface = depthFromSurface + dLayer[layer];
            depthFromSurface = Math.Min(depthFromSurface, actualEffectiveDepth);

            // assume water content to c%hydrol_effective_depth affects runoff
            // sum of wf should = 1 - may need to be bounded? <dms 7-7-95>
            layerWF = scalingFactor * (1.0 - Math.Exp(-4.16 * MathUtility.Divide(depthFromSurface, actualEffectiveDepth, 0.0)));
            result[layer] = layerWF - auxWFx;
            auxWFx = layerWF;
            sumWF = sumWF + result[layer];
        }

        checkBounds(sumWF, 1.0 - myEpsilon, 1.0 + myEpsilon, "total runnof weighting factor", false, true);

        return result;
    }

    /// <summary>
    /// Evaluates the reduction in curve number after tillage and the 'recover' afterwards
    /// </summary>
    /// <remarks>
    /// The value of cn is altered (reduced) instantaneously after a tillage event occurs (controlled by tillage_cn_red)
    /// This effect is reduced by rainfall+irrigation, ceasing after a given amount is accumulated (set by tillage_cn_rain)
    /// </remarks>
    /// <param name="precipitation">Amount of precipitation (rain + irrigation)</param>
    /// <param name="runOn">Amount of water rununing in</param>
    /// <param name="totalInterception">Amount of water intercepted by canopy and residues</param>
    /// <returns>Change in cn number </returns>
    private double EvaluateTillageEffect(double precipitation, double runOn, double totalInterception)
    {
        double result = 0.0;
        if (tillage_cn_rain > 0.0)
        {
            // get the tillageFactor, adjusted by precipitation accumulated since last tillage
            result = tillage_cn_red * (MathUtility.Divide(tillage_rain_sum, tillage_cn_rain, 0.0) - 1.0);

            // accumulate rainfall+irrigation to be used next dat
            tillage_rain_sum = tillage_rain_sum + precipitation + runOn - totalInterception;
        }

        if (tillage_rain_sum > tillage_cn_rain)
        {
            // effect of tillage have ceased
            tillage_cn_rain = 0.0;
            tillage_cn_red = 0.0;
            tillage_rain_sum = 0.0;

            Console.WriteLine("Reduction of CN due to tillage is finished");
        }

        return result;
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
    /// <remarks>
    /// Eventually eo will be in a separate module entirely, and soilwater will get as an input,
    /// but for now we either retrieve it "manually", or use Priestley-Taylor.
    /// The possibility to use micromet's value has been added, but this need testing (RCichota, Jun/2017)
    /// </remarks>
    private double PotentialEvapotranspiration()
    {
        double result = 0.0;
        suppliedPET = double.NaN;
        if (myPETsource == "")
        {
            // no external source was indicated, eo is calculated here
            result = calcPriestleyTaylor();
        }
        else if((myPETsource == "micromet")|| (myPETsource == "microclimate"))
        {
            //get eo from micromet
            if (double.IsNaN(pet_total))
                throw new Exception("No values for pet_total were found, check that micromet is present in the simulation");
            else
                result = pet_total;
        }
        else
        {
            // try to get the value from the system, uses the name indicated by eo_source
            if (MyPaddock.Get(myPETsource, out suppliedPET))
                result = suppliedPET;
            else
                throw new Exception("No values for potential evaporation were found, given by \'" + myPETsource 
                    + "\'. Check the setting of \'eo_source\'");
        }

        return result;
    }

    /// <summary>
    /// Calculate the potential evapotranspiration using the Priestley and Taylor method
    /// </summary>
    /// <returns>Daily potential evapotranspiration</returns>
    private double calcPriestleyTaylor()
    {
        double albedo;           // albedo taking into account plant material
        double canopyGreenCover; // sum of crop green covers (0-1)
        double eeq;              // equilibrium evaporation rate (mm)
        double tempFactor;       // temperature factor for equilibrium evaporation rate
        double weightedTemp;     // weighted mean temperature for the day (oC)

        // get the total plant cover
        canopyGreenCover = 0.0;
        for (int crop = 0; crop < nCrops; ++crop)
            canopyGreenCover = 1.0 - (1.0 - canopyGreenCover) * (1.0 - this.canopyGreenCover[crop]);

        // corect soil albedo considering plant cover
        albedo = max_albedo - (max_albedo - salb) * (1.0 - canopyGreenCover);

        // weighted mean temperature, weighted towards maximum
        weightedTemp = (0.60 * MetFile.MaxT) + (0.40 * MetFile.MinT);

        // get basic value for equilibrium evaporation rate
        eeq = MetFile.Radn * 23.8846 * (0.000204 - 0.000183 * albedo) * (weightedTemp + 29.0);

        // get the temperature factor
        if (MetFile.MaxT > max_crit_temp)
        {
            // at very high max temps eo/eeq increases beyond its normal value
            tempFactor = 1.1 + (MetFile.MaxT - max_crit_temp) * 0.05;
        }
        else if (MetFile.MaxT < min_crit_temp)
        {
            // at very low max temperatures eo/eeq decreases below its normal value
            tempFactor = 0.01 * Math.Exp(0.18 * (MetFile.MaxT + 20.0));
            //// Note: there is a discontinuity at maxT = 5 when using the default parameters
            ////  Thies could be solved if min_crit_temp = 6.115, or change the 0.18 to 0.1881, or change the 20 to 21.12
        }
        else
        {
            // 'normal' value
            tempFactor = 1.1;
        }


        // potential evapotranspiration (eo)
        return eeq * tempFactor;
    }

    /// <summary>
    /// Compute the potential and actual evaporation rate
    /// </summary>
    private void CalcEvaporation()
    {
        // correct potential soil water evaporation, after effect of crop and residue cover
        eos = eo * canopyCoverFactor() * residuesCoverFactor();

        // get available soil water for evaporation. Ritchie method uses top layer only
       double evaporableWater = Math.Max(0.0, swAmount[0] - swAirDry[0]);

        // get actual soil water evaporation
        evaporation = CalcActualEvaporation(evaporableWater);
    }

    /// <summary>
    /// Compute the factor to adjust evaporation due to canopy cover
    /// </summary>
    /// <remarks>
    /// Based on: Adams, J. E.; Arkin, G. F.; & Ritchie, J. T. (1976). Influence of row spacing and straw mulch on first stage drying.
    /// Soil Science Society of America Journal, 40(3):436-442.
    /// 
    /// Reduction in potential soil evaporation under a canopy is determined by the fraction cover of the crop canopy,
    /// this should include both green and dead canopy, but does not account for residues.
    /// From fig. 5 and eqn 2: Default value for canopy_eos_coef = 1.7
    ///        ...minimum reduction, for cover = 0.0, has factor = 1.0
    ///        ...maximum reduction, for cover = 1.0, has factor = 0.183
    /// </remarks>
    /// <returns>Factor to adjust evaporation</returns>
    private double canopyCoverFactor()
    {
        // get total cover, for all crops
        double coverTotal = 0.0;
        for (int i = 0; i < nCrops; i++)
            coverTotal = 1.0 - (1.0 - coverTotal) * (1.0 - canopyTotalCover[i]);

        // get fraction of potential soil evaporation, as limited by crop canopy
        double coverFactor = Math.Exp(-canopy_eos_coef * coverTotal);
        return coverFactor;
    }

    /// <summary>
    /// Compute the factor to adjust evaporation due to residue cover
    /// </summary>
    /// <remarks>
    /// Based on Adams et al, (1975), as used in Perfect
    /// [DM. Silburn, June/95] temporary value - will reproduce Adams et al 75 effect
    ///   A_to_evap_fact = 0.00022 / 0.0005 = 0.44
    /// </remarks>
    /// <returns>Factor to adjust evaporation</returns>
    private double residuesCoverFactor()
    {
        // get factor for adjusting evaporation due to crop residue
        double coverFactor = 0.0;
        if (surfaceom_cover < 1.0)
            coverFactor = Math.Pow((1.0 - surfaceom_cover), A_to_evap_fact);

        return coverFactor;
    }

    /// <summary>
    /// Gets the actual evaporation from soil surface
    /// </summary>
    /// <param name="maxEvaporation">maximum evaporation</param>
    private double[] CalcActualEvaporation(double maxEvaporation)
    {
        // Wrapper for various evaporation models.
        // 'es' is an array because some methods do evaporation from more than one layer in the soil,
        //  although most only do the surface. Ritchie only uses top layer.

        if (evapMethod == defaultEvapMethod)
        {
            return EvaporationRitchie(maxEvaporation);
        }
        else
        {
            throw new Exception("Undefined evaporation method");
        }
    }

    /// <summary>
    /// Calculate actual evaporation from soil surface using Ritchie method
    /// </summary>
    /// <remarks>
    /// Evaporation (es) takes place in two stages, after Philip (1957): 
    ///  Stage1: the constant rate stage. The soil is sufficiently wet for water to be transported to the surface at a
    ///   rate at least equal to that of potential evaporation (eos).
    ///  Stage2: the falling rate stage. The water content near the soil surface has decreased below a threshold value,
    ///   so that evaporation is limited by the flow rate of water through the upper layer of soil towards the surface.
    /// </remarks>
    /// <param name="maxEvaporation">maximum evaporation</param>
    /// <returns>actual evaporation from each soil layer</returns>
    private double[] EvaporationRitchie(double maxEvaporation)
    {
        double evapStage1;      // actual soil evap in stage 1
        double evapStage2;      // actual soil evap in stage 2
        double sumes1Max;       // upper limit of sumes1
        double w_inf;      // infiltration into top layer (mm)
        double[] result = new double[nLayers];

        // get the evaporation parameters for different season
        //  need to add 12 hours to move from "midnight" to "noon", or this won't work as expected - TODO: is this still needed?
        if (DateUtility.WithinDates(winterdate, Clock.Today, summerdate))
        {
            myConA = wintercona;
            myU = winteru;
        }
        else
        {
            myConA = summercona;
            myU = summeru;
        }

        sumes1Max = myU;
        w_inf = infiltration;

        // if infiltration, reset sumes1, reset sumes2 if infiltration exceeds sumes1
        if (infiltration > 0.0)
        {
            // reset sumes2 if infiltration exceeds sumes1
            sumes1 = Math.Max(0.0, sumes1 - infiltration);
            sumes2 = Math.Max(0.0, sumes2 - sumes1);

            // update time (incase sumes2 changed)
            timeStage2 = Math.Pow(MathUtility.Divide(sumes2, myConA, 0.0), 0.5);
        }

        // check which stage we are at
        if (sumes1 < sumes1Max)
        {
            // we are in stage1, set evaporation to potential, or limited by u.
            evapStage1 = Math.Min(eos, sumes1Max - sumes1);

            if ((eos > evapStage1) && (evapStage1 < maxEvaporation))
            {
                // eos was not satisfied by 1st stage drying and there is still evaporable water in the soil
                // calculate some stage 2 evaporation
                if (sumes2 > 0.0)
                {
                    timeStage2 += 1.0;
                    evapStage2 = Math.Min(eos - evapStage1, myConA * Math.Pow(timeStage2, 0.5) - sumes2);
                }
                else
                {
                    // using Ritchie's empirical transition constant (0.6)
                    evapStage2 = 0.6 * (eos - evapStage1);
                }
            }
            else
            {
                // all evaporation was realised or there is no more water to evaporate
                evapStage2 = 0.0;
            }

            // check that evaporation did not exceed the maximum possible
            evapStage2 = Math.Min(evapStage2, maxEvaporation - evapStage1);

            // update 1st and 2nd stage soil evaporation.
            sumes1 = sumes1 + evapStage1;
            sumes2 = sumes2 + evapStage2;
            timeStage2 = Math.Pow(MathUtility.Divide(sumes2, myConA, 0.0), 0.0);
        }
        else
        {
            // no stage 1 evaporation, consider 2nd stage
            evapStage1 = 0.0;

            timeStage2 += 1.0;
            evapStage2 = Math.Min(eos, myConA * Math.Pow(timeStage2, 0.5) - sumes2);

            // check that evaporation did not exceed the maximum possible
            evapStage2 = Math.Min(evapStage2, maxEvaporation);

            //  update 2nd stage soil evaporation.
            sumes2 = sumes2 + evapStage2;
        }

        result[0] = evapStage1 + evapStage2;

        // make sure we are within bounds
        result[0] = checkBounds(result[0], 0.0, eos, "Ritchie evaporation rate", true, true);

        return result;
    }

    /// <summary>
    /// Compute the effective evaporation from soil
    /// </summary>
    /// <remarks>
    /// If there is a pond, evaporation is primarily out of it, and only the remaining is removed from the soil
    /// This partition is reported as pond_evap
    /// </remarks>
    private void CalcEffectiveSoilEvaporation()
    {
        if ((pond > 0.0) && (eos>0.0))
        {
            // there is some ponding water and some evaporation
            if (pond >= eos)
            {
                // all evaporation is supplied by pond
                pond = pond - eos;
                pond_evap = eos;
                eos = 0.0;
            }
            else
            {
                // part of evaporation is supplied by pond
                eos = eos - pond;
                pond_evap = pond;
                pond = 0.0;
            }
        }
    }

    #endregion

    #region Drainage

    /// <summary>
    /// Calculate water flow above DUL for each layer
    /// </summary>
    private void CalcWaterFlowAboveDUL()
    {
        // Local Variables
        double waterToAdd;              // water to add to layer
        double waterToBackup;           // water to backup
        double waterExcess;             // amount above saturation(overflow)(mm)
        double waterByPassing = 0.0;    // amount above saturation(overflow) that moves on down (mm)
        double[] newWaterAmount;        // record of results of sw calculations ensure mass balance. (mm)
        double waterDraining;           // water draining by gravity (mm)
        double incomingWater = 0.0;     // water coming into layer (mm)
        double outgoingWater;           // water going out of layer (mm)
        double newWaterAmountLayer;     // total water in layer at start (mm)

        // calculate drainage and water redistribution.
        newWaterAmount = new double[nLayers];

        for (int layer = 0; layer < nLayers; layer++)
        {
            // get 1st estimate for water amount in this layer
            newWaterAmountLayer = swAmount[layer] + incomingWater;

            // get excess water above saturation, this will cascade down as per bucket model.
            if (newWaterAmountLayer > swSAT[layer])
            {
                waterExcess = newWaterAmountLayer - swSAT[layer];
                newWaterAmountLayer = swSAT[layer];
            }
            else
                waterExcess = 0.0;

            // get water drainage, between sat and dul, controlled by swcon
            if (newWaterAmountLayer > swDUL[layer])
            {
                waterDraining = (newWaterAmountLayer - swDUL[layer]) * mySWCon[layer];
            }
            else
                waterDraining = 0.0;

            // get total water draining out of layer, the amount bypassing and that backing up
            if (waterExcess > 0.0)
            {
                // there is excess water entering this layer (there will be bypass or back up)
                // top up this layer to saturation
                waterToAdd = Math.Min(waterExcess, waterDraining);
                waterExcess = waterExcess - waterToAdd;
                newWaterAmount[layer] = swSAT[layer] - waterDraining + waterToAdd;

                // partition between bypass flow and water backing up
                if (usingKsat)
                    waterByPassing = Math.Min(myKSat[layer] - waterDraining, waterExcess);
                else
                    waterByPassing = myMWCon[layer] * waterExcess;
                waterToBackup = waterExcess - waterByPassing;
                
                // gather water flow amounts
                waterBypass[layer] = waterByPassing;
                waterFlowAboveDUL[layer] = waterDraining;
                outgoingWater = waterByPassing + waterDraining;
                flux[layer] = outgoingWater;

                // now back up the excess water from this layer up to the top
                // keep track of this for reducing actual flow rates when computing solute movement
                if (waterToBackup > 0.0)
                {
                    for (int z = layer - 1; z >= 0; z--)
                    {
                        waterBypass[z] -= waterToBackup * MathUtility.Divide(waterBypass[z], flux[z], 0.0);
                        waterFlowAboveDUL[z] -= waterToBackup * MathUtility.Divide(waterFlowAboveDUL[z], flux[z], 0.0);
                        flux[z] -= waterToBackup;
                        waterToAdd = Math.Min(swSAT[z] - newWaterAmount[z], waterToBackup);
                        newWaterAmount[z] += waterToAdd;
                        waterToBackup -= waterToAdd;
                    }
                    if(Clock.Today.Day ==5)
                        extraRunoff += 0.0;

                    extraRunoff += waterToBackup;
                }
            }
            else
            {
                // there is no water excess, so just gather flow amounts
                outgoingWater = waterDraining;
                flux[layer] = outgoingWater;
                newWaterAmount[layer] = swAmount[layer] + incomingWater - outgoingWater;
            }

            // drainage out of this layer is the input into next layer down
            incomingWater = outgoingWater;
        }
    }

    /// <summary>
    /// Calculate water flow above DUL from each layer
    /// </summary>
    private void soilwat2_drainage_old()
    {
        // Local Variables
        double waterToAdd;              // water to add to layer
        double waterToBackup;           // water to backup
        double waterExcess;             // amount above saturation(overflow)(mm)
        double waterByPassing = 0.0;    // amount above saturation(overflow) that moves on down (mm)
        double[] newWaterAmount;        // record of results of sw calculations ensure mass balance. (mm)
        double waterDraining;           // water draining by gravity (mm)
        double incomingWater = 0.0;     // water coming into layer (mm)
        double outgoingWater;           // water going out of layer (mm)
        double newWaterAmountLayer;     // total water in layer at start (mm)

        // potential increase in runoff
        extraRunoff = 0.0;

        // calculate drainage and water redistribution.
        newWaterAmount = new double[nLayers];

        for (int layer = 0; layer < nLayers; layer++)
        {
            // get 1st estimate for water amount in this layer
            newWaterAmountLayer = swAmount[layer] + incomingWater;

            // get excess water above saturation, this will cascade down as per bucket model.
            if (newWaterAmountLayer > swSAT[layer])
            {
                waterExcess = newWaterAmountLayer - swSAT[layer];
                newWaterAmountLayer = swSAT[layer];
            }
            else
                waterExcess = 0.0;

            // get water drainage, between sat and dul, controlled by swcon
            if (newWaterAmountLayer > swDUL[layer])
            {
                waterDraining = (newWaterAmountLayer - swDUL[layer]) * mySWCon[layer];
            }
            else
                waterDraining = 0.0;

            // get total water draining out of layer, the amount bypassing and that backing up
            if (waterExcess > 0.0)
            {
                // there is excess water entering this layer (there will be bypass or back up)
                // top up this layer to saturation
                waterToAdd = Math.Min(waterExcess, waterDraining);
                waterExcess = waterExcess - waterToAdd;
                newWaterAmount[layer] = swSAT[layer] - waterDraining + waterToAdd;

                // partition between flow back up and flow down
                waterToBackup = (1.0 - myMWCon[layer]) * waterExcess;
                waterByPassing = myMWCon[layer] * waterExcess;

                // gather water flow amounts
                waterBypass[layer] = waterByPassing;
                waterFlowAboveDUL[layer] = waterDraining;
                outgoingWater = waterByPassing + waterDraining;
                flux[layer] = outgoingWater;

                // now back up the excess water from this layer up to the top
                // keep track of this for reducing actual flow rates when computing solute movement
                if (waterToBackup > 0.0)
                {
                    for (int z = layer - 1; z >= 0; z--)
                    {
                        waterBypass[z] -= waterToBackup * MathUtility.Divide(waterBypass[z], flux[z], 0.0);
                        waterFlowAboveDUL[z] -= waterToBackup * MathUtility.Divide(waterFlowAboveDUL[z], flux[z], 0.0);
                        flux[z] -= waterToBackup;
                        waterToAdd = Math.Min(swSAT[z] - newWaterAmount[z], waterToBackup);
                        newWaterAmount[z] += waterToAdd;
                        waterToBackup -= waterToAdd;
                    }

                    extraRunoff += waterToBackup;
                }
            }
            else
            {
                // there is no water excess, so just gather flow amounts
                outgoingWater = waterDraining;
                flux[layer] = outgoingWater;
                newWaterAmount[layer] = swAmount[layer] + incomingWater - outgoingWater;
            }

            // drainage out of this layer is the input into next layer down
            incomingWater = outgoingWater;
        }
    }

    /// <summary>
    /// Calculate water flow below DUL for each layer
    /// </summary>
    /// <remarks>
    /// Water can move down or upwards depending on the difference in water content between two layer
    /// Water diffusivity and moisture gradient regulate this flow
    /// </remarks>
    private void CalcWaterFlowBelowDUL()
    {
        double extractableWater_thislayer;  // extractable soil water in current layer (mm)
        double extractableWater_nextlayer;  // extractable soil water in next layer below (mm)
        double waterDiffusivity;            // average water diffusivity
        int nextLayer;                      // layer counter for the layer below current
        double flow_max;                    // maximum water flow between layers
        double incomingW;                   // water moving into this layer (mm), positive downwards
        double holdCapacity;                // capacity of a layer to accept water from another layer (mm)
        double theta1;                      // sw for current layer (mm/mm)
        double theta2;                      // sw for next lower layer (mm/mm)
        double meanTheta;                   // mean soil water content for current and next layer (mm/mm)
        double gradient;                    // driving force for flow
        double sum_inverse_dlayer;          // aux variable
        double ave_dlayer;                  // average distance between current and next layer (mm)
        double gravityFactor;               // sw differential due to gravitational pressure head (mm)
        double potentialFlow;

        // this process only considers diffusion between layers,
        // so no water is flowing down from the surface, nor at the bottom of the soil
        incomingW = 0.0;
        waterFlowBelowDUL[nLayers - 1] = 0.0;

        for (int thisLayer = 0; thisLayer < nLayers - 1; thisLayer++)
        {
            nextLayer = thisLayer + 1;
            ave_dlayer = (dLayer[thisLayer] + dLayer[nextLayer]) * 0.5;

            // get amount of water able to move
            extractableWater_thislayer = Math.Max((swAmount[thisLayer] + incomingW) - swLL15[thisLayer], 0.0);
            extractableWater_nextlayer = Math.Max(swAmount[nextLayer] - swLL15[nextLayer], 0.0);

            // get the water diffusivity, a function of mean moisture above LL15.
            meanTheta = 0.5 * (extractableWater_thislayer / dLayer[thisLayer] + extractableWater_nextlayer / dLayer[nextLayer]);
            waterDiffusivity = diffus_const * Math.Exp(diffus_slope * meanTheta);
            waterDiffusivity = Math.Min(waterDiffusivity, 10000.0);
            //// limiting diffusivity to 10000 (as used in ceres-maize) was shown to reduce
            //// instability of flow direction for consecutive days, in some situations.

            // get the soil moisture gradient (uses absolute sw content)
            theta1 = (swAmount[thisLayer] + incomingW) / dlayer[thisLayer];
            theta2 = swAmount[nextLayer] / dlayer[nextLayer];
            gradient = MathUtility.Divide((theta1 - theta2), ave_dlayer, 0.0) + gravity_gradient;
            //// (cjh):  subtract gravity gradient to prevent gradient being +ve when flow_max is +ve, resulting in sw > sat.

            //  get the potential flow rate (positive down)
            potentialFlow = waterDiffusivity * gradient;

            // calculate maximum flow
            // flow will cease when the gradient becomes zero, adjusted for gravity
            gravityFactor = gravity_gradient * ave_dlayer;
            sum_inverse_dlayer = (1.0 / dlayer[thisLayer]) + (1.0 / dlayer[nextLayer]);
            flow_max = (theta1 - theta2 + gravityFactor) / sum_inverse_dlayer;

            // stop a saturated layer diffusing water into a partially saturated layer, for Water_table height calculations
            if ((swAmount[thisLayer] >= swDUL[thisLayer]) && (swAmount[nextLayer] >= swDUL[nextLayer]))
                potentialFlow = 0.0;

            // stop unsaturated flow downwards through an impermeable layer, but will allow flow up
            if ((myMWCon[thisLayer] < myEpsilon) && (potentialFlow > 0.0))
                potentialFlow = 0.0;
            //// TODO: check whether any correction is needed when ks is used instead of mwcon

            if (potentialFlow > 0.0)
            {
                // flow is down from this to the next layer
                // check ability of this layer to supply the water and capacity of next layer to hold the incoming water
                holdCapacity = Math.Max(swDUL[nextLayer] - swAmount[nextLayer], 0.0);
                flow_max = Math.Min(flow_max, Math.Min(extractableWater_thislayer, holdCapacity));
                waterFlowBelowDUL[thisLayer] = Math.Min(potentialFlow, flow_max);
            }
            else if (potentialFlow < 0.0)
            {
                // flow is up from layer below
                // check capacity of this layer to hold the incoming water and ability of next layer supply the water
                holdCapacity = Math.Max(swDUL[thisLayer] - (swAmount[thisLayer] + incomingW), 0.0);
                flow_max = Math.Max(flow_max, -Math.Min(extractableWater_nextlayer, holdCapacity));
                waterFlowBelowDUL[thisLayer] = Math.Max(potentialFlow, flow_max);
            }
            // else { no flow }

            // Store amount of water moving between adjacent layers to use for calculations in the next pair of layers
            incomingW = waterFlowBelowDUL[thisLayer];
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

        double incomingWater;           // water draining into this layer (mm)
        double outgoingWater;           // water draining out of layer (mm)
        double incomingSolute;          // solute leaching into layer from above (kg/ha)
        double outgoingSolute;          // solute leaching out of layer (kg/ha)
        double newSoluteAmountLayer;    // quantity of solute in layer (kg/ha)
        double waterAmount;             // quantity of water in layer (mm)

        double[] soluteLeaching = new double[nLayers];
        incomingSolute = 0.0;
        incomingWater = infiltration;
        // TODO: check the order of water changes

        for (int layer = 0; layer < nLayers; layer++)
        {
            // get water draining out of layer and n content of layer includes that leaching down
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
            outgoingSolute = Math.Min(outgoingSolute, newSoluteAmountLayer);

            // keep the leaching and set the input for the next layer
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

        double outgoingWater; // water draining out of layer (mm)
        double incomingSolute; // solute leaching into layer from above (kg/ha)
        double outgoingSolute; // solute leaching out of layer (kg/ha)
        double newSoluteAmountLayer; // quantity of solute in layer (kg/ha)
        double waterAmount; // quantity of water in layer (mm)

        double[] soluteLeaching = new double[nLayers];
        incomingSolute = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            // get water draining out of layer and n content of layer includes that leaching down
            outgoingWater = flux[layer];
            newSoluteAmountLayer = existingSolute[layer] + incomingSolute;

            waterAmount = swAmount[layer] + outgoingWater;
            double fractionDrained = MathUtility.Divide(outgoingWater, waterAmount, 0.0);
            outgoingSolute = newSoluteAmountLayer * fractionDrained * solute_flux_eff[layer];

            // check that leaching is within bounds
            outgoingSolute = Math.Min(outgoingSolute, newSoluteAmountLayer);

            // keep the leaching and set the input for the next layer
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

        double bottomw;         // water movement to/from next layer (kg/ha)
        double incomingSolute;  // solute moving into layer from above (kg/ha)
        double[] solute_down;   // solute moving downwards out of each layer (kg/ha)
        double outgoingSolute;  // solute moving out of layer (kg/ha)
        double outgoingWater;   // water draining out of layer (mm)
        double[] remain;        // n remaining in each layer between movement up (kg/ha)
        double soluteAmountLayer; // quantity of solute in layer (kg/ha)
        double top_w;           // water movement to/from above layer (kg/ha)
        double waterAmountLayer; // quantity of water in layer (mm)

        // flow here is assumed positive if up from lower layer, as typically we have upward movement
        //  loop thought profile from bottom to top layer

        solute_up = new double[nLayers];
        solute_down = new double[nLayers];
        remain = new double[nLayers];

        incomingSolute = 0.0;
        for (int layer = nLayers - 1; layer > 0; layer--)
        {
            // keep the solute flow upwards
            solute_up[layer] = incomingSolute;

            // get water moving up and out of layer to the one above
            outgoingWater = -waterFlowBelowDUL[layer - 1];
            if (outgoingWater <= 0.0)
            {
                outgoingSolute = 0.0;
            }
            else
            {
                // get water movement between this and next layer
                bottomw = -waterFlowBelowDUL[layer];

                // get new solute content of layer includes that moving from other layer
                soluteAmountLayer = solute_kg[layer] + incomingSolute;
                waterAmountLayer = swAmount[layer] + outgoingWater - bottomw;

                // solute moving out of layer is proportional to the water moving out.
                outgoingSolute = soluteAmountLayer * MathUtility.Divide(outgoingWater, waterAmountLayer, 0.0) *
                                 solute_flow_eff[layer];

                // check that leaching is within bounds
                outgoingSolute = Math.Min(outgoingSolute, soluteAmountLayer);
            }

            // set the input for the next layer
            incomingSolute = outgoingSolute;
        }

        solute_up[0] = incomingSolute;
        // now get remaining solute in each layer between movements
        // this is needed to adjust the amount in each layer before calculating
        // downwards movement.  I think we shouldn't do this within a time
        // step. i.e. there should be no movement within a time step. jngh
        remain[0] = solute_up[0];
        for (int layer = 1; layer < nLayers; layer++)
        {
            remain[layer] = solute_up[layer] - solute_up[layer - 1];
        }

        // -ve flow - downward movement
        incomingSolute = 0.0;
        top_w = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            // get water moving out of layer
            outgoingWater =  waterFlowBelowDUL[layer];
            if (outgoingWater <= 0.0)
            {
                outgoingSolute = 0.0;
            }
            else
            {
                // get solute content of layer, includes that moving from other layer
                soluteAmountLayer = solute_kg[layer] + incomingSolute + remain[layer];
                waterAmountLayer = swAmount[layer] + outgoingWater - top_w;

                // solute moving out of layer is proportional to the water moving out.
                outgoingSolute = soluteAmountLayer * MathUtility.Divide(outgoingWater, waterAmountLayer, 0.0) *
                                 solute_flow_eff[layer];

                // check that leaching is within bounds
                outgoingSolute = MathUtility.RoundToZero(outgoingSolute);
                outgoingSolute = Math.Min(outgoingSolute, soluteAmountLayer);
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
        double mass_solute; // mass of solute in this rainfall (kg/ha)

        //assume all rainfall goes into surface layer
        // rain is given in mm (=kg/m2) while rain_conc in ppm (mg/kg),
        // so, to get amount of solute rain * 10^4 * rain_conc / 10^6

        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            mass_solute = MetFile.Rain * solutes[solnum].ConcentrationInRain * 0.01;
            solutes[solnum].Amount[0] += mass_solute;
            solutes[solnum].Delta[0] += mass_solute;
        }
    }

    /// <summary>
    /// Add flow (deltas) to the amount of a given soil variable
    /// </summary>
    /// <remarks>
    /// Assumes that the flow is the delta of given variable at the bottom of each layer (thus first flow is zero)
    /// Assumes that positive values represent downwards flow
    /// </remarks>
    /// <param name="givenFlow">flows of the given variable</param>
    /// <param name="givenAmount">amounts of the given variable</param>
    private void DoFlowChanges(double[] givenFlow, ref double[] givenAmount)
    {
        double incomingA = 0.0;
        double outgoingA = 0.0;
        for (int layer = 0; layer < givenFlow.Length; layer++)
        {
            outgoingA = givenFlow[layer];
            givenAmount[layer] += (incomingA - outgoingA);
            incomingA = outgoingA;
        }
    }

    private void MoveDownReal(double[] DownAmount, ref double[] A)
    {
        //+ Sub-Program Arguments
        //   real       array (*)             ! (INPUT/OUTPUT) amounts currently in
        //                                    !   each layer
        //   real       down (*)              ! (INPUT) amounts to move into each
        //                                    !   layer from the one above

        //+ Purpose
        //     move amounts specified, downwards from one element to the next

        //+  Definition
        //     Each of the "nlayr" elements of "array" holds quantities
        //     for a given soil layer.  "array"(1) corresponds to the
        //     uppermost layer.   "array"(n) corresponds to the layer
        //     (n-1) layers down from the uppermost layer.  "down"(n)
        //     indicates a quantity to be moved from the layer
        //     corresponding to "array"(n) down into the layer
        //     corresponding to "array"(n+1).  This subroutine subtracts
        //     "down"(n) from "array"(n) and adds it to "array"(n+1) for
        //     n=1 .. ("nlayr"-1).  "down"("nlayr") is subtracted from
        //     "array"("nlayr").

        //+  Mission Statement
        //      Move amounts of %1 down array %2

        //+ Changes
        //       031091  jngh changed variable movedn to down - cr157

        //+ Local Variables
        int layer;  // layer number
        double win;    // amount moving from layer above to current layer
        double wout;   // amount moving from current layer to the one below

        //- Implementation Section ----------------------------------

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

        int solnum;              // solute number counter variable

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

        //     ===========================================================
        //   subroutine Move_up_real (up, array, nlayr)
        //     ===========================================================

        //+ Sub-Program Arguments
        //eal        array (*)             // (INPUT/OUTPUT) amounts currently in each layer
        //int         nlayr                 // (INPUT) number of layers
        //real        up (*)                // (INPUT) amounts to move into each layer from the one below

        //+ Purpose
        //       move amounts specified, upwards from one element to the next

        //+  Definition
        //     Each of the "nlayr" elements of "array" holds quantities
        //     for a given soil layer.  "array"(1) corresponds to the
        //     uppermost layer.   "array"(n) corresponds to the layer
        //     (n-1) layers down from the uppermost layer.  "up"(n)
        //     indicates a quantity to be moved from the layer
        //     corresponding to "array"(n+1) up into the layer
        //     corresponding to "array"(n).  This subroutine subtracts
        //     "up"(n) from "array"(n+1) and adds it to "array"(n) for
        //     n=1..("nlayr"-1).  "up"("nlayr") is added to "array"("nlayr").

        //+  Mission Statement
        //      Move amounts %1 up array %2

        //+ Changes
        //       031091  jngh renamed moveup to up - cr158
        //                    included description of variables in parameter list
        //                      - cr159
        //                    corrected description - cr160

        //+ Calls

        //+ Local Variables
        int layer;                 // layer number
        double win;                   // amount moving from layer below to current layer
        double wout;                  // amount moving from current layer to the one above

        //- Implementation Section ----------------------------------

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

        int solnum;              // solute number counter variable

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
    /// <returns>The depth (in mm) below the surface of the first layer whose water content is above saturation</returns>
    private double DepthToWaterTable()
    {
        int saturatedLayer = -1;
        double drainableFraction = 0.0;
        double drainableFractionFromAbove=0.0;
        double depthWaterTable;

        for (int layer = 0; layer < nLayers; layer++)
        {
            //Find the first layer that is above saturation or, 
            // if mwcon is used to define 'impermeable' layers (mwcon=0.0), find the first layer with sw above dul
            if (swSAT[layer] - swAmount[layer] <= myEpsilon)
            {
                saturatedLayer = layer;
                break;
            }
            else if ((!usingKsat) && (myMWCon[layer] < myEpsilon) && (swDUL[layer] - swAmount[layer] <= myEpsilon))
            {
                saturatedLayer = layer;
                break;
            }
        }

        // Do the calculation of the water table depth
        if (saturatedLayer >= 0)
        {
            //There is a saturated layer, get the fraction of water that is drainable (over dul) in that layer
            drainableFraction = MathUtility.Divide(swAmount[saturatedLayer] - swDUL[saturatedLayer],
                                                    swSAT[saturatedLayer] - swDUL[saturatedLayer], 0.0);

            if (drainableFraction >= 1.0 - myEpsilon)
            {
                // this is a fully saturated layer, all of it is part of the water table
                depthWaterTable = MathUtility.Sum(dLayer, 0, saturatedLayer, 0.0);

                // check how much of the layer above is within the water table (based on fraction over dul)
                if (saturatedLayer >= 1)
                {
                    drainableFractionFromAbove = MathUtility.Divide(swAmount[saturatedLayer - 1] - swDUL[saturatedLayer - 1],
                                                                  swSAT[saturatedLayer - 1] - swDUL[saturatedLayer - 1], 0.0);
                    depthWaterTable -= drainableFractionFromAbove * dLayer[saturatedLayer - 1];
                }
            }
            else
            {
                // this is a partially saturated layer (sw>dul), so only a fraction of it is considered within the water table
                depthWaterTable = MathUtility.Sum(dLayer, 0, Math.Min(nLayers, saturatedLayer + 1), 0.0);
                depthWaterTable -= drainableFraction * dLayer[saturatedLayer];
            }
        }
        else
        {
            // there is no saturated layer, water table is below the profile
            depthWaterTable = MathUtility.Sum(dLayer);
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
                // top of water table is in this layer
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
            if (myKLat[layer] > 0.0)
            {
                depthAboveDUL = MathUtility.Divide(swAmount[layer] - swDUL[layer], swSAT[layer] - swDUL[layer], 0.0);
                depthAboveDUL = dLayer[layer] * Math.Max(0.0, depthAboveDUL);
                nominator = myKLat[layer] * depthAboveDUL * (discharge_width * 1000.0) * slope;
                denominator = catchment_area * 1000000.0 * Math.Pow((1.0 + Math.Pow(slope, 2)), 0.5);
                result[layer] = MathUtility.Divide(nominator, denominator, 0.0);

                // Cannot drop sw below dul
                maxFlow = Math.Max(0.0, swAmount[layer] - swDUL[layer]);
                result[layer] = Math.Min(result[layer], maxFlow);
            }
            else
                result[layer] = 0.0;
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
        //save initial total
        double oldSWAmount = MathUtility.Sum(swAmount);

        //do initialisation checks
        CheckSoilWaterInitialisation();

        //do reporting
        WriteReport();

        //tell the "System Balance" module (if there is one) that soil water was changed by this amount
        double newSWAmount = MathUtility.Sum(swAmount);
        sendExternalMassFlow(newSWAmount - oldSWAmount);
    }

    /// <summary>
    /// Initialise SoilWater module
    /// </summary>
    private void CheckSoilWaterInitialisation()
    {
        checkEvaporationParameters();
        checkSoluteTransportEfficiencies();
        checkSaturatedFlowParameters();
        checkLateralFlowParameters();

        if (maxCNreduction >= bareSoilCN)
            maxCNreduction = bareSoilCN - myEpsilon;

        isInitialising = false;

        for (int layer = 0; layer < nLayers; layer++)
            checkSoilWaterThresholds(layer);

        //publish event telling other modules that there is a new soil profile.
        sendNewProfileEvent();
    }

    /// <summary>
    /// Report SoilWat module summary details
    /// </summary>
    private void WriteReport()
    {
        double depthTopOfLayer;     // depth to top of layer (mm)
        double depthBottomOfLayer;  // depth to bottom of layer (mm)
        string line;                // temp output record
        double[] runoff_wf;           // weighting factor for runoff
        double[] unavailableWater;                 // unavail. sw (mm)
        double[] availableWater;                 // avail. sw (mm)
        double[] maxAvailableWater;                // max unavail. sw (mm)
        double[] drainableWater;                 // drainable sw (mm)

        runoff_wf = new double[nLayers];
        unavailableWater = new double[nLayers];
        availableWater = new double[nLayers];
        maxAvailableWater = new double[nLayers];
        drainableWater = new double[nLayers];

        Console.WriteLine();
        line = "             Soil Profile Properties";
        Console.WriteLine(line);
        line = "   -----------------------------------------------------------------------------";
        Console.WriteLine(line);

        if (usingKsat)
        {
            line = "       Depth     AirDry   LL15   Dul    Sat    Sw     BD   Runoff SWCON    Ks";
            Console.WriteLine(line);
            line = "         mm       mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc  w.fact  /day  mm/day";
            Console.WriteLine(line);
        }
        else
        {
            line = "       Depth     AirDry   LL15   Dul    Sat    Sw     BD   Runoff SWCon   MWCon";
            Console.WriteLine(line);
            line = "         mm       mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc  w.fact  /day";
            Console.WriteLine(line);
        }

        line = "   -----------------------------------------------------------------------------";
        Console.WriteLine(line);

        depthTopOfLayer = 0.0;
        runoff_wf = CalcWeightFactorRunoff();

        for (int layer = 0; layer < nLayers; layer++)
        {
            depthBottomOfLayer = depthTopOfLayer + dLayer[layer];

            if (usingKsat)
            {
                line = String.Format("   {0,5:0.#} {1} {2,-4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000} {11,7:0.0}",
                                     depthTopOfLayer,
                                     "-",
                                     depthBottomOfLayer,
                                     MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0),
                                     myBD[layer],
                                     runoff_wf[layer],
                                     mySWCon[layer],
                                     myKSat[layer]);
            }
            else
            {
                line = String.Format("   {0,5:0.#} {1} {2,-4:0.#} {3,6:0.000} {4,6:0.000} {5,6:0.000} {6,6:0.000} {7,6:0.000} {8,6:0.000} {9,6:0.000} {10,6:0.000} {11,7:0.000}",
                                     depthTopOfLayer,
                                     "-",
                                     depthBottomOfLayer,
                                     MathUtility.Divide(swAirDry[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swLL15[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swDUL[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swSAT[layer], dLayer[layer], 0.0),
                                     MathUtility.Divide(swAmount[layer], dLayer[layer], 0.0),
                                     myBD[layer],
                                     runoff_wf[layer],
                                     mySWCon[layer],
                                     myMWCon[layer]);
            }
            Console.WriteLine(line);
            depthTopOfLayer = depthBottomOfLayer;
        }

        line = "   -----------------------------------------------------------------------------";
        Console.WriteLine(line);
        Console.WriteLine();
        Console.WriteLine();
        line = "             Soil Water Holding Capacity";
        Console.WriteLine(line);
        line = "     ----------------------------------------------------------";
        Console.WriteLine(line);
        line = "         Depth    Unavailable Available  Max Avail.  Drainable";
        Console.WriteLine(line);
        line = "                     (LL15)   (SW-LL15)  (DUL-LL15)  (SAT-DUL)";
        Console.WriteLine(line);
        line = "                       mm        mm          mm         mm";
        Console.WriteLine(line);
        line = "     ----------------------------------------------------------";
        Console.WriteLine(line);
        depthTopOfLayer = 0.0;

        for (int layer = 0; layer < nLayers; layer++)
        {
            depthBottomOfLayer = depthTopOfLayer + dLayer[layer];
            unavailableWater[layer] = swLL15[layer];
            availableWater[layer] = Math.Max((swAmount[layer] - swLL15[layer]), 0.0);
            maxAvailableWater[layer] = swDUL[layer] - swLL15[layer];
            drainableWater[layer] = swSAT[layer] - swDUL[layer];

            line = String.Format("   {0,6:0.#} {1} {2,-4:0.#} {3,10:0.00} {4,10:0.00} {5,10:0.00} {6,10:0.00}",
                                 depthTopOfLayer,
                                 "-",
                                 depthBottomOfLayer,
                                 unavailableWater[layer],
                                 availableWater[layer],
                                 maxAvailableWater[layer],
                                 drainableWater[layer]);

            Console.WriteLine(line);
            depthTopOfLayer = depthBottomOfLayer;
        }

        line = "     ----------------------------------------------------------";
        Console.WriteLine(line);
        line = String.Format("          Totals {0,10:0.00} {1,10:0.00} {2,10:0.00} {3,10:0.00}",
                             MathUtility.Sum(unavailableWater),
                             MathUtility.Sum(availableWater),
                             MathUtility.Sum(maxAvailableWater),
                             MathUtility.Sum(drainableWater));

        Console.WriteLine(line);
        line = "     ----------------------------------------------------------";
        Console.WriteLine(line);
        Console.WriteLine();
        Console.WriteLine();

        line = "          General Soil Parameters";
        Console.WriteLine(line);
        line = "     -----------------------------------------------";
        Console.WriteLine(line);
        line = "         Soil albedo: " + salb.ToString("0.00");
        Console.WriteLine(line);
        line = "         Water diffusivity constant: " + diffus_const.ToString("0.00");
        Console.WriteLine(line);
        line = "         Water diffusivity slope: " + diffus_slope.ToString("0.00");
        Console.WriteLine(line);
        line = "         Maximum pond capacity (mm): " + maxPondCapacity.ToString("0.00");
        Console.WriteLine(line);
        line = "     -----------------------------------------------";
        Console.WriteLine(line);
        Console.WriteLine();
        Console.WriteLine();

        if (obsrunoff_name != "")
        {
            // runnof data has supplied
            string obsrunoff_name_trunc;
            obsrunoff_name_trunc = obsrunoff_name.Trim();
            line = String.Format("      {0} {1} {2}",
                                 "             Observed runoff data ( ",
                                 obsrunoff_name_trunc,
                                 " ) will be used in water balance");

            Console.WriteLine(line);
        }
        else
        {
            // no data was supplied, use CN
            line = "          Runoff is predicted using scs curve number";
            Console.WriteLine(line);
            line = "     ------------------------------------------------";
            Console.WriteLine(line);
            line = "         Bare soil curve number: " + bareSoilCN.ToString("0.00");
            Console.WriteLine(line);
            line = "         Max reduction in curve number: " + maxCNreduction.ToString("0.00");
            Console.WriteLine(line);
            line = "         Soil cover for max reduction: " + coverAtMinimumCN.ToString("0.00");
            Console.WriteLine(line);
            line = "         Effective hydraulic depth (mm): " + hydrol_effective_depth.ToString("0.00");
            Console.WriteLine(line);
            line = "     ------------------------------------------------";
            Console.WriteLine(line);
        }

        Console.WriteLine();
        Console.WriteLine();

        if (evapMethod == defaultEvapMethod)
        {
            line = "          Using Ritchie evaporation model";
            Console.WriteLine(line);
            line = "     --------------------------------------------------------";
            Console.WriteLine(line);

            if (winteru == summeru)
            {
                line = String.Format("       {0} {1,8:0.00} {2}",
                                     "Cuml evap (U):        ", myU,
                                     " (mm^0.5)");

                Console.WriteLine(line);
            }
            else
            {
                line = String.Format("        {0} {1,8:0.00} {2}        {3} {4,8:0.00} {5}",
                                     "Stage 1 duration (U): Summer    ", summeru,
                                     " (mm)" + Environment.NewLine,
                                     "                      Winter    ", winteru,
                                     " (mm)");
                Console.WriteLine(line);
            }

            if (wintercona == summercona)
            {
                line = String.Format("       {0} {1,8:0.00} {2}",
                                     "CONA:                 ", myConA,
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
                Console.WriteLine("        Critical dates:       Summer        " + summerdate + Environment.NewLine +
                "                              Winter        " + winterdate);
            }

            line = "     --------------------------------------------------------";
            Console.WriteLine(line);
        }
        else
        {
            line = "     Using unknown evaporation method!";
            Console.WriteLine(line);
        }

        Console.WriteLine();
        if (myPETsource != "")
        {
            line = String.Format("    - {0} {1}", "Potential evapotranspiration source:             ", myPETsource);
            Console.WriteLine(line);
        }
        else
        {
            line = "     - Potential evapotranspiration predicted using Priestley-Taylor";
            Console.WriteLine(line);
        }

        Console.WriteLine();
    }

    #endregion

    #region Daily processes

    /// <summary>
    /// Perform daily prepare
    /// </summary>
    [EventHandler]
    public void OnPrepare()
    {
        // zero some variables
        zeroVariables();

        // get the potential evapotranspiration
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

        // Lateral flow
        for (int layer = 0; layer < nLayers; layer++)
            swAmount[layer] += inflowLateral[layer];

        outflowLateral = CalcLateralFlow();
        for (int layer = 0; layer < nLayers; layer++)
            swAmount[layer] -= outflowLateral[layer];

        // Water runoff
        if (irrigationCanRunoff)
            potentialRunoff = PotentialRunoff(MetFile.Rain + irrigation, runon, interception + residueinterception);
        else
            potentialRunoff = PotentialRunoff(MetFile.Rain, runon, interception + residueinterception);

        // Note: potentialRunoff is the runoff which would have occurred without ponding.
        //  This value is then adjusted after taking any ponding into account, and later any backup water

        pond = pond + potentialRunoff;
        runoff = Math.Max(pond - maxPondCapacity, 0.0);
        pond = Math.Min(pond, maxPondCapacity);

        // Water infiltration
        infiltration = CalcInfiltration();
        swAmount[0] += infiltration;

        // Irrigation water, if subsurface
        if (irrigation_layer > 0.0)
            swAmount[irrigation_layer] += irrigation;

        // Add solutes from rain + irrigation
        soilwat2_irrig_solute();
        soilwat2_rainfall_solute();

        // Dispose off the data from irrigation
        irrigation = 0.0;
        for (int solnum = 0; solnum < nSolutes; solnum++)
            solutes[solnum].AmountInIrrigation = 0.0;

        // Water flow above DUL (aka flux, aka Drainage)
        extraRunoff = 0.0;
        CalcWaterFlowAboveDUL();

        if (extraRunoff > 0.0)
        {
            // The "ExtraRunoff" is caused by water backing up of top layer due to inability of soil to drain.
            // Any ExtraRunoff then is added to pond, if possible, or will actually runoff
            pond = Math.Min(extraRunoff, maxPondCapacity);
            runoff = runoff + extraRunoff - pond;
            //Deduct the ExtraRunoff from the infiltration because it did not infiltrate
            infiltration = infiltration - extraRunoff;
            swAmount[0] -= extraRunoff;
        }

        // Change water due to flow above DUL
        DoFlowChanges(flux, ref swAmount);

        // Drainage out of bottom layer
        drain = flux[nLayers - 1];

        // Move the solutes with flow above dul
        soilwat2_move_solute_down();

        // Evaporation
        CalcEvaporation();
        CalcEffectiveSoilEvaporation();
        for (int layer = 0; layer < nLayers; layer++)
            swAmount[layer] -= evaporation[layer];

        // Water flow below DUL (aka flow)
        CalcWaterFlowBelowDUL();

        // Change water due to flow above DUL
        DoFlowChanges(waterFlowBelowDUL, ref swAmount);
        //MoveUpReal(waterFlowBelowDUL, ref swAmount);

        // now check that the soil water is not silly
        for (int layer = 0; layer < nLayers; layer++)
            checkSoilWaterContent(layer);

        // WATER TABLE
        waterTableDepth = DepthToWaterTable();

        // UNSATURATED FLOW SOLUTE MOVEMENT
        // now move the solutes with flow
        soilwat2_move_solute_up();

        //Change the variables in other modules
        sendSoluteChanges();
        RaiseRunoffEvent();
    }

    #endregion

    #region Sporadic Events

    /// <summary>
    /// Gets the initial information about solutes
    /// </summary>
    /// <param name="newsolute">Solute data</param>
    [EventHandler(EventName = "new_solute")]
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
            irrigationCanRunoff = true;
            if (irrigationData.Depth > 0.0)
            {
                // get the specific layer that the irrigation is to go into.
                irrigation_layer = FindLayerNo(irrigationData.Depth);

                // check runof option
                irrigationCanRunoff = false;
                My.Warning("The irrigation applied was set to allow runoff, but it is not a surface irrigation \n" +
                           "Only surface irrigation can runoff, thus this irrigation event will not be allowed to runoff \n" +
                           " note: runoff may happen even with subsurface irrigation when soil is saturated and ponding occurs.");
            }
        }
        else
        {
            irrigationCanRunoff = false;
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
    /// <remarks>
    /// This method is used by Plant2 and AgPasture
    /// </remarks>
    /// <param name="WaterChanged">Changes in water</param>
    [EventHandler]
    public void OnWaterChanged(WaterChangedType WaterChanged)
    {
        double oldSWAmount = MathUtility.Sum(swAmount);
        for (int layer = 0; layer < WaterChanged.DeltaWater.Length; layer++)
        {
            swAmount[layer] = swAmount[layer] + WaterChanged.DeltaWater[layer];
            checkSoilWaterContent(layer);
        }

        //tell the "System Balance" module (if there is one) that soil water was changed by this amount
        double newSWAmount = MathUtility.Sum(swAmount);
        sendExternalMassFlow(newSWAmount - oldSWAmount);
    }

    [EventHandler]
    public void OnReset()
    {
        //save initial total
        double oldSWAmount = MathUtility.Sum(swAmount);

        //Reset state
        zeroVariables();
        waterTableDepth = 0.0;
        pond = 0.0;
        sumes1 = 0.0;
        sumes2 = 0.0;
        timeStage2 = 0.0;
        tillage_cn_red = 0.0;
        tillage_cn_rain = 0.0;
        tillage_rain_sum = 0.0;
        GetCropVariables();
        GetSoluteVariables();
        resetSoilWater();

        //publish event telling other modules that there is a new soil profile.
        sendNewProfileEvent();

        //tell the "System Balance" module (if there is one) that soil water was changed by this amount
        double newSWAmount = MathUtility.Sum(swAmount);
        sendExternalMassFlow(newSWAmount - oldSWAmount);
    }

    /// <summary>
    /// Set up the reduction in curve number after tillage
    /// </summary>
    /// <remarks>
    /// cn_red is the instantaneous reduction in the cn value being used to compute runoff, the cn value bounces back
    /// with time, following rainfal+irrigation. n_rain is the total amount of rainfall+irrigation after which the reduction
    /// is due to tillage has no effect.
    /// If the data does not contain cn_red and cn_rain, then default values will be used. This departs slightly from the 
    /// Fortran version, where they were optional arguments, and zero were used whn not supplied
    /// </remarks>
    /// <param name="TillageData">Tillage data</param>
    [EventHandler]
    public void OnTillage(TillageType TillageData)
    {
        string myMessage;   // message to write in the summary file

        if ((TillageData.cn_red <= 0) || (TillageData.cn_rain <= 0))
        {
            // no tillage data was given, look for defaults
            TillageType defaultData = SoilWatTillageType.GetTillageData(TillageData.type);

            if (defaultData != null)
            {
                // tillage type was found, if it had no values use zero
                tillage_cn_red = Math.Max(0.0, defaultData.cn_red);
                tillage_cn_rain = Math.Max(0.0, defaultData.cn_rain);
                myMessage = " Using default tillage parameters for type: " + TillageData.type;
            }
            else
            {
                // no tillage information wasw given and the tillage type was not recognised
                throw new Exception(" Cannot find information for tillage type: " + TillageData.type);
            }
        }
        else
        {
            // tillage parameters were given, check bounds
            tillage_cn_red = checkBounds(TillageData.cn_red, 0.0, bareSoilCN - myEpsilon, "tillage cn reduction", true);
            tillage_cn_rain = checkBounds(TillageData.cn_rain, 0.0, 1000000.0, "tillage accum. rain", true);
            myMessage = " Using tillage parameters supplied for type: " + TillageData.type;
        }

        // let the user know which values are going to be used
        Console.WriteLine();
        Console.WriteLine(myMessage);
        myMessage = "  CN reduction: " + tillage_cn_red.ToString("#0.0#") + ", accum. rain: " + tillage_cn_rain.ToString("#0") + " mm";
        Console.WriteLine(myMessage);

        // reset the rain accumulator
        tillage_rain_sum = 0.0;
    }

    /// <summary>
    /// Write a summary of this module's paramters out to the summary file
    /// </summary>
    [EventHandler]
    public void Onsum_report()
    {
        //external module has request that we write our parameters out to the summary file
        WriteReport();
    }

    #endregion

    #region Publish Events

    /// <summary>
    ///Advertise to other modules the new soil profile specification 
    /// </summary>
    private void sendNewProfileEvent()
    {
        if (New_profile != null)
        {
            NewProfileType newProfile = new NewProfileType();
            // Convert array values from doubles to floats
            newProfile.air_dry_dep = ToFloatArray(swAirDry);
            newProfile.bd = ToFloatArray(myBD);
            newProfile.dlayer = ToFloatArray(dLayer);
            newProfile.dul_dep = ToFloatArray(swDUL);
            newProfile.ll15_dep = ToFloatArray(swLL15);
            newProfile.sat_dep = ToFloatArray(swSAT);
            newProfile.sw_dep = ToFloatArray(swAmount);
            New_profile.Invoke(newProfile);
        }
    }

    /// <summary>
    /// Send some info about changes in mass balance of soil water
    /// </summary>
    /// <remarks>
    /// External Mass Flow event is used for a model called "System Balance" which just keeps track of all the water, solutes, etc.
    /// It is used mainly for debugging purposes. It helps track errors when water is forcibly changed externally, such as from 
    ///  a manager and this breakes the mass balance. So every time water changes from outside, this info should be send out.
    /// </remarks>
    /// <param name="deltaSW">Amount of soil water changed</param>
    private void sendExternalMassFlow(double deltaSW)
    {
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();

        if (deltaSW >= 0.0)
            massBalanceChange.FlowType = "gain";
        else
            massBalanceChange.FlowType = "loss";

        massBalanceChange.PoolClass = "soil";
        massBalanceChange.DM = 0.0F;
        massBalanceChange.C = 0.0F;
        massBalanceChange.N = 0.0F;
        massBalanceChange.P = 0.0F;
        massBalanceChange.SW = Math.Abs((float)deltaSW);

        if (ExternalMassFlow != null)
            ExternalMassFlow.Invoke(massBalanceChange);
    }

    /// <summary>
    /// Send the amounts changed for each solute
    /// </summary>
    /// <remarks>
    /// The major N solutes (urea, NH4 and NO3) are send via NitrogenChanged event, deltas are directly set for other solutes
    /// </remarks>
    private void sendSoluteChanges()
    {
        // initialise the data package for N, assumes these will practically always be present
        NitrogenChangedType NitrogenChanges = new NitrogenChangedType();
        NitrogenChanges.Sender = "SoilWater";
        NitrogenChanges.SenderType = "WateModule";
        NitrogenChanges.DeltaUrea = new double[nLayers];
        NitrogenChanges.DeltaNH4 = new double[nLayers];
        NitrogenChanges.DeltaNO3 = new double[nLayers];
        bool isNChanging = false;

        //for all solutes in this simulation.
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            float[] temp_dlt_solute = ToFloatArray(solutes[solnum].Delta);

            //set the change in solutes for the modules
            if (solutes[solnum].Name == "urea")
            {
                isNChanging = true;
                NitrogenChanges.DeltaUrea = solutes[solnum].Delta;
            }
            else if (solutes[solnum].Name == "nh4")
            {
                isNChanging = true;
                NitrogenChanges.DeltaNH4 = solutes[solnum].Delta;
            }
            else if (solutes[solnum].Name == "no3")
            {
                isNChanging = true;
                NitrogenChanges.DeltaNO3 = solutes[solnum].Delta;
            }
            else
            {
                // not a N solute, set delta
                string propName;
                if (solutes[solnum].OwnerName != "")
                    propName = solutes[solnum].OwnerName + ".dlt_" + solutes[solnum].Name;
                else
                    propName = "dlt_" + solutes[solnum].Name;
                MyPaddock.Set(propName, temp_dlt_solute);
            }
        }

        if (isNChanging)
            NitrogenChanged.Invoke(NitrogenChanges);
    }

    /// <summary>
    /// Advertise that a runoff event has occured
    /// </summary>
    private void RaiseRunoffEvent()
    {
        if (runoff > 0.0)
        {
            RunoffEventType runoffData = new RunoffEventType();
            runoffData.runoff = (float)runoff;
            if (Runoff != null)
                Runoff.Invoke(runoffData);
        }
    }

    #endregion

    #region Generic functions

    private void ResizeProfileArrays()
    {
        Array.Resize(ref dLayer, nLayers);
        Array.Resize(ref myBD, nLayers);
        Array.Resize(ref swSAT, nLayers);
        Array.Resize(ref swDUL, nLayers);
        Array.Resize(ref swLL15, nLayers);
        Array.Resize(ref swAirDry, nLayers);
        Array.Resize(ref swAmount, nLayers);
        Array.Resize(ref mySWCon, nLayers);
        Array.Resize(ref evaporation, nLayers);
        Array.Resize(ref waterBypass, nLayers);
        Array.Resize(ref waterFlowAboveDUL, nLayers);
        Array.Resize(ref waterFlowBelowDUL, nLayers);
        Array.Resize(ref flux, nLayers);
        Array.Resize(ref inflowLateral, nLayers);
        Array.Resize(ref outflowLateral, nLayers);

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

    /// <summary>
    /// Infraestructure that allows other modules to get leaching outputs
    /// </summary>
    /// <param name="propID">The id of the variable desired (flow_ or leach_)</param>
    /// <param name="value">the output container</param>
    /// <param name="isRequestingSet">whether a set is being requested</param>
    /// <returns></returns>
    public bool getPropertyValue(int propID, ref TPropertyInfo value, bool isRequestingSet)
    {
        // currently only handling read requests (get), so fail if this is not the case
        if (isRequestingSet)
            return false;

        // look for the solute and property required
        for (int solnum = 0; solnum < nSolutes; solnum++)
        {
            if (solutes[solnum].leach_id == propID)
            {
                // value requested was leaching, return value
                value.setValue(solutes[solnum].FlowAboveDUL[solutes[solnum].FlowAboveDUL.Length - 1]);
                return true;
            }

            if (solutes[solnum].flow_id == propID)
            {
                // value requested was flow, return array
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
    /// Converts all elements of a double array to floats
    /// </summary>
    /// <param name="theArray">Array of doubles</param>
    /// <returns>Array of floats</returns>
    private float[] ToFloatArray(double[] theArray)
    {
        float[] result = new float[theArray.Length];

        for (int i = 0; i < theArray.Length; i++)
            result[i] = (float)theArray[i];
        return result;
    }

    /// <summary>
    /// Zeores all the elements of an array
    /// </summary>
    /// <param name="theArray">Array to be zeroed</param>
    private void ZeroArray(ref double[] theArray)
    {
        for (int i = 0; i < theArray.Length; i++)
            theArray[i] = 0.0;
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