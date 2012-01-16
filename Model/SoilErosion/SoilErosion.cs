using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ModelFramework;
using CSGeneral;


/// <summary>
/// A more-or-less direct port of the Fortran Erosion model
/// Ported by Eric Zurcher Feb 2011
/// </summary>

public partial class SoilErosion
{
    [Link]
    private Component MyComponent = null;

#region Parameters used to initialise the model
  #region Parameters we expect to see provided by the user

    [Param]
    [Description("Model name")]
    protected string model   // Currently either "freebairn" or "rose"
    {
        get { return model_type.ToString(); }
        set
        {
            if (value.StartsWith("free"))
                model_type = model_type_enum.freebairn;
            else if (value.StartsWith("rose"))
                model_type = model_type_enum.rose;
            else
                model_type = model_type_enum.unknown;
        }
    }

    [Param]
    [Description("Profile reduction")]
    protected string profile_reduction // either "on" or "off"
    {
        get { return reduce_profile ? "on" : "off"; }
        set { reduce_profile = (value == "on"); }
    }

    [Param(MinVal=0.0, MaxVal=1.0)]
    [Description("Fraction of original size in which the lowest layer is merged into the layer above")]
    [Units("")]
    protected double profile_layer_merge = 0.0;

    [Param(MinVal=0.0, MaxVal=1000.0)]
    [Description("If the profile erodes to this depth, the simulation is stopped")]
    [Units("mm")]
    protected double minimum_depth = 0.0;

    [Param(MinVal=0.0, MaxVal=100.0)]
    [Description("Slope of plot in percent")]
    [Units("%")]
    protected double slope = 0.0;

    [Param(MinVal=0.0, MaxVal=100.0)]
    [Description("Length of plot")]
    [Units("m")]
    protected double slope_length = 0.0;

    [Param(MinVal=0.0, MaxVal=5000.0)]
    [Output]
    [Description("Depth to bedrock")]
    [Units("mm")]
    protected double bed_depth;

  #endregion
#region Optional parameters
    protected double _cover_extra = 0.0;
    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 1.0)]
    [Output]
    [Description("\"Fudge factor\" added to total cover")]
    [Units("")]
    protected double cover_extra
    {
        get { return _cover_extra; }
        set { _cover_extra = value; }
    }

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 1.0)]
    [Description("Soil erodibility factor")]
    [Units("t/ha/EI30")]
    protected double k_factor = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 1.0)]
    [Description("Soil erodibility factor for bed load")]
    [Units("t/ha/EI30")]
    protected double k_factor_bed = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 1.0)]
    [Description("Soil erodibility factor for suspended load")]
    [Units("t/ha/EI30")]
    protected double k_factor_susp = -1.0;

    [Param(IsOptional = true)]
    [Description("Supporting practise factor")]
    [Units("")]
    protected double p_factor = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 2.0)]
    [Description("Efficiency of entrainment - bare surface")]
    [Units("")]
    protected double entrain_eff = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 2.0)]
    [Description("Efficiency of entrainment - bare surface - bed load")]
    [Units("")]
    protected double entrain_eff_bed = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 2.0)]
    [Description("Efficiency of entrainment - bare surface - suspended load")]
    [Units("")]
    protected double entrain_eff_susp = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 0.2)]
    [Description("Coefficient for calculating lambda in \"rose\" sub-model")]
    [Units("")]
    protected double eros_rose_b2 = -1.0;

    [Param(IsOptional = true, MinVal = -1.0, MaxVal = 0.2)]
    [Description("Coefficient for calculating lambda in \"rose\" sub-model - bed load")]
    [Units("")]
    protected double eros_rose_b2_bed = -1.0;

    [Param(IsOptional = true, MinVal =-1.0, MaxVal = 0.2)]
    [Description("Coefficient for calculating lambda in \"rose\" sub-model - suspended load")]
    [Units("")]
    protected double eros_rose_b2_susp = -1.0;

#endregion
#endregion

#region Outputs we make available
    [Output]
    [Units("t/ha")]
    [Description("")]
    double soil_loss
    {
        get { return soil_loss_bed + soil_loss_susp; }
    }

    [Output]
    [Units("t/ha")]
    [Description("")]
    double soil_loss_bed = 0.0;

    [Output]
    [Units("t/ha")]
    [Description("")]
    double soil_loss_susp = 0.0;

    [Output]
    [Units("mm")]
    [Description("")]
    double soil_loss_mm
    {
        get
        {
            return MathUtility.Divide(soil_loss * t2g / ha2scm, bd[0], 0.0) * cm2mm;
        }
    }

    [Output]
    [Units("g/l")]
    [Description("")]
    double sed_conc
    {
        get
        {
            return MathUtility.Divide(soil_loss * t2g / ha2sm, runoff * mm2lpsm, 0.0);
        }
    }

    [Output]
    [Units("g/l")]
    [Description("")]
    double sed_conc_bed
    {
        get
        {
            return MathUtility.Divide(soil_loss_bed * t2g / ha2sm, runoff * mm2lpsm, 0.0);
        }
    }

    [Output]
    [Units("g/l")]
    [Description("")]
    double sed_conc_susp
    {
        get
        {
            return MathUtility.Divide(soil_loss_susp * t2g / ha2sm, runoff * mm2lpsm, 0.0);
        }
    }

    [Output]
    [Description("")]
    double erosion_cover
    {
        get { return cover_surface_runoff + _cover_extra; }
    }

#endregion

#region Drivers we obtain from other components

// "year" and "day" are present in the Fortran version, but are never used
//    [Input]
//    protected int year;

//    [Input]
//    protected int day;

    [Input]
    [Units("mm")]
    [Description("Daily runoff.")]
    protected double runoff = 0.0;

// But what cover values to we REALLY need?
// The "plant" components provide cover_green, cover_tot and cover_dead (called cover_sen in Plant)
// SuraceOM provide surfaceom_cover (that is, residue)
// SoilWat combines these and provides cover_surface_runoff, which is pulled in here
// For wind erosion calculations, we'll need a bit more; probably need to query each "plant"
// component for cover and height....

    [Input]
    [Units("0-1")]
    [Description("Effective total cover (crop and residue)")]
    protected double cover_surface_runoff = 0.0;

    [Input]
    [Units("mm")]
    [Description("Thickness of soil layers")]
    protected double[] dlayer = null;  // Thickness of soil layer (mm)

    [Input]
    [Units("g/cm^3")]
    [Description("Moist bulk density of soil layers")]
    public double[] bd = null;  // Bulk density

    [Input]
    [Units("0-1")]
    [Description("Water content of soil layers")]
    public double[] sw = null;  // Soil water

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Daily average wind speed")]
    public double wind = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 2300 hours to midnight")]
    public double wind0 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from midnight to 0100 hours")]
    public double wind1 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0100 to 0200 hours")]
    public double wind2 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0200 to 0300 hours")]
    public double wind3 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0300 to 0400 hours")]
    public double wind4 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0400 to 0500 hours")]
    public double wind5 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0500 to 0600 hours")]
    public double wind6 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0600 to 0700 hours")]
    public double wind7 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0700 to 0800 hours")]
    public double wind8 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0800 to 0900 hours")]
    public double wind9 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 0900 to 1000 hours")]
    public double wind10 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1000 to 1100 hours")]
    public double wind11 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1100 to 1200 hours")]
    public double wind12 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1200 to 1300 hours")]
    public double wind13 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1300 to 1400 hours")]
    public double wind14 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1400 to 1500 hours")]
    public double wind15 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1500 to 1600 hours")]
    public double wind16 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1600 to 1700 hours")]
    public double wind17 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1700 to 1800 hours")]
    public double wind18 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1800 to 1900 hours")]
    public double wind19 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 1900 to 2000 hours")]
    public double wind20 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 2000 to 2100 hours")]
    public double wind21 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 2100 to 2200 hours")]
    public double wind22 = Double.NaN;

    [Input(IsOptional = true)]
    [Units("m/s")]
    [Description("Wind speed from 2200 to 2300 hours")]
    public double wind23 = Double.NaN;

#endregion

    protected double[] dlt_dlayer = null;

#region Events to which we subscribe, and their handlers
    [EventHandler]
    public void OnInit2()
    {
        //ZeroVariables();
        //GetOtherVariables();
        Init();
        WriteSummary();
    }

    [EventHandler]
    public void OnProcess()
    {
        ZeroDailyVariables();
        // GetOtherVariables();
        Process();
        SetOtherVariables();
    }
#endregion

    protected enum model_type_enum
    {
        unknown = 0,
        freebairn = 1,
        rose = 2
    }

    protected enum windDataType
    {
        none,
        daily,
        hourly,
        three_hourly
    }

    protected windDataType wind_type = windDataType.none;

    protected model_type_enum model_type = model_type_enum.unknown;
    Boolean reduce_profile = false;
    protected double ls_factor = 0.0; // Slope-length factor
    protected double layer_merge_mm;

#region Useful constants
    protected const double pcnt2fract = 1.0 / 100.0;    // convert percent to fraction
    protected const double fract2pcnt = 100.0;  // convert fraction to percent
    protected const double t2g = 1000.0 * 1000.0; // tonnes to grams
    protected const double g2t = 1.0/(1000.0 * 1000.0); // convert grams to tonnes
    protected const double ha2scm = 10000.0 * 10000.0; // ha to sq. cm.
    protected const double ha2sm = 10000.0;            // convert hectares to sq metres
    protected const double sm2ha = 1.0 / 10000.0;  // convert m^2 to hectares
    protected const double g2mm = 1.0e3/1.0e6;   // convert g water/m^2 to mm water
    protected const double cm2mm = 10.0;  // cm to mm
    protected const double mm2lpsm = 1.0; // mm depth to litres per sq m
#endregion

    protected void ZeroDailyVariables()
    {
        int nLayers = dlayer.Length;
        // No need to zero these!
        //dlayer = new double[nLayers];
        //bd = new double[nLayers];
        dlt_dlayer = new double[nLayers];
        soil_loss_bed = 0.0;
        soil_loss_susp = 0.0;
    }

    protected void Init()
    {
        Console.WriteLine("      Initialising:");
        Console.WriteLine("     ");
        Console.WriteLine("        - Reading Parameters");
        Console.WriteLine("     ");

        if (dlayer == null)
        {
            MyComponent.Get("dlayer", out dlayer);
        }

        if (bed_depth < MathUtility.Sum(dlayer))
            throw new Exception("Depth to bedrock is less than profile depth");

        if (model_type == model_type_enum.unknown)
            throw new Exception("Unknown model_type");
        else if (model_type == model_type_enum.freebairn)
        {
            if (k_factor >= 0.0)
            {
                k_factor_bed = k_factor;
                k_factor_susp = 0.0;
            }
            else if (k_factor_bed < 0.0 || k_factor_susp < 0.0)
                throw new Exception("Required parameter(s) for k_factor not supplied");
            if (p_factor < 0.0)
                throw new Exception("Required parameter for p_factor not supplied");
        }
        else if (model_type == model_type_enum.rose)
        {
            if (entrain_eff >= 0.0)
            {
                entrain_eff_bed = entrain_eff;
                entrain_eff_susp = 0.0;
            }
            else if (entrain_eff_bed < 0.0 || entrain_eff_susp < 0.0)
                throw new Exception("Required parameter(s) for entrain_eff not supplied");
            if (eros_rose_b2 >= 0.0)
            {
                eros_rose_b2_bed = eros_rose_b2;
                eros_rose_b2_susp = 0.0;
            }
            else if (eros_rose_b2_bed < 0.0 || eros_rose_b2_susp < 0.0)
                throw new Exception("Required parameter(s) for eros_rose_b2 not supplied");
        }

        // Calculate USLE LS factor
        double s = slope * pcnt2fract;
        double a = 0.6 * (1.0 - Math.Exp(-35.835 * s));
        ls_factor = Math.Pow(slope_length / 22.1, a)
            * (65.41 * s * s + 4.56 *s + 0.065);

        if (reduce_profile) 
            layer_merge_mm = dlayer[dlayer.Length - 1] * profile_layer_merge;
        else
            layer_merge_mm = 0.0;


    }

    protected void WriteSummary()
    {
        Console.WriteLine("     ");
        Console.WriteLine("     ");
        Console.WriteLine("                      Erosion Parameters");
        Console.WriteLine("          -----------------------------------------------");

        if (reduce_profile)
        {
            Console.WriteLine("           Profile reduction:                     on");
            Console.WriteLine("           Fraction of original layer for merge: {0,4:F3}", profile_layer_merge);
        }
        else
        {
            Console.WriteLine("           Profile reduction:                     off");
        }

        if (model_type == model_type_enum.freebairn)
        {
            Console.WriteLine("           Freebairn cover-sediment concentration model");
            Console.WriteLine("     ");
            Console.WriteLine("           LS factor:                             {0,6:F4}", ls_factor);
            Console.WriteLine("     ");
            if (k_factor_susp <= 0.0)
            {
                Console.WriteLine("           K factor:                              {0,6:F4}", k_factor_bed);
                Console.WriteLine("     ");
            }
            else
            {
                Console.WriteLine("           K factor (bedload):                    {0,6:F4}", k_factor_bed);
                Console.WriteLine("     ");
                Console.WriteLine("           K factor (suspended load):             {0,6:F4}", k_factor_susp);
                Console.WriteLine("     ");
            }
            Console.WriteLine("           P factor:                              {0,6:F4}", p_factor);
            Console.WriteLine("     ");
        }
        else if (model_type == model_type_enum.rose)
        {
            Console.WriteLine("           Rose sediment concentration model");
            Console.WriteLine("     ");
            if (entrain_eff_susp <= 0.0)
            {
                Console.WriteLine("            Efficiency of entrainment:            {0,6:F4}", entrain_eff_bed);
                Console.WriteLine("     ");
            }
            else
            {
                Console.WriteLine("            Efficiency of bed load entrainment:   {0,6:F4}", entrain_eff_bed);
                Console.WriteLine("     ");
                Console.WriteLine("            Efficiency of susp. load entrainment: {0,6:F4}", entrain_eff_susp);
                Console.WriteLine("     ");
            }
            Console.WriteLine("            Slope (%):                            {0,6:F2}", slope);
            Console.WriteLine("     ");
        }
        else
        {
            Console.WriteLine("           ? Unknown model type ?");
            Console.WriteLine("     ");
        }

        if (Double.IsNaN(wind))
        {
            MyComponent.Get("wind", out wind);
        }
        if (Double.IsNaN(wind1))
        {
            MyComponent.Get("wind1", out wind1);
        }
        if (Double.IsNaN(wind3))
        {
            MyComponent.Get("wind3", out wind3);
        }

        // Should also allow for the use of gust data....
        if (!Double.IsNaN(wind1))
        {
            wind_type = windDataType.hourly;
            Console.WriteLine("           Wind erosion will be assessed based on hourly wind speeds");
        }
        else if (!Double.IsNaN(wind3))
        {
            wind_type = windDataType.three_hourly;
            Console.WriteLine("           Wind erosion will be assessed based on 3-hourly wind speeds");
        }
        else if (!Double.IsNaN(wind))
        {
            wind_type = windDataType.daily;
            Console.WriteLine("           Wind erosion will be assessed based on daily wind speeds");
        }
        else
        {
            wind_type = windDataType.none;
            Console.WriteLine("           Wind data are not available; wind erosion will not be assessed");
        }

        Console.WriteLine("          -----------------------------------------------");
     }

    protected void Process()
    {
        soil_loss_bed = 0.0;
        soil_loss_susp = 0.0;
        if (model_type == model_type_enum.freebairn)
            Freebairn();
        else if (model_type == model_type_enum.rose)
            Rose();

        if (soil_loss_bed + soil_loss_susp > 0.0 && reduce_profile)
            MoveProfile();

        if (wind_type == windDataType.three_hourly)
        {
            // Need to multiply by 100 to convert m/s to cm/s
            // Currently using 200 just to get the speeds up enough from some results...
            double flux = VerticalFlux(wind0 * 200.0) * 3.0 * 3600.0;  // Flux will have units of g/cm^2
            flux += VerticalFlux(wind3 * 200.0) * 3.0 * 3600.0;
            flux += VerticalFlux(wind6 * 200.0) * 3.0 * 3600.0;
            flux += VerticalFlux(wind9 * 200.0) * 3.0 * 3600.0;
            flux += VerticalFlux(wind12 * 200.0) * 3.0 * 3600.0;
            flux += VerticalFlux(wind15 * 200.0) * 3.0 * 3600.0;
            flux += VerticalFlux(wind18 * 200.0) * 3.0 * 3600.0;
            flux += VerticalFlux(wind21 * 200.0) * 3.0 * 3600.0;
            if (flux > 0.0)
                Console.WriteLine("Got some wind erosion!");
        }

    }

    protected void SetOtherVariables()
    {
        if (soil_loss_bed + soil_loss_susp > 0.0 && reduce_profile)
        {
            MyComponent.Set("dlt_dlayer", dlt_dlayer);
        }
    }

    protected void Freebairn()
    {

//     Freebairn cover-sediment concentration model
//     from PERFECT. Sets t/ha bed and suspended loss

        double sed_conc;
        double erosion_cover_pcnt = erosion_cover * fract2pcnt;
        if (erosion_cover < 0.5)
            sed_conc = 16.52
                       - 0.46 * erosion_cover_pcnt
                       + 0.0031 * erosion_cover_pcnt * erosion_cover_pcnt;
        else
            sed_conc = 2.54 - 0.0254 * erosion_cover_pcnt;

        soil_loss_bed = sed_conc * pcnt2fract * g2t / (g2mm * sm2ha)
                        * ls_factor * k_factor_bed
                        * p_factor * runoff;
        soil_loss_susp = sed_conc * pcnt2fract * g2t / (g2mm * sm2ha)
                        * ls_factor * k_factor_susp
                        * p_factor * runoff;
    }

    protected void Rose()
    {
//     Simplified rose model from PERFECT
//     Sets t/ha bed and suspended loads

//+  Notes
//******************************************************************
//                                                                 *
//  This subroutine calculates soil loss using the simplified Rose *
//  algorithm.                                                     *
//     apsim         perfect   descr                               *
//     ---           ---       ------------                        *
//     total_cover - covm   -  mulch cover     ( 0 - 1)            *
//     entrain_eff - kusle  -  efficiency of entrainment (bare conditions)*
//     runoff      - runf   -  event runoff (mm)                   *
//     (returned)  - sed    -  soil loss (t/ha)                    *
//     slope       - aslope -  slope (%)                           *
//                                                                 *
//******************************************************************
        double lambda_bed = entrain_eff_bed
            * Math.Exp(-eros_rose_b2_bed * erosion_cover * fract2pcnt);

        soil_loss_bed = 2700.0 * (slope * pcnt2fract)
            * (1.0 - erosion_cover)
            * lambda_bed * runoff / 100.0;

        double lambda_susp = entrain_eff_susp
            * Math.Exp(-eros_rose_b2_susp * erosion_cover * fract2pcnt);

        soil_loss_susp = 2700.0 * (slope * pcnt2fract)
            * (1.0 - erosion_cover)
            * lambda_susp * runoff / 100.0;

    }

    protected void MoveProfile()
    {
        double dlt_bed_depth = MoveDLayer();
        int nLayers = dlayer.Length;

        if (MathUtility.Sum(dlayer) +
            MathUtility.Sum(dlt_dlayer) < minimum_depth)
            throw new Exception("Out of soil to erode. Giving up.");

        bed_depth += dlt_bed_depth;
    }

    protected double MoveDLayer()
    {
        // Move the layers - erode from the bottom up

        double dlt_bed_depth = 0.0;
        int nLayers = dlayer.Length;
        dlt_dlayer = new double[nLayers];

        double top = (soil_loss_bed + soil_loss_susp) * t2g / ha2scm;
        double dlt_depth_mm = MathUtility.Divide(top, bd[0], 0.0) * cm2mm;

        // What happens when layer completely eroded?
        if (dlt_depth_mm > dlayer[0])
            MyComponent.Warning("Eroding more than top layer depth.\n This may affect SoilN loss");

        dlt_depth_mm = MathUtility.Divide(top, bd[nLayers - 1], 0.0) * cm2mm;

        // What happens when layer completely eroded?
        if (dlt_depth_mm > dlayer[nLayers - 1])
        {
            MyComponent.Warning("Eroding more than bottom layer depth. (layer" +
               nLayers.ToString() + ").\n" +
               "PAWC calculations may be incorrect if BD is different to layer above.");
        }

        // Check whether we've moved bedrock
        // into the profile. If so, we have to change dlayer.
        double tot_depth = MathUtility.Sum(dlayer) + dlt_depth_mm;

        if (tot_depth > bed_depth)
        {
            double overrun = tot_depth - bed_depth;
            dlt_bed_depth = - overrun;
            for (int i = nLayers - 1; i >= 0; i--)
            {
                if (overrun > 0.0) 
                {
                    // yes - eroded into bedrock.
                    if (overrun <= dlayer[i]) 
                    {
                        // move portion of layer
                        dlt_dlayer[i] = - overrun;
                        // find if layers merge
                        double new_depth = dlt_dlayer[i] + dlayer[i];
                        if (new_depth < layer_merge_mm) 
                        {
                            if (i <= 0)
                                throw new Exception("Out of soil to erode. Giving up.");
                            dlt_dlayer[i - 1] = new_depth;
                            dlt_dlayer[i] = - dlayer[i];
                            layer_merge_mm = dlayer[i - 1] * profile_layer_merge;
                        }
                        overrun = 0.0;
                    }
                    else
                    {
                        // remove entire layer
                        dlt_dlayer[i] = - dlayer[i];
                        overrun = overrun - dlayer[i];
                    }
                }
            }
        }
        return dlt_bed_depth;
    }
}

