using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

/// <summary>
/// This is an attempt to incorporate a model of wind erosion. On the advice of Adrian Chappell (CLW), this model is
/// a process-based model, largely based on the dust emission models (and their variations) summarised by
/// Darmenova et al., 2010, Journal of Geophysical Research 114(D14201)
/// Initiated by Eric Zurcher Nov 2011
/// </summary>
public partial class SoilErosion
{

    /// <summary>
    /// What cover values to we REALLY need?
    /// The "plant" components provide cover_green, cover_tot and cover_dead (called cover_sen in Plant)
    /// SuraceOM provide surfaceom_cover (that is, residue)
    /// SoilWat combines these and provides cover_surface_runoff, which is pulled in and used for the old water erosion model.
    /// For wind erosion calculations, we'll need a bit more; we probably need to query each "plant"
    /// component for cover and height....
    /// I've placed this here, but don't really expect to use it in this form, especially as there may be multiple "crops"
    /// in a single paddock, and the APSIM engine will only return the first it finds.
    /// </summary>
    //[Input]
    [Units("0-1")]
    [Description("Effective total crop cover (green and dead)")]
    public double cover_tot = 0.0;

    /// <summary>
    /// There are currently two basic models to choose from, which are similar in overall approach, but differ in
    /// algorithmic details. These are "shao" (based on Shao et al. 1996 and Shao 2004) and "mb95" (based on the work
    /// of Marticorena and Bergametti, 1995). Setting this string variable to either "shao" or "mb95" determines which
    /// model will be used. Any other string will trigger an error.
    /// </summary>
    [Param(IsOptional = true)]
    [Description("Wind model (Shao or MB95)")]
    public string wind_model = "shao";

    /// <summary>
    /// There are also two alternative formulations for calculating salation flux from the friction velocity.
    /// Setting this string variable to either "white" or "owen" determines which of these will be used. 
    /// Any other string will trigger an error.
    /// </summary>
    [Param(IsOptional = true)]
    [Description("Model used for calculating saltation flux (White or Owen)")]
    public string flux_model = "white";

    /// <summary>
    /// Gravitational acceleration, converted to cm/sec^2
    /// </summary>
    protected double gravity = 9.80665 * 100.0;

    /// <summary>
    /// Density of air, in g/cm^3 (default value is typical at sea level)
    /// Slightly lower values may be used at higher elevations
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0010, MaxVal = 0.0015)]
    [Description("Density of air")]
    [Units("g/cm^3")]
    public double rho_air = 0.00123;

    /// <summary>
    /// Density of soil particles, in g/cm^3 (default value is typical of quartz)
    /// This varies with mineralogy
    /// </summary>
    [Param(IsOptional = true, MinVal = 2.0, MaxVal = 3.0)]
    [Description("Density of soil particles")]
    [Units("g/cm^3")]
    public double rho_particle = 2.65;

    /// <summary>
    /// Density of water, in g/cm^3
    /// </summary>
    public double rho_water = 1.0;

    /// <summary>
    /// Gamma parameter in Shao and Lu relationship for determining threshold friction velocity of a smooth surface
    /// Is a measure of the cohesive force
    /// Values used in Darmenova et al. were 1.65e-4, 3e-4, and 5e-4 kg/s^2
    /// Note that here we are using units of g/s^2, not kg/s^2
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.1, MaxVal = 0.6)]
    [Description("Gamma parameter of Shao and Lu")]
    [Units("g/s^2")]
    public double Shao_gamma = 0.165;

    /// <summary>
    /// An parameter in Shao and Lu relationship for determining threshold friction velocity of a smooth surface
    /// There is probably little reason to change from the default value
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.011, MaxVal = 0.013)]
    [Description("An parameter of Shao and Lu")]
    [Units("-")]
    public double Shao_An = 0.0123;

    /// <summary>
    /// Indicates option to use for moisture correction
    /// Currently applied only to the Shao model.
    /// If 1, use the Fecan version (as used in MB95)
    /// If 2, use the Shao et al. 1996 version
    /// Otherwise, use the Zhao et al. 2006 variant
    /// </summary>
    [Param(IsOptional = true)]
    public int moisture_option = 1;

    /// <summary>
    /// Amount of clay in the soil surface layer, expressed as a percentage
    /// This is used to generate the particle size distribution
    /// This is also used in the MB95 moisture correction algorithm
    /// It would be better if APSIM could reliably provide us with soil texture information!
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction clay in the surface layers")]
    [Units("0-1")]
    public double fraction_clay = 0.30;

    /// <summary>
    /// Amount of clay in the soil surface layer, expressed as a percentage
    /// This is used to generate the particle size distribution
    /// It would be better if APSIM could reliably provide us with soil texture information!
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction silt in the surface layers")]
    [Units("0-1")]
    public double fraction_silt = 0.30;

    /// <summary>
    /// Amount of clay in the soil surface layer, expressed as a percentage
    /// This is used to generate the particle size distribution
    /// It would be better if APSIM could reliably provide us with soil texture information!
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction fine sand in the surface layers")]
    [Units("0-1")]
    public double fraction_fine_sand = 0.3;

    /// <summary>
    /// Maximum radius of particles considered to be clay sized
    /// Note that this is radius, not diameter
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10.0)]
    [Description("Maximum radius of clay particles")]
    [Units("um")]
    public double max_clay_radius = 1.0;

    /// <summary>
    /// Maximum radius of particles considered to be silt sized
    /// Note that this is radius, not diameter
    /// </summary>
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 100.0)]
    [Description("Maximum radius of silt particles")]
    [Units("um")]
    public double max_silt_radius = 10.0;

    /// <summary>
    /// Maximum radius of particles considered to be fine-sand sized
    /// Note that this is radius, not diameter
    /// </summary>
    [Param(IsOptional = true, MinVal = 10.0, MaxVal = 1000.0)]
    [Description("Maximum radius of fine sand particles")]
    [Units("um")]
    public double max_fine_sand_radius = 100.0;

    /// <summary>
    /// Smooth roughness length; used in calculating the MB95 drag partition correction
    /// Darmenova et al. suggest a range from 1e-4 to 3e-3
    /// An alternative is to use 1 / 30 of the mass median diameter of the coarse mode of undisturbed soil particles
    /// </summary>
    [Param(IsOptional = true, MinVal = 1e-4, MaxVal = 3e-3)]
    [Description("z0s for MB drag partition correction")]
    [Units("cm")]
    public double smooth_roughness_length = 0.0015;

    /// <summary>
    /// Aeolian roughness length; used in calculating the MB95 drag partition correction
    /// Depends on the surface
    /// See Darmenove et al. Table 1 for representative values
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.001, MaxVal = 12)]
    [Description("z0 for MB drag partition correction")]
    [Units("cm")]
    public double aeolian_roughness_length = 0.2;

    /// <summary>
    /// Indicates option to use for drag partition correction
    /// Currently applied only to the MB95 model.
    /// If 1, use the original MB95 version
    /// Otherwise, use the MacKinnon et al. (2004) variant
    /// </summary>
    [Param(IsOptional = true)]
    public int drag_partition_option = 2;

    /// <summary>
    /// Sigma is defined as the ratio of the basal to frontal area of a roughness element
    /// This is the value for the bare (unvegetated) surface
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.2, MaxVal = 5.0)]
    [Description("sigma for bare ground for Shao drag partition correction")]
    [Units("-")]
    public double sigma_bare = 1.0;

    /// <summary>
    /// Sigma is defined as the ratio of the basal to frontal area of a roughness element
    /// This is the value for the vegetated surface
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.2, MaxVal = 5.0)]
    [Description("sigma for vegetated surface for Shao drag partition correction")]
    [Units("-")]
    public double sigma_vegetated = 1.45;

    /// <summary>
    /// Beta is defined as the ratio of the drag coefficient for a single roughness element
    /// to that of the surface without roughness elements.
    /// This is the value for the bare (unvegetated) surface
    /// </summary>
    [Param(IsOptional = true, MinVal = 10, MaxVal = 500)]
    [Description("beta for bare ground for Shao drag partition correction")]
    [Units("-")]
    public double beta_bare = 90.0;

    /// <summary>
    /// Beta is defined as the ratio of the drag coefficient for a single roughness element
    /// to that of the surface without roughness elements.
    /// This is the value for the vegetated surface
    /// </summary>
    [Param(IsOptional = true, MinVal = 10, MaxVal = 500)]
    [Description("beta for vegetated surface for Shao drag partition correction")]
    [Units("-")]
    public double beta_vegetated = 202.0;

    /// <summary>
    /// m is a value accounting for the spatiotemporal variations of the stress of the underlying surface
    /// This is the value for the bare (unvegetated) surface
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Description("m for bare ground for Shao drag partition correction")]
    [Units("-")]
    public double m_bare = 0.5;

    /// <summary>
    /// m is a value accounting for the spatiotemporal variations of the stress of the underlying surface
    /// This is the value for the vegetated surface
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Description("m for vegetated surface for Shao drag partition correction")]
    [Units("-")]
    public double m_vegetated = 0.16;

    /// <summary>
    /// C-lambda is a coefficent accounting for the distribution and orientation of (vegetative) roughness elements.
    /// This is the value for the bare (unvegetated) surface
    /// The default value of 0.35 "is appropriate for stubble roughness"
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Description("C-lambda for vegetation for Shao drag partition correction")]
    [Units("-")]
    public double c_lambda = 0.35;

    /// <summary>
    /// coeff_flux is a coefficient used in the calculation of saltation flux
    /// The original MB scheme used c_mb = 2.61, but Darmenova et al. used 1
    /// for both MB and Shao
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.1, MaxVal = 25.0)]
    [Description("C-mb coefficient for saltation flux in MB95")]
    [Units("-")]
    public double coeff_flux = 1.0;

    /// <summary>
    /// shao_k_gamma is a coefficent used in the Shao dust emission scheme
    /// In Shao's papers, a value of 1.0 is used, but his source code uses 0.5
    /// See text near Eq. 7.39 of Shao's book
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.1, MaxVal = 2.0)]
    [Description("Coefficient used in the calculation of gamma in Shao dust emission")]
    [Units("")]
    public double shao_k_gamma = 1.0;

    /// <summary>
    /// shao_c_y is a coefficent used in the Shao dust emission scheme
    /// </summary>
    [Param(IsOptional = true)]
    [Description("Coefficent used in the Shao dust emission scheme")]
    [Units("-")]
    public double shao_c_y = 0.000057;

    /// <summary>
    /// Plastic pressure
    /// </summary>
    [Param(IsOptional = true, MinVal = 10000, MaxVal = 30000)]
    [Description("Plastic pressure")]
    [Units("N/m^2")]
    public double plastic_pressure = 20250.0;

    /// <summary>
    /// Particle size, in microns, below which material can be considered "dust"
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100.0)]
    [Description("Maximum particle size for dust")]
    [Units("um")]
    public double dust_cutoff = 20.0;

    [Param(IsOptional = true, MinVal = 1)]
    [Description("Number of particle size classes")]
    [Units("-")]
    public int n_classes = 200;

    [Param(IsOptional = true, MinVal = 1)]
    [Description("Soil type, for particle size distribution")]
    public string soil_type = "clay";

    [Param(IsOptional = true, MinVal = 0, MaxVal = 15)]
    [Description("Height at which wind speed was measured")]
    public double wind_height = 10;

    [Param(IsOptional = true, MinVal = 0.75, MaxVal = 1.15)]
    [Description("Factor indicating wind speed constancy; lower values indicate higher variability (see Justus et al 1977)")]
    public double justus_wind_variability = 0.95;

    /// <summary>
    /// Variable to hold the overall plant cover
    /// </summary>
    public double plant_cover = 0.0;

    [Link]
    Paddock MyPaddock = null;

    /// <summary>
    /// Variables to hold particle size distribution
    /// </summary>
    protected double[] dd = null;
    protected double[] psd = null;    // Minimally dispersed
    protected double[] dpsd = null;
    protected double[] ppsd = null;
    protected double[] psd_disp = null;  // Fully dispersed
    protected double[] dpsd_disp = null;
    protected double[] ppsd_disp = null;
    protected double[] smoothThreshold = null;  // Threshold friction velocity for a smooth surface for a given particle size
    protected double[] horizFlux = null;

    // Use these if you want to use Shao's weighting of minimally vs. fully dispersed PSDs when calculating horizontal flux
//    protected double[] psds = null;  
//    protected double[] dpsds = null;
//    protected double[] ppsds = null;

    [Output]
    [Description("Total horizonal flux")]
    [Units("g/cm/s")]
    public double totHorizFlux;

    protected double moistureCorrection = 1.0; //
    protected double dragCorrection = 1.0;

    protected double frictionVelocity = 0.0;

    [Output]
    [Description("Friction velocity at the soil surface")]
    [Units("m/s")]
    protected double ustar 
    {
        get { return frictionVelocity / 100.0; } // Convert from cm/s to m/s 
    }

    [Output]
    [Description("Smallest threshold friction velocity for any particle size")]
    [Units("m/s")]
    protected double ustar_t_min;  // Minimum value for threshold velocity, expressed in m/s

    [Output]
    [Description("Wind erosion threshold")]
    [Units("m/s")]
    protected double wind_threshold;  // Minimum value for measured wind speed at which erosion may begin, expressed in m/s
    protected double windThreshold; 

    int iDust = 0; // Index of last particle size class considered to be dust

    /// <summary>
    /// Query all local "crop" components for their "cover_green" and "cover_tot" values.
    /// This will need refinement in the future.
    /// </summary>
    protected void GetCover()
    {
        // The default APSIM connection logic does not suffice here, since if we live in a mixed paddock, it
        // would provide us with cover values from only the first "crop" component.
        // We need to combine cover values from the various sources.
        // The AusFarm pasture component also provides "cover_green" and "cover_tot"
        // Note that "cover_tot" and "cover_green" are also provided in combined form by the AusFarm paddock component.
        // Perhaps we need to be using the AusFarm paddock component for these simulations...
        plant_cover = 0.0;
        if (MyPaddock != null)
        {
            foreach (Component Comp in MyPaddock.Crops)
            {
                double covgreen; //  = Comp.Variable("cover_tot").ToDouble(); 
                double covtot;
                double height;
                string FullCompName = Comp.FullName;
                Comp.Get(FullCompName + ".height", out height);
                Comp.Get(FullCompName + ".cover_tot", out covtot);
                Comp.Get(FullCompName + ".cover_green", out covgreen);
                // double height = Comp.Variable("height").ToDouble();
                plant_cover = 1.0 - (1.0 - plant_cover) * (1.0 - covtot);
            }
        }
    }

    /// <summary>
    /// Calculate the threshold friction velocity over a smooth, bare, surface for particles of a given size
    /// </summary>
    /// <param name="diameter">Diameter, in millimeters, of the particles</param>
    /// <returns>Threshold friction velocity in cm/sec</returns>
    protected double ThresholdFrictionVelocitySmooth(double diameter) // diameter in mm
    {
        // Within this routine, we're doing calculations with masses in grams, and lengths in cm
        diameter /= 10.0; // Convert diameter to cm
        if (wind_model.ToLower() == "mb95")
        {
            double result;
            double Reynolds = 1331.0 * Math.Pow(diameter, 1.56) + 0.38;
            double K = Math.Sqrt(diameter * gravity * rho_particle / rho_air) *
                       Math.Sqrt(1 + 0.006 / (rho_particle * gravity * Math.Pow(diameter, 2.5)));
            if (Reynolds <= 10.0)
                result = 0.129 * K / Math.Sqrt(1.928 * Math.Pow(Reynolds, 0.092) - 1.0);
            else
                result = 0.129 * K * (1.0 - 0.0858 * Math.Exp(-0.0617 * (Reynolds - 10.0)));
            return result;
        }

        else if (wind_model.ToLower() == "shao")
        {
             return Math.Sqrt(Shao_An * (diameter * gravity * rho_particle / rho_air +
                                         Shao_gamma / (diameter * rho_air)));
        }

        else
            throw new Exception("Unknown wind model: " + wind_model);
    }

    private double minSoilWat = 1.0;

    /// <summary>
    /// Calculate a moisture correction factor for adjusting the threshold friction velocity
    /// </summary>
    /// <param name="soilWater">Volumetric soil water content at the surface. If this is 0, the value will be obtained by querying "sw" from another components</param>
    /// <returns>The moisture correction factor</returns>
    protected double CalcMoistureCorrection(double soilWater) // volumetric soil moisture
    {
        if (soilWater == 0.0) // If soil water isn't provided as an argument, get it from another component
        {
            if (sw == null)
            {
                MyComponent.Get("sw", out sw);
            }
            if (sw == null)
            {
                double[] sw_dep = null;
                MyComponent.Get("sw_dep", out sw_dep);
                if (sw_dep == null)
                    throw new Exception("Could not obtain a value for soil water");
                if (dlayer == null)
                {
                   MyComponent.Get("dlayer", out dlayer);
                }
                if (dlayer == null)
                    throw new Exception("Could not obtain a value for soil layer depths");
                soilWater = MathUtility.Divide(sw_dep[0], dlayer[0], 0.0);
            }
            soilWater = sw[0];
            if (soilWater < minSoilWat)
                minSoilWat = soilWater;
        }

        if (wind_model.ToLower() == "mb95" || moisture_option == 1)
        {
            if (bd == null)
            {
                MyComponent.Get("bd", out bd);
            }

            double swGrav = 100.0 * soilWater * rho_water / bd[0]; //Gravimetric soil water, as a % - YUCK. Why did M&B use percentages for this stuff?
            double percent_clay = fraction_clay * 100.0;
            double wPrime = 0.0014 * MathUtility.Sqr(percent_clay) + 0.17 * percent_clay;
            if (swGrav < wPrime)
                return 1.0;
            else
                return Math.Sqrt(1.0 + 1.21 * Math.Pow(swGrav - wPrime, 0.68));
        }
        else if (wind_model.ToLower() == "shao")
        {
            if ((moisture_option == 2) || soilWater <= 0.003)
                return Math.Exp(22.7 * soilWater);
            else
                return Math.Exp(95.3 * soilWater - 2.029);
        }
        else
            throw new Exception("Unknown wind model: " + wind_model);
    }

    /// <summary>
    /// Calculate a drag partition correction factor for adjusting the threshold friction velocity
    /// </summary>
    /// <returns>The drag partition correction factor</returns>
    protected double CalcDragCorrection()
    {
        if (wind_model.ToLower() == "shao")
        {

            double lambda_vegetated = -c_lambda * Math.Log(1.0 - plant_cover);

            // Use this block of code for the algorithm from Shao et al. 1996
            // I'm not sure why, but Shao's source code mulitplies the input values for cover by 2
            // For now, I'm using Shao's logic (since it doesn't require guessing a value for lambda_bare),
            // but using an unmodified cover value.
            // double lambda_vegetated = -c_lambda * Math.Log(1.0 - plant_cover * 2.0); 

            //double xc = 1.0 / (sigma_bare * m_bare);
            if (lambda_vegetated >= 1.0 /* xc */)
                return 0.20; 
            else
                return (1.0 / Math.Sqrt(1.0 - sigma_bare * m_bare * lambda_vegetated)) * (1.0 / Math.Sqrt(1.0 + m_bare * beta_bare * lambda_vegetated));


            // The following block implements the modified version appearing in Darmenova et al. 2009
            // which merges the values for bare and vegetated surfaces in a slightly different way.
            /// I don't really know what should be used for "lambda_bare". I gather it depends on the roughness of the bare surface
            /// Darmenova et al. used 0.002 for "sandy" and 0.15 for "gobi" deserts
            // double lambda_bare = 0.01;
            // return (1.0 / Math.Sqrt(1.0 - sigma_vegetated * m_vegetated * lambda_vegetated)) *
            //       (1.0 / Math.Sqrt(1.0 + m_vegetated * beta_vegetated * lambda_vegetated)) *
            //       (1.0 / Math.Sqrt(1.0 - sigma_bare * m_bare * lambda_bare / (1.0 - plant_cover))) *
            //       (1.0 / Math.Sqrt(1.0 + m_bare * beta_bare * lambda_bare / (1.0 - plant_cover)));
        }
        else if (wind_model.ToLower() == "mb95")
        {
            if (drag_partition_option == 1)
            {
                return 1.0 - (Math.Log(aeolian_roughness_length / smooth_roughness_length) /
                         Math.Log(0.7 * Math.Pow(10.0 / smooth_roughness_length, 0.8)));
            }
            else
            {   // The MacKinnon (2004) variation
                return 1.0 - (Math.Log(aeolian_roughness_length / smooth_roughness_length) /
                         Math.Log(0.7 * Math.Pow(12255.0 / smooth_roughness_length, 0.8)));
            }
        }
        else
            throw new Exception("Unknown wind model: " + wind_model);
    }

    /// <summary>
    /// Calculate the threshold friction velocity for particles of a given size, with correction for moisture and "roughness"
    /// </summary>
    /// <param name="diameter">Diameter, in millimeters, of the particles</param>
    /// <returns>Threshold friction velocity in cm/sec</returns>
    protected double ThresholdFrictionVelocity(double diameter) // diameter in mm
    {
        return moistureCorrection * ThresholdFrictionVelocitySmooth(diameter) / dragCorrection;
    }

    protected double ThresholdFrictionVelocity(int diam_class)
    {
        if (smoothThreshold != null)
            return moistureCorrection * smoothThreshold[diam_class] / dragCorrection;
        else
            return ThresholdFrictionVelocity(dd[diam_class] / 1000.0);
    }

    /// <summary>
    /// Calculate the wind friction velocity
    /// </summary>
    /// <param name="windSpeed">wind speed, in cm/sec</param>
    /// <param name="height">height at which the speed was measured, in m</param>
    /// <returns>wind friction velocity in cm/sec</returns>
    protected double WindFrictionVelocity(double windSpeed, double height)
    {
        double k = 0.4; // The von Karman constant
        double D = 0.0; // The zero-displacment height, in cm
        double height_cm = height * 100.0; // Convert measurement height to cm; must be same units as roughness length
        if (height_cm == D)
            return windSpeed;
        // For now, assume aerodynamic roughness length = aeolian roughness length
        return (k * windSpeed) / (Math.Log((height_cm - D) / aeolian_roughness_length) /* + Psi ??? */ );
        // The "Psi" part refers to stablity function(s) which provide an adjustment for turbulence, which
        // can be expected to vary with the time of day.
        // This is a rather messy calculation that I frankly don't understand. Note that Venkatram (1980) provides an
        // empirically developed formula which doesn't require knowing surface temperature and heat fluxes.
        // If we need to make an adjustment, that may be a better way to do it...
    }

    /// <summary>
    /// Calculate the total horizontal flux of saltating particles
    /// </summary>
    /// <param name="windSpeed">wind speed, in cm/sec</param>
    /// <param name="height">height at which the speed was measured, in m</param>
    /// <param name="diameter">Diameter, in millimeters, of the particles</param>
    /// <returns>Horizontal flux in g/cm/s</returns>
    protected double HorizontalFlux(double frictionVelocity, int diam_class, out double thresholdVelocity)
    {
        thresholdVelocity = ThresholdFrictionVelocity(diam_class);
        if (frictionVelocity <= thresholdVelocity)
            return 0.0;
        else
        {
            double fv2 = MathUtility.Sqr(frictionVelocity); // square of friction velocity
            double fv3 = fv2 * frictionVelocity; // cube of friction velocity
            if (flux_model.ToLower() == "owen")
            {
                return (coeff_flux * rho_air * fv3 / gravity) * (1.0 - MathUtility.Sqr(thresholdVelocity) / fv2);
            }
            else if (flux_model.ToLower() == "white")
            {
// Shao's code uses an incorrect form of White's equation (Actually, the original mistake was White's)
// The incorrect form yields a value roughly half what it should be.
//                double bad = (coeff_flux * rho_air * fv3 / gravity) * (1.0 + MathUtility.Sqr(thresholdVelocity) / fv2)
//                    * (1.0 - thresholdVelocity / frictionVelocity);
                return (coeff_flux * rho_air * fv3 / gravity) * (1.0 - MathUtility.Sqr(thresholdVelocity) / fv2)
                    * (1.0 + thresholdVelocity / frictionVelocity);
            }
            else
                throw new Exception("Unknown flux model: " + wind_model);
        }
    }

    // private double[] qbin; // Used for making comparisions with Shao's code
    // private int[] ins_l;
    // private int[] ins_h;
    /// <summary>
    /// This function is intended to integrate the horizontal flux over all particle sizes 
    /// (within the range of saltation).
    /// </summary>
    /// <param name="windSpeed">wind speed, in cm/sec</param>
    /// <param name="height">height at which the speed was measured, in m</param>
    /// <returns>Total horizonal flux in g/cm/s</returns>
    protected double TotalHorizontalFlux(double windSpeed, double height)
    {
        int iClay = 0;
        // If we haven't already created a particle size distribution, do it now
        // Also calculate the smooth theshold friction velocity for each size class
        // and the maximum index for "dust"
        if (dd == null)
        {
            CreateSkaggsSizeDist(out dd, out psd, out dpsd, out ppsd, n_classes);
            CreateSkaggsSizeDist(out dd, out psd_disp, out dpsd_disp, out ppsd_disp, n_classes);
            //logNormalDist[] dist = null;
            //PSD.GetDist(soil_type, true, ref dist);
            //CreateSizeDist(dist, out dd, out psd_disp, out dpsd_disp, out ppsd_disp, n_classes);
            //PSD.GetDist(soil_type, false, ref dist);
            //CreateSizeDist(dist, out dd, out psd, out dpsd, out ppsd, n_classes);
            smoothThreshold = new double[dd.Length];
            for (int i = 0; i < dd.Length; i++)
            {
                smoothThreshold[i] = ThresholdFrictionVelocitySmooth(dd[i] / 1000.0);
                if (dd[i] <= dust_cutoff)
                    iDust = i;
                if (dd[i] <= max_clay_radius * 2.0) // Compare with maximum clay diameters
                    iClay = i;
            }

            // Creates "bins" of sand-sized particles
            // This is solely for comparison of outputs with Shao code
            // Rather quirky....
            //ins_l = new int [32] {117,135,141,146,149,153,156,159,161,163,
            //                      166,168,169,171,172,173,175,176,177,179,
            //                      180,181,182,183,184,185,186,187,188,189,190,191};
            //ins_h = new int [32] {134,140,145,149,152,156,158,161,163,166,
            //                      168,169,171,173,174,176,177,178,180,181,
            //                      182,183,184,185,186,187,188,189,190,191,192,193};
            //qbin = new double[32];
                 
        }

        if (fraction_clay < 0.0)
            fraction_clay = ppsd_disp[iClay];

        // Initialise those values that don't change with particle size, but do change with time
        moistureCorrection = CalcMoistureCorrection(0.0);
        dragCorrection = CalcDragCorrection();
        frictionVelocity = WindFrictionVelocity(windSpeed, height);

        // Now get the flux associated with each particle size, and weight the result
        // by the probability density of that size class
        double[] rawHorizFlux = new double[dd.Length];
        totHorizFlux = 0.0;
        double ustar_t;
        ustar_t_min = Double.MaxValue;
        for (int i = 0; i < dd.Length; i++)
        {
            rawHorizFlux[i] = HorizontalFlux(frictionVelocity, i, out ustar_t);
            horizFlux[i] = rawHorizFlux[i] * dpsd[i];
            totHorizFlux += horizFlux[i];
            if (ustar_t / 100.0 < ustar_t_min)
                ustar_t_min = ustar_t / 100.0;  // Convert cm/sec to m/sec
        }

        double k = 0.4; // The von Karman constant
        double D = 0.0; // The zero-displacment height, in cm
        double height_cm = height * 100.0; // Convert measurement height to cm; must be same units as roughness length
        windThreshold = ustar_t_min * (Math.Log((height_cm - D) / aeolian_roughness_length)) / k;
        
        // Use this to parallel Shao's code
        // By making use of a weighting of fully vs. minimally dispersed...
//        if (wind_model.ToLower() == "shao" && frictionVelocity / 100.0 > ustar_t_min)
//        {
//            totHorizFlux = 0.0;
//            double ghl = Math.Exp(-0.5 * Math.Pow(frictionVelocity / 100.0 - ustar_t_min, 3.0));
//            for (int i = 0; i < dd.Length; i++)
//            {
//                dpsds[i] = ghl * dpsd[i] + (1.0 - ghl) * dpsd_disp[i];
//                psds[i] = ghl * psd[i] + (1.0 - ghl) * psd_disp[i];
//                ppsds[i] = ghl * ppsd[i] + (1.0 - ghl) * ppsd_disp[i];
//                horizFlux[i] = rawHorizFlux[i] * dpsds[i];
//                totHorizFlux += horizFlux[i];
//            }
//        }

        // Calculate flux values for comparision with Shao's code
        //    for (int k = 0; k < 32; k++)
        //    {
        //        qbin[k] = 0.0;
        //        for (int ki = ins_l[k]; ki <= ins_h[k]; ki++)
        //        {
        //            qbin[k] += horizFlux[ki - 1];
        //        }
        //       qbin[k] /= ins_h[k] - ins_l[k] + 1;
        //    }

      
        return totHorizFlux;
    }

    /// <summary>
    /// Another integration problem.
    /// This function is what we're ultimately after: given current conditions, what is the flux of soil particles into the air?
    /// Note that we are interested only in small particles, that remain suspended once lifted, so the maximum size we'd want to
    /// consider would be 100 microns or so.
    /// </summary>
    /// <param name="windSpeed">wind speed, in cm/sec</param>
    /// <param name="height">height at which the speed was measured, in m</param>
    /// <returns>Vertical flux in g/cm^2/s</returns>
    public double VerticalFlux(double windSpeed)
    {
        double totHorizFlux = TotalHorizontalFlux(windSpeed, wind_height);
        wind_threshold = Math.Min(wind_threshold, windThreshold);
        if (wind_model.ToLower() == "mb95")
        {
            // The units of alpha are cm^-1
            // Shao's source multiplies this result by 100, since he's using m, not cm
            double alpha = Math.Pow(10, 0.134 * Math.Min(100.0 * fraction_clay, 20.0) - 6.0); 
            return totHorizFlux * alpha;
        }
        else if (wind_model.ToLower() == "shao") // See code for imod .eq. 5 in Shao's source
        {
            if (bd == null)
            {
                MyComponent.Get("bd", out bd);            
            }
            double bulkDensity = bd[0] * 1000.0;  // Shao uses units of kg/m^3 for density
            double frictionVelocity = WindFrictionVelocity(windSpeed, wind_height) / 100.0; // NOTE: It this context, we want friction velocity in m/sec, not cm/sec
            if (frictionVelocity < ustar_t_min)
                return 0.0;
            double zeta = frictionVelocity * Math.Sqrt(bulkDensity / plastic_pressure);  // zeta is dimensionless
            double sigma_m = 12.0 * MathUtility.Sqr(zeta) * (1.0 + 14.0 * zeta);       // sigma_m is dimenionless
            double gamma = Math.Exp(-shao_k_gamma * Math.Pow(frictionVelocity - ustar_t_min, 3.0)); // ghl of Shao's code. I think the result is meant to be dimensionless, although the velocities start with units

            double[,] flux = new double[dd.Length, iDust + 1];
            for (int i = iDust + 1; i < dd.Length; i++)
                for (int j = 0; j <= iDust; j++)
                {
                    double sigma_p = dpsd[j] / dpsd_disp[j];
                    double term1 = shao_c_y * dpsd_disp[j] * ((1.0 - gamma) + gamma * sigma_p);
                    double term2 = 1.0 + sigma_m;      // dimensionless
                    double term3 = horizFlux[i] * gravity / MathUtility.Sqr(frictionVelocity); // This gives us units of g/m^2/sec
                    flux[i, j] = term1 * term2 * term3 * 1e-4; // Convert units to g/cm^2/sec
                }

            double totVertFlux = 0.0;
            for (int j = 0; j <= iDust; j++)
            {
                double vertFlux = 0.0;
                for (int i = iDust + 1; i < dd.Length; i++)
                    vertFlux += flux[i, j];
                totVertFlux += vertFlux;
            }
//            Console.Write(" " + (totHorizFlux * 1e8).ToString() + " " + (totVertFlux * 1e10).ToString());
            return totVertFlux;
        }
        else
            throw new Exception("Unknown wind model: " + wind_model);

    }

    public enum SoilTypes
    {
        clay,
        silt,
        sand,
        loam,
        sand_loam,
        silt_loam,
        clay_loam,
        jade
    }

    public struct logNormalDist
    {
        public double weight;
        public double mean;
        public double sigma;
    }

 
    public class PSD
    {
        static SoilTypes StrToSoilType(string typename)
        {
            if (typename.ToLower() == "clay")
                return SoilTypes.clay;
            else if (typename.ToLower() == "silt")
                return SoilTypes.silt;
            else if (typename.ToLower() == "sand")
                return SoilTypes.sand;
            else if (typename.ToLower() == "loam")
                return SoilTypes.loam;
            else if (typename.ToLower() == "sand_loam")
                return SoilTypes.sand_loam;
            else if (typename.ToLower() == "silt_loam")
                return SoilTypes.silt_loam;
            else if (typename.ToLower() == "clay_loam")
                return SoilTypes.clay_loam;
            else if (typename.ToLower() == "jade")
                return SoilTypes.jade;
            else
                throw new Exception("Unknown soil type: " + typename);
        }

        /// <summary>
        /// Obtain the parameters for particle size distribution for a given type of soil. The values below are taken from Shao's source code;
        /// see also Shao 2004, Table 1. 
        /// These values could be improved with additional data (of course).
        /// </summary>
        /// <param name="typename">Type of soil</param>
        /// <param name="dispersed">If true, return values for a fully-dispersed soil; otherwise return those for a minimally-dispersed soil</param>
        /// <param name="dist">Structure to receive the results</param>
        public static void GetDist(string typename, bool dispersed, ref logNormalDist[] dist)
        {
            SoilTypes soiltype = StrToSoilType(typename);
            switch (soiltype)
            {
                // Yuck. Clay looks to be just plain wrong! Even for the fully dispered values, most of the weight is given to sand-sized classes!
                case SoilTypes.clay:                                   
                    if (dispersed)  // Clay, fully dispersed
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.0872, mean = 0.6931, sigma = 1.0000 };
                        dist[1] = new logNormalDist { weight = 0.4464, mean = 3.9323, sigma = 0.9181 };
                        dist[2] = new logNormalDist { weight = 0.4465, mean = 5.4486, sigma = 0.3916 };
                    }
                    else           // Clay, minimally dispersed
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.3902, mean = 3.5542, sigma = 1.0000 };
                        dist[1] = new logNormalDist { weight = 0.2813, mean = 4.2239, sigma = 0.2507 };
                        dist[2] = new logNormalDist { weight = 0.3286, mean = 5.1638, sigma = 0.4632 };
                    }
                    break;

                case SoilTypes.silt:
                    if (dispersed)  // Silt, fully dispersed
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.8900, mean = 4.3235, sigma = 0.4658 };
                        dist[1] = new logNormalDist { weight = 0.0748, mean = 1.9996, sigma = 0.8649 };
                        dist[2] = new logNormalDist { weight = 0.0352, mean = 6.0420, sigma = 0.3464 };
                    }
                    else           // Silt, minimally dispersed
                    {
                        Array.Resize(ref dist, 2);
                        dist[0] = new logNormalDist { weight = 0.8888, mean = 4.5646, sigma = 0.2610 };
                        dist[1] = new logNormalDist { weight = 0.1112, mean = 3.9110, sigma = 0.1488 };
                    }
                    break;

                case SoilTypes.sand:
                    if (dispersed)  // Sand, fully dispersed
                    {
                        Array.Resize(ref dist, 2);
                        dist[0] = new logNormalDist { weight = 0.0338, mean = 0.6931, sigma = 1.0000 };
                        dist[1] = new logNormalDist { weight = 0.9662, mean = 5.6300, sigma = 0.2542 };
                    }
                    else           // Sand, minimally dispersed
                    {
                        Array.Resize(ref dist, 2);
                        dist[0] = new logNormalDist { weight = 0.0329, mean = 4.3733, sigma = 0.8590 };
                        dist[1] = new logNormalDist { weight = 0.9671, mean = 5.7689, sigma = 0.2526 };
                    }
                    break;

                case SoilTypes.loam:
                    if (dispersed)  // Loam, fully dispersed
                    {
                        Array.Resize(ref dist, 4);
                        dist[0] = new logNormalDist { weight = 0.5844, mean = 4.6079, sigma = 0.6141 };
                        dist[1] = new logNormalDist { weight = 0.3304, mean = 5.2050, sigma = 0.2897 };
                        dist[2] = new logNormalDist { weight = 0.0522, mean = 7.0553, sigma = 1.0000 };
                        dist[3] = new logNormalDist { weight = 0.0330, mean = 0.6931, sigma = 1.0000 };
                    }
                    else           // Loam, minimally dispersed
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.1114, mean = 4.3565, sigma = 0.4257 };
                        dist[1] = new logNormalDist { weight = 0.4554, mean = 5.1674, sigma = 0.3824 };
                        dist[2] = new logNormalDist { weight = 0.4331, mean = 5.4092, sigma = 1.0000 };
                    }
                    break;

                case SoilTypes.sand_loam:
                    if (dispersed)  // Sandy loam, fully dispersed (Manilla)
                    {
                        Array.Resize(ref dist, 2);
                        dist[0] = new logNormalDist { weight = 0.8700, mean = 4.4500, sigma = 0.9400 };
                        dist[1] = new logNormalDist { weight = 0.1300, mean = 2.4900, sigma = 0.9400 };
                    }
                    else           // Sandy loam, minimally dispersed (Manilla)
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.0800, mean = 5.0600, sigma = 0.2200 };
                        dist[1] = new logNormalDist { weight = 0.8800, mean = 4.5000, sigma = 0.9500 };
                        dist[2] = new logNormalDist { weight = 0.0400, mean = 2.4000, sigma = 0.9200 };
                    }
                    break;

                case SoilTypes.silt_loam:
                    if (dispersed)  // Silty loam, fully dispersed (Cooper)
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.1000, mean = 4.9900, sigma = 0.8900 };
                        dist[1] = new logNormalDist { weight = 0.5600, mean = 3.9200, sigma = 0.6200 };
                        dist[2] = new logNormalDist { weight = 0.3400, mean = 2.2800, sigma = 0.9100 };
                    }
                    else           // Sandy loam, minimally dispersed (Manilla)
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.7700, mean = 4.5000, sigma = 0.3900 };
                        dist[1] = new logNormalDist { weight = 0.1100, mean = 4.1900, sigma = 0.3900 };
                        dist[2] = new logNormalDist { weight = 0.1200, mean = 3.4600, sigma = 0.4600 };
                    }
                    break;

                case SoilTypes.clay_loam:
                    if (dispersed)  // Clay loam, fully dispersed (actually described as sandy clay loam)
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.2344, mean = 1.8079, sigma = 0.6141 };
                        dist[1] = new logNormalDist { weight = 0.3634, mean = 4.2050, sigma = 0.2897 };
                        dist[2] = new logNormalDist { weight = 0.4022, mean = 5.6553, sigma = 1.0000 };
                    }
                    else           // Clay loam, minimally dispersed (actually described as sandy clay loam)
                    {
                        Array.Resize(ref dist, 3);
                        dist[0] = new logNormalDist { weight = 0.0722, mean = 2.2675, sigma = 1.0000 };
                        dist[1] = new logNormalDist { weight = 0.6266, mean = 4.9654, sigma = 0.3496 };
                        dist[2] = new logNormalDist { weight = 0.3012, mean = 5.5819, sigma = 0.5893 };
                    }
                    break;

                case SoilTypes.jade:
                    if (dispersed)  // loamy sand at Shao's Jade site, fully dispersed
                    {
                        Array.Resize(ref dist, 4);
                        dist[0] = new logNormalDist { weight = 0.2280, mean = 5.4200, sigma = 0.3500 };
                        dist[1] = new logNormalDist { weight = 0.2770, mean = 4.8600, sigma = 0.5950 };
                        dist[2] = new logNormalDist { weight = 0.2950, mean = 3.0800, sigma = 1.0500 };
                        dist[3] = new logNormalDist { weight = 0.2000, mean = 1.3000, sigma = 1.4000 };
                    }
                    else           // loamy sand at Shao's Jade site, minimally dispersed
                    {
                        Array.Resize(ref dist, 4);
                        dist[0] = new logNormalDist { weight = 0.3500, mean = 5.4000, sigma = 0.3450 };
                        dist[1] = new logNormalDist { weight = 0.3200, mean = 4.6300, sigma = 0.4900 };
                        dist[2] = new logNormalDist { weight = 0.2300, mean = 4.1000, sigma = 0.6500 };
                        dist[3] = new logNormalDist { weight = 0.1000, mean = 2.7500, sigma = 0.9500 };
                    }
                    break;

            }
        }
    }

    /// <summary>
    /// Generate a particle size distribution function as the sum of four log-normal distributions.
    /// This code shamelessly derived from the Fortran source of Shao.
    /// </summary>
    /// <param name="diamMid"></param>
    /// <param name="PSDMid"></param>
    /// <param name="deltaPSD"></param>
    /// <param name="prob"></param>
    /// <param name="nSizes"></param>
    protected void CreateSizeDist(logNormalDist[] dist, out double[] diamMid, out double[] PSD, out double[] deltaPSD, out double[] cumProb,
                   int nSizes)
    {
        diamMid = new double[nSizes];
        PSD = new double[nSizes];
        deltaPSD = new double[nSizes];
        cumProb = new double[nSizes];
        horizFlux = new double[nSizes];
//        psds = new double[nSizes];
//        dpsds = new double[nSizes];
//        ppsds = new double[nSizes];

        // Estimate diameters using log10 scale
        const double minSize = 0.1;     // Smallest particle size class starts at 0.1 microns
        const double maxSize = 1000.0;  // Largest particle size class ends at 1000 microns
        double cn = 1.0/Math.Sqrt(2.0 * Math.PI);

        double logMinSize = Math.Log10(minSize);    // Log (base 10) of the diameter of the smallest size class
        double logMaxSize = Math.Log10(maxSize);    // Log (base 10) of the diamater of the largest size class 
        double deltaLog = (logMaxSize - logMinSize) / nSizes;  // Change in log (base 10) as we go from one class to the next
        double deltaLn = deltaLog * Math.Log(10.0);  // Change in log (base e) as we go from one class to the next

        for (int i = 0; i < nSizes; i++)  // Now calculate the values for each size class
        {
            diamMid[i] = Math.Pow(10.0, logMinSize + (i + 0.5) * deltaLog);   // Get the diameter of the mid-point of the class
            double probDensity = 0.0;
            // We combine probability densities from 4 distributions
            for (int j = 0; j < dist.Length; j++)
            {
                double weight = dist[j].weight;
                double diamMean = dist[j].mean;
                double diamSD = dist[j].sigma;
                double partialProbDensity = 0;
                if (weight > 0.0 && diamSD > 0.0)
                    partialProbDensity = weight * cn / diamSD * Math.Exp(-(MathUtility.Sqr(Math.Log(diamMid[i]) - diamMean)) / (2.0 * MathUtility.Sqr(diamSD)));
                probDensity += partialProbDensity;
            }
            deltaPSD[i] = probDensity * deltaLn;
            if (i == 0)
                cumProb[i] = deltaPSD[i];
            else
                cumProb[i] = cumProb[i-1] + deltaPSD[i];

            // Why divide by the diameter? Is it because we are interested in the surface probs, not bulk probs???
            // It doesn't appear that this is really needed anyway, as far as I can tell....
            PSD[i] = probDensity / diamMid[i];  

        }

        // double sumPSD = 0.0;
        // Renormalisation
        for (int i = 0; i < nSizes; i++)
        {
            deltaPSD[i] /= cumProb[nSizes - 1];
            PSD[i] /= cumProb[nSizes - 1]; // Is this the right normalization to use here? Since we don't use this, it doesn't really matter...
            cumProb[i] /= cumProb[nSizes - 1];
        }
    }

    /// <summary>
    /// Generate a particle size distribution based on the algorithm of Skaggs et al. 2001.
    /// </summary>
    /// <param name="diamMid"></param>
    /// <param name="PSD"></param>
    /// <param name="deltaPSD"></param>
    /// <param name="cumProb"></param>
    /// <param name="nSizes"></param>
    protected void CreateSkaggsSizeDist(out double[] diamMid, out double[] PSD, out double[] deltaPSD, out double[] cumProb,
                   int nSizes)
    {
        diamMid = new double[nSizes];
        PSD = new double[nSizes];
        deltaPSD = new double[nSizes];
        cumProb = new double[nSizes];
        horizFlux = new double[nSizes];
        //        psds = new double[nSizes];
        //        dpsds = new double[nSizes];
        //        ppsds = new double[nSizes];

        // See Skaggs et al., page 1039
        double alpha = 1.0 / Math.Log((max_silt_radius - max_clay_radius) / (max_fine_sand_radius - max_clay_radius));
        double beta = alpha * Math.Log((max_silt_radius - max_clay_radius) / max_clay_radius);
        double v = Math.Log((1.0 / (fraction_clay + fraction_silt) - 1.0) / (1.0 / fraction_clay - 1.0));
        double w = Math.Log((1.0 / Math.Min(0.9999, fraction_clay + fraction_silt + fraction_fine_sand) - 1.0) / (1.0 / fraction_clay - 1.0));

        double c = alpha * Math.Log(v / w);
        double u = Math.Pow(-v, 1.0 - beta) * Math.Pow(-w, beta);

        // Estimate diameters using log10 scale
        // const double minSize = 0.1;     // Smallest particle size class starts at 0.1 microns
        double minSize = max_clay_radius * 2.0;     // Smallest particle size class starts at 0.1 microns
        const double maxSize = 1000.0;  // Largest particle size class ends at 1000 microns

        double logMinSize = Math.Log10(minSize);    // Log (base 10) of the diameter of the smallest size class
        double logMaxSize = Math.Log10(maxSize);    // Log (base 10) of the diamater of the largest size class 
        double deltaLog = (logMaxSize - logMinSize) / nSizes;  // Change in log (base 10) as we go from one class to the next
        double deltaLn = deltaLog * Math.Log(10.0);  // Change in log (base e) as we go from one class to the next

        for (int i = 0; i < nSizes; i++)  // Now calculate the values for each size class
        {
            diamMid[i] = Math.Pow(10.0, logMinSize + (i + 0.5) * deltaLog);   // Get the diameter of the mid-point of the class
            double R = (diamMid[i] / 2.0 - max_clay_radius) / max_clay_radius;
            cumProb[i] = 1.0 / (1.0 + (1.0 / fraction_clay - 1.0) * Math.Exp(-u * Math.Pow(R, c)));

            if (i > 0)
                deltaPSD[i] = cumProb[i] - cumProb[i - 1];
            else
                deltaPSD[i] = cumProb[i];

            PSD[i] = deltaPSD[i] / deltaLn / diamMid[i];
        }

        // Renormalisation
        for (int i = 0; i < nSizes; i++)
        {
            deltaPSD[i] /= cumProb[nSizes - 1];
            PSD[i] /= cumProb[nSizes - 1]; // Is this the right normalization to use here? Since we don't use this, it doesn't really matter...
            cumProb[i] /= cumProb[nSizes - 1];
        }
    }
}