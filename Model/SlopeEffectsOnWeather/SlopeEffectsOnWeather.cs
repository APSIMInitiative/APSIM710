using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// This model can be used to adjust some of the weather variables when simulating a slopping surface (defined by slope and aspect angles)
/// </summary>
/// <remarks>
/// - This include routines to modify the incoming solar radiation as well as minimum and maximum air temperatures;
/// - Adjusts for rainfall, RHmin and RHmax, vp, and windspeed, can be also done, but these are simply relative changes supplied by the user, not calculated. 
/// - Calculations happens on PreNewMet event and take also into account the latitude of the site (read from the MetFile).
/// - Altitude is also needed, it is a parameter for now (so are slope and aspect). These need to be read from 'Paddock' eventually.
/// + References:
///     Allen, R.G.; Pereira, L.S.; Raes, D.; & Smith, M., 1998. Crop evapotranspiration: guidelines for computing crop water requirements. Irrigation and Drainage Paper No. 56, FAO, Rome, Italy. 300 p.
///     Allen, R.G.; Trezza, R.; & Tasumi, M. 2006. Analytical integrated functions for daily solar radiation on slopes. Agricultural and Forest Meteorology, 139(1–2):55-73.
///     Almorox, J. & Hontoria, C. 2004. Global solar radiation estimation using sunshine duration in Spain. Energy Conversion and Management, 45(9-10):1529-1535.
///     Boland, J.; Scott, L.; & Luther, M. 2001. Modelling the diffuse fraction of global solar radiation on a horizontal surface. Environmetrics, 12(2):103-116.
///     Dervishi, S. & Mahdavi, A. 2012. Computing diffuse fraction of global horizontal solar radiation: A model comparison. Solar Energy, 86(6):1796-1802.
///     Iqbal, M. 2012. An introduction to solar radiation: Elsevier Science. 408 p.
/// </remarks>
public class SlopeEffectsOnWeather
{
    #region Links and Parameters

    /// <summary>
    /// Link to APSIM's clock
    /// </summary>
    [Link]
    public Clock Clock = null;

    /// <summary>
    /// Link to APSIM's met file
    /// </summary>
    [Link]
    public MetFile MyMetFile = null;

    /// <summary>
    /// Angle of the slope, from horizontal (degrees)
    /// </summary>
    [Param]
    [Units("degrees")]
    private double SlopeAngle;

    /// <summary>
    /// Angle of the aspect, from north (degrees)
    /// </summary>
    [Param]
    [Units("degrees")]
    private double AspectAngle;

    /// <summary>
    /// Local altitude (meters above sea level)
    /// </summary>
    [Param]
    [Units("m")]
    private double Altitude = 50;

    /// <summary>
    /// Albedo of the surrounding environment (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    private double SurroundsAlbedo = 0.2;

    /// <summary>
    /// Parameter A for diffuse radiation fraction
    /// </summary>
    [Param]
    private double A_diffuseRadn = -5.0;

    /// <summary>
    /// Parameter B for diffuse radiation fraction
    /// </summary>
    [Param]
    private double B_diffuseRadn = 8.0;

    /// <summary>
    /// Mean air turbidity for direct radiation (0-1)
    /// </summary>
    /// <remarks>
    /// The value should be, in practice, between 0.5 (for very dusty/polluted places) and 1.0 (for areas with natural vegetation)
    /// Following Allen et al (2006)
    /// </remarks>
    [Param]
    private double TurbidityCoefficient = 0.95;

    /// <summary>
    /// Parameter a of the function for transimissivity index for direct radn
    /// </summary>
    [Param]
    private double a_ki = 0.98;

    /// <summary>
    /// Parameter b of the function for transimissivity index for direct radn
    /// </summary>
    [Param]
    private double b_ki = 0.00146;

    /// <summary>
    /// parameter c of the function for transimissivity index for direct radn
    /// </summary>
    [Param]
    private double c_ki = 0.075;

    /// <summary>
    /// Parameter d of the function for transimissivity index for direct radn
    /// </summary>
    [Param]
    private double d_ki = 0.40;

    /// <summary>
    /// Parameter a of the function for precipitable water (mm)
    /// </summary>
    [Param]
    private double a_pw = 2.10;

    /// <summary>
    /// Parameter b for the function for precipitable water (mm/kPa^2)
    /// </summary>
    [Param]
    private double b_pw = 0.14;
    
    /// <summary>
    /// Parameter aT0 of dltTemp × dltRadn function, max rate of change (oC per MJ/m2/day)
    /// </summary>
    [Param]
    private double aT0 = 0.0;

    /// <summary>
    /// Parameter bT of dltTemp × dltRadn function, non linear coefficient (exponent)
    /// </summary>
    [Param]
    private double bT = 0.0;

    /// <summary>
    /// Parameter cT of dltTemp × dltRadn function, accounts for wind effects (0-1)
    /// </summary>
    [Param]
    private double cT = 0.0;

    /// <summary>
    /// Parameter FN of dltTemp × dltRadn function, used when dltRadn is negative (0-1)
    /// </summary>
    [Param]
    private double FN = 0.0;

    /// <summary>
    /// Parameter FM of dltTemp × dltRadn function, adjust for Tmin (0-1)
    /// </summary>
    [Param]
    private double FM = 0.0;

    /// <summary>
    /// Relative change in rainfall
    /// </summary>
    [Param]
    [Units("%")]
    private double dRain = 0.0;

    /// <summary>
    /// Relative change in wind
    /// </summary>
    [Param]
    [Units("%")]
    private double dWind = 0.0;

    /// <summary>
    /// Relative change in vapour pressure
    /// </summary>
    [Param]
    [Units("%")]
    private double dVapPressure = 0.0;

    /// <summary>
    /// Default value for wind speed, to be used if no value are supplied by APSIM
    /// </summary>
    [Param]
    [Units("m/s")]
    private double defaultWindSpeed = 0.0;

    #region Internal parameters and constants

    /// <summary>
    /// Mean value of solar constant (w/m2)
    /// </summary>
    private double SolarConstant = 1367.0;

    /// <summary>
    /// Mean atmospheric pressure at sea level (hPa)
    /// </summary>
    private double StandardAtmosphericPressure = 101.325;

    /// <summary>
    /// Air temperature at standard conditions (Kelvin)
    /// </summary>
    private double StandardAtmosphericTemperature = 288.15;

    /// <summary>
    /// Mean acceleration of gravity at sea level (m/s^2)
    /// </summary>
    private double StandardGravitationalAcceleration = 9.80665;

    /// <summary>
    /// Standard environmental temperature lapse rate in dry air (K/m)
    /// </summary>
    private double StandardTemperatureLapseRate = 0.00649;

    /// <summary>
    /// Mean molar mass of Earth's dry air (kg/mol)
    /// </summary>
    private double StandardAtmosphericMolarMass = 0.0289644;

    /// <summary>
    /// Universal gas constant for air (J/mol/K)
    /// </summary>
    private double UniversalGasConstant = 8.31432;

    /// <summary>
    /// A threshold to evaluate significant values
    /// </summary>
    private double epsilon = 1e-10;

    #endregion

    #endregion

    #region Outputs

    /// <summary>
    /// Original solar radiation input (MJ/m2)
    /// </summary>
    [Output]
    [Description("Original radiation value")]
    [Units("MJ/m2")]
    public double RadnMeasured;

    /// <summary>
    /// Direct solar radiation (MJ/m2)
    /// </summary>
    [Output]
    [Description("Direct solar radiation")]
    [Units("MJ/m2")]
    public double RadnDirect;

    /// <summary>
    /// Diffuse solar radiation (MJ/m2)
    /// </summary>
    [Output]
    [Description("Diffuse solar radiation")]
    [Units("MJ/m2")]
    public double RadnDiffuse;

    /// <summary>
    /// Reflected solar radiation  (MJ/m2)
    /// </summary>
    [Output]
    [Description("Reflected solar radiation (from terrain)")]
    [Units("MJ/m2")]
    public double RadnReflected;

    /// <summary>
    /// Extraterrestrial solar radiation (MJ/m2)
    /// </summary>
    [Output]
    [Description("Extraterrestrial solar radiation")]
    [Units("MJ/m2")]
    public double ExtraterrestrialRadn;

    /// <summary>
    /// Time length of direct sunlight on a horizontal surface (hrs)
    /// </summary>
    [Output]
    [Description("Length of direct sunlight on a horizontal surface")]
    [Units("hours")]
    public double MaxDirSunlightLength;

    /// <summary>
    /// Time length of direct sunlight on tilted surface (hrs)
    /// </summary>
    [Output]
    [Description("Length of direct sunlight on tilted surface")]
    [Units("hours")]
    public double ActualDirSunlightLength;

    /// <summary>
    /// Sky clearness index (0-1)
    /// </summary>
    /// <remarks>
    /// Provide an idea of how overcast the day is
    /// </remarks>
    [Output]
    [Description("Sky clearness index")]
    [Units("0-1")]
    public double ClearnessIndex;

    /// <summary>
    /// Fraction of total radiation that is diffuse (0-1)
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation that is diffuse (flat)")]
    [Units("0-1")]
    public double DiffuseRadnFraction;

    /// <summary>
    /// Ratio of direct radiation between slope and horizontal surfaces
    /// </summary>
    [Output]
    [Description("Ratio of direct solar radiation (slope-to-flat)")]
    [Units("-")]
    public double DirRadnRatio;

    /// <summary>
    /// Ratio of diffuse radiation between slope and horizontal surfaces
    /// </summary>
    [Output]
    [Description("Ratio of diffuse solar radiation (slope-to-flat)")]
    [Units("-")]
    public double DiffRadnRatio;

    /// <summary>
    /// Fraction of solar radiation that is direct beam (0-1)
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation direct")]
    [Units("0-1")]
    public double FracRadnDirect;

    /// <summary>
    /// Fraction of solar radiation that is diffuse (0-1)
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation diffuse")]
    [Units("0-1")]
    public double FracRadnDiffuse;

    /// <summary>
    /// Fraction of solar radiation reflected from terrain (0-1)
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation reflected from terrain")]
    [Units("0-1")]
    public double FracRadnReflected;

    /// <summary>
    ///  Original value of minimum temperature (oC)
    /// </summary>
    [Output]
    [Description("Original value of Tmin")]
    [Units("oC")]
    public double TminMeasured;

    /// <summary>
    ///  Original value of maximum  temperature (oC)
    /// </summary>
    [Output]
    [Description("Original value of Tmax")]
    [Units("oC")]
    public double TmaxMeasured;

    /// <summary>
    ///  Actual Tmean value, after adjusts (oC)
    /// </summary>
    [Output]
    [Description("Actual Tmean value, after adjusts")]
    [Units("oC")]
    public double TmeanActual
    {
        get { return 0.5 * (MyMetFile.MaxT + MyMetFile.MinT); }
    }

    /// <summary>
    ///  Variation in Tmin (oC)
    /// </summary>
    [Output]
    [Description("Variation in Tmin")]
    [Units("oC")]
    public double dltTmin;

    /// <summary>
    ///  Variation in Tmax (oC)
    /// </summary>
    [Output]
    [Description("Variation in Tmax")]
    [Units("oC")]
    public double dltTmax;

    /// <summary>
    /// Mean local atmospheric pressure (hPa)
    /// </summary>
    [Output]
    [Description("Mean local atmospheric pressure")]
    [Units("hPa")]
    public double AtmosphericPressure = 0.0;

    /// <summary>
    /// Original rain input (mm)
    /// </summary>
    [Output]
    [Description("Original rain value")]
    [Units("mm")]
    public double RainMeasured = -0.1;

    /// <summary>
    /// Original wind speed input (m/s)
    /// </summary>
    [Output]
    [Description("Original wind speed value")]
    [Units("m/s")]
    public double WindSpeedMeasured = -0.1;

    /// <summary>
    /// Original vapour pressure input (hPa)
    /// </summary>
    [Output]
    [Description("Original vapour pressure value")]
    [Units("hPa")]
    public double VPMeasured = -0.1;

    /// <summary>
    /// Original RH input (%)
    /// </summary>
    [Output]
    [Description("Original RH value")]
    [Units("%")]
    public double RHMeasured = -0.1;

    /// <summary>
    /// Original (or default) RHmean input (%)
    /// </summary>
    [Output]
    [Description("Original RHmean value (or default)")]
    [Units("%")]
    public double RHmeanMeasured = -0.1;

    /// <summary>
    /// Original RHmin input (%)
    /// </summary>
    [Output]
    [Description("Original RHmin value")]
    [Units("%")]
    public double RHminMeasured = -0.1;

    /// <summary>
    /// Original RHmax input (%)
    /// </summary>
    [Output]
    [Description("Original RHmax value")]
    [Units("%")]
    public double RHmaxMeasured = -0.1;

    #endregion

    #region Internal variables

    /// <summary>
    /// Latitude converted to radians
    /// </summary>
    private double LatitudeAngle;

    /// <summary>
    /// Slope factor for diffuse and reflected radiation (also called sky view)
    /// </summary>
    private double SlopeFactor;

    /// <summary>
    /// Mean daily solar radiation after correction due to slope and aspect
    /// </summary>
    private double myRadn = 0.0;

    /// <summary>
    /// Daily RH after correction imposed by the user (typicially this from is 9:00 reading)
    /// </summary>
    private double myRH = 80.0;

    /// <summary>
    /// Minimum daily RH after correction imposed by the user
    /// </summary>
    private double myRHmin = 0.0;

    /// <summary>
    /// Maximum daily RH after correction imposed by the user
    /// </summary>
    private double myRHmax = 1.0;

    /// <summary>
    /// Mean daily RH after correction imposed by the user
    /// </summary>
    private double myRHmean = 80.0;

    /// <summary>
    /// Flag whether RH values can change (after user specification) or not
    /// </summary>
    /// <remarks>
    /// RH values are not mandatory in APSIM met files, so if there are no values in the file, no changes can happen.
    /// </remarks>
    private bool canChangeRH = false;

    /// <summary>
    /// Flag whether RH values has changed or not
    /// </summary>
    private bool hasChangedRH = false;

    /// <summary>
    /// Flag whether there are values for RH min and max and they can be changed
    /// </summary>
    private bool canChangeRHminmax = false;

    /// <summary>
    /// Mean daily vapour pressure after correction imposed by the user
    /// </summary>
    private double myVP = 20.0;

    /// <summary>
    /// Mean daily wind speed after correction imposed by the user
    /// </summary>
    private double myWindSpeed = 0.0;

    /// <summary>
    /// Hour angle for sunrise/sunset on a horizontal surface
    /// </summary>
    private double SunriseAngleHorizontal = 0.0;

    /// <summary>
    /// Hour angle for first sunrise on tilted surface
    /// </summary>
    private double SunriseAngle1Slope = 0.0;

    /// <summary>
    /// Hour angle for first sunset on tilted surface
    /// </summary>
    private double SunsetAngle1Slope = 0.0;

    /// <summary>
    /// Hour angle for second sunrise on tilted surface
    /// </summary>
    private double SunriseAngle2Slope = 0.0;

    /// <summary>
    /// Hour angle for second sunset on tilted surface
    /// </summary>
    private double SunsetAngle2Slope = 0.0;

    /// <summary>
    /// Flag whether initialisation procedure has been finished
    /// </summary>
    private bool hasInitialised = false;

    #endregion

    #region Calculations

    [EventHandler]
    public void OnInitialised()
    {
        // Check parameter values
        if ((SlopeAngle < 0.0) || (SlopeAngle > 90))
            throw new Exception("Slope angle is out of the expected range (0-90deg)");
        if ((AspectAngle < 0.0) || (AspectAngle > 360))
            throw new Exception("Aspect angle is out of the expected range (0-600deg)");
        if ((Altitude < -100.0) || (SurroundsAlbedo > 5000))
            throw new Exception("Altitude value is out of bounds (0-1)");
        if ((SurroundsAlbedo < 0.0) || (SurroundsAlbedo > 1.0))
            throw new Exception("Albedo value is out of bounds (0-1)");
        if ((TurbidityCoefficient < 0.0) || (TurbidityCoefficient > 1.0))
            throw new Exception("Turbidity coefficient value is out of bounds (0-1)");

        // Convert and fix some parameters
        if (AspectAngle > 180)
            AspectAngle -= 180;
        SlopeAngle = Math.PI * SlopeAngle / 180;
        AspectAngle = Math.PI * AspectAngle / 180;
        LatitudeAngle = Math.PI * MyMetFile.Latitude / 180;

        // Get the local mean atmospheric pressure, similar to Allen et al (1998)
        double expPressure = StandardGravitationalAcceleration * StandardAtmosphericMolarMass / (UniversalGasConstant * StandardTemperatureLapseRate);
        AtmosphericPressure = Math.Pow(1 - (StandardTemperatureLapseRate * Altitude / StandardAtmosphericTemperature), expPressure);
        AtmosphericPressure *= StandardAtmosphericPressure;

        // Get the slope factor for correcting reflected radiation, based on Allen et al. (2006)
        SlopeFactor = 0.75 + (0.25 * Math.Cos(SlopeAngle)) - (0.5 * SlopeAngle / Math.PI);

        // Covert the user defined changes from percent into fraction
        dRain = Math.Max(-1.0, 0.01 * dRain);
        dWind = Math.Max(-1.0, 0.01 * dWind);
        dVapPressure = Math.Max(-1.0, 0.01 * dVapPressure);
        /// NOTE: dVapPressure has a variable upper bound, it cannot be higher than the saturated VP
        ///       this upper limit will be assumed equal to 95% of saturation at daily Tmax

        // Finish initialisation
        hasInitialised = true;
        /// NOTE: ideally the initialisation would happen at OnInit1 (which is triggered before OnPreNewMet), so we could start modifying
        /// the weather data on the first day. However, there is a flaw in the way APSIM handles its initialisation. The PreNewMet event
        /// happens before the [Link] to the met file is resolved, so it is not possible to set any of the variables for the first day of
        /// simulation. And that's why we have to use the 'hasInitialised' variable here

        Console.WriteLine();
        Console.WriteLine("     Weather variables will be adjusted for slope and aspect");
        Console.WriteLine("      - Radiation and temperature adjusted based on the model described by Cichota (2015)");
        Console.WriteLine("      - Rainfall, wind, and vapour pressure are simple relative changes - not explicitly linked to slope");
        Console.WriteLine("      - The values of RH, if existent, will be adjusted whenever the temperature or vp change");
        Console.WriteLine();
    }

    /// <summary>
    /// Evaluate whether weather data is to be adjusted due to slope and aspect
    /// </summary>
    /// <param name="NewMetData">some weather data (not used here)</param>
    [EventHandler]
    public void OnPreNewMet(NewMetType NewMetData)
    {
        // Get the basic met values, evaluate their changes and re-set them on MetFile
        if (hasInitialised)
        {
            // Get and adjust windspeed
            if (MyMetFile.Get("wind", out WindSpeedMeasured))
            {
                myWindSpeed = WindSpeedMeasured;
                if (Math.Abs(dWind) > epsilon)
                    myWindSpeed *= 1.0 + dWind;
            }
            else
            {
                WindSpeedMeasured = -1.0;
                myWindSpeed = defaultWindSpeed;
            }


            // Evaluate changes in VP and RH
            VPMeasured = MyMetFile.vp;
            HumidityChanges();

            // Evaluate the changes in radiation due to slope and aspect
            RadnMeasured = MyMetFile.Radn;
            RadiationOnSlope();

            // Evaluate the changes in temperature
            TmaxMeasured = MyMetFile.MaxT;
            TminMeasured = MyMetFile.MinT;
            DeltaTemperature();

            // Check whether RH needs further adjusting
            if (canChangeRH)
                CheckRH();

            // Set the adjusted weather variables
            RainMeasured = MyMetFile.Rain;
            if (Math.Abs(dRain) > epsilon)
                MyMetFile.Rain *= (float)(1.0 + dRain);

            if (Math.Abs(dltTmax) > epsilon)
                MyMetFile.MaxT += (float)dltTmax;
            if (Math.Abs(dltTmin) > epsilon)
            {
                if (MyMetFile.MinT + dltTmin > MyMetFile.MaxT)
                    MyMetFile.MinT = MyMetFile.MaxT;
                else
                    MyMetFile.MinT += (float)dltTmin;
            }

            if (Math.Abs(MyMetFile.Radn - myRadn) > epsilon)
                MyMetFile.Radn = (float)myRadn;

            if ((WindSpeedMeasured > 0.0) && (Math.Abs(dWind) > epsilon))
                MyMetFile.Set("wind", (float)myWindSpeed);

            if (Math.Abs(dVapPressure) > epsilon)
                MyMetFile.vp = (float)myVP;

            if (canChangeRH && hasChangedRH)
            {
                if (RHmeanMeasured > 0.0)
                    MyMetFile.Set("rhmean", (float)myRHmean);

                if (RHMeasured > 0.0)
                    MyMetFile.Set("rh", (float)myRH);

                if (canChangeRHminmax)
                {
                    MyMetFile.Set("rhmin", (float)myRHmin);
                    MyMetFile.Set("rhmax", (float)myRHmax);
                }
            }
        }
    }

    /// <summary>
    /// Gather the values of vp and RH, check them, and adjust if possible
    /// </summary>
    /// <remarks>
    /// The changes here are imposed by the user (when setting a value for dVapPressure, which is used for both vp and RH)
    /// If vp is not supplied a value is calculated based on Tmean and RHmean
    /// The changes in humidity are done based on RHmean. RHmin and RHmax, if existent, are changed so that their average equals RHmean
    /// The value of RH is assumed to be the measurement at 9:00 o'clock (often reported by weather stations)
    /// The value of RHmean, if existent, is assumed to be the true mean. 
    /// If only RH is available, it is assumed to represent RHmean
    /// If RH and and the max and min values are given, their average is taken as RHmean
    /// If no values is given for RH a default values is used
    ///   - TODO: these assumptions may have to be discussed/examined further
    /// </remarks>
    private void HumidityChanges()
    {
        canChangeRHminmax = false;

        // get the value of RHmean
        if (MyMetFile.Get("rhmean", out RHmeanMeasured))
        { // there is a value representing the mean RH of the day
            canChangeRH = true;
            myRHmean = RHmeanMeasured;
            myRH = RHmeanMeasured;
            if (MyMetFile.Get("rhmin", out RHminMeasured) && MyMetFile.Get("rhmax", out RHmaxMeasured))
            { // we have values for both RHmin and max
                myRHmin = RHminMeasured;
                myRHmax = RHmaxMeasured;
                canChangeRHminmax = true;
            }
            else
            {
                myRHmin = RHmeanMeasured;
                myRHmax = RHmeanMeasured;
            }
        }
        else if (MyMetFile.Get("rh", out RHMeasured))
        { // there is a value representing the daily RH (typically measured at 9 o'clock)
            canChangeRH = true;
            myRH = RHMeasured;
            if (MyMetFile.Get("rhmin", out RHminMeasured) && MyMetFile.Get("rhmax", out RHmaxMeasured))
            { // we have values for both RHmin and max, update mean
                myRHmin = RHminMeasured;
                myRHmax = RHmaxMeasured;
                myRHmean = Math.Min(100.0, Math.Max(0.0, (0.5 * myRH) + (0.25 * (myRHmin + myRHmax))));
                canChangeRHminmax = true;
            }
            else
            {
                myRHmin = RHMeasured;
                myRHmax = RHMeasured;
            }
        }
        else if (MyMetFile.Get("rhmin", out RHminMeasured) && MyMetFile.Get("rhmax", out RHmaxMeasured))
        { // we have values for both RHmin and max only
            myRHmin = RHminMeasured;
            myRHmax = RHmaxMeasured;
            myRHmean = 0.5 * (myRHmin + myRHmax);
            canChangeRH = true;
            canChangeRHminmax = true;
        }
        else
        { // there are no values for RH, no changes allowed then
            canChangeRH = false;
        }

        // make sure that we have values (replace NaN with a negative value)
        if (Double.IsNaN(RHmeanMeasured))
            RHmeanMeasured = -1.0;
        if (Double.IsNaN(RHMeasured))
            RHMeasured = -1.0;
        if (Double.IsNaN(RHminMeasured))
            RHminMeasured = -1.0;
        if (Double.IsNaN(RHmaxMeasured))
            RHmaxMeasured = -1.0;

        // Adjust the values of VP and RH
        myVP = VPMeasured;
        if (Math.Abs(dVapPressure) > epsilon)
        {
            // vp cannot go above saturation (assumed to be 95% of that at MaxT)
            myVP = Math.Min(myVP * (1.0 + dVapPressure),
                   0.95 * 6.1078 * Math.Exp(17.269 * MyMetFile.MaxT / (237.3 + MyMetFile.MaxT)));

            double dRH = myVP / VPMeasured;  // RH varies in sinc with vp
            if (canChangeRH)
            {
                double vRHmin = myRH - myRHmin;
                double vRHmax = myRHmax - myRH;
                myRH *= dRH;
                myRHmean *= dRH;
                myRH = Math.Min(100.0, Math.Max(0.0, myRH));
                myRHmean = Math.Min(100.0, Math.Max(0.0, myRHmean));

                // correct RH min and max, keep within bounds (0-100) and at the same distance from RH (or RHmean)
                if (canChangeRHminmax && (myRHmax > epsilon))
                {
                    double dVRH = Math.Min(MathUtility.Divide(Math.Min(vRHmin, myRH), vRHmin, 1.0),
                                           MathUtility.Divide(Math.Min(vRHmax, 100 - myRH), vRHmax, 1.0));
                    myRHmax = Math.Min(100.0, Math.Max(0.0, myRH + dVRH));
                    myRHmin = Math.Min(myRHmax, Math.Max(0.0, myRH - dVRH));
                }
                else
                {
                    myRHmin = myRH;
                    myRHmax = myRH;
                }

                hasChangedRH = true;
            }
        }
    }

    /// <summary>
    /// Check the values of RH after the temperature was changed
    /// </summary>
    /// <remarks>
    /// It is assumed here that vp remains constant (any changes in vp were accounted for earlier),
    /// so the value of RH has to change here if temperature has changed (and there was originally a RH value)
    /// </remarks>
    private void CheckRH()
    {
        double TmeanMeasured = 0.5 * (TminMeasured + TmaxMeasured);
        double TmeanNew = 0.5 * (TminMeasured + dltTmin + TmaxMeasured + dltTmax);
        double dRH = Math.Exp(17.269 * TmeanMeasured / (TmeanMeasured + 237.3))
                   / Math.Exp(17.269 * TmeanNew / (TmeanNew + 237.3));

        if (Math.Abs(1.0 - dRH) > epsilon)
        { // temperature varied, update RH
            double vRHmin = myRH - myRHmin;
            double vRHmax = myRHmax - myRH;
            myRH *= dRH;
            myRHmean *= dRH;
            myRH = Math.Min(100.0, Math.Max(0.0, myRH));
            myRHmean = Math.Min(100.0, Math.Max(0.0, myRHmean));

            // correct RH min and max, keep within bounds (0-100) and at the same distance from RH (or RHmean)
            if (canChangeRHminmax && (myRHmax > epsilon))
            {
                double dVRH = Math.Min(MathUtility.Divide(Math.Min(vRHmin, myRH), vRHmin, 1.0),
                                       MathUtility.Divide(Math.Min(vRHmax, 100 - myRH), vRHmax, 1.0));
                myRHmax = Math.Min(100.0, Math.Max(0.0, myRH + dVRH));
                myRHmin = Math.Min(myRHmax, Math.Max(0.0, myRH - dVRH));
            }
            else
            {
                myRHmin = myRH;
                myRHmax = myRH;
            }

            hasChangedRH = true;
        }
    }

    /// <summary>
    /// Computes the solar radiation received by a tilted surface, based on measured values on horizontal
    /// </summary>
    /// <remarks>
    /// Uses the methodology described by Cichota (2015), adapted from Allen et al. (2006) and Iqbal (2015)
    /// </remarks>
    private void RadiationOnSlope()
    {
        // Extraterrestrial radiation - following classic formulae (Almorox and Hontoria, 2004; Iqbal, 2015)
        double DayAngle = 2 * Math.PI * Math.Min(0.9995, (Clock.day_of_year - 0.5) / 365.25);
        double SolarDeclination = 0.006918 - (0.399912 * Math.Cos(DayAngle))
                                + (0.070257 * Math.Sin(DayAngle))
                                - (0.006758 * Math.Cos(2 * DayAngle))
                                + (0.000907 * Math.Sin(2 * DayAngle))
                                - (0.002697 * Math.Cos(3 * DayAngle))
                                + (0.001480 * Math.Sin(3 * DayAngle));
        double EarthEccentricity = 1.00011 + (0.034221 * Math.Cos(DayAngle))
                                 + (0.00128 * Math.Sin(DayAngle))
                                 + (0.000719 * Math.Cos(2 * DayAngle))
                                 + (0.000077 * Math.Sin(2 * DayAngle));
        SunriseAngleHorizontal = Math.Acos(Math.Max(-1, Math.Min(1, -Math.Tan(LatitudeAngle) * Math.Tan(SolarDeclination))));
        double RelativeSolarIrradiance = ((Math.Cos(LatitudeAngle) * Math.Cos(SolarDeclination) * Math.Sin(SunriseAngleHorizontal))
                                        + (Math.Sin(LatitudeAngle) * Math.Sin(SolarDeclination) * SunriseAngleHorizontal)) / Math.PI;
        ExtraterrestrialRadn = RelativeSolarIrradiance * EarthEccentricity * SolarConstant * 24 * 3600 / 1000000;

        // Sky clearness index - following typical approach (Boland et al., 2008; Dervishi and Mahdavi, 2012)
        ClearnessIndex = Math.Min(1.0, Math.Max(0.0, MyMetFile.Radn / ExtraterrestrialRadn));

        // Diffuse radiation fraction - same approach as Boland et al. (2008), equivalent to Allen et al. (2006) 
        DiffuseRadnFraction = 1.0 / (1.0 + Math.Exp(A_diffuseRadn + (B_diffuseRadn * ClearnessIndex)));

        if (SlopeAngle > epsilon)
        {
            // Auxiliary variables for radiation (Allen et al., 2006)
            double a_ = Math.Sin(SolarDeclination) * ((Math.Sin(LatitudeAngle) * Math.Cos(SlopeAngle))
                      - (Math.Cos(LatitudeAngle) * Math.Sin(SlopeAngle) * Math.Cos(AspectAngle)));
            double b_ = Math.Cos(SolarDeclination) * ((Math.Cos(LatitudeAngle) * Math.Cos(SlopeAngle))
                      + (Math.Sin(LatitudeAngle) * Math.Sin(SlopeAngle) * Math.Cos(AspectAngle)));
            double c_ = Math.Cos(SolarDeclination) * (Math.Sin(SlopeAngle) * Math.Sin(AspectAngle));
            double g_ = Math.Sin(SolarDeclination) * Math.Sin(LatitudeAngle);
            double h_ = Math.Cos(SolarDeclination) * Math.Cos(LatitudeAngle);

            // Hour angles for the sunrise/sunset on slope (Cichota, 2015)
            SunriseSunsetOnSlope(a_, b_, c_);

            // Length of daylight, horizontal (max) and slope (actual)
            MaxDirSunlightLength = 24 * SunriseAngleHorizontal / Math.PI;
            ActualDirSunlightLength = 12 * (Math.Max(0.0, SunsetAngle1Slope - SunriseAngle1Slope)
                            + Math.Max(0.0, SunsetAngle2Slope - SunriseAngle2Slope))
                            / Math.PI;

            // Extraterrestrial radiation on slope (Allen et al., 2006)
            double RelativeIrradianceOnSlope = ((a_ * (SunsetAngle1Slope - SunriseAngle1Slope))
                                             + (b_ * (Math.Sin(SunsetAngle1Slope) - Math.Sin(SunriseAngle1Slope)))
                                             - (c_ * (Math.Cos(SunsetAngle1Slope) - Math.Cos(SunriseAngle1Slope)))
                                             + (a_ * (SunsetAngle2Slope - SunriseAngle2Slope))
                                             + (b_ * (Math.Sin(SunsetAngle2Slope) - Math.Sin(SunriseAngle2Slope)))
                                             - (c_ * (Math.Cos(SunsetAngle2Slope) - Math.Cos(SunriseAngle2Slope))))
                                             / (2 * Math.PI);

            // Mean path for direct beam radiation through the atmosphere - for horizontal and slope (Allen et al., 2006)
            double MeanPathHorz = ((2 * SunriseAngleHorizontal * Math.Pow(g_, 2)) + (4 * g_ * h_ * Math.Sin(SunriseAngleHorizontal))
                               + ((SunriseAngleHorizontal + (0.5 * Math.Sin(2 * SunriseAngleHorizontal))) * Math.Pow(h_, 2)))
                               / (2 * ((g_ * SunriseAngleHorizontal) + (h_ * Math.Sin(SunriseAngleHorizontal))));
            double MeanPathSlope = 0.0;
            if (ActualDirSunlightLength > epsilon)
            {
                double aNominator = (((b_ * g_) + (a_ * h_))
                       * (Math.Sin(SunsetAngle1Slope) - Math.Sin(SunriseAngle1Slope) + Math.Sin(SunsetAngle2Slope) - Math.Sin(SunriseAngle2Slope)))
                       - ((c_ * g_)
                       * (Math.Cos(SunsetAngle1Slope) - Math.Cos(SunriseAngle1Slope) + Math.Cos(SunsetAngle2Slope) - Math.Cos(SunriseAngle2Slope)))
                       + (((0.5 * b_ * h_) + (a_ * g_))
                       * (SunsetAngle1Slope - SunriseAngle1Slope + SunsetAngle2Slope - SunriseAngle2Slope))
                       + ((0.25 * b_ * h_)
                       * (Math.Sin(2 * SunsetAngle1Slope) - Math.Sin(2 * SunriseAngle1Slope) + Math.Sin(2 * SunsetAngle2Slope) - Math.Sin(2 * SunriseAngle2Slope)))
                       + ((0.5 * c_ * h_)
                       * (Math.Pow(Math.Sin(SunsetAngle1Slope), 2.0) - Math.Pow(Math.Sin(SunriseAngle1Slope), 2.0)
                       + Math.Pow(Math.Sin(SunsetAngle2Slope), 2.0) - Math.Pow(Math.Sin(SunriseAngle2Slope), 2.0)));
                double aDenominator = (a_ * (SunsetAngle1Slope - SunriseAngle1Slope + SunsetAngle2Slope - SunriseAngle2Slope))
                       + (b_ * (Math.Sin(SunsetAngle1Slope) - Math.Sin(SunriseAngle1Slope) + Math.Sin(SunsetAngle2Slope) - Math.Sin(SunriseAngle2Slope)))
                       - (c_ * (Math.Cos(SunsetAngle1Slope) - Math.Cos(SunriseAngle1Slope) + Math.Cos(SunsetAngle2Slope) - Math.Cos(SunriseAngle2Slope)));
                MeanPathSlope = aNominator / aDenominator;
            }

            // amount of precipitable water in the atmosphere  (Allen et al., 2006)
            double PrecipitableWater = a_pw + (b_pw * (myVP * 0.1) * AtmosphericPressure);

            // Transmissivity index for direct beam radiation - horizontal and slope (Allen et al., 2006)
            double KIh = a_ki * Math.Exp((-b_ki * AtmosphericPressure / (TurbidityCoefficient * MeanPathHorz))
                        - (c_ki * Math.Pow(PrecipitableWater / MeanPathHorz, d_ki)));
            double KIs = a_ki * Math.Exp((-b_ki * AtmosphericPressure / (TurbidityCoefficient * MeanPathSlope))
                        - (c_ki * Math.Pow(PrecipitableWater / MeanPathSlope, d_ki)));

            // Direct radiation ratio for slope
            DirRadnRatio = (RelativeIrradianceOnSlope / RelativeSolarIrradiance) * (KIs / KIh);

            // Diffuse radiation ratio for slope
            DiffRadnRatio = ((1.0 - (ClearnessIndex * (1 - DiffuseRadnFraction)))
                          * (1 + (Math.Sqrt(1 - DiffuseRadnFraction) * Math.Pow(Math.Sin(0.5 * SlopeAngle), 3.0)))
                          * SlopeFactor) + (DirRadnRatio * ClearnessIndex * (1 - DiffuseRadnFraction));

            // Prepare the radiation outputs
            RadnDirect = RadnMeasured * DirRadnRatio * (1 - DiffuseRadnFraction);
            RadnDiffuse = RadnMeasured * DiffRadnRatio * DiffuseRadnFraction;
            RadnReflected = RadnMeasured * SurroundsAlbedo * (1 - SlopeFactor);
            myRadn = RadnDirect + RadnDiffuse + RadnReflected;
        }
        else
        {
            // Length of daylight, horizontal (max) and slope (actual)
            MaxDirSunlightLength = 24 * SunriseAngleHorizontal / Math.PI;
            ActualDirSunlightLength = MaxDirSunlightLength;

            // Direct and diffuse radiation ratios for slope
            DirRadnRatio = 1.0;
            DiffRadnRatio = 1.0;

            // Prepare the radiation outputs
            RadnMeasured = MyMetFile.Radn;
            RadnDirect = RadnMeasured * (1 - DiffuseRadnFraction);
            RadnDiffuse = RadnMeasured * DiffuseRadnFraction;
            RadnReflected = 0.0;
            myRadn = RadnDirect + RadnDiffuse + RadnReflected;
        }

        if (myRadn > 0.0)
        {
            FracRadnDirect = RadnDirect / myRadn;
            FracRadnDiffuse = RadnDiffuse / myRadn;
            FracRadnReflected = RadnReflected / myRadn;
        }
        else
        {
            FracRadnDirect = 0.0;
            FracRadnDiffuse = 0.0;
            FracRadnReflected = 0.0;
        }
    }

    /// <summary>
    /// Calculate the hour angles for sunrise and sunset on a tilted surface
    /// </summary>
    /// <param name="a_">Auxiliary parameter a</param>
    /// <param name="b_">Auxiliary parameter b</param>
    /// <param name="c_">Auxiliary parameter c</param>
    private void SunriseSunsetOnSlope(double a_, double b_, double c_)
    {
        double WS1 = -Math.Acos(CosineWS(1, a_, b_, c_));
        double WS2 = Math.Acos(CosineWS(-1, a_, b_, c_));
        double adjWS1 = EvaluateSunAngles(-WS1, -(2 * Math.PI) - WS1, -Math.PI, WS1, a_, b_, c_);
        double adjWS2 = EvaluateSunAngles((2 * Math.PI) - WS2, -WS2, Math.PI, WS2, a_, b_, c_);
        double bSunrise1 = Math.Min(Math.Max(-SunriseAngleHorizontal, adjWS1), SunriseAngleHorizontal);
        double bSunrise2 = -SunriseAngleHorizontal;
        double bSunset1 = Math.Max(Math.Min(SunriseAngleHorizontal, adjWS2), -SunriseAngleHorizontal);
        double bSunset2 = SunriseAngleHorizontal;
        if (c_ < epsilon)
        {
            bSunrise2 = Math.Max(-SunriseAngleHorizontal, Math.Min(SunriseAngleHorizontal, Math.Min(Math.PI, adjWS1 + (2 * Math.PI))));
        }
        if (c_ >= epsilon)
        {
            bSunset2 = Math.Min(SunriseAngleHorizontal, Math.Max(-SunriseAngleHorizontal, Math.Max(-Math.PI, adjWS2 - (2 * Math.PI))));
        }

        // Assign the angles in the right order
        if (bSunset1 > bSunrise1)
        {
            if (bSunset2 > bSunrise2)
            {
                if (bSunrise1 <= bSunrise2)
                {
                    SunriseAngle1Slope = bSunrise1;
                    SunsetAngle2Slope = bSunset2;
                    if (bSunrise2 > bSunset1)
                    {
                        SunriseAngle2Slope = bSunrise2;
                        SunsetAngle1Slope = bSunset1;
                    }
                    else
                    {
                        SunriseAngle2Slope = bSunset2;
                        SunsetAngle1Slope = bSunset2;
                    }
                }
                else
                {
                    SunriseAngle1Slope = bSunrise2;
                    SunsetAngle2Slope = bSunset1;
                    if (bSunrise1 > bSunset2)
                    {
                        SunsetAngle1Slope = bSunset2;
                        SunriseAngle2Slope = bSunrise1;
                    }
                    else
                    {
                        SunsetAngle1Slope = bSunset1;
                        SunriseAngle2Slope = bSunset1;
                    }
                }
            }
            else
            {
                SunriseAngle1Slope = bSunrise1;
                SunsetAngle1Slope = bSunset1;
                SunriseAngle2Slope = bSunset1;
                SunsetAngle2Slope = bSunset1;
            }
        }
        else
        {
            if (bSunset2 > bSunrise2)
            {
                SunriseAngle1Slope = bSunrise2;
                SunsetAngle1Slope = bSunset2;
                SunriseAngle2Slope = bSunset2;
                SunsetAngle2Slope = bSunset2;
            }
            else
            {
                SunriseAngle1Slope = 0.0;
                SunsetAngle1Slope = 0.0;
                SunriseAngle2Slope = 0.0;
                SunsetAngle2Slope = 0.0;
            }
        }
    }

    /// <summary>
    /// Compute the base cosine of ws (sunrise angle) on a tilted surface, uses a quadratic function
    /// </summary>
    /// <param name="mySwith">Whether the root is positive or negative</param>
    /// <param name="a_">Auxiliary parameter a</param>
    /// <param name="b_">Auxiliary parameter b</param>
    /// <param name="c_">Auxiliary parameter c</param>
    /// <returns>The value of the cosine of ws</returns>
    private double CosineWS(double mySwith, double a_, double b_, double c_)
    {
        double result = 0.0;
        if ((Math.Abs(a_) < epsilon) && (Math.Abs(b_) < epsilon))
        {
            result = mySwith;
        }
        else if (Math.Abs(c_) < epsilon)
        {
            if (Math.Abs(a_) < epsilon)
                result = 0.0;
            else if (Math.Abs(b_) < epsilon)
            {
                if (a_ < epsilon)
                    result = -1000.0;
                else
                    result = 1000.0;
            }
            else
            {
                result = -a_ / b_;
            }
        }
        else
        {
            result = (-(a_ * b_) + (mySwith * c_ * Math.Sqrt(Math.Max(0.0, -(a_ * a_) + (b_ * b_) + (c_ * c_))))) / ((b_ * b_) + (c_ * c_));
        }

        // limit the result to valid range
        result = Math.Max(-1.0, Math.Min(1.0, result));

        return result;
    }

    /// <summary>
    /// Evaluate the results for sunrise/sunset angle (ws)
    /// </summary>
    /// <param name="WSoption1">Option 1 for ws</param>
    /// <param name="WSoption2">Option 2 for ws</param>
    /// <param name="WSoption3">Option 3 for ws</param>
    /// <param name="WSdefault">Default option for ws</param>
    /// <param name="a_">Auxiliary parameter a</param>
    /// <param name="b_">Auxiliary parameter b</param>
    /// <param name="c_">Auxiliary parameter c</param>
    /// <returns>The appropriate value for ws</returns>
    private double EvaluateSunAngles(double WSoption1, double WSoption2, double WSoption3, double WSdefault, double a_, double b_, double c_)
    {
        double result = 0.0;
        if ((c_ > epsilon) && (Math.Abs(Math.Asin(a_ + (b_ * Math.Cos(WSoption1)) + (c_ * Math.Sin(WSoption1)))) < epsilon))
        {
            result = WSoption1;
        }
        else if ((c_ < -epsilon) && (Math.Abs(Math.Asin(a_ + (b_ * Math.Cos(WSoption2)) + (c_ * Math.Sin(WSoption2)))) < epsilon))
        {
            result = WSoption2;
        }
        else
        {
            if (Math.Asin(a_ + (b_ * Math.Cos(WSdefault)) + (c_ * Math.Sin(WSdefault))) > epsilon)
            {
                result = WSoption3;
            }
            else if (Math.Asin(a_ + (b_ * Math.Cos(WSdefault)) + (c_ * Math.Sin(WSdefault))) < -epsilon)
            {
                result = 0.0;
            }
            else
            {
                if ((Math.Abs(c_) < epsilon) && (b_ < epsilon))
                {
                    result = WSoption2;
                }
                else
                {
                    result = WSdefault;
                }
            }
        }

        return result;
    }

    /// <summary>
    /// Computes the variation in temperature caused by changes in incident radiation in tilted surfaces
    /// </summary>
    private void DeltaTemperature()
    {
        // Base temperature response to variation in direct radiation, as affected by wind
        double aT = aT0 * Math.Exp(-cT * myWindSpeed);
        double dltRadn = myRadn - RadnMeasured;
        dltTmax = 0.0;
        dltTmin = 0.0;
        if (aT > 0.0)
        {
            if (dltRadn < 0.0)
            {
                dltTmax = -FN * aT * Math.Pow(Math.Abs(dltRadn), bT);
                dltTmin = -FN * FM * aT * Math.Pow(Math.Abs(dltRadn), bT);
            }
            else if (dltRadn > 0.0)
            {
                dltTmax = aT * Math.Pow(dltRadn, bT);
                dltTmin = FM * aT * Math.Pow(dltRadn, bT);
            }
        }
    }

    #endregion
}