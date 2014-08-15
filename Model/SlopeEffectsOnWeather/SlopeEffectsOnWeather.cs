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
/// -This include routines to modify the incoming solar radiation as well as minimum and maximum air temperatures;
/// - Adjusts for rainfall, RHmin and RHmax, and windspeed, can be also done, but these are simply relative changes supplied by the user, not calculated. 
/// - Calculations happens on PreNewMet event and take also into account the latitude of the site (read from the metfile).
/// </remarks>
public class SlopeEffectsOnWeather
{
    #region Links and Parameters

    [Link]
    public Clock Clock = null;

    [Link]
    public MetFile MyMetFile = null;

    /// <summary>
    /// Angle of the slope, from horizontal
    /// </summary>
    [Param]
    [Units("degrees")]
    private double SlopeAngle;
    /// <summary>
    /// Angle of the aspect, from north
    /// </summary>
    [Param]
    [Units("degrees")]
    private double AspectAngle;
    /// <summary>
    /// Albedo of surrounding environment
    /// </summary>
    [Param]
    private double EnviroAlbedo;
    /// <summary>
    /// Parameter A for diffuse radiation fraction
    /// </summary>
    [Param]
    private double A_diffuseR;
    /// <summary>
    /// Parameter B for diffuse radiation fraction
    /// </summary>
    [Param]
    private double B_diffuseR;
    /// <summary>
    /// Parameter kT for temperature correction (rate of change)
    /// </summary>
    [Param]
    private double kT_dTemp;
    /// <summary>
    /// Parameter uT0 for temperature correction (maximum change)
    /// </summary>
    [Param]
    private double uT_dTemp;
    /// <summary>
    /// Parameter bT for temperature correction (wind effect)
    /// </summary>
    [Param]
    private double bT_dTemp;
    /// <summary>
    /// Parameter Fn for temperature correction (increasing-decreasing radiation)
    /// </summary>
    [Param]
    private double Fn_dTemp;
    /// <summary>
    /// Parameter Fx for temperature correction (min-max temperatures)
    /// </summary>
    [Param]
    private double Fx_dTemp;
    /// <summary>
    /// Relative change in rainfall
    /// </summary>
    [Param]
    [Units("%")]
    private double dRain;
    /// <summary>
    /// Relative change in wind
    /// </summary>
    [Param]
    [Units("%")]
    private double dWind;
    /// <summary>
    /// Relative change in vapour pressure
    /// </summary>
    [Param]
    [Units("%")]
    private double dVapPressure;
    /// <summary>
    /// Relative change in relative humidity
    /// </summary>
    [Param]
    [Units("%")]
    private double dRH;

    #endregion

    #region Outputs

    /// <summary>
    /// Original radn input
    /// </summary>
    [Output]
    [Description("Original radiation value")]
    [Units("MJ/m2")]
    public double RadnMeasured;
    /// <summary>
    /// Direct solar radiation
    /// </summary>
    [Output]
    [Description("Direct solar radiation")]
    [Units("MJ/m2")]
    public double RadnDirect;
    /// <summary>
    /// Diffuse solar radiation (from sky)
    /// </summary>
    [Output]
    [Description("Diffuse solar radiation")]
    [Units("MJ/m2")]
    public double RadnDiffuse;
    /// <summary>
    /// Reflected solar radiation (from terrain)
    /// </summary>
    [Output]
    [Description("Reflected solar radiation (from terrain)")]
    [Units("MJ/m2")]
    public double RadnReflected;
    /// <summary>
    /// Extraterestrial solar radiation
    /// </summary>
    [Output]
    [Description("Extraterestrial solar radiation")]
    [Units("MJ/m2")]
    public double ExtraterrestrialRadn;
    /// <summary>
    /// Sky cleaness index
    /// </summary>
    /// <remarks>
    /// Provide an idea of how overcast the day is
    /// </remarks>
    [Output]
    [Description("Atmospheric cleaness index")]
    [Units("MJ/m2")]
    public double ClearnessIndex;
    /// <summary>
    /// Fraction of total radiation that is diffuse
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation that is diffuse (flat)")]
    [Units("0-1")]
    public double DiffuseRadnFraction;
    /// <summary>
    /// Direct fraction of total radiation
    /// </summary>
    [Output]
    [Description("Ratio of direct solar radiation (flat-to-slope)")]
    [Units("-")]
    public double DirRadnRatio;

    /// <summary>
    /// Fraction solar radiation direct
    /// </summary>
    [Output]
    [Description("Fraction solar radiation direct")]
    [Units("0-1")]
    public double FracRadnDirect;
    /// <summary>
    /// Fraction of solar radiation diffuse
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation diffuse")]
    [Units("0-1")]
    public double FracRadnDiffuse;
    /// <summary>
    /// Fraction of solar radiation reflected from terrain
    /// </summary>
    [Output]
    [Description("Fraction of solar radiation reflected from terrain")]
    [Units("0-1")]
    public double FracRadnReflected;

    /// <summary>
    ///  Upper threshold for the variation of Tmax
    /// </summary>
    [Output]
    [Description("Upper threshold for the variation of Tmax")]
    [Units("oC")]
    public double upperTmaxVariation;
    /// <summary>
    ///  Upper threshold for the variation of Tmin
    /// </summary>
    [Output]
    [Description("Upper threshold for the variation of Tmin")]
    [Units("oC")]
    public double upperTminVariation;
    /// <summary>
    ///  Original value of Tmin
    /// </summary>
    [Output]
    [Description("Original value of Tmin")]
    [Units("oC")]
    public double TminMeasured;
    /// <summary>
    ///  Original value of Tmin
    /// </summary>
    [Output]
    [Description("Original value of Tmin")]
    [Units("oC")]
    public double TmaxMeasured;

    /// <summary>
    ///  Actual Tmean value, after adjusts
    /// </summary>
    [Output]
    [Description("Actual Tmean value, after adjusts")]
    [Units("oC")]
    public double TmeanActual;
    /// <summary>
    ///  Variation in Tmax
    /// </summary>
    [Output]
    [Description("Variation in Tmax")]
    [Units("oC")]
    public double dltTmax;
    /// <summary>
    ///  Variation in Tmin
    /// </summary>
    [Output]
    [Description("Variation in Tmin")]
    [Units("oC")]
    public double dltTmin;

    #endregion

    #region Internal variables

    /// <summary>
    /// Value of solar constant (w/m2)
    /// </summary>
    private double SolarConstant = 1367;

    private double LatitudeAngle;
    private double SlopeFactor;

    private bool HasInitialised = false;
    
    #endregion

    #region Calculations

    [EventHandler]
    public void OnInitialised()
    {

        // Check parameter values
        if ((EnviroAlbedo < 0.0) || (EnviroAlbedo > 1))
            throw new Exception("Albedo value is out of bounds (0-1)");
        if ((SlopeAngle < 0.0) || (SlopeAngle > 90))
            throw new Exception("Slope angle is out of the expected range (0-90deg)");
        if ((AspectAngle < 0.0) || (AspectAngle > 360))
            throw new Exception("Aspect angle is out of the expected range (0-600deg)");

        // Convert and fix some parameters
        if (AspectAngle > 180)
            AspectAngle -= 180;
        SlopeAngle = Math.PI * SlopeAngle / 180;
        AspectAngle = Math.PI * AspectAngle / 180;
        double AngleTolerance = 1 / 1000 / 3600;
        //latitude close to zero yields an error, thus we limit it to a thousanth of a second
        if (Math.Abs(MyMetFile.Latitude) < AngleTolerance)
            LatitudeAngle = Math.PI * AngleTolerance / 180;
        else
            LatitudeAngle = Math.PI * MyMetFile.Latitude / 180;
        dRain = Math.Max(0.0, 1 + dRain / 100);
        dWind = Math.Max(0.0, 1 + dWind / 100);
        dVapPressure = Math.Max(0.0, 1 + dVapPressure / 100);
        dRH = Math.Max(0.0, 1 + dRH / 100);

        SlopeFactor = 1 - (SlopeAngle / Math.PI);
        HasInitialised = true;

        Console.WriteLine("");
        Console.WriteLine("     Weather variables will be adjusted for slope and aspect");
        Console.WriteLine("      - Radiation adjusted based on the model of Revfeim (1978), with diffuse fraction from Boland et al (2008)");
        Console.WriteLine("      - Temperature max and min adjusted as function of changes in direct radiation, Cichota (2014)");
        Console.WriteLine("      - Rainfall, wind, vapour pressure, and RH are simple relative changes - not explicitly linked to slope");
        Console.WriteLine("");
    }

    [EventHandler]
    public void OnPreNewMet(NewMetType NewMetData)
    {
        double WindSpeed;

        if (HasInitialised)
        {
            // Extraterrestrial radiation - following classic formulae (Almorox and Hontoria, 2004; Allen et al., 2005)
            double DayAngle = (2 * Math.PI * (Math.Min(Clock.day_of_year, 365) - 1)) / 365;
            double SolarDeclination = 0.006918 - 0.399912 * Math.Cos(DayAngle)
               + 0.070257 * Math.Sin(DayAngle)
               - 0.006758 * Math.Cos(2 * DayAngle)
               + 0.000907 * Math.Sin(2 * DayAngle)
               - 0.002697 * Math.Cos(3 * DayAngle)
               + 0.001480 * Math.Sin(3 * DayAngle);
            double EarthEccentricity = 1.00011 + 0.034221 * Math.Cos(DayAngle)
               + 0.00128 * Math.Sin(DayAngle)
               + 0.000719 * Math.Cos(2 * DayAngle)
               + 0.000077 * Math.Sin(2 * DayAngle);
            double SunriseAngle = Math.Acos(Math.Max(-1, Math.Min(1, -Math.Tan(LatitudeAngle) * Math.Tan(SolarDeclination))));
            //DayLength = 2 * SunriseAngle / (15 * Math.PI / 180)
            ExtraterrestrialRadn = (24 * 3600 * SolarConstant / 1000000) * EarthEccentricity
               * (Math.Cos(LatitudeAngle) * Math.Cos(SolarDeclination) * Math.Sin(SunriseAngle)
               + SunriseAngle * Math.Sin(LatitudeAngle) * Math.Sin(SolarDeclination)) / Math.PI;

            // Sky clearness index - following typical approach (Boland et al., 2008; Dervishi and Mahdavi, 2012)
            ClearnessIndex = Math.Min(1.0, Math.Max(0.0, MyMetFile.Radn / ExtraterrestrialRadn));

            // Diffuse radiation fraction - approach similar to Boland et al. (2008); Fitted to NZ 
            DiffuseRadnFraction = 1 / (1 + Math.Exp(A_diffuseR + B_diffuseR * ClearnessIndex));

            // Adjust for direct and diffuse radiation - based on geometric corrections (Revfeim, 1978, Tian et al. 2001)
            double EffectiveLatitude = Math.Asin(Math.Sin(LatitudeAngle) * Math.Cos(SlopeAngle) + Math.Cos(LatitudeAngle) * Math.Sin(SlopeAngle) * Math.Cos(AspectAngle));
            double DayLightShift = Math.Asin(-Math.Sin(SlopeAngle) * Math.Sin(AspectAngle) / Math.Cos(EffectiveLatitude));
            double SunriseAngleSlope = Math.Acos(Math.Max(-1, Math.Min(1, -Math.Tan(EffectiveLatitude) * Math.Tan(SolarDeclination))));
            double SunriseHour = Math.Min(SunriseAngle, DayLightShift + SunriseAngleSlope);
            double SunsetHour = Math.Max(-SunriseAngle, DayLightShift - SunriseAngleSlope);
            double DayDur = (SunriseHour - SunsetHour) / 2;
            double MidDay = (SunriseHour + SunsetHour) / 2;
            //double DayLengthSlope = 2 * DayDur / (15 * Math.PI / 180)
            DirRadnRatio = Math.Sin(EffectiveLatitude) * (DayDur - Math.Sin(DayDur) * Math.Cos(MidDay) * Math.Cos(DayLightShift) / Math.Cos(SunriseAngleSlope))
               / (Math.Sin(LatitudeAngle) * (SunriseAngle - Math.Tan(SunriseAngle)));

            // Prepare the radiation outputs
            RadnMeasured = MyMetFile.Radn;
            RadnDirect = RadnMeasured * DirRadnRatio * (1 - DiffuseRadnFraction);
            RadnDiffuse = RadnMeasured * SlopeFactor * DiffuseRadnFraction;
            RadnReflected = RadnMeasured * EnviroAlbedo * (1 - SlopeFactor);
            double ActualRadn = RadnDirect + RadnDiffuse + RadnReflected;

            FracRadnDirect = RadnDirect / ActualRadn;
            FracRadnDiffuse = RadnDiffuse / ActualRadn;
            FracRadnReflected = RadnReflected / ActualRadn;

            // Get and adjust windspeed

            bool myAux = MyMetFile.Get("wind", out WindSpeed);
            if (Math.Abs(dWind) > 0.000001)
                WindSpeed *= dWind;

            // Prepare the temperature outputs
            upperTmaxVariation = uT_dTemp * Math.Exp(-bT_dTemp * WindSpeed);
            upperTminVariation = upperTmaxVariation * Fx_dTemp;
            double F_rR = (DirRadnRatio >= 1.0 ? 1.0 : 0.5);
            if (Math.Abs(DirRadnRatio - 1) > 0.000001)
            {
                dltTmax = upperTmaxVariation * F_rR * (DirRadnRatio - 1) / (kT_dTemp + Math.Abs(DirRadnRatio - 1));
                dltTmin = upperTminVariation * F_rR * (DirRadnRatio - 1) / (kT_dTemp + Math.Abs(DirRadnRatio - 1));
            }
            TmaxMeasured = MyMetFile.MaxT;
            TminMeasured = MyMetFile.MinT;

            // Get and adjust RH
            double myRHmin;
            double myRHmax;
            myAux = MyMetFile.Get("rhmin", out myRHmin);
            myRHmin = Math.Min(100.0, Math.Max(0.0, myRHmin * dRH));
            myAux = MyMetFile.Get("rhmax", out myRHmax);
            myRHmax = Math.Min(100.0, Math.Max(myRHmin, myRHmax * dRH));

            // Set the adjusted weather variables
            if (Math.Abs(dRain - 1) > 0.000001)
                MyMetFile.Rain *= (float)dRain;
            if (Math.Abs(dVapPressure - 1) > 0.000001)
                MyMetFile.vp *= (float)dVapPressure;
            if (Math.Abs(dRH - 1) > 0.000001)
            {
                MyMetFile.Set("rhmin", (float)myRHmin);
                MyMetFile.Set("rhmax", (float)myRHmax);
            }
            if (Math.Abs(dWind - 1) > 0.000001)
                MyMetFile.Set("wind", (float)WindSpeed);
            if (MyMetFile.Radn != ActualRadn)
                MyMetFile.Radn = (float)ActualRadn;
            if (dltTmax != 0.0)
                MyMetFile.MaxT += (float)dltTmax;
            if (dltTmin != 0.0)
            {
                if (MyMetFile.MinT + dltTmin > MyMetFile.MaxT)
                    MyMetFile.MinT = MyMetFile.MaxT;
                else
                    MyMetFile.MinT += (float)dltTmin;
            }

            // Prepare outputs
            TmeanActual = (MyMetFile.MaxT + MyMetFile.MinT) / 2;
        }
    }

    #endregion
}