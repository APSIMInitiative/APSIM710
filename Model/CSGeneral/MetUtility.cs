using System;

namespace CSGeneral
{
    /// <summary>
    /// Various utilities for manipulating weather data
    /// The code in this unit came from VBMet. following automated translation into c#
    /// The VB code was probably a conversion from still earlier Fortran
    /// Better naming of methods, and a bit more documentation would be useful!
    /// </summary>
    public class MetUtility
    {
        //atmospheric pressure (Pa)
        public const double P = 101000.0;
        //molecular weight of dry air (g/mole)
        public const double Ma = 28.966;
        //specific heat of air (J/g/K)
        public const double Cp = 1.01;
        //Gas constand (J/mole/K)
        public const double R = 8.3143;
        //Latent heat of vaporisation (J/g)
        public const double L = 2440;

        // iterations
        public const double itns = 10.0;
        //Atmospheric Transmissivity at Azimuth - From Thornton and Running.
        public const double Taz = 0.87;
        // Decrease in Atmospheric Transmissivity per unit water vapour - From Thornton and Running
        public const double Alpha = 0.0061;


        public static double RelHum(double Ta, double Tw)
        {
            // Relative Humidity - unitless
            // Ta is air temperature (oC)
            // Tw is wet bulb temperature (oC)

            double RhoV_t = 0;
            double RhoVs_t = 0;
            RhoV_t = RhoVs(Tw) - gamma(Ta) * (Ta - Tw);
            RhoVs_t = RhoVs(Ta);
            return RhoV_t / RhoVs_t;
        }

        public static double RhoV(double Ta, double Tw)
        {
            // Vapour Density
            return RhoVs(Tw) - gamma(Ta) * (Ta - Tw);
        }

        public static double RhoVs(double T)
        {
            // Saturated Vapour Density
            double Tk = 0;
            Tk = T + 273.3;
            return 217 * (Math.Exp(54.87819 - (6790.4985 / Tk + 5.02808 * Math.Log(Tk)))) / Tk;
        }

        public static double gamma(double T)
        {
            double Tk = 0;
            // Absolute Temperature in Kelvin
            Tk = T + 273.3;
            return P * Ma * Cp / (R * Tk * L);
        }

        public static double svp(double temp_c)
        {
            //Saturation Vapour Pressure
            return 6.1078 * Math.Exp(17.269 * temp_c / (237.3 + temp_c));
        }

        // ------------------------------------------------------------------------
        public static double day_length(int day, double lat, double sun_angle)
        {
            // ------------------------------------------------------------------------
            double DEC = 0;
            // Declination - degrees
            double DECr = 0;
            // Declination - radians
            double LATr = 0;
            // Latitude - radians
            double HS = 0;
            // Hour angle
            double sun_alt = 0;
            // solar altitude
            double coshra = 0;
            // Cosine of hour angle
            double slsd = 0;
            double clcd = 0;
            double altmn = 0;
            double altmx = 0;
            double alt = 0;

            sun_alt = sun_angle * 2.0 * Math.PI / 365.25;

            DEC = 23.45 * Math.Sin(2.0 * Math.PI / 365.25 * (day - 79.25));
            DECr = DEC * 2.0 * Math.PI / 360.0;
            LATr = lat * 2.0 * Math.PI / 360.0;

            if (lat == 90)
            {
                coshra = Math.Sign(-DEC) * Math.Sign(lat);
            }
            else
            {
                slsd = Math.Sin(LATr) * Math.Sin(DECr);
                clcd = Math.Cos(LATr) * Math.Cos(DECr);

                altmn = Math.Asin(MathUtility.Bound(slsd - clcd, -1.0, 1.0));
                altmx = Math.Asin(MathUtility.Bound(slsd + clcd, -1.0, 1.0));
                alt = MathUtility.Bound(sun_alt, altmn, altmx);

                // get cos of the hour angle

                coshra = (Math.Sin(alt) - slsd) / clcd;
                coshra = MathUtility.Bound(coshra, -1.0, 1.0);
            }

            HS = Math.Acos(coshra);

            return HS * 2.0 * 24.0 / (2.0 * Math.PI);

        }

        public static double TandR_radn(int day, double lat, double dT, double vp, double rain, double dT30)
        {
            double TransMax = 0;
            double b = 0;
            double c = 0;
            double Tfmax = 0;

            // NOTE - this needs to be converted back to stardard published approach

            TransMax = TMax(day, lat, Taz, Alpha, vp);

            //b = 0.031 + 0.201 * Exp(-0.185 * dT30)
            b = 0.08;

            c = 1.5;

            //If rain > 0 Then
            //   dT = dT * 0.75
            //End If

            Tfmax = 1 - 0.9 * Math.Exp(-b * Math.Pow(dT, c));
            if (rain > 25)
            {
                Tfmax = Tfmax * 0.5;
                //Tfmax = max(Tfmax, 0.5)
            }

            return Q0(day, lat) * Tfmax * TransMax;

        }


        public static double Q0(int day, double lat)
        {
            double functionReturnValue = 0;

            // Total Daily Extraterrestrial SW radiation - MJ

            double DEC = 0;
            double DECr = 0;
            double LATr = 0;
            double HS = 0;


            DEC = 23.45 * Math.Sin(2.0 * Math.PI / 365.25 * (day - 79.25));
            DECr = DEC * 2.0 * Math.PI / 360.0;
            LATr = lat * 2.0 * Math.PI / 360.0;

            HS = Math.Acos(-Math.Tan(LATr) * Math.Tan(DECr));
            functionReturnValue = 86400.0 * 1360.0 * (HS * Math.Sin(LATr) * Math.Sin(DECr) + Math.Cos(LATr) * Math.Cos(DECr) * Math.Sin(HS)) / Math.PI;
            functionReturnValue = functionReturnValue / 1000000.0;
            return functionReturnValue;

        }

        // ------------------------------------------------------------------------
        public static double Transmissivity(int day, double lat, double Radn)
        {
            // ------------------------------------------------------------------------
            return Radn / Q0(day, lat);
        }

        // ------------------------------------------------------------------------
        public static double Q0i(int day, double HS, double lat)
        {
            double functionReturnValue = 0;
            // ------------------------------------------------------------------------

            // Instantaneous Extraterrestrial SW radiation - MJ

            double DEC = 0;
            double DECr = 0;
            double LATr = 0;
            double HSmax = 0;

            HSmax = Math.Acos(-Math.Tan(LATr) * Math.Tan(DECr));
            // half daylength

            if ((HS <= HSmax))
            {
                DEC = 23.45 * Math.Sin(2.0 * Math.PI / 365.25 * (day - 79.25));

                DECr = DEC * 2.0 * Math.PI / 360.0;
                LATr = lat * 2.0 * Math.PI / 360.0;

                functionReturnValue = 1360.0 * (Math.Sin(LATr) * Math.Sin(DECr) + Math.Cos(LATr) * Math.Cos(DECr) * Math.Cos(HS));
                functionReturnValue = functionReturnValue / 1000000.0;
                // Convert to MJ
            }
            else
            {
                functionReturnValue = 0;
            }
            return functionReturnValue;

        }

        // ------------------------------------------------------------------------
        public static double Q0int(int day, double lat)
        {
            double functionReturnValue = 0;
            // ------------------------------------------------------------------------
            // Total Daily Extraterrestrial SW radiation - MJ
            // Integrated from instantaneous values of S0

            double DEC = 0;
            double DECr = 0;
            double LATr = 0;
            double T = 0;
            //time of day as hour angle in radians
            double S1 = 0;
            double S2 = 0;
            double HS = 0;

            DEC = 23.45 * Math.Sin(2.0 * Math.PI / 365.25 * (day - 79.25));
            DECr = DEC * 2.0 * Math.PI / 360.0;
            LATr = lat * 2.0 * Math.PI / 360.0;

            HS = Math.Acos(-Math.Tan(LATr) * Math.Tan(DECr));
            // half daylength

            // Integrate using trapezoidal rule type thing
            functionReturnValue = 0.0;
            for (T = HS / itns; T <= HS; T += HS / itns)
            {
                S1 = Q0i(day, T - HS / itns, lat);
                S2 = Q0i(day, T, lat);
                functionReturnValue = functionReturnValue + (S1 + S2) / 2 / itns;
            }
            functionReturnValue = functionReturnValue * HS / (2.0 * Math.PI / (24.0 * 60.0 * 60.0));
            functionReturnValue = functionReturnValue * 2.0;
            return functionReturnValue;


        }
        // ------------------------------------------------------------------------
        public static double QMax(int day, double lat, double Taz, double alpha, double vp)
        {
            double functionReturnValue = 0;
            // ------------------------------------------------------------------------
            double DEC = 0;
            double DECr = 0;
            double LATr = 0;
            double T = 0;
            //time of day as hour angle in radians
            double S1 = 0;
            double S2 = 0;
            double HS = 0;
            // Sunrise hour angle
            double M = 0;
            // Mixing Length

            DEC = 23.45 * Math.Sin(2.0 * Math.PI / 365.25 * (day - 79.25));
            DECr = DEC * 2.0 * Math.PI / 360.0;
            LATr = lat * 2.0 * Math.PI / 360.0;

            HS = Math.Acos(-Math.Tan(LATr) * Math.Tan(DECr));
            // half daylength

            // Integrate using trapezoidal rule type thing
            functionReturnValue = 0.0;
            for (T = HS / itns; T <= HS; T += HS / itns)
            {
                //avoid very small angles causing numerical errors
                T = Math.Min(T, HS * 0.999);
                S1 = Q0i(day, T - HS / itns, lat);
                S2 = Q0i(day, T, lat);
                // note that sin(a) = cos(z) therefore
                M = 1 / (Math.Sin(LATr) * Math.Sin(DECr) + Math.Cos(LATr) * Math.Cos(DECr) * Math.Cos(T));

                if (M > 2)
                {
                    // 1/cos(z) rule breaks down at low angles due to earth's curvature
                    // add correction fitted to Smithsonian met table data
                    M = 2.0 + Math.Pow((M - 2.0), 0.955);
                }
                functionReturnValue = functionReturnValue + (S1 + S2) / 2 / itns * ((Math.Pow(Taz, M)) - alpha * vp);
            }
            functionReturnValue = functionReturnValue * HS / (2.0 * Math.PI / (24.0 * 60.0 * 60.0));
            functionReturnValue = functionReturnValue * 2.0;
            return functionReturnValue;

        }

        // ------------------------------------------------------------------------
        public static double TMax(int day, double lat, double Taz, double alpha, double vp)
        {
            // ------------------------------------------------------------------------
            double S = 0;
            double Q = 0;
            S = QMax(day, lat, Taz, alpha, vp);
            Q = Q0(day, lat);
            return S / Q;

        }
        // ------------------------------------------------------------------------
        public static double M(double day, double lat, double HS)
        {
            double functionReturnValue = 0;
            // ------------------------------------------------------------------------
            double DEC = 0;
            double DECr = 0;
            double LATr = 0;
            double HSmax = 0;

            DEC = 23.45 * Math.Sin(2.0 * 3.14159265 / 365.25 * (day - 79.25));
            DECr = DEC * 2.0 * 3.14159265 / 360.0;
            LATr = lat * 2.0 * 3.14159265 / 360.0;

            HSmax = Math.Acos(-Math.Tan(LATr) * Math.Tan(DECr));
            // half daylength
            HS = Math.Min(HS, HSmax * 0.99);

            // note that sin(a) = cos(z) therefore
            functionReturnValue = 1 / (Math.Sin(LATr) * Math.Sin(DECr) + Math.Cos(LATr) * Math.Cos(DECr) * Math.Cos(HS));

            if (functionReturnValue > 2)
            {
                // 1/cos(z) rule breaks down at low angles due to earth's curvature
                // add correction fitted to Smithsonian met table data
                functionReturnValue = 2.0 + Math.Pow((functionReturnValue - 2.0), 0.955);
                functionReturnValue = Math.Min(functionReturnValue, 20.0);
                functionReturnValue = Math.Max(functionReturnValue, 0.0);

            }
            return functionReturnValue;

        }
        // ------------------------------------------------------------------------
        public static double Mint(double day, double lat, double HS1, double HS2)
        {
            double functionReturnValue = 0;
            // ------------------------------------------------------------------------
            double dHS = 0;
            double start = 0;
            double finish = 0;
            double T = 0;
            double M1 = 0;
            double M2 = 0;
            int i = 0;

            dHS = Math.Abs(HS2 - HS1);
            start = Math.Min(HS1, HS2);
            finish = Math.Max(HS1, HS2);

            functionReturnValue = 0.0;


            for (i = 1; i <= Convert.ToInt32(itns); i++)
            {
                T = start + i * dHS / itns;
                M1 = M(day, lat, T - dHS / itns);
                M2 = M(day, lat, T);
                functionReturnValue = functionReturnValue + (M1 + M2) / 2 / itns;
            }
            return functionReturnValue;
            //Beep

        }

        // ------------------------------------------------------------------------
        public static double Q0iint(int day, double lat, double HS1, double HS2)
        {
            double functionReturnValue = 0;
            double T = 0;
            double S1 = 0;
            double S2 = 0;
            double dHS = 0;
            double start = 0;
            double finish = 0;

            functionReturnValue = 0.0;
            dHS = Math.Abs(HS2 - HS1);
            start = Math.Min(HS1, HS2);
            finish = Math.Max(HS1, HS2);

            for (T = start + dHS / itns; T <= finish; T += dHS / itns)
            {
                S1 = Q0i(day, T - dHS / itns, lat);
                S2 = Q0i(day, T, lat);
                functionReturnValue = functionReturnValue + (S1 + S2) / 2 / itns;
            }
            return functionReturnValue;

        }
    }
}
