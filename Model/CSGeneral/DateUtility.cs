using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CSGeneral
{
    /// <summary>
    /// Some date manipulation routines, transcribed from their Fortran counterparts
    /// </summary>
    public class DateUtility
    {

        //============================================================================
        /// <summary>
        /// Converts a y m d to a Julian date. Assumes midday.
        /// </summary>
        /// <param name="y">Year</param>
        /// <param name="m">Month</param>
        /// <param name="d">Day. Can be a part day</param>
        /// <returns>Julian date</returns>
        //============================================================================
        static public double dateToJulianDate(int y, uint m, double d)
        {
            double yr;
            double a, b = 0;
            double JD;

            //make a yyyy.MMDDdd value
            yr = y + ((double)m / 100) + (d / 10000.0);

            if ((m == 1) || (m == 2))
            {
                y -= 1;
                m += 12;
            }
            if (yr >= 1582.1015)
            { //use yyyy.MMDDdd value
                a = y / 100;  // Integer division
                b = 2 - a + Math.Truncate(a / 4.0);
            }

            JD = b + Math.Truncate(365.25 * y) + Math.Truncate(30.6001 * (m + 1)) + d + 1720994.5;

            return JD;
        }
        //============================================================================
        /// <summary>
        /// Converts a date time to Julian Date
        /// </summary>
        /// <param name="y">Year</param>
        /// <param name="m">Month</param>
        /// <param name="d">Day</param>
        /// <param name="hr">Hour</param>
        /// <param name="min">Minute</param>
        /// <param name="sec">Whole seconds</param>
        /// <returns> Julian date</returns>
        //============================================================================
        static public double dateTimeToJulianDate(int y, uint m, uint d, uint hr, uint min, double sec)
        {
            double partDay = d + (hr / 24.0) + (min / 1440.0) + (sec / 86400.0);
            return dateToJulianDate(y, m, partDay);
        }
        //============================================================================
        /// <summary>
        /// Converts a date time to Julian Date
        /// </summary>
        /// <param name="dt"></param>
        /// <returns>Julian date</returns>
        //============================================================================
        static public double dateTimeToJulianDate(DateTime dt)
        {
            return dateTimeToJulianDate(dt.Year, (uint)dt.Month, (uint)dt.Day, (uint)dt.Hour, (uint)dt.Minute, dt.Second);
        }
        //=======================================================================
        /// <summary>
        /// Converts a Julian Date to DateTime (.net) values. 
        /// </summary>
        /// <param name="JD"> Julian date.</param>
        /// <returns>Date time value.</returns>
        //=======================================================================
        static public DateTime JulianDateToDateTime(double JD)
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;
            int second, msec;

            JulianDateToDateTime(out yr, out mon, out day, out hr, out min, out sec, JD);
            second = (int)Math.Truncate(sec);
            msec = (int)Math.Truncate((sec - second) * 1000);
            return new DateTime(yr, (int)mon, (int)day, (int)hr, (int)min, second, msec);
        }
        //============================================================================
        /// <summary>
        /// Converts a Julian Date to date time values
        /// </summary>
        /// <param name="yr"></param>
        /// <param name="mon"></param>
        /// <param name="day"></param>
        /// <param name="hr"></param>
        /// <param name="min"></param>
        /// <param name="sec">Decimal seconds</param>
        /// <param name="JD">Julian Date to convert</param>
        //============================================================================
        static public void JulianDateToDateTime(out int yr, out uint mon, out uint day, out uint hr, out uint min, out double sec, double JD)
        {
            double a, b, c, d, e, f, z, alpha, decDay;
            Double decHr, decMin;

            JD += 0.5;
            z = Math.Truncate(JD); //store int part of JD
            f = JD - z;    //store the frac part of JD
            if (z < 2299161)
                a = z;
            else
            {
                alpha = Math.Truncate((z - 1867216.25) / 36524.25);
                a = z + 1 + alpha - Math.Truncate(alpha / 4);
            }
            b = a + 1524;
            c = Math.Truncate((b - 122.1) / 365.25);
            d = Math.Truncate(365.25 * c);
            e = Math.Truncate((b - d) / 30.6001);

            decDay = b - d - Math.Truncate(30.6001 * e) + f;
            if (e < 13.5)
                mon = Convert.ToUInt32(e - 1);
            else
                mon = Convert.ToUInt32(e - 13);

            if (mon > 2)
                yr = Convert.ToInt32(c - 4716);
            else
                yr = Convert.ToInt32(c - 4715);

            //convert decDay to d,hr,min,sec
            day = Convert.ToUInt32(Math.Truncate(decDay));
            decHr = (decDay - day) * 24;
            hr = Convert.ToUInt32(Math.Truncate(decHr));
            decMin = (decHr - hr) * 60;
            min = Convert.ToUInt32(Math.Truncate(decMin));
            sec = (decMin - min) * 60;  //decimal seconds
        }
        

        static public void JulianDateToDayOfYear(double julday, out int dyoyr, out int year)
        {
            uint day;
            uint month;
            uint hr, min;
            double sec;
            JulianDateToDateTime(out year, out month, out day, out hr, out min, out sec, julday);
            dyoyr = (int)(julday - dateTimeToJulianDate(year, 1, 1, hr, min, sec) + 1);
        }

        static public int OffsetDayOfYear(int iyr, int doy, int ndays)
        {
            //+ Purpose
            //       adds or subtracts specified days to/from day of year number

            //+  Definition
            //     Returns the day of year for the day "ndays" after the day
            //     specified by the day of year, "doy", in the year, "iyr".
            //     "ndays" may well be negative.

            //+  Mission Statement
            //      %1 offset by %3 days

            //+ Changes
            //       4-mar-91 programmed jngh
            //       051191  jngh changed algorithm to use julian days - cr166, cr168
            //                    added variable descriptions - cr167
            //       100393  jngh changed date_to_jday arguments to integer. Impacted
            //                    jday_to_date arguments.
            //       010494  jngh put year in argument
            int result, year;
            double days = dateTimeToJulianDate(iyr, 1, 1, 0, 0, 0.0) - 1.0 + doy + ndays;
            JulianDateToDayOfYear(days, out result, out year);
            return result;
        
        }
    }
}
