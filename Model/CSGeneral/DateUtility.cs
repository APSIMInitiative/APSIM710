using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace CSGeneral
{
    /// <summary>
    /// Some date manipulation routines, transcribed from their Fortran counterparts
    /// </summary>
    public class DateUtility
    {
        static List<string> LowerCaseMonths = new List<string>(new[] { null, "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" });

        static Regex
            rxDD = new Regex(@"\d\d?"),
            rxMMM = new Regex(@"\w{3}");

        public static DateTime GetDate(double julian_date)
        {
            double a, b, c, d, e, f, z, alpha, decDay;
            int yr, mnth, day, hr, min, sec, ms;
            double decHr, decMin, decSec;

            julian_date += 0.5;
            z = Math.Truncate(julian_date); //store int part of JD
            f = julian_date - z;    //store the frac part of JD
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
                mnth = Convert.ToInt32(e - 1);
            else
                mnth = Convert.ToInt32(e - 13);

            if (mnth > 2)
                yr = Convert.ToInt32(c - 4716);
            else
                yr = Convert.ToInt32(c - 4715);

            //convert decDay to d,hr,min,sec
            day = Convert.ToInt32(Math.Truncate(decDay));
            decHr = (decDay - day) * 24;
            hr = Convert.ToInt32(Math.Truncate(decHr));
            decMin = (decHr - hr) * 60;
            min = Convert.ToInt32(Math.Truncate(decMin));
            decSec = (decMin - min) * 60;
            sec = Convert.ToInt32(Math.Truncate(decSec));
            ms = Convert.ToInt32(Math.Truncate(decSec - sec * 1000));

            return new DateTime(yr, mnth, day, hr, min, sec, ms);
        }

        public static DateTime GetDate(string ddMMM)
        {
            return GetDate(ddMMM, 1900);
        }

        public static DateTime GetDate(string ddMMM, int year)
        {
            try
            {
                return new DateTime(
                    year,
                    LowerCaseMonths.IndexOf(rxMMM.Match(ddMMM).Value.ToLower()),
                    int.Parse(rxDD.Match(ddMMM).Value),
                    12,
                    0,
                    0
                    );
            }
            catch
            {
                throw new Exception("Error in 'GetDate' - input string should be in form ddmmm (any delimiter may appear between dd and mmm), input string: " + ddMMM);
            }
        }

        public static DateTime GetDate(string ddMMM, DateTime today)
        {
            return GetDate(ddMMM, today.Year);
        }

        public static DateTime GetNextDate(DateTime thedate, DateTime today)
        {
            return today.CompareTo(thedate) < 0 ? thedate : thedate.AddYears(1);
        }
        
        public static DateTime GetNextDate(string ddMMM, DateTime today)
        {
            return GetNextDate(GetDate(ddMMM, today), today);
        }

        public static int CompareDates(string ddMMM, DateTime today)
        {
            return today.CompareTo(GetDate(ddMMM, today));
        }

        public static bool DatesEqual(string ddMMM, DateTime today)
        {
            return CompareDates(ddMMM, today) == 0;
        }

        public static bool WithinDates(string ddMMM_date1, DateTime today, string ddMMM_date2)
        {
            DateTime
                start = GetDate(ddMMM_date1, today.Year),
                end = GetDate(ddMMM_date2, today.Year);

            //if start after end (could be after if spanning years)
            if (start.CompareTo(end) > 0)
                end = end.AddYears(1);

            return WithinDates(start, today, end);
        }

        public static bool WithinDates(DateTime date1, DateTime today, DateTime date2)
        {
            return today.CompareTo(date1) >= 0 && today.CompareTo(date2) <= 0;
        }

        public static double GetJulianDate(DateTime date)
        {
            double yr;
            double a, b = 0;
            double JD;

            double
                y = date.Year,
                m = date.Month,
                d = date.Day + date.TimeOfDay.TotalHours / 24d;

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

        /// <summary>
        /// Converts a Julian Day Number to Day of year. 
        /// </summary>
        /// <param name="JDN"> Julian day number.</param>
        /// <returns>Date time value.</returns>
        public static void JulianDayNumberToDayOfYear(int JDN, out int dyoyr, out int year)
        {
            uint day;
            uint month;
            DateTime date = GetDate(JDN);
            dyoyr = date.DayOfYear;
            year = date.Year;
        }
    }
}
