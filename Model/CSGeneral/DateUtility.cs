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

        /// <summary>
        /// Convert a Julian Date to a DateTime object
        /// </summary>
        /// <param name="julian_date"></param>
        /// <returns></returns>
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

        /// <summary>
        /// Get a DateTime from a ddMMM string (ie '01Jan' OR '1-Jan' OR '1 Jan' etc), year is automatically set to 1900
        /// </summary>
        /// <param name="ddMMM">String containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <returns>A DateTime with the specified date and month, year = 1900</returns>
        public static DateTime GetDate(string ddMMM)
        {
            return GetDate(ddMMM, 1900);
        }

        /// <summary>
        /// Get a DateTime from a 'ddMMM' string (ie '01Jan' OR '1-Jan' OR '1 Jan' etc)
        /// </summary>
        /// <param name="ddMMM">String containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <param name="year">The year to use when constructing the DateTime object</param>
        /// <returns>A DateTime constructed from <paramref name="ddMMM"/> using <paramref name="year"/></returns>
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

        /// <summary>
        /// Get a DateTime from a 'ddMMM' string (ie '01Jan' OR '1-Jan' OR '1 Jan' etc), using <paramref name="today"/> to get the year to use
        /// </summary>
        /// <param name="ddMMM">String containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <param name="today">The year in this parameter will be used to construct the result</param>
        /// <returns>A DateTime constructed from <paramref name="ddMMM"/> using the year of <paramref name="today"/></returns>
        public static DateTime GetDate(string ddMMM, DateTime today)
        {
            return GetDate(ddMMM, today.Year);
        }

        /// <summary>
        /// Given today's date (<paramref name="today"/>), get the next occurrence of <paramref name="thedate"/> by adding/subtracting year(s)
        /// </summary>
        /// <param name="thedate">The date to change</param>
        /// <param name="today">Today's date</param>
        /// <returns>The next occurrence of <paramref name="thedate"/></returns>
        public static DateTime GetNextDate(DateTime thedate, DateTime today)
        {
            thedate = thedate.AddYears(today.Year - thedate.Year);
            return today.CompareTo(thedate) < 0 ? thedate : thedate.AddYears(1);
        }
        
        /// <summary>
        /// Given a 'ddMMM' string (ie '01Jan' OR '1-Jan' OR '1 Jan' etc) and <paramref name="today"/>, return the next occurrence of <paramref name="ddMMM"/>
        /// </summary>
        /// <param name="ddMMM">String containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <param name="today">Today's date</param>
        /// <returns>The next occurrence of <paramref name="ddMMM"/></returns>
        public static DateTime GetNextDate(string ddMMM, DateTime today)
        {
            return GetNextDate(GetDate(ddMMM, today), today);
        }

        /// <summary>
        /// Construct a DateTime from <paramref name="ddMMM"/> and <paramref name="today"/> then 'CompareTo' <paramref name="today"/>
        /// </summary>
        /// <param name="ddMMM">String containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <param name="today">Today's date</param>
        /// <returns>+1 if <paramref name="ddMMM"/> is less than <paramref name="today"/>, 0 if equal, -1 if greater</returns>
        public static int CompareDates(string ddMMM, DateTime today)
        {
            return today.CompareTo(GetDate(ddMMM, today));
        }

        /// <summary>
        /// Compare <paramref name="ddMMM"/> and <paramref name="today"/> (ignoring year component)
        /// </summary>
        /// <param name="ddMMM">String containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <param name="today">The date to check</param>
        /// <returns>true if the day and month components of <paramref name="today"/> match ddMMM, else false</returns>
        public static bool DatesEqual(string ddMMM, DateTime today)
        {
            return 
                today.Month == LowerCaseMonths.IndexOf(rxMMM.Match(ddMMM).Value.ToLower())
                &&
                today.Day == int.Parse(rxDD.Match(ddMMM).Value);
        }

        /// <summary>
        /// Look to see if <paramref name="today"/> lies between <paramref name="ddMMM_start"/> and <paramref name="ddMMM_end"/> (will handle year boundaries)
        /// </summary>
        /// <param name="ddMMM_start">The start date - a string containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <param name="today">The date to check</param>
        /// <param name="ddMMM_end">The end date - a string containing 'day of month' and at least the first 3 letters of a month's name</param>
        /// <returns><paramref name="ddMMM_start"/> &lt;&eq; <paramref name="today"/> &lt;&eq; <paramref name="ddMMM_end"/></returns>
        public static bool WithinDates(string ddMMM_start, DateTime today, string ddMMM_end)
        {
            DateTime
                start = GetDate(ddMMM_start, today.Year),
                end = GetDate(ddMMM_end, today.Year);

            //if start after end (could be after if spanning years)
            if (start.CompareTo(end) > 0)
                end = end.AddYears(1);

            return WithinDates(start, today, end);
        }

        /// <summary>
        /// Look to see if <paramref name="today"/> lies between <paramref name="start"/> and <paramref name="end"/> (will handle year boundaries)
        /// </summary>
        /// <param name="start">The start date</param>
        /// <param name="today">The date to check</param>
        /// <param name="end">The end date</param>
        /// <returns><paramref name="start"/> &lt;&eq; <paramref name="today"/> &lt;&eq; <paramref name="end"/></returns>
        public static bool WithinDates(DateTime start, DateTime today, DateTime end)
        {
            return today.CompareTo(start) >= 0 && today.CompareTo(end) <= 0;
        }

        /// <summary>
        /// Get a Julian Date from a DateTime
        /// </summary>
        /// <param name="date">The DateTime to convert</param>
        /// <returns>The Julian Date representation of <paramref name="date"/></returns>
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
