using System;
using System.Text.RegularExpressions;

namespace CMPServices
{
    //============================================================================
    /// <summary>
    /// TTimeValue is used to store a date time value and provide manipulation
    /// routines for that date and time.
    /// The Julian Day begins at Greenwich mean noon, that is at 12:00 Universal
    /// Time. For example, 1977 April 26.4 = JD 2443259.9
    /// </summary>
    //============================================================================
    public class TTimeValue
    {
        //used to specify the unit of time. see advTime()
        /// <summary>
        /// ID for second. See <see cref="advTime"/>
        /// </summary>
        public const int SEC = 1;
        /// <summary>
        /// ID for minute. See <see cref="advTime"/>
        /// </summary>
        public const int MIN = 2;
        /// <summary>
        /// ID for hour. See <see cref="advTime"/>
        /// </summary>
        public const int HR = 3;
        /// <summary>
        /// ID for day. See <see cref="advTime"/>
        /// </summary>
        public const int DAY = 4;
        /// <summary>
        /// ID for month. See <see cref="advTime"/>
        /// </summary>
        public const int MON = 5;
        /// <summary>
        /// ID for year. See <see cref="advTime"/>
        /// </summary>
        public const int YR = 6;
        /// <summary>
        /// Time interval strings. "second...year"
        /// </summary>
        public static string[] TIMETEXTS = { "second", "minute", "hour", "day", "month", "year" };

        private int iDay;        //Julian day number. x.0 = midday
        private uint iSec;       //Seconds past midnight.
        private double dSecPart; //Fraction of seconds.

        //============================================================================
        /// <summary>
        /// Default constructor
        /// </summary>
        //============================================================================
        public TTimeValue()
        {
            iDay = 0;
            iSec = 0;
            dSecPart = 0;
        }
        //============================================================================
        /// <summary>
        /// Constructs a TimeValue from a correct Julian Date
        /// </summary>
        /// <param name="JD">Julian date. Midday = JD.0</param>
        //============================================================================
        public TTimeValue(double JD)
        {
            //convert the Julian date to local class variable requirements
            setDate(JD);
        }
        //============================================================================
        /// <summary>
        /// Constructs a TTimeValue using the fields from a Protocol Compliant Time value.
        /// </summary>
        /// <param name="day">Julian day number of the day. Value at Midday for the day.</param>
        /// <param name="sec">Seconds past midnight</param>
        /// <param name="secPart">Fraction of seconds</param>
        //============================================================================
        public TTimeValue(int day, uint sec, double secPart)
        {
            iDay = day;
            iSec = sec;
            dSecPart = secPart;
        }
        //============================================================================
        /// <summary>
        /// Constructs a TTimeValue from date time fields.
        /// </summary>
        /// <param name="year">Year value yyyy</param>
        /// <param name="month">Month value 1-12</param>
        /// <param name="day">Day value 1-31</param>
        /// <param name="hour">Hour 0-23</param>
        /// <param name="min">Minutes 0-59</param>
        /// <param name="sec">Seconds 0-59</param>
        //============================================================================
        public TTimeValue(int year, uint month, uint day, uint hour, uint min, double sec)
        {
            setDate(dateTimeToJD(year, month, day, hour, min, sec));
        }
        //============================================================================
        /// <summary>
        /// Returns the integer value closest to zero of val.
        /// </summary>
        /// <param name="val">Value to truncate.</param>
        /// <returns>The integer value as a double.</returns>
        //============================================================================
        protected double INT(double val)
        {
            if (val > 0)
                return Math.Floor(val);
            else
                return Math.Ceiling(val);

        }
        //============================================================================
        /// <summary>
        /// Converts a y m d to a Julian date. Assumes midday.
        /// </summary>
        /// <param name="y">Year</param>
        /// <param name="m">Month</param>
        /// <param name="d">Day</param>
        /// <returns>Julian date</returns>
        //============================================================================
        protected double dateToJD(int y, uint m, double d)
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
                a = INT(y / 100);
                b = 2 - a + INT(a / 4);
            }

            JD = b + INT(365.25 * y) + INT(30.6001 * (m + 1)) + d + 1720994.5;

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
        protected double dateTimeToJD(int y, uint m, uint d, uint hr, uint min, double sec)
        {
            double partDay = d + (hr / 24.0) + (min / 1440.0) + (sec / 86400.0);
            return dateToJD(y, m, partDay);
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
        public void JDToDateTime(ref int yr, ref uint mon, ref uint day, ref uint hr, ref uint min, ref double sec, double JD)
        {
            double a, b, c, d, e, f, z, alpha, decDay;
            Double decHr, decMin;

            JD += 0.5;
            z = INT(JD); //store int part of JD
            f = JD - z;    //store the frac part of JD
            if (z < 2299161)
                a = z;
            else
            {
                alpha = INT((z - 1867216.25) / 36524.25);
                a = z + 1 + alpha - INT(alpha / 4);
            }
            b = a + 1524;
            c = INT((b - 122.1) / 365.25);
            d = INT(365.25 * c);
            e = INT((b - d) / 30.6001);

            decDay = b - d - INT(30.6001 * e) + f;
            if (e < 13.5)
                mon = Convert.ToUInt32(e - 1);
            else
                mon = Convert.ToUInt32(e - 13);

            if (mon > 2)
                yr = Convert.ToInt32(c - 4716);
            else
                yr = Convert.ToInt32(c - 4715);

            //convert decDay to d,hr,min,sec
            day = Convert.ToUInt32(INT(decDay));
            decHr = (decDay - day) * 24;
            hr = Convert.ToUInt32(INT(decHr));
            decMin = (decHr - hr) * 60;
            min = Convert.ToUInt32(INT(decMin));
            sec = (decMin - min) * 60;  //decimal seconds
        }
        //============================================================================
        /// <summary>
        /// Sets the object's date fields from a Julian date
        /// </summary>
        /// <param name="JD">True Julian date</param>
        //============================================================================
        public void setDate(double JD)
        {
            double decSec;

            //convert the Julian date to local class variable requirements
            iDay = Convert.ToInt32(INT(JD + 0.5));   //always rounds up when frac part >= 0.5 (past midnight)

            decSec = ((JD + 0.5) - iDay) * 86400.0;
            iSec = Convert.ToUInt32(Math.Floor(decSec));
            dSecPart = decSec - iSec;
        }
        //============================================================================
        /// <summary>
        /// Set the object's date fields from a DateTime value
        /// </summary>
        /// <param name="dDateTime">The DateTime value to initialise this instance with.</param>
        //============================================================================ 
        public void setFromDateTime(DateTime dDateTime)
        {
            setDate(dateTimeToJD(dDateTime.Year, (uint)dDateTime.Month, (uint)dDateTime.Day, (uint)dDateTime.Hour, (uint)dDateTime.Minute, dDateTime.Second));
        }
        //============================================================================
        /// <summary>
        /// Greater than test.
        /// </summary>
        /// <param name="op1">Value to compare</param>
        /// <param name="op2">Value to compare</param>
        /// <returns>True if lhs value is greater than the rhs</returns>
        //============================================================================
        public static bool operator >(TTimeValue op1, TTimeValue op2)
        {
            bool result = false;

            if (op1.iDay > op2.iDay)
                result = true;
            else
            {
                if ((op1.iDay == op2.iDay) && ((op1.iSec + op1.dSecPart) > (op2.iSec + op2.dSecPart)))
                    result = true;
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Less than test.
        /// </summary>
        /// <param name="op1">Value to compare</param>
        /// <param name="op2">Value to compare</param>
        /// <returns>True if lhs value is smaller than the rhs</returns>
        //============================================================================
        public static bool operator <(TTimeValue op1, TTimeValue op2)
        {
            bool result = false;

            if (op1.iDay < op2.iDay)
                result = true;
            else
            {
                if ((op1.iDay == op2.iDay) && ((op1.iSec + op1.dSecPart) < (op2.iSec + op2.dSecPart)))
                    result = true;
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Accuracy to 1000th of a second.
        /// </summary>
        /// <param name="op1"></param>
        /// <param name="op2"></param>
        /// <returns>True if lhs >= rhs</returns>
        //============================================================================
        public static bool operator >=(TTimeValue op1, TTimeValue op2)
        {
            bool result = false;

            if (op1.iDay > op2.iDay)
                result = true;
            else
            {
                if (op1.iDay == op2.iDay)
                {
                    if (((op1.iSec + op1.dSecPart) - (op2.iSec + op2.dSecPart)) < 0.001)
                        result = true;
                }
            }
            return result;
        }

        //============================================================================
        /// <summary>
        /// Accuracy to 1000th of a second.
        /// </summary>
        /// <param name="op1"></param>
        /// <param name="op2"></param>
        /// <returns>true if the lhs lt or equal to rhs</returns>
        //============================================================================
        public static bool operator <=(TTimeValue op1, TTimeValue op2)
        {
            bool result = false;

            if (op1.iDay < op2.iDay)
                result = true;
            else
            {
                if (op1.iDay == op2.iDay)
                {
                    if (((op2.iSec + op2.dSecPart) - (op1.iSec + op1.dSecPart)) < 0.001)
                        result = true;
                }
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Compare equality. Accuracy to 1000th of a second.
        /// </summary>
        /// <param name="op1"></param>
        /// <param name="op2"></param>
        /// <returns>True if the values are equal.</returns>
        //============================================================================
        public static bool operator ==(TTimeValue op1, TTimeValue op2)
        {
            return ((op1.iDay == op2.iDay) && (Math.Abs((op1.iSec + op1.dSecPart) - (op2.iSec + op2.dSecPart)) < 0.001));
        }

        //============================================================================
        /// <summary>
        /// Test inequality. Accuracy to 1000th of a second.
        /// </summary>
        /// <param name="op1"></param>
        /// <param name="op2"></param>
        /// <returns></returns>
        //============================================================================
        public static bool operator !=(TTimeValue op1, TTimeValue op2)
        {
            bool result = false;

            if (op1.iDay != op2.iDay)
                result = true;
            else
            {
                if (Math.Abs((op1.iSec + op1.dSecPart) - (op2.iSec + op2.dSecPart)) > 0.001)
                    result = true;
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Test for equality.
        /// </summary>
        /// <param name="obj"></param>
        /// <returns>True if the objects are equal.</returns>
        //============================================================================
        public override bool Equals(object obj)
        {
            if (!(obj is TTimeValue))
            {
                return false;
            }
            else
            {
                return this == (TTimeValue)obj;
            }
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        //============================================================================
        public override int GetHashCode()
        {
            return Convert.ToInt32(asJulianDate());
        }
        //============================================================================
        /// <summary>
        /// Advance the TimeValue's date by the number of years.
        /// </summary>
        /// <param name="years">Number of years to increment/decrement.</param>
        //============================================================================
        public void advYear(int years)
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            DateTime dt = new DateTime((int)yr, (int)mon, (int)day, (int)hr, (int)min, (int)sec, (int)((int)sec - sec) * 1000);
            DateTime newdt = dt.AddYears(years);
            setDate(dateTimeToJD(newdt.Year, (uint)newdt.Month, (uint)newdt.Day, (uint)newdt.Hour, (uint)newdt.Minute, (double)newdt.Second));
        }
        //============================================================================
        /// <summary>
        /// Advance the TimeValue's date by this number of months.
        /// </summary>
        /// <param name="months">The number of months to increment/decrement by.</param>
        //============================================================================
        public void advMonth(int months)
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            DateTime dt = new DateTime((int)yr, (int)mon, (int)day, (int)hr, (int)min, (int)sec, (int)((int)sec - sec) * 1000);
            DateTime newdt = dt.AddMonths(months);
            setDate(dateTimeToJD(newdt.Year, (uint)newdt.Month, (uint)newdt.Day, (uint)newdt.Hour, (uint)newdt.Minute, (double)newdt.Second));
        }
        //============================================================================
        /// <summary>
        /// Advance the TimeValue's date by this number of days.
        /// </summary>
        /// <param name="days">The number of days to increment/decrement by.</param>
        //============================================================================
        public void advDay(int days)
        {
            iDay = iDay + days;
        }
        //============================================================================
        /// <summary>
        /// Advance the TimeValue's date by this number of seconds.
        /// </summary>
        /// <param name="secs">Amount of seconds to increment/decrement by.</param>
        //============================================================================
        public void advSec(double secs)
        {
            double JD;  //true Julian date

            JD = asJulianDate();
            JD += secs / 86400.0;   //add the difference

            //store the date to local object date values
            setDate(JD);
        }
        //============================================================================
        /// <summary>
        /// General purpose function used to advance the time value by a certain amount.
        /// </summary>
        /// <param name="amount">Increment amount</param>
        /// <param name="unit">Unit - (specified constants  SEC, MIN, HR, DAY, MON, YR)</param>
        //============================================================================
        public void advTime(double amount, int unit)
        {
            switch (unit)
            {
                case SEC: advSec(amount);
                    break;
                case MIN: advSec(amount * 60.0);
                    break;
                case HR: advSec(amount * 3600.0);
                    break;
                case DAY: advDay(Convert.ToInt32(amount));
                    break;
                case MON: advMonth(Convert.ToInt32(amount));
                    break;
                case YR: advYear(Convert.ToInt32(amount));
                    break;
            }
        }

        //============================================================================
        /// <summary>
        /// Returns the date time values for this object.
        /// </summary>
        /// <param name="yr">Year</param>
        /// <param name="mon">Month</param>
        /// <param name="day">Day</param>
        /// <param name="hr">Hour</param>
        /// <param name="min">Minute</param>
        /// <param name="sec">Decimal seconds</param>
        //============================================================================
        public void asDateTime(ref int yr, ref uint mon, ref uint day, ref uint hr, ref uint min, ref double sec)
        {
            JDToDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec, asJulianDate());
        }
        //============================================================================
        /// <summary>
        /// Returns this TTimeValue as a DateTime value.
        /// </summary>
        /// <returns>The DateTime value of this instance.</returns>
        //============================================================================
        public DateTime asDateTime()
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            DateTime dt = new DateTime((int)yr, (int)mon, (int)day, (int)hr, (int)min, (int)sec, (int)((int)sec - sec) * 1000);
            return dt;
        }
        //============================================================================
        /// <summary>
        /// Construct a date time string from this TTimeValue's date value
        /// </summary>
        /// <returns>Formatted date time string e.g. dd/mm/yyyy hh:mm:sec.00</returns>
        //============================================================================
        public string asDateTimeStr()
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;
            String buf;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            buf = String.Format("{0, 2}/{1, 2}/{2, 4:d4} {3, 02:d2}:{4, 02:d2}:{5, 2:f2}", day, mon, yr, hr, min, sec);

            return buf;
        }
        //============================================================================
        /// <summary>
        /// Construct a date time string from this TTimeValue's date value
        /// </summary>
        /// <returns>Formatted date time string e.g. yyyy/mm/dd hh:mm:sec.00</returns>
        //============================================================================
        public string asISODateTimeStr()
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;
            String buf;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            buf = String.Format("{0, 4:d4}/{1, 2}/{2, 2} {3, 02:d2}:{4, 02:d2}:{5, 2:f2}", yr, mon, day, hr, min, sec);

            return buf;
        }
        //============================================================================
        /// <summary>
        /// See <see cref="asDateTimeStr">asDateTimeStr()</see>
        /// </summary>
        /// <returns>Formatted date time string e.g. dd/mm/yyyy hh:mm:sec.00</returns>
        //============================================================================
        public override string ToString()
        {
            return asDateTimeStr();
        }
        //============================================================================
        /// <summary>
        /// Constructs a formatted date string.
        /// </summary>
        /// <returns>Formatted date string e.g. dd/mm/yyyy</returns>
        //============================================================================
        public string asDateStr()
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;
            String buf;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            buf = String.Format("{0, 2}/{1, 2}/{2, 4:d4}", day, mon, yr);

            return buf;
        }
        //============================================================================
        /// <summary>
        /// Constructs a formatted date string.
        /// </summary>
        /// <returns>Formatted date string e.g. dd/mm/yyyy</returns>
        //============================================================================
        public string asISODateStr()
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;
            String buf;

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);
            buf = String.Format("{0, 4:d4}/{1, 2}/{2, 2}", yr, mon, day);

            return buf;
        }
        //============================================================================
        /// <summary>
        /// Converts the objects date value to a true Julian Date
        /// </summary>
        /// <returns>The true Julian Date</returns>
        //============================================================================
        public double asJulianDate()
        {
            return (iDay - 0.5) + (iSec + dSecPart) / 86400.0;
        }
        //============================================================================
        /// <summary>
        /// Calculates the day of the year. Jan 1 = day 1
        /// </summary>
        /// <returns>The day of the year</returns>
        //============================================================================
        public uint dayOfYear()
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            double sec = 0;
            double jan1; //JD of Jan 1 this year

            asDateTime(ref yr, ref mon, ref day, ref hr, ref min, ref sec);      //get the y m d of current date

            //adjust the m d to Jan 1
            mon = 1;
            day = 1;

            jan1 = dateTimeToJD(yr, mon, day, hr, min, sec);   //get the JD of Jan 1
            //find the diff between JD's
            return Convert.ToUInt32(INT(asJulianDate() - jan1) + 1);
        }
        //============================================================================
        /// <summary>
        /// Initialise this object using an Ansi formatted date time string
        /// Expects date to be formatted with '/' delimiters between yyyy mm dd.
        /// </summary>
        /// <param name="sDateTime">Date time string formatted as: yyyy/mm/dd [hh:mm] (time optional)</param>
        /// <returns>True if the conversion succeeds</returns>
        //============================================================================
        public bool setFromAnsiDate(String sDateTime)
        {
            return setFromAnsiDate(sDateTime, "/");
        }
        //============================================================================
        /// <summary>
        /// Initialise this object using an Ansi formatted date time string
        /// Expects date to be formatted correctly.
        /// </summary>
        /// <param name="sDateTime">Date time string formatted as: yyyy/mm/dd [hh:mm] (time optional)</param>
        /// <param name="delim">Delimiter used between yyyy mm dd</param>
        /// <returns>True if the conversion succeeds</returns>
        //============================================================================
        public bool setFromAnsiDate(String sDateTime, String delim)
        {
            int yr = 0;
            uint mon = 0, day = 0, hr = 0, min = 0;
            bool result = false;

            String expr = @"\b(?<Year>\d{4})" + delim + @"(?<Month>\d{1,2})" + delim + @"(?<Day>\d{1,2})\s(?<Hour>\d{1,2}):(?<Min>\d{1,2})|\b(?<Year>\d{4})" + delim + @"(?<Month>\d{1,2})" + delim + @"(?<Day>\d{1,2})";
            Regex dateExp = new Regex(expr);

            Match m = dateExp.Match(sDateTime);
            if (m.Success)
            {
                GroupCollection gc = m.Groups;
                if (gc.Count > 0)
                {
                    yr = Convert.ToInt32(gc["Year"].Captures[0].Value);
                    mon = Convert.ToUInt32(gc["Month"].Captures[0].Value);
                    day = Convert.ToUInt32(gc["Day"].Captures[0].Value);
                    if (gc["Hour"].Captures.Count > 0)
                        hr = Convert.ToUInt32(gc["Hour"].Captures[0].Value);
                    if (gc["Min"].Captures.Count > 0)
                        min = Convert.ToUInt32(gc["Min"].Captures[0].Value);
                }
                setDate(dateTimeToJD(yr, mon, day, hr, min, 0));
                result = true;
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Set the value of this object from another of the same type.
        /// </summary>
        /// <param name="init"></param>
        //============================================================================
        public void Set(TTimeValue init)
        {
            iDay = init.iDay;
            iSec = init.iSec;
            dSecPart = init.dSecPart;
        }
        //============================================================================
        /// <summary>
        /// Set the value of this object.
        /// </summary>
        /// <param name="day">Julian day number</param>
        /// <param name="sec">Seconds of the day past midnight</param>
        /// <param name="secPart">Fractional seconds</param>
        //============================================================================
        public void Set(int day, uint sec, double secPart)
        {
            iDay = day;
            iSec = sec;
            dSecPart = secPart;
        }
        //============================================================================
        /// <summary>
        /// Gets the Julian day number
        /// </summary>
        /// <returns></returns>
        //============================================================================
        public int getDay()
        {
            return iDay;
        }
        //============================================================================
        /// <summary>
        /// Gets the sec part of the day after midnight
        /// </summary>
        /// <returns></returns>
        //============================================================================
        public uint getSec()
        {
            return iSec;
        }
        //============================================================================
        /// <summary>
        /// Gets the fractional part of seconds
        /// </summary>
        /// <returns></returns>
        //============================================================================
        public double getSecPart()
        {
            return dSecPart;
        }

    }
}
