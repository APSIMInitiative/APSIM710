using System;
using System.Collections;
using System.Collections.Generic;

namespace CSGeneral
   {
   /// <summary>
   /// Various math utilities.
   /// </summary>
   public class MathUtility
      {
      //------------------------------------------------
      // Returns true if specified value is 'missing'
      // -----------------------------------------------
      public static double MissingValue
         {
         get { return 999999; }
         }

      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static bool FloatsAreEqual(double value1, double value2)
         {
         return FloatsAreEqual(value1, value2, 0.00001);
         }
      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static bool FloatsAreEqual(double value1, double value2, double tolerance)
         {
         return (Math.Abs(value1 - value2) < tolerance);
         }
      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static double[] Multiply(double[] value1, double[] value2)
         {
         double[] results = new double[value1.Length];
         if (value1.Length == value2.Length)
            {
            results = new double[value1.Length];
            for (int iIndex = 0; iIndex < value1.Length; iIndex++)
               {
               if (value1[iIndex] == MissingValue || value2[iIndex] == MissingValue)
                  results[iIndex] = MissingValue;
               else
                  results[iIndex] = (value1[iIndex] * value2[iIndex]);
               }
            }
         return results;
         }
      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static double[] Multiply_Value(double[] value1, double value2)
         {
         double[] results = null;
         results = new double[value1.Length];
         for (int iIndex = 0; iIndex < value1.Length; iIndex++)
            {
            if (value1[iIndex] == MissingValue)
               results[iIndex] = MissingValue;
            else
               results[iIndex] = (value1[iIndex] * value2);
            }
         return results;
         }
      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static double[] Divide(double[] value1, double[] value2)
         {
         double[] results = null;
         if (value1.Length == value2.Length)
            {
            results = new double[value1.Length];
            for (int iIndex = 0; iIndex < value1.Length; iIndex++)
               {
               if (value1[iIndex] == MissingValue || value2[iIndex] == MissingValue)
                  results[iIndex] = MissingValue;
               else if (value2[iIndex] != 0)
                  {
                  results[iIndex] = (value1[iIndex] / value2[iIndex]);
                  }
               else
                  {
                  results[iIndex] = value1[iIndex];
                  }
               }
            }
         return results;
         }
      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static double[] Divide_Value(double[] value1, double value2)
         {
         double[] results = new double[value1.Length];
         //Avoid divide by zero problems
         if (value2 != 0)
            {
            for (int iIndex = 0; iIndex < value1.Length; iIndex++)
               {
               if (value1[iIndex] == MissingValue)
                  results[iIndex] = MissingValue;
               else
                  results[iIndex] = (value1[iIndex] / value2);
               }
            }
         else
            {
            results = value1;
            }
         return results;
         }


      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static double[] Add_Value(double[] value1, double value2)
         {
         double[] results = new double[value1.Length];
         for (int iIndex = 0; iIndex < value1.Length; iIndex++)
            {
            if (value1[iIndex] == MissingValue)
               results[iIndex] = MissingValue;
            else
               results[iIndex] = (value1[iIndex] + value2);
            }
         return results;
         }

      //-------------------------------------------------------------------------
      //
      //-------------------------------------------------------------------------
      public static double[] Subtract_Value(double[] value1, double value2)
         {
         double[] results = new double[value1.Length];
         for (int iIndex = 0; iIndex < value1.Length; iIndex++)
            {
            if (value1[iIndex] == MissingValue)
               results[iIndex] = MissingValue;
            else
               results[iIndex] = (value1[iIndex] - value2);
            }
         return results;
         }
      //-------------------------------------------------------------------------
      // Sum an array of numbers 
      //-------------------------------------------------------------------------
      public static double Sum(IEnumerable Values)
         {
         return Sum(Values, 0, 0, 0.0);
         }

      //-------------------------------------------------------------------------
      // Sum an array of numbers starting at startIndex up to (but not including) endIndex
      // beginning with an initial value
      //-------------------------------------------------------------------------
      public static double Sum(IEnumerable Values, int iStartIndex, int iEndIndex,
                              double dInitialValue)
         {
         double result = dInitialValue;
         if (iStartIndex < 0)
            {
            throw new Exception("MathUtility.Sum: End index or start index is out of range");
            }
         int iIndex = 0;
         foreach (double Value in Values)
            {
            if ((iStartIndex == 0 && iEndIndex == 0) ||
               (iIndex >= iStartIndex && iIndex < iEndIndex) && Value != MissingValue)
               result += Value;
            iIndex++;
            }

         return result;
         }
      //-------------------------------------------------------------------------
      //Linearly interpolates a value y for a given value x and a given
      //set of xy co-ordinates.
      //When x lies outside the x range_of, y is set to the boundary condition.
      //Returns true for Did_interpolate if interpolation was necessary.
      //-------------------------------------------------------------------------
      public static double LinearInterpReal(double dX, double[] dXCoordinate, double[] dYCoordinate, ref bool bDidInterpolate)
         {
         bDidInterpolate = false;
         if (dXCoordinate == null || dYCoordinate == null)
            return 0;
         //find where x lies in the x coordinate
         if (dXCoordinate.Length == 0 || dYCoordinate.Length == 0 || dXCoordinate.Length != dYCoordinate.Length)
            {
            throw new Exception("MathUtility.LinearInterpReal: Lengths of passed in arrays are incorrect");
            }

         for (int iIndex = 0; iIndex < dXCoordinate.Length; iIndex++)
            {
            if (dX <= dXCoordinate[iIndex])
               {
               //Chcek to see if dX is exactly equal to dXCoordinate[iIndex]
               //If so then don't calcuate dY.  This was added to remove roundoff error.
               if (dX == dXCoordinate[iIndex])
                  {
                  bDidInterpolate = false;
                  return dYCoordinate[iIndex];
                  }
               //Found position
               else if (iIndex == 0)
                  {
                  bDidInterpolate = true;
                  return dYCoordinate[iIndex];
                  }
               else
                  {
                  //interpolate - y = mx+c
                  if ((dXCoordinate[iIndex] - dXCoordinate[iIndex - 1]) == 0)
                     {
                     bDidInterpolate = true;
                     return dYCoordinate[iIndex - 1];
                     }
                  else
                     {
                     bDidInterpolate = true;
                     return ((dYCoordinate[iIndex] - dYCoordinate[iIndex - 1]) / (dXCoordinate[iIndex] - dXCoordinate[iIndex - 1]) * (dX - dXCoordinate[iIndex - 1]) + dYCoordinate[iIndex - 1]);
                     }
                  }
               }
            else if (iIndex == (dXCoordinate.Length - 1))
               {
               bDidInterpolate = true;
               return dYCoordinate[iIndex];
               }
            }// END OF FOR LOOP
         return 0.0;
         }

      static public double Constrain(double dLowerLimit, double dUpperLimit, double dValue)
         {
         double dConstrainedValue = 0.0;
         dConstrainedValue = Math.Min(dUpperLimit, Math.Max(dLowerLimit, dValue));
         return dConstrainedValue;
         }

      static public double Round(double Value, int NumDecPlaces)
         // rounds properly rather than the math.round function.
         // e.g. 3.4 becomes 3.0
         //      3.5 becomes 4.0
         {
         double Multiplier = Math.Pow(10.0, NumDecPlaces);  // gives 1 or 10 or 100 for decplaces=0, 1, or 2 etc
         Value = Math.Truncate(Value * Multiplier + 0.5);
         return Value / Multiplier;
         }

      static public double[] Round(double[] Values, int NumDecPlaces)
         // rounds properly rather than the math.round function.
         // e.g. 3.4 becomes 3.0
         //      3.5 becomes 4.0
         {
         double ExtraBit = 1.0 / Math.Pow(10.0, NumDecPlaces);  // gives 0.1 or 0.01 or 0.001 etc
         for (int i = 0; i != Values.Length; i++)
            Values[i] = Round(Values[i], NumDecPlaces);
         return Values;
         }

      static public bool AreEqual(double[] Values1, double[] Values2)
         {
         // Return true if the 2 arrays of numbers are equal.
         if (Values1.Length == Values2.Length)
            {
            for (int i = 0; i < Values1.Length; i++)
               {
               if (!MathUtility.FloatsAreEqual(Values1[i], Values2[i]))
                  return false;
               }
            }
         else
            return false;
         return true;
         }

      // ---------------------------------------------
      // Reverse the contents of the specified array.
      // ---------------------------------------------
      static public double[] Reverse(double[] Values)
         {
         double[] ReturnValues = new double[Values.Length];

         int Index = 0;
         for (int Layer = Values.Length - 1; Layer >= 0; Layer--)
            {
            ReturnValues[Index] = Values[Layer];
            Index++;
            }
         return ReturnValues;
         }

      static public bool ValuesInArray(double[] Values)
         {
         if (Values != null)
            {
            foreach (double Value in Values)
               {
               if (Value != MathUtility.MissingValue)
                  return true;
               }
            }
         return false;
         }
      static public bool ValuesInArray(string[] Values)
         {
         if (Values != null)
            {
            foreach (string Value in Values)
               {
               if (Value != "")
                  return true;
               }
            }
         return false;
         }
      // --------------------------------------------------
      // Convert an array of strings to an array of doubles
      // --------------------------------------------------
      static public double[] StringsToDoubles(string[] Values)
         {
         double[] ReturnValues = new double[Values.Length];

         for (int Index = 0; Index != Values.Length; Index++)
            {
            if (Values[Index] == "")
               ReturnValues[Index] = MathUtility.MissingValue;
            else
               ReturnValues[Index] = Convert.ToDouble(Values[Index]);
            }
         return ReturnValues;
         }

      static public double[] ProbabilityDistribution(int NumPoints, bool Exceed)
         {
         double[] Probability = new double[NumPoints];

         for (int x = 1; x <= NumPoints; x++)
            Probability[x - 1] = (x - 0.5) / NumPoints * 100;

         if (Exceed)
            Array.Reverse(Probability);
         return Probability;
         }


      public class RegrStats
         {
         public int n;
         public double m;
         public double c;
         public double SEslope;
         public double SEcoeff;
         public double R2;
         public double ADJR2;
         public double R2YX;
         public double VarRatio;
         public double RMSD;
         };

      static public RegrStats CalcRegressionStats(List<double> X, List<double> Y)
         {
         // ------------------------------------------------------------------
         //    Calculate regression stats.   
         // ------------------------------------------------------------------
         RegrStats stats = new RegrStats();
         double SumX = 0;
         double SumY = 0;
         double SumXY = 0;
         double SumX2 = 0;
         double SumY2 = 0;
         double SumXYdiff2 = 0;
         double CSSX, CSSXY;
         double Xbar, Ybar;
         double TSS, TSSM;
         double REGSS, REGSSM;
         double RESIDSS, RESIDSSM;
         double S2;

         stats.n = 0;
         stats.m = 0.0;
         stats.c = 0.0;
         stats.SEslope = 0.0;
         stats.SEcoeff = 0.0;
         stats.R2 = 0.0;
         stats.ADJR2 = 0.0;
         stats.R2YX = 0.0;
         stats.VarRatio = 0.0;
         stats.RMSD = 0.0;
         int Num_points = X.Count;

         if (Num_points > 1)
            {

            for (int Point = 0;
                 Point < Num_points;
                 Point++)
               {
               SumX = SumX + X[Point];
               SumX2 = SumX2 + X[Point] * X[Point];       // SS for X
               SumY = SumY + Y[Point];
               SumY2 = SumY2 + Y[Point] * Y[Point];       // SS for y
               SumXY = SumXY + X[Point] * Y[Point];       // SS for products
               }
            Xbar = SumX / Num_points;
            Ybar = SumY / Num_points;

            CSSXY = SumXY - SumX * SumY / Num_points;     // Corrected SS for products
            CSSX = SumX2 - SumX * SumX / Num_points;      // Corrected SS for X
            stats.n = Num_points;
            stats.m = CSSXY / CSSX;                             // Calculate slope
            stats.c = Ybar - stats.m * Xbar;                          // Calculate intercept

            TSS = SumY2 - SumY * SumY / Num_points;       // Corrected SS for Y = Sum((y-ybar)^2)
            TSSM = TSS / (Num_points - 1);                // Total mean SS
            REGSS = stats.m * CSSXY;                            // SS due to regression = Sum((yest-ybar)^2)
            REGSSM = REGSS;                               // Regression mean SS
            RESIDSS = TSS - REGSS;                        // SS about the regression = Sum((y-yest)^2)

            if (Num_points > 2)                           // MUST HAVE MORE THAN TWO POINTS FOR REG
               RESIDSSM = RESIDSS / (Num_points - 2);     // Residual mean SS, variance of residual
            else
               RESIDSSM = 0.0;

            stats.RMSD = Math.Sqrt(RESIDSSM);                        // Root mean square deviation
            stats.VarRatio = REGSSM / RESIDSSM;                  // Variance ratio - for F test (1,n-2)
            stats.R2 = 1.0 - (RESIDSS / TSS);                   // Unadjusted R2 calculated from SS
            stats.ADJR2 = 1.0 - (RESIDSSM / TSSM);              // Adjusted R2 calculated from mean SS
            if (stats.ADJR2 < 0.0)
               stats.ADJR2 = 0.0;
            S2 = RESIDSSM;                                // Resid. MSS is estimate of variance
            // about the regression
            stats.SEslope = Math.Sqrt(S2) / Math.Sqrt(CSSX);              // Standard errors estimated from S2 & CSSX
            stats.SEcoeff = Math.Sqrt(S2) * Math.Sqrt(SumX2 / (Num_points * CSSX));

            // Statistical parameters of Butler, Mayer and Silburn

            stats.R2YX = 1.0 - (SumXYdiff2 / TSS);              // If you are on the 1:1 line then R2YX=1

            // If R2YX is -ve then the 1:1 line is a worse fit than the line y=ybar

            //      MeanAbsError = SumXYdiff / Num_points;
            //      MeanAbsPerError = SumXYDiffPer / Num_points;  // very dangerous when y is low
            // could use MeanAbsError over mean
            }
         return stats;
         }

      /// <summary>
      /// Return the time elasped in hours between the specified sun angle
      ///  from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.
      /// </summary>
      /// <param name="SunAngle">Angle to measure time between such as twilight (deg).
      ///  angular distance between 90 deg and end of twilight - altitude of sun. +ve up, -ve down.</param>
      static public double DayLength(double DayOfYear, double SunAngle, double Latitude)
         {
         //+ Constant Values
         const double aeqnox = 82.25;   //  average day number of autumnal equinox
         const double pi = 3.14159265359;
         const double dg2rdn = (2.0 * pi) / 360.0; // convert degrees to radians
         const double decsol = 23.45116 * dg2rdn; // amplitude of declination of sun
         //   - declination of sun at solstices.
         // cm says here that the maximum
         // declination is 23.45116 or 23 degrees
         // 27 minutes.
         // I have seen else_where that it should
         // be 23 degrees 26 minutes 30 seconds -
         // 23.44167
         const double dy2rdn = (2.0 * pi) / 365.25; // convert days to radians
         const double rdn2hr = 24.0 / (2.0 * pi); // convert radians to hours

         //+ Local Variables
         double alt;// twilight altitude limited to max/min
         //   sun altitudes end of twilight
         //   - altitude of sun. (radians)
         double altmn;// altitude of sun at midnight
         double altmx;// altitude of sun at midday
         double clcd;// cos of latitude * cos of declination
         double coshra;// cos of hour angle - angle between the
         //   sun and the meridian.
         double dec;// declination of sun in radians - this
         //   is the angular distance at solar
         //   noon between the sun and the equator.
         double hrangl;// hour angle - angle between the sun
         //   and the meridian (radians).
         double hrlt;// day_length in hours
         double latrn;// latitude in radians
         double slsd;// sin of latitude * sin of declination
         double sun_alt;// angular distance between
         // sunset and end of twilight - altitude
         // of sun. (radians)
         // Twilight is defined as the interval
         // between sunrise or sunset and the
         // time when the true centre of the sun
         // is 6 degrees below the horizon.
         // Sunrise or sunset is defined as when
         // the true centre of the sun is 50'
         // below the horizon.

         sun_alt = SunAngle * dg2rdn;

         // calculate daylangth in hours by getting the
         // solar declination (radians) from the day of year, then using
         // the sin and cos of the latitude.

         // declination ranges from -.41 to .41 (summer and winter solstices)

         dec = decsol * Math.Sin(dy2rdn * (DayOfYear - aeqnox));

         // get the max and min altitude of sun for today and limit
         // the twilight altitude between these.

         if (MathUtility.FloatsAreEqual(Math.Abs(Latitude), 90.0))
            {
            coshra = Sign(1.0, -dec) * Sign(1.0, Latitude);
            }
         else
            {
            latrn = Latitude * dg2rdn;
            slsd = Math.Sin(latrn) * Math.Sin(dec);
            clcd = Math.Cos(latrn) * Math.Cos(dec);

            altmn = Math.Asin(Math.Min(Math.Max(slsd - clcd, -1.0), 1.0));
            altmx = Math.Asin(Math.Min(Math.Max(slsd + clcd, -1.0), 1.0));
            alt = Math.Min(Math.Max(sun_alt, altmn), altmx);

            // get cos of the hour angle
            coshra = (Math.Sin(alt) - slsd) / clcd;
            coshra = Math.Min(Math.Max(coshra, -1.0), 1.0);
            }

         // now get the hour angle and the hours of light
         hrangl = Math.Acos(coshra);
         hrlt = hrangl * rdn2hr * 2.0;
         return hrlt;
         }

      // ------------------------------------------------------------------
      // Transfer of sign - from FORTRAN.
      // The result is of the same type and kind as a. Its value is the abs(a) of a,
      // if b is greater than or equal positive zero; and -abs(a), if b is less than
      // or equal to negative zero.
      // Example a = sign (30,-2) ! a is assigned the value -30
      // ------------------------------------------------------------------
      static public double Sign(double a, double b)
         {
         if (b >= 0)
            return Math.Abs(a);
         else
            return -Math.Abs(a);
         }

      public static double Min(IEnumerable Values)
         {
         double Minimum = 9999999;
         foreach (double Value in Values)
            Minimum = Math.Min(Value, Minimum);
         return Minimum;
         }

      public static double Max(IEnumerable Values)
         {
         double Maximum = -9999999;
         foreach (double Value in Values)
            Maximum = Math.Max(Value, Maximum);
         return Maximum;
         }
      }
   }
