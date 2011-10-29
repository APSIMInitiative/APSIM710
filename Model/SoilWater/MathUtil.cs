﻿using System;
using System.Collections.Generic;
using System.Text;


   class mu
      {


      static public double error_margin(double Variable)
         {
/*
         double margin_val;
         //error margin = size of the variable mutiplied by error in the number 0. 
         margin_val = Math.Abs(Variable) * double.Epsilon; 
         //if Variable was zero hence error margin is now zero
         if (margin_val == 0.0)
            {
            //set the error margin to the error in the number 0 by Operating System
            margin_val = Double.Epsilon;
            }
         return margin_val;
 */
         return 0.0001;
         }


     static public bool reals_are_equal(double First, double Second)
         {
         double tolerance = error_margin(Second);
         if (Math.Abs(First - Second) <= tolerance)
            {
            return true;
            }
         else
            {
            return false;
            }
         } 

      static public bool reals_are_equal(double First, double Second, double Tolerance)
         {
         if (Math.Abs(First - Second) <= Math.Abs(Tolerance))
            {
            return true;
            }
         else
            {
            return false;
            }
         }


      static public double divide(double Dividend, double Divisor, double Default)
         {
         //!+ Purpose
         //!       Divides one number by another.  If the divisor is zero or overflow
         //!       would occur a specified default is returned.  If underflow would
         //!       occur, nought is returned.

         //!+  Definition
         //!     Returns (dividend / divisor) if the division can be done
         //!     without overflow or underflow.  If divisor is zero or
         //!     overflow would have occurred, default is returned.  If
         //!     underflow would have occurred, zero is returned.

         //!+ Assumptions
         //!       largest/smallest real number is 1.0e+/-30
         double largest = 1.0 * Math.Exp(30);
         double nought = 0.0;
         double smallest = 1.0 * Math.Exp(-30);
         double quotient; 

         if (mu.reals_are_equal(Divisor, nought)) 
            {
            quotient = Default;
            }
         else if (mu.reals_are_equal(Dividend, nought))
            {
            quotient = nought;
            }
         else if (Math.Abs(Divisor) < 1.0) //if the divisor is not large
            {
            //if the denominator is really small -> numerator is still larger then a very big number x denominator.
            //divide by very small number = infinity -> so set to default.
            if (Math.Abs(Dividend) > Math.Abs(largest*Divisor))
               {
               quotient = Default;
               }
            else
               {
               quotient = Dividend/Divisor;
               }
            }
         else if (Math.Abs(Divisor) > 1.0) //if the divisor is not small
            {
            //if the denominator is really large -> numerator is still smaller then a really small number x denominator
            //divide by a really large number = zero (eg. 1/infinity)-> so set it to zero 
            if (Math.Abs(Dividend) < Math.Abs(smallest*Divisor))
               {
               quotient = nought;
               }
            else
               {
               quotient = Dividend/Divisor;
               }
            }
         else
            {
            quotient = Dividend/Divisor;
            }

         return quotient;

         }


      static public double bound(double A, double MinVal, double MaxVal)
         {
         //force A to stay between the MinVal and the MaxVal. Set A to the MaxVal or MinVal if it exceeds them.
         if (MinVal > MaxVal)
            {
            ApsimUtil.warning_error("Lower bound " + MinVal + " is > upper bound " + MaxVal + Environment.NewLine 
                               + "        Variable is not constrained");
            return A;
            }

         double temp;
         temp = u_bound(A, MaxVal);
         temp = l_bound(temp, MinVal);
         return temp;
         }

      static public double l_bound(double A, double MinVal)
         {
         //force A to stay above the MinVal. Set A to MinVal if A is below it.
         return Math.Max(A, MinVal);
         }

      static public double u_bound(double A, double MaxVal)
         {
          //force A to stay below the MaxVal. Set A to MaxVal if A is above it.
         return Math.Min(A, MaxVal);
         }


      static public double round_to_zero(double var)
         {
         double close_enough_to_zero = 1.0 * Math.Exp(-15);

         if (Math.Abs(var) <= close_enough_to_zero)
            {
            return 0;
            }
         else
            {
            return var;
            }
         }


      public const double mm2m = 1.0/1000.0;          //! conversion of mm to m

      public const double sm2smm  = 1000000.0;       //! conversion of square metres to square mm

 
      }

