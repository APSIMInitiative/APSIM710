using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace ManagedComponent.MvOZCOT
{
    public class mvDataFunctions
    {


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

            if (decimals_are_equal(Divisor, nought))
            {
                quotient = Default;
            }
            else if (decimals_are_equal(Dividend, nought))
            {
                quotient = nought;
            }
            else if (Math.Abs(Divisor) < 1.0) //if the divisor is not large
            {
                //if the denominator is really small -> numerator is still larger then a very big number x denominator.
                //divide by very small number = infinity -> so set to default.
                if (Math.Abs(Dividend) > Math.Abs(largest * Divisor))
                {
                    quotient = Default;
                }
                else
                {
                    quotient = Dividend / Divisor;
                }
            }
            else if (Math.Abs(Divisor) > 1.0) //if the divisor is not small
            {
                //if the denominator is really large -> numerator is still smaller then a really small number x denominator
                //divide by a really large number = zero (eg. 1/infinity)-> so set it to zero 
                if (Math.Abs(Dividend) < Math.Abs(smallest * Divisor))
                {
                    quotient = nought;
                }
                else
                {
                    quotient = Dividend / Divisor;
                }
            }
            else
            {
                quotient = Dividend / Divisor;
            }

            return quotient;

        }

        

   //    ===========================================================
      static public double divide_check (double dividend, double divisor, double default_result)
      {
   //    ===========================================================
       
      // Sub-Program Arguments
      //   dividend  (INPUT) dividend
      //   divisor   (INPUT) divisor
      //   default_result   (INPUT) default value if overflow

      // Purpose
      //   Divides one number by another.  If the divisor is zero, or overflow
      //   would occur, a specified default is returned.  If underflow would
      //   occur, nought is returned.

      //  Definition
      //   Returns (dividend / divisor) if the division can be done
      //   without overflow or underflow.  If divisor is zero or
      //   overflow would have occurred, default is returned.  If
      //   underflow would have occurred, zero is returned.

      //  Assumptions
      //   largest/smallest real number is 1.0e+/-30

      //  Changes
      //   310792 jngh specified and programmed original version in Fortran
      //   20070814 dbj recoded in C#

      //  Constant Values
          double largest = 1.0e30;               // largest acceptable no. for quotient
          double nought = 0.0;                   // 0.0
          double smallest = 1.0e-30;             // smallest acceptable no. for quotient

      // Local Variables
          double  quotient;                      // quotient
          double functionReturnValue;

      // Implementation Section ----------------------------------

         functionReturnValue = 0.0;

         if (decimals_are_equal(divisor, nought))     // dividing by 0
                                                     // If divisor is nought, the rest is irrelevant 
                                                     //    - it is undefined
                                                     // and so we return the user-defined default value
          {
              quotient = default_result;
          }
          else if (decimals_are_equal(dividend, nought))   // multiplying by 0
          {
              quotient = nought;
          }
          else if (Math.Abs(divisor) < 1.0)              // possible overflow
          {
              if (Math.Abs(dividend)> Math.Abs(largest*divisor))  // overflow
              {
                  quotient = default_result;
              }
              else                                                // ok
              {
                  quotient = dividend/divisor;
              }
          }
          else if (Math.Abs(divisor) > 1.0)              // possible underflow
          {
             if (Math.Abs(dividend) < Math.Abs(smallest*divisor))  // underflow
             {
                quotient = nought;
             }
             else                                                  // ok
             {
                quotient = dividend/divisor;
             }
          }
          else                                           // ok
          {
              quotient = dividend / divisor;
          }
          functionReturnValue = quotient;

          return functionReturnValue;
      
      }
      // ====================================================================
      static public bool decimals_are_equal(double val1, double val2)
      {

          //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          //      Checks that decimal values are effectively equal
          //ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          bool functionReturnValue = false;
          double default_tolerance = 1.0e-12;

          //- implementation section ----------------------------------

          functionReturnValue = decimals_are_equal(val1,val2,default_tolerance);

          return functionReturnValue;
      }
      // ====================================================================
      static public bool decimals_are_equal(double val1, double val2, double tolerance)
      {
          // Function Arguments
          //    val1  (INPUT) Number to search for
          //    val2  (INPUT) Number to search for
          //    tolerance   (INPUT) difference tolerance

          // Purpose
          //    Returns true if decimal numbers are almost equal

          //  Definition
          //    Returns true if "first" and "second" are almost equal.
          //    "first" and "second" are considered to be almost equal
          //    if ("first"+error_margin("first") >= "second"  .AND.
          //    "first"-error_margin("first") <= "second").


          //  Changes
          //      070994 jngh specified and programmed original version
          //      20070824 dbj converted to C#


          //  Local Variables
          bool functionReturnValue = true;

          //- implementation section ----------------------------------

          if (Math.Abs(val1-val2)<= tolerance)
          {
             functionReturnValue = true;
          }
          else
          {
             functionReturnValue = false;
          }
                
          return functionReturnValue;

      }



      //starting index or soil index for arrays  
      public const int si = 0;

      //less than si index for arrays  
      public const int ltsi = si - 1;



      //convert the layer number to the appropriate array index based on the starting index.
      static public int ci(int LayerNumber)
      {
          if (LayerNumber > 0)
          {
              return LayerNumber - 1;  //convert to a Zero Based Index
          }
          else
          {
              return 0;
          }
      }

      //convert the array index to the appropriate layer number based on the starting index.
      static public int cl(int Index)
      {
          return Index + 1;  //convert to index to layer number   
      }


      static public double SumArray(double[] A)
      {
          double sum = 0;
          foreach (double d in A)
          {
              sum = sum + d;
          }
          return sum;
      }

      //This replaces the fortran array Sum 
      static public double SumArray(double[] A, int StopLayer)
      {
          double sum = 0;
          for (int i = si; i <= ci(StopLayer); i++)
          {
              sum = sum + A[i];
          }
          return sum;
      }

      //INBUILT APSIM ARRAY FUNCTIONS (names are all lower case with underscores)
      //#########################################################################


      //actual function call for apsim modules
      static public double sum_real_array(double[] A, int StopLayer)
      {
          //sum_real_array(p.dlayer, sat_layer-1)
          return SumArray(A, StopLayer);
      }

      static public int find_layer_no(double Depth, double[] DepthOfLayers, int StopLayer)
      {
          //TODO: error check for what if Depth exceeds the depth of even the bottom layer?
          double depth_cum = 0.0;
          int layer_no = -1;
          for (int i = si; i <= ci(StopLayer); i++)
          {
              depth_cum = depth_cum + DepthOfLayers[i];
              if (depth_cum < Depth)
              {
              }
              else
              {
                  layer_no = i + 1;
                  break;
              }
          }
          return layer_no;
      }


      static public void fill_real_array(ref double[] A, double Value, int StopLayer)
      {
          if (A.Length < StopLayer)
          {
              StopLayer = A.Length;
          }
          for (int i = si; i <= ci(StopLayer); i++)
          {
              A[i] = Value;
          }
      }


      static public int count_of_real_vals(double[] A, int StopLayer)
      {

          //counts the layers until the first layer with a value of 0.0. Used only on dlayer[] to find num_layers.
          int count = 0;

          //sv- make sure that default value of max_layer is not larger than the actual array length.
          if (A.Length < StopLayer)
          {
              return A.Length;
          }

          for (int i = si; i <= ci(StopLayer); i++)
          {
              if (A[i] != 0.000)
              {
                  Console.WriteLine(count);
                  count++;
              }
              else
              {
                  break;
              }
          }
          return count;
      }



      static public void move_down_real(double[] DownAmount, ref double[] A, int NLayr)
      {

          //!+ Sub-Program Arguments
          //   real       array (*)             ! (INPUT/OUTPUT) amounts currently in
          //                                    !   each layer
          //   real       down (*)              ! (INPUT) amounts to move into each
          //                                    !   layer from the one above
          //   integer    nlayr                 ! (INPUT) number of layers

          //!+ Purpose
          //!     move amounts specified, downwards from one element to the next

          //!+  Definition
          //!     Each of the "nlayr" elements of "array" holds quantities
          //!     for a given soil layer.  "array"(1) corresponds to the
          //!     uppermost layer.   "array"(n) corresponds to the layer
          //!     (n-1) layers down from the uppermost layer.  "down"(n)
          //!     indicates a quantity to be moved from the layer
          //!     corresponding to "array"(n) down into the layer
          //!     corresponding to "array"(n+1).  This subroutine subtracts
          //!     "down"(n) from "array"(n) and adds it to "array"(n+1) for
          //!     n=1 .. ("nlayr"-1).  "down"("nlayr") is subtracted from
          //!     "array"("nlayr").

          //!+  Mission Statement
          //!      Move amounts of %1 down array %2

          //!+ Changes
          //!       031091  jngh changed variable movedn to down - cr157

          //!+ Local Variables
          int layer;  //! layer number
          double win;    //! amount moving from layer above to current layer
          double wout;   //! amount moving from current layer to the one below

          //!- Implementation Section ----------------------------------

          if (NLayr > A.Length)
          {
              NLayr = A.Length;
          }

          win = 0.0;
          for (layer = si; layer <= ci(NLayr); layer++)
          {
              wout = DownAmount[layer];
              A[layer] = A[layer] + win - wout;
              win = wout;
          }

      }


      static public void move_up_real(double[] UpAmount, ref double[] A, int NLayr)
      {
          //move_up_real(leach, temp_solute, num_layers);


          //!     ===========================================================
          //   subroutine Move_up_real (up, array, nlayr)
          //!     ===========================================================


          //!+ Sub-Program Arguments
          //eal        array (*)             //! (INPUT/OUTPUT) amounts currently in each layer
          //int         nlayr                 //! (INPUT) number of layers
          //real        up (*)                //! (INPUT) amounts to move into each layer from the one below

          //!+ Purpose
          //!       move amounts specified, upwards from one element to the next

          //!+  Definition
          //!     Each of the "nlayr" elements of "array" holds quantities
          //!     for a given soil layer.  "array"(1) corresponds to the
          //!     uppermost layer.   "array"(n) corresponds to the layer
          //!     (n-1) layers down from the uppermost layer.  "up"(n)
          //!     indicates a quantity to be moved from the layer
          //!     corresponding to "array"(n+1) up into the layer
          //!     corresponding to "array"(n).  This subroutine subtracts
          //!     "up"(n) from "array"(n+1) and adds it to "array"(n) for
          //!     n=1..("nlayr"-1).  "up"("nlayr") is added to "array"("nlayr").

          //!+  Mission Statement
          //!      Move amounts %1 up array %2

          //!+ Changes
          //!       031091  jngh renamed moveup to up - cr158
          //!                    included description of variables in parameter list
          //!                      - cr159
          //!                    corrected description - cr160

          //!+ Calls

          //!+ Local Variables
          int layer;                 //! layer number
          double win;                   //! amount moving from layer below to current layer
          double wout;                  //! amount moving from current layer to the one above

          //!- Implementation Section ----------------------------------

          if (NLayr > A.Length)
          {
              NLayr = A.Length;
          }

          wout = 0.0;
          for (layer = si; layer <= ci(NLayr); layer++)
          {
              win = UpAmount[layer];
              A[layer] = A[layer] + win - wout;
              wout = win;
          }

      }


      static public int position_in_char_array(String Name, String[] A, int ArraySize)
      {

          //!+ Sub-Program Arguments
          //   character Array(*)*(*)      ! (INPUT) Array to search
          //   integer   Array_size        ! (INPUT) Number of elements in array
          //   character String*(*)        ! (INPUT) string to search for

          //!+ Purpose
          //!     returns the index number of the first occurrence of specified value

          //!+  Definition
          //!     Returns the index of the first element of the "array_size"
          //!     elements of "array" that compares equal to "string".
          //!     Returns 0 if there are none.

          //!+  Mission Statement
          //!     position of %1 in the list %2

          //!+ Changes
          //!     040995 nih created from position_in_iteger_array

          //!+ Calls

          //!+ Local Variables
          int i;                      //! Index into array
          int position;               //! position of number in array

          //!- Implementation Section ----------------------------------

          position = -1;

          if (ArraySize > A.Length)
          {
              ArraySize = A.Length;
          }

          for (i = si; i <= ci(ArraySize); i++)
          {
              if (A[i] == Name)
              {
                  position = i;
              }
              else
              {
                  //! Not found
              }
          }

          return position;

      }


      static public double linear_interp_real(double X, double[] X_cord, double[] Y_cord, int Num_Cord)
      {

          //   integer    num_cord              ! (INPUT) size_of of tables
          //   real       x                     ! (INPUT) value for interpolation
          //   real       x_cord(*)             ! (INPUT) x co-ordinates of function
          //   real       y_cord(*)             ! (INPUT) y co_ordinates of function

          //!+ Purpose
          //!       Linearly interpolates a value y for a given value x and a given
          //!       set of xy co-ordinates.
          //!       When x lies outside the x range_of, y is set to the boundary condition.

          //!+  Definition
          //!     The "num_cord" elements of "x_cord" and the corresponding
          //!     "num_cord" elements of "y_cord" define a piecewise linear
          //!     function F(x) with ("num_cord"-1) pieces and "num_cord"
          //!     nodes.  The values in "x_cord" must be in strictly
          //!     ascending order, but may well define different widths.  If
          //!     "x" is less than "x_cord"(1), "y_cord"(1) is returned,
          //!     else if "x" is greater than "x_cord"("num_cord"),
          //!     "y_cord"("num_cord") is returned, else linear
          //!     interpolation is performed, and F("x") is returned.

          //!+ Assumptions
          //!       XY pairs are ordered by x in ascending order.

          //!+  Mission Statement
          //!     A linearly interpolated value based upon %1 (where X = %2, Y = %3)..



          int indx;                  //! position in table
          double y = 0.0;                     //! interpolated value
          double m, dx, c;

          //! find where x lies in the x cord
          for (indx = si; indx <= ci(Num_Cord); indx++)
          {
              if (X <= X_cord[indx])
              {
                  //! found position
                  if (indx == si)
                  {

                      //! out of range_of
                      y = Y_cord[indx];
                  }
                  else
                  {
                      //! check to see if x is exactly equal to x_cord(indx).
                      //! if so then dont calculate y.  This was added to
                      //! remove roundoff error.  (DPH)
                      if (decimals_are_equal(X, X_cord[indx]))
                      {
                          y = Y_cord[indx];
                      }
                      else
                      {
                          //! interpolate - y = mx+c
                          m = divide((Y_cord[indx] - Y_cord[indx - 1]), (X_cord[indx] - X_cord[indx - 1]), 0.0);   //m = dy/dx  //there are different slopes between any two points in the array
                          dx = (X - X_cord[indx - 1]);  //distance from the first point that we going to go along this slope 
                          c = Y_cord[indx - 1];        //add the y value of the first point to the dy value we have calculated using dx. 
                          y = m * dx + c;
                      }
                  }

                  //! have a value now - exit
                  break;
              }
              else if (indx == Num_Cord)
              {
                  //! not found - out of range_of
                  y = Y_cord[indx];
              }
              else
              {
                  //! position not found - keep looking
                  y = 0.0;
              }
          }

          return y;
      }





    }

    public class FixedSizedQueue<T> : Queue<T>
    {
        readonly Queue<T> queue = new Queue<T>();

        public int Size { get; private set; }

        public FixedSizedQueue(int size)
        {
            Size = size;
        }

        public new void Enqueue(T obj)
        {
            queue.Enqueue(obj);

                while (queue.Count > Size)
                {
                    // T outObj;
                    queue.Dequeue();
                }
        }

    }


}
