
using System;
using System.Collections.Generic;
using System.Text;


//The purpose of this is so in the future if you need to swap from zero based arrays back to 1 based. 
//OR some flexible approach where you can use any starting layer and any ending layer (eg. -5 to +5) you can just change this ArrayUtil to map to the correct array index. 
//This allows you compensate for when there is not a 1 to 1 relationship between layer number and array index. 

//eg. if you want to add some ponding layers or mulch layers. You will still have to start these ponding or layers at index 0 
//but the first actual soil layer can start at index 2 with index 0 and 1 used for ponding or mulch.
//In this situation perhaps add another constant bi and decide in your for loops (in Soil Process functions) if you want to start from the pond/mulch or from the soil.


//Also makes it easy to see in your code when you are subtracting 1 because you want the layer above the current layer (you will see the "-1")
//and when you are subtracting 1 to convert a layer number into its zero based index equivalent value (you will see au.ci).

   class au
      {

      //starting index or soil index for arrays  
      public const int si = 0;

      //less than si index for arrays  
      public const int ltsi = au.si-1;



      //convert the layer number to the appropriate array index based on the starting index.
      static public int ci(int LayerNumber)
         {
         if (LayerNumber > 0)
            {
            return LayerNumber -1;  //convert to a Zero Based Index
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



      static public float[] ToFloatArray(double[] D)
         {
         float [] f = new float[D.Length];
         for (int i=au.si; i<=au.ci(D.Length); i++)
            {
            f[i] = (float)D[i];
            }
         return f;
         }

      static public double[] ToDoubleArray(float[] F)
         {
         double [] d = new double[F.Length];
         for (int i=au.si; i<=au.ci(F.Length); i++)
            {
            d[i] = (double)F[i];
            }
         return d;
         }



      static public bool IsIniDefault(double[] A) 
         {
         if ((A.Length == 3) && (A[0] == 0.0) && (A[1] == 0.0) && (A[2] == 0.0)) 
            {
            return true;
            }
         else
	         {
            return false;
	         }
         }






      //FORTRAN LANGUAGE ARRAY EQUIVALENTS (names start with capitals)
      //########################################################

      static public void ZeroArray(ref string[] A)
         {
         for (int i = si; i <= ci(A.Length); i++)
            {
            A[i] = "";
            }
         }

      static public void ZeroArray(ref double[] A)
         {
         for (int i = si; i <= ci(A.Length); i++)
            {
            A[i] = 0.0;
            }
         }

      static public void ZeroArray(ref double[] A, int StopLayer)
         {
         for (int i = si; i <= ci(StopLayer); i++)
            {
            A[i] = 0.0;
            }
         }

      //Used for Integer arrays not Double arrrays
      static public void ZeroArray(ref int[] A, int StopLayer)
         {
         for (int i = si; i <= ci(StopLayer); i++)
            {
            A[i] = 0;
            }
         }


      static public void Zero2DArray(ref double[,] A)
         {
         for (int i = si; i<=ci(A.GetLength(0)); i++)
            {
            for (int j = si; j<=ci(A.GetLength(1)); j++)
               {
               A[i,j] = 0.0;
               }
            }
         }


      static public double[] Dup(double[] Source)
         {
         //Dup is short for Duplicate.
         //this is different to CopyTo() in that this creates new memory 
         //whereas CopyTo just overwrites the existing memory of an existing array.
         double[] returnArray = new double[Source.Length];
         CopyTo(ref returnArray, Source, Source.Length);
         return returnArray;
         }


      static public void CopyTo(ref double[] Destination, double[] Source, int StopLayer)
         {
         //Array.Copy(Source, Destination, StopLayer);
         for(int i=si; i<=ci(StopLayer); i++)
            {
            Destination[i] = Source[i];
            }
         }


      static public double[] Subtract(double[] A, double[] B)
         {
         //subtract B from A
         //TODO: need to do some error checking to make sure A and B are same length or B shorter then A. If B is longer this is a problem.
         double[] C =  new double[A.Length];
         CopyTo(ref C, A, A.Length);
         for (int i = si; i <= ci(B.Length); i++)
            {
            C[i] = A[i] - B[i];
            }  
         return C;
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
         for (int i = si; i<=ci(StopLayer); i++)
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
         for(int i=si; i <= ci(StopLayer); i++)
            {
            depth_cum = depth_cum + DepthOfLayers[i];
            if (depth_cum < Depth)
               {
               }
            else
               {
               layer_no = i+1;
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
         for(int i=si; i<=ci(StopLayer); i++)
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

         for(int i=si; i<=ci(StopLayer); i++)
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
         int         layer;  //! layer number
         double      win;    //! amount moving from layer above to current layer
         double      wout;   //! amount moving from current layer to the one below

         //!- Implementation Section ----------------------------------

         if (NLayr > A.Length)
            {
            NLayr = A.Length;                                 
            }

         win = 0.0;
         for (layer=au.si; layer<=au.ci(NLayr); layer++)
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
         int         layer;                 //! layer number
         double       win;                   //! amount moving from layer below to current layer
         double       wout;                  //! amount moving from current layer to the one above

         //!- Implementation Section ----------------------------------

         if (NLayr > A.Length)
            {
            NLayr = A.Length;                                 
            }

         wout = 0.0;
         for (layer=au.si;layer<=au.ci(NLayr); layer++)
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
         int   i;                      //! Index into array
         int   position;               //! position of number in array

         //!- Implementation Section ----------------------------------

         position = -1;

         if (ArraySize > A.Length)
            {
            ArraySize = A.Length;                                 
            }

         for (i = au.si; i<=au.ci(ArraySize); i++)
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


      static public double linear_interp_real (double X, double[] X_cord, double[] Y_cord, int Num_Cord)
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



         int         indx;                  //! position in table
         double      y = 0.0;                     //! interpolated value
         double      m, dx, c;

         //! find where x lies in the x cord
         for(indx = si; indx<=ci(Num_Cord); indx++)
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
                  if (mu.reals_are_equal(X, X_cord[indx]))
                     {
                     y = Y_cord[indx];
                     }
                  else
                     {
                     //! interpolate - y = mx+c
                     m = mu.divide((Y_cord[indx] - Y_cord[indx-1]), (X_cord[indx] - X_cord[indx-1]), 0.0);   //m = dy/dx  //there are different slopes between any two points in the array
                     dx = (X - X_cord[indx-1]);  //distance from the first point that we going to go along this slope 
                     c =  Y_cord[indx-1];        //add the y value of the first point to the dy value we have calculated using dx. 
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

