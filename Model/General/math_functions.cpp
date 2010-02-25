//---------------------------------------------------------------------------
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <numeric>
#include <strstream>
#include <iomanip>

#include "math_functions.h"

#ifndef __WIN32__
   #include <ext/algorithm>
#endif

using namespace std;

// ------------------------------------------------------------------
//  Short description:
//    cycle through a number series from 1 to Max_number.
//    returns the next number in the sequence.
//    eg.  if current_number = 1   and max_number = 4 returns 1
//         if current_number = 2   and max_number = 4 returns 2
//         if current_number = 3   and max_number = 4 returns 3
//         if current_number = 4   and max_number = 4 returns 4
//         if current_number = 5   and max_number = 4 returns 1

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
int Cycle (int Current_number, int Max_number)
   {
   // current_number = 1, max_number = 4, m = 4
   // current_number = 2, max_number = 4, m = 4
   // current_number = 3, max_number = 4, m = 4
   // current_number = 4, max_number = 4, m = 4
   // current_number = 5, max_number = 4, m = 8
   int m = ((Current_number-1) / Max_number + 1) * Max_number;

   return Max_number - (m - Current_number);
   }

// ------------------------------------------------------------------
//  Short description:
//    Calculate regression stats.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
void calcRegressionStats (const vector<double>& X, const vector<double>& Y,
                          Regr_stats& stats)
   {
   float SumX = 0;
   float SumY = 0;
   float SumXY = 0;
   float SumX2 = 0;
   float SumY2 = 0;
//   float SumXYdiff = 0;
   float SumXYdiff2 = 0;
//   float XYdiff = 0;
//   float SumXYDiffPer = 0;
   float CSSX, CSSXY;
   float Xbar, Ybar;
   float TSS, TSSM;
   float REGSS, REGSSM;
   float RESIDSS, RESIDSSM;
   float S2;
//   float MeanAbsError, MeanAbsPerError;

   stats.m = 0.0;
   stats.c = 0.0;
   stats.SEslope = 0.0;
   stats.SEcoeff = 0.0;
   stats.R2 = 0.0;
   stats.ADJR2 = 0.0;
   stats.R2YX = 0.0;
   stats.VarRatio = 0.0;
   stats.RMSD = 0.0;
   int Num_points = X.size();

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
//         XYdiff = fabs(Y[Point] - X[Point]);        // calculate XY diff
//         SumXYdiff = SumXYdiff + XYdiff;
//         SumXYdiff2 = SumXYdiff2 + XYdiff * XYdiff; // SS for XY differences
//         if (Y[Point] != 0.0)
//            SumXYDiffPer = SumXYDiffPer + XYdiff * 100.0 / Y[Point];
                                                    // Sum of XYdiff adj. for Y
//         else
//            SumXYDiffPer = -999;                    // Cannot calculate if Y[i]=0
         }
      Xbar = SumX / Num_points;
      Ybar = SumY / Num_points;

      CSSXY = SumXY - SumX * SumY / Num_points;     // Corrected SS for products
      CSSX = SumX2 - SumX * SumX / Num_points;      // Corrected SS for X
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

      stats.RMSD = sqrt(RESIDSSM);                        // Root mean square deviation
      stats.VarRatio= REGSSM / RESIDSSM;                  // Variance ratio - for F test (1,n-2)
      stats.R2 = 1.0 - (RESIDSS / TSS);                   // Unadjusted R2 calculated from SS
      stats.ADJR2 = 1.0 - (RESIDSSM / TSSM);              // Adjusted R2 calculated from mean SS
      if (stats.ADJR2 < 0.0)
         stats.ADJR2 = 0.0;
      S2 = RESIDSSM;                                // Resid. MSS is estimate of variance
                                                    // about the regression
      stats.SEslope = sqrt(S2) / sqrt(CSSX);              // Standard errors estimated from S2 & CSSX
      stats.SEcoeff = sqrt(S2) * sqrt(SumX2 / (Num_points * CSSX));

      // Statistical parameters of Butler, Mayer and Silburn

      stats.R2YX = 1.0 - (SumXYdiff2 / TSS);              // If you are on the 1:1 line then R2YX=1

      // If R2YX is -ve then the 1:1 line is a worse fit than the line y=ybar

//      MeanAbsError = SumXYdiff / Num_points;
//      MeanAbsPerError = SumXYDiffPer / Num_points;  // very dangerous when y is low
                                                    // could use MeanAbsError over mean
      }
   }

// ------------------------------------------------------------------
//  Short description:
//       Linearly interpolates a value y for a given value x and a given
//       set of xy co-ordinates.
//       When x lies outside the x range_of, y is set to the boundary condition.
//       Returns true for Did_interpolate if interpolation was necessary.

//  Notes:
//       XY pairs are ordered by x in ascending order.

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double linear_interp_real (double x,
                           const vector<double>& x_cord,
                           const vector<double>& y_cord,
                           bool& Did_interpolate)
   {
   // find where x lies in the x cord

   if (x_cord.size() == 0 || y_cord.size() == 0 || x_cord.size() != y_cord.size())
       throw std::runtime_error("Sizes are wrong in linear_interp_real.");
   for (unsigned int indx = 0; indx < x_cord.size(); indx++)
      {
      if (x <= x_cord[indx])
         {
         // check to see if x is exactly equal to x_cord(indx).
         // if so then dont calculate y.  This was added to
         // remove roundoff error.  (DPH)

         if (x == x_cord[indx])
            {
            Did_interpolate = false;
            return y_cord[indx];
            }

         // found position
         if (indx == 0)
            {
            Did_interpolate = true;
            return y_cord[indx];
            }

         else
            {
            // interpolate - y = mx+c
            if (x_cord[indx] - x_cord[indx-1] == 0)
               {
               Did_interpolate = true;
               return y_cord[indx-1];
               }

            else
               {
               Did_interpolate = true;
               return ( (y_cord[indx] - y_cord[indx-1]) /
                        (x_cord[indx] - x_cord[indx-1])
                       * (x - x_cord[indx-1])
                       + y_cord[indx-1]);
               }
            }
         }
      else if (indx == x_cord.size()-1)
         {
         Did_interpolate = true;
         return y_cord[indx];
         }
      }
//   assert (false);
   return 0;
   }

// ------------------------------------------------------------------
//  Short description:
//       Calculates a set of probability values from a given set
//       of XValues.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
void Calculate_prob_dist(vector<double>& X,
                         bool Prob_exceed,
                         vector<double>& Prob_values)
   {
   // sort our values.
   std::sort(X.begin(), X.end());

   // create another vector for interpolation.
   for (unsigned int x = 1; x <= X.size(); x++)
      Prob_values.push_back( (x-0.5)/X.size()*100 );

   if (Prob_exceed)
     std::reverse(Prob_values.begin(), Prob_values.end());
   }

// ------------------------------------------------------------------
//  Short description:
//       Calculates an average

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_mean(vector<double>& X)
   {
   if (X.size() > 0)
      return std::accumulate (X.begin(), X.end(), 0.0) / X.size();
   else
      return 0.0;
   }

// ------------------------------------------------------------------
//  Short description:
//       Calculates a percentile from a given set
//       of XValues.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_percentile(vector<double>& X,
                            bool Prob_exceed,
                            int Percentile)
   {
   Percentile = min(Percentile, 100);
   Percentile = max(Percentile, 0);

   // if user wants a prob exceedence then we need to flip the percentile around.
   if (Prob_exceed)
      Percentile = 50 - (Percentile - 50);

   vector<double> Prob_values;
   Calculate_prob_dist(X, false, Prob_values);

   bool Did_interpolate;
   return linear_interp_real (Percentile, Prob_values, X, Did_interpolate);
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate a frequency distribution from the specified values.
//      Return the frequency labels and numebrs.

//  Changes:
//    DPH 15/7/98

//  Usage(JW 13/8/04):
//  Returns occurance count of elements in Values in the range of
//  [Start_values, End_values].
//
//  Input:  Values, Start_values, End_values, Label_precision
//  Output: Frequency_labels, Frequency_numbers
//
//  Example:
//  Calculate_freq_dist( [1.5 1.6 3 7],  /*Values*/
//                                   1,  /*Start_values*/
//                                   2,  /*End_values*/
//                                  fl,  /*Frequency_labels*/
//                                  fn,  /*Frequency_numbers*/
//                                   2   /*Label_precision*/
//  will result in output:
//         fl = "(1.00 to 2.00)"
//         fn = 2
//

// ------------------------------------------------------------------
void Calculate_freq_dist(vector<double>& Values,
                         vector<double>& Start_values,
                         vector<double>& End_values,
                         vector<string>& Frequency_labels,
                         vector<double>& Frequency_numbers,
                         int Label_precision)
   {
   Frequency_labels.erase (Frequency_labels.begin(), Frequency_labels.end());
   Frequency_numbers.erase (Frequency_numbers.begin(), Frequency_numbers.end());

   // loop through all frequency intervals.
   for (unsigned int interval = 0; interval < Start_values.size(); interval++)
      {
      double Start_of_interval = Start_values[interval];
      double End_of_interval = End_values[interval];

      range<double> Range(Start_of_interval, End_of_interval);
      int count = count_if (Values.begin(), Values.end(), Range);
      Frequency_numbers.push_back (count);

      // create a label for this interval.
      ostrstream out;
      out.setf(std::ios::fixed, std::ios::floatfield);
      out << '(';
      out << std::setprecision(Label_precision) << Start_of_interval << " to " << std::setprecision(Label_precision) << End_of_interval;
      out << ')' << std::ends;
      Frequency_labels.push_back (out.str());
      delete out.str();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    round a number up or down to nearest number depending on the
//    magnitude of the number passed in.

//  Notes:
//    eg.  if Value = 0.1   Rounds down to  0 Rounds up to 1
//    eg.  if Value = 369   Rounds down to 300 Rounds up to 400
//    eg.  if Value = 1234  Rounds down to 1000 Rounds up to 2000
//    eg.  if Value = 12345 Rounds down to 10000 Rounds up to 20000


//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
void Round_using_magnitude (double& Value, bool Round_up)
   {
   if (fabs(Value) > 0)
      {
      int Magnitude = (int)log10(fabs(Value));
      int Nearest = (int)pow(10.0, Magnitude);
      Round_to_nearest (Value, Nearest, Round_up);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    round a number up or down to nearest specified number

//  Notes:
//    eg.  if Value = 369 and Nearest = 200 Rounds down to 200 Rounds up to 400

//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
void Round_to_nearest (double& Value, double Nearest, bool Round_up)
   {
   Value = floor(Value / Nearest);
   if (Round_up)
      Value++;
   Value *= Nearest;
   }


//void CopyParamsFromVector(vector<double>& source, double dest[])
//   {
//   for (unsigned i = 0; i != source.size(); i++)
//      dest[i+1] = source[i];
//   }
void CopyParamsToVector(double source[], vector<double>& dest)
   {
   unsigned numValues = dest.size();
   for (unsigned i = 0; i != numValues; i++)
      dest[i] = source[i+1];
   }
// ------------------------------------------------------------------
//  Short description:
//    SIMPLEX method of optimisation.

//  Notes:
//     A PROGRAM FOR FUNCTION MINIMIZATION USING THE SIMPLEX METHOD.
//     The minimum found will often be a local, not a global, minimum.
//
//     FOR DETAILS, SEE NELDER & MEAD, THE COMPUTER JOURNAL, JANUARY 1965
//
//     PROGRAMMED BY D.E.SHAW,
//     CSIRO, DIVISION OF MATHEMATICS & STATISTICS
//    P.O. BOX 218, LINDFIELD, N.S.W. 2070

//    WITH AMENDMENTS BY R.W.M.WEDDERBURN
//    ROTHAMSTED EXPERIMENTAL STATION
//    HARPENDEN, HERTFORDSHIRE, ENGLAND

//    Further amended by Alan Miller,
//    CSIRO, Division of Mathematics & Statistics
//    Private Bag 10, CLAYTON, VIC. 3168

//    Added LU_OUT to parameter list.  Done by Dean Holzworth (28/11/95)

//    ARGUMENTS:-
//    LU_OUT  = LOGICAL UNIT FOR OUTPUT   -- Added by Dean Holzworth (28/11/95)
//    P()     = INPUT, STARTING VALUES OF PARAMETERS
//              OUTPUT, FINAL VALUES OF PARAMETERS
//    STEP()  = INPUT, INITIAL STEP SIZES
//    NOP     = INPUT, NO. OF PARAMETERS, INCL. ANY TO BE HELD FIXED
//    FUNC    = OUTPUT, THE FUNCTION VALUE CORRESPONDING TO THE FINAL
//              PARAMETER VALUES
//    MAX     = INPUT, THE MAXIMUM NO. OF FUNCTION EVALUATIONS ALLOWED
//    IPRINT  = INPUT, PRINT CONTROL PARAMETER
//                    < 0 NO PRINTING
//                    = 0 PRINTING OF PARAMETER VALUES AND THE FUNCTION
//                        VALUE AFTER INITIAL EVIDENCE OF CONVERGENCE.
//                    > 0 AS FOR IPRINT = 0 PLUS PROGRESS REPORTS AFTER
//                        EVERY IPRINT EVALUATIONS, PLUS PRINTING FOR THE
//                        INITIAL SIMPLEX.
//    STOPCR  = INPUT, STOPPING CRITERION
//    NLOOP   = INPUT, THE STOPPING RULE IS APPLIED AFTER EVERY NLOOP
//              FUNCTION EVALUATIONS.
//    IQUAD   = INPUT, = 1 IF THE FITTING OF A QUADRATIC SURFACE IS REQUIRED
//                     = 0 IF NOT
//    SIMP    = INPUT, CRITERION FOR EXPANDING THE SIMPLEX TO OVERCOME
//              ROUNDING ERRORS BEFORE FITTING THE QUADRATIC SURFACE.
//    VAR()   = OUTPUT, CONTAINS THE DIAGONAL ELEMENTS OF THE INVERSE OF
//              THE INFORMATION MATRIX.
//    FUNCTN  = INPUT, NAME OF THE USER'S SUBROUTINE - ARGUMENTS (P,FUNC)
//              WHICH RETURNS THE FUNCTION VALUE FOR A GIVEN SET OF
//              PARAMETER VALUES IN ARRAY P.
//****   FUNCTN MUST BE DECLARED EXTERNAL IN THE CALLING PROGRAM.
//      IFAULT  = OUTPUT, = 0 FOR SUCCESSFUL TERMINATION
//                        = 1 IF MAXIMUM NO. OF FUNCTION EVALUATIONS EXCEEDED
//                        = 2 IF INFORMATION MATRIX IS NOT +VE SEMI-DEFINITE
//                        = 3 IF NOP < 1
//                        = 4 IF NLOOP < 1

//      Advice on usage:
//      If the function minimized can be expected to be smooth in the vicinity
//      of the minimum, users are strongly urged to use the quadratic-surface
//      fitting option.   This is the only satisfactory way of testing that the
//      minimum has been found.   The value of SIMP should be set to at least
//      1000 times the rounding error in calculating the fitted function.
//      e.g. in double precision on a micro- or mini-computer with about 16
//      decimal digit representation of floating-point numbers, the rounding
//      errors in calculating the objective function may be of the order of
//      1.E-12 say in a particular case.   A suitable value for SIMP would then
//      be 1.E-08.   However, if numerical integration is required in the
//      calculation of the objective function, it may only be accurate to say
//      1.E-05 and an appropriate value for SIMP would be about 0.1.
//      If the fitted quadratic surface is not +ve definite (and the function
//      should be smooth in the vicinity of the minimum), it probably means
//      that the search terminated prematurely and you have not found the
//      minimum.

//      N.B. P, STEP AND VAR (IF IQUAD = 1) MUST HAVE DIMENSION AT LEAST NOP
//           IN THE CALLING PROGRAM.
//      THE DIMENSIONS BELOW ARE FOR A MAXIMUM OF 20 PARAMETERS.
//     The dimension of BMAT should be at least NOP*(NOP+1)/2.

//****      N.B. This version is in DOUBLE PRECISION throughout

//      LATEST REVISION - 11 August 1991

//  Changes:
//    DPH 7/7/1996

// ------------------------------------------------------------------
/* crw commented
void Minim (vector<double>& param,
            vector<double>& step,
            double& FUNC,
            int MAX,
            double STOPCR,
            int NLOOP,
            int& IFAULT,
            TOptimEvent f)
   {
      double G[22][21];
      double H[22];
      double PBAR[21];
      double PSTAR[21];
      double PSTST[21];
      double P[22], STEP[22];
      int NOP;
//      double AVAL[21];
//      double BMAT[211];
//      double PMIN[21];
//      double VC[211];
//      double TEMP[21];

      const double ZERO = 0.0;
//      const double ONE = 1.0;
//      const double TWO = 2.0;
//      const double THREE = 3.0;
      const double HALF = 0.5;

//    A = REFLECTION COEFFICIENT, B = CONTRACTION COEFFICIENT, AND
//    C = EXPANSION COEFFICIENT.

      const double A = 1.0;
      const double B = 0.5;
      const double C = 2.0;

      NOP = param.size();
      for (int i = 0; i != NOP; i++)
         {
         P[i+1] = param[i];
         STEP[i+1] = step[i];
         }

//    SET LOUT = LOGICAL UNIT NO. FOR OUTPUT

//      LOUT = LU_OUT

//    IF PROGRESS REPORTS HAVE BEEN REQUESTED, PRINT HEADING


//    CHECK INPUT ARGUMENTS

      IFAULT=0;
      if(NOP <= 0) IFAULT=3;
      if(NLOOP<=0) IFAULT=4;
      if(IFAULT!=0)
         {
         CopyParamsToVector(P, param);
         return;
         }

//    SET NAP = NO. OF PARAMETERS TO BE VARIED, I.E. WITH STEP!=0

      int NAP=0;
      int LOOP=0;
      int IFLAG=0;
      int I, J;
      for (I=1; I<=NOP; I++)
         if(STEP[I]!=ZERO) NAP=NAP+1;

//    IF NAP = 0 EVALUATE FUNCTION AT THE STARTING POINT AND RETURN

      if(NAP>0) goto L30;
      CopyParamsToVector(P, param);
      FUNC = f(param);
      return;

//    SET UP THE INITIAL SIMPLEX

L30:   for (I=1;I <= NOP; I++)
         G[1][I]=P[I];

      int IROW=2;
      for (I=1; I <= NOP; I++)
         {
         if (STEP[I] == ZERO) continue;
         for (J=1; J <= NOP; J++)
            G[IROW][J]=P[J];
         G[IROW][I]=P[I]+STEP[I];
         IROW=IROW+1;
         }

      int NP1=NAP+1;
      int NEVAL=0;
      for (I=1; I <= NP1; I++)
         {
         for (J=1;J <= NOP; J++)
            P[J]=G[I][J];
         CopyParamsToVector(P, param);
         H[I] = f(param);

         NEVAL=NEVAL+1;
         }

//    START OF MAIN CYCLE.

//    FIND MAX. & MIN. VALUES FOR CURRENT SIMPLEX (HMAX & HMIN).

L100:  LOOP=LOOP+1;
      int IMAX=1;
      int IMIN=1;
      double HMAX=H[1];
      double HMIN=H[1];
      for (I=2;I<=NP1;I++)
         {
         if(H[I]<=HMAX) goto L110;
         IMAX=I;
         HMAX=H[I];
         continue;
L110:     if(H[I]>=HMIN) continue;
         IMIN=I;
         HMIN=H[I];
         }

//    FIND THE CENTROID OF THE VERTICES OTHER THAN P(IMAX)

      for (I=1;I<=NOP;I++)
         PBAR[I]=ZERO;
      for (I=1;I<=NP1;I++)
         {
         if(I==IMAX) continue;
         for (J=1;J<=NOP;J++)
            PBAR[J]=PBAR[J]+G[I][J];
         }
      for (J=1;J<=NOP;J++)
         {
         double FNAP = NAP;
         PBAR[J]=PBAR[J]/FNAP;
         }

//    REFLECT MAXIMUM THROUGH PBAR TO PSTAR,
//    HSTAR = FUNCTION VALUE AT PSTAR.

      for (I=1;I<=NOP;I++)
         PSTAR[I]=A*(PBAR[I]-G[IMAX][I])+PBAR[I];
      CopyParamsToVector(PSTAR, param);
      double HSTAR = f(param);
      NEVAL=NEVAL+1;

//    IF HSTAR < HMIN, REFLECT PBAR THROUGH PSTAR,
//    HSTST = FUNCTION VALUE AT PSTST.

      double HSTST;
      double HSTD;
      double HMEAN;
      double FNP1;
      double SAVEMN;

      if(HSTAR>=HMIN) goto L220;
      for (I=1;I<=NOP;I++)
         PSTST[I]=C*(PSTAR[I]-PBAR[I])+PBAR[I];
      CopyParamsToVector(PSTST, param);
      HSTST = f(param);
      NEVAL=NEVAL+1;

//    IF HSTST < HMIN REPLACE CURRENT MAXIMUM POINT BY PSTST AND
//    HMAX BY HSTST, THEN TEST FOR CONVERGENCE.

      if(HSTST>=HMIN) goto L320;
      for (I=1;I<=NOP;I++)
         if(STEP[I]!=ZERO) G[IMAX][I]=PSTST[I];

      H[IMAX]=HSTST;
      goto L340;

//    HSTAR IS NOT < HMIN.
//    TEST WHETHER IT IS < FUNCTION VALUE AT SOME POINT OTHER THAN
//    P(IMAX).   IF IT IS REPLACE P(IMAX) BY PSTAR & HMAX BY HSTAR.

L220:  for (I=1;I<=NP1;I++)
         {
         if(I==IMAX) continue;
         if(HSTAR<H[I]) goto L320;
         }

//    HSTAR > ALL FUNCTION VALUES EXCEPT POSSIBLY HMAX.
//    IF HSTAR <= HMAX, REPLACE P(IMAX) BY PSTAR & HMAX BY HSTAR.

      if(HSTAR>HMAX) goto L260;
      for (I=1;I<=NOP;I++)
         if(STEP[I]!=ZERO) G[IMAX][I]=PSTAR[I];

      HMAX=HSTAR;
      H[IMAX]=HSTAR;

//    CONTRACTED STEP TO THE POINT PSTST,
//    HSTST = FUNCTION VALUE AT PSTST.

L260:  for (I=1;I<=NOP;I++)
         PSTST[I]=B*G[IMAX][I] + (1.0-B)*PBAR[I];
      CopyParamsToVector(PSTST, param);
      HSTST = f(param);
      NEVAL=NEVAL+1;

//    IF HSTST < HMAX REPLACE P(IMAX) BY PSTST & HMAX BY HSTST.

      if(HSTST>HMAX) goto L300;
      for (I=1;I<=NOP;I++)
         if(STEP[I]!=ZERO) G[IMAX][I]=PSTST[I];

      H[IMAX]=HSTST;
      goto L340;

//    HSTST > HMAX.
//    SHRINK THE SIMPLEX BY REPLACING EACH POINT, OTHER THAN THE CURRENT
//    MINIMUM, BY A POINT MID-WAY BETWEEN ITS CURRENT POSITION AND THE
//    MINIMUM.

L300:  for(I=1;I<=NP1;I++)
         {
         if(I==IMIN) continue;
         for (J=1;J<=NOP;J++)
            {
            if(STEP[J]!=ZERO) G[I][J]=(G[I][J]+G[IMIN][J])*HALF;
            P[J]=G[I][J];
            }
         CopyParamsToVector(P, param);

         H[I] = f(param);
         NEVAL=NEVAL+1;
         }
      goto L340;


//    REPLACE MAXIMUM POINT BY PSTAR & H(IMAX) BY HSTAR.

L320:  for (I=1;I<=NOP;I++)
         if(STEP[I]!=ZERO) G[IMAX][I]=PSTAR[I];

      H[IMAX]=HSTAR;

//    IF LOOP = NLOOP TEST FOR CONVERGENCE, OTHERWISE REPEAT MAIN CYCLE.

L340:  if(LOOP<NLOOP) goto L100;

//    CALCULATE MEAN & STANDARD DEVIATION OF FUNCTION VALUES FOR THE
//    CURRENT SIMPLEX.

      HSTD=ZERO;
      HMEAN=ZERO;
      for (I=1;I<=NP1;I++)
         HMEAN=HMEAN+H[I];
      FNP1 = NP1;
      HMEAN=HMEAN/FNP1;
      for (I=1;I<=NP1;I++)
         HSTD=HSTD+pow((H[I]-HMEAN),2);
      HSTD=sqrt(HSTD/NP1);

//    IF THE RMS > STOPCR, SET IFLAG & LOOP TO ZERO AND GO TO THE
//    START OF THE MAIN CYCLE AGAIN.

      if(HSTD<=STOPCR || NEVAL>MAX) goto L410;
      IFLAG=0;
      LOOP=0;
      goto L100;

//    FIND THE CENTROID OF THE CURRENT SIMPLEX AND THE FUNCTION VALUE THERE.

L410:  for (I=1;I<=NOP;I++)
         {
         if(STEP[I]==ZERO) continue;
         P[I]=ZERO;
         for (J=1;J<=NP1;J++)
            P[I]=P[I]+G[J][I];
         FNP1 = NP1;
         P[I]=P[I]/FNP1;
         }

      CopyParamsToVector(P, param);
      FUNC = f(param);
      NEVAL=NEVAL+1;

//    TEST WHETHER THE NO. OF FUNCTION VALUES ALLOWED, MAX, HAS BEEN
//    OVERRUN; IF SO, EXIT WITH IFAULT = 1.

      if(NEVAL<=MAX) goto L430;
      IFAULT=1;
      CopyParamsToVector(P, param);
      return;

//    CONVERGENCE CRITERION SATISFIED.
//    IF IFLAG = 0, SET IFLAG & SAVE HMEAN.
//    IF IFLAG = 1 & CHANGE IN HMEAN <= STOPCR THEN SEARCH IS COMPLETE.

L430:  if(IFLAG>0) goto L450;
      IFLAG=1;
L440:  SAVEMN=HMEAN;
      LOOP=0;
      goto L100;
L450:  if(fabs(SAVEMN-HMEAN)>=STOPCR) goto L440;
      CopyParamsToVector(P, param);
      return;

   }

*/
