//---------------------------------------------------------------------------

#include "NaturalSpline.h"
using namespace Maize;
//---------------------------------------------------------------------------
//Constructor - Set up the arrays
//---------------------------------------------------------------------------
NaturalSpline::NaturalSpline(double *x, double *y, int count)
   {
   //Set global
   ElementNo = count;
   sx = new double[count];
   sy = new double[count];
   sc = new double[count];

   for(int i = 0; i < count; i++)
      {
      sx[i] = x[i];
      sy[i] = y[i];
      sc[i] = 0.0;
      }
   //Setup the Spline
   NaturalSplineSetUp(sx, sy, sc, ElementNo);
   }
//---------------------------------------------------------------------------
//Destructor - Tidy up memory
//---------------------------------------------------------------------------
NaturalSpline::~NaturalSpline(void)
   {
   if(sx) delete []sx;
   if(sy) delete []sy;
   if(sc) delete []sc;
   }
//---------------------------------------------------------------------------
//This routine is based on "Numerical Recipes in C"
//by Press, et al. (1988)
//sx - The array of sample x values
//sy - The array of corresponding sample y values
//sc - A returned array of interpolation constants
//n - The number of entries in the above arrays
//---------------------------------------------------------------------------
void NaturalSpline::NaturalSplineSetUp(double *sx, double *sy, double *sc, int n)
   {
//   int i, j;
   double r1, r2;                 //Intermediate results
   double *tsv = new double[n];    //Temporary storage vector

   sc[1] = 0.0;
   tsv[1] = 0.0;

   for(int i = 1; i < (n - 2); i++)
      {
      r2 = 0;
 //     if(sx[(i + 1)] != sx[(i - 1)])
 //        {
         r2 = (sx[i] - sx[(i - 1)]) / (sx[(i + 1)] - sx[(i - 1)]);
 //        }
      r1 = r2 * sc[(i - 1)] + 2;
      sc[i] = (r2 - 1) / r1;
      tsv[i] = 0;
 //     if((sx[i] - sx[(i - 1)]) != 0)
 //        {
         tsv[i] = (sy[(i + 1)] - sy[i]) / (sx[(i + 1)] - sx[i]) - (sy[i] - sy[(i - 1)]) / (sx[i] - sx[(i - 1)]);
 //        }
 //     if(tsv[i])
 //        {
         tsv[i] = (6 * tsv[i] / (sx[(i + 1)] - sx[(i - 1)]) - r2 * tsv[(i - 1)]) / r1;
 //        }
      }

   sc[(n - 1)] = 0;

   for(int i = (n - 2); i >= 0; i--)
      {
      sc[i] = sc[i] * sc[i + 1] + tsv[i];
      }

   delete []tsv;
   return;
   }
//---------------------------------------------------------------------------
///* This routine is based on "Numerical Recipes in C" */
///* by Press, et al. (1988)                           */
//double *xs,      /* The array of sample x values              */
//double *sy,      /* The array of corresponding sample y values*/
//double *sc,      /* Interpolation constants from  ...SetUp()  */
//double x,         /* The x value for which f(x) is desired     */
//int    n     /* The number of sample data points          */
//---------------------------------------------------------------------------
double NaturalSpline::SplineInterpolation(double *xs, double *sy, double *sc, double X, int n)
    {
    //Local Vars
    int ll, lu, m;
    double dx, dxl, dxu, Y;

    ll = 0;
    lu = n - 1;

    m = (lu - ll) / 2;

    while (lu - ll > 1)
      {
      m = (lu + ll) / 2;
      if(xs[m] > X)
         {
         lu = m;
         }
      else
         {
         ll = m;
         }
      }
   dx = xs[lu] - xs[ll];
   dxu = (xs[lu] - X) / dx;
   dxl = (X - xs[ll]) / dx;

   Y = dxu * sy[ll] + dxl * sy[lu] + ((dxu * dxu * dxu - dxu) *
            sc[ll] + (dxl * dxl * dxl - dxl) * sc[lu]) * (dx * dx) / 6.0;

   return Y;
   }
//---------------------------------------------------------------------------
//Returns Y value on the spline given X
//---------------------------------------------------------------------------
double NaturalSpline::GetValue(double Val)
   {
   return  SplineInterpolation(sx, sy, sc, Val, ElementNo);
   }
