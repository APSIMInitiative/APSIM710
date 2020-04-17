//---------------------------------------------------------------------------

#ifndef NaturalSplineH
#define NaturalSplineH
//---------------------------------------------------------------------------
namespace Maize {
class NaturalSpline
   {
   private:
      void NaturalSplineSetUp(double*, double*, double*, int);
      double SplineInterpolation(double*, double*, double*, double, int);

      double *sx;
      double *sy;
      double *sc;
      int ElementNo;

   public:
      NaturalSpline(double*, double*, int);
      ~NaturalSpline(void);

      double GetValue(double);
   };
}
#endif
