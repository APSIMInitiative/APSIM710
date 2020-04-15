//---------------------------------------------------------------------------

#ifndef LayerModelH
#define LayerModelH

namespace Maize {
class NaturalSpline;
//---------------------------------------------------------------------------
class LayerModel
   {
   private:

   public:
      //Constructors
      LayerModel(double*, double*, int);                //Default
      ~LayerModel(void);
      double GetVal(double);

      NaturalSpline *NS;
      };
}
#endif

