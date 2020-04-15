//---------------------------------------------------------------------------

#include "LayerModel.h"
#include "NaturalSpline.h"

using namespace Maize;
//---------------------------------------------------------------------------
LayerModel::LayerModel(double *X, double *Y, int Num)
   {
   NS = new NaturalSpline(X, Y, Num);
   }
//---------------------------------------------------------------------------
LayerModel::~LayerModel(void)
   {
   if(NS) delete NS;
   }
//---------------------------------------------------------------------------
double LayerModel::GetVal(double Val)
   {
   return NS->GetValue(Val);
   }
//---------------------------------------------------------------------------



