using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

class MinimumFunction : Function
   {
   [Link] Plant Plant = null;

   [Param] string[] Property = null;

   [Output]
   public override double Value
      {
      get
         {
         double ReturnValue = 999999999;
         foreach (string PropertyName in Property)
            {
                double Val = Convert.ToDouble(ExpressionFunction.Evaluate(PropertyName));
            ReturnValue = Math.Min(ReturnValue, Val);
            }
         return ReturnValue;
         }
      }

   }
