using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

class MinimumFunction : Function
   {
   [Link] Plant Plant = null;

   [Param] string[] Propertys = null;

   [Output]
   public override double Value
      {
      get
         {
         double ReturnValue = 999999999;
         foreach (string PropertyName in Propertys)
            {
                double Val = Convert.ToDouble(ExpressionFunction.Evaluate(Plant, PropertyName));
            ReturnValue = Math.Min(ReturnValue, Val);
            }
         return ReturnValue;
         }
      }

   }
