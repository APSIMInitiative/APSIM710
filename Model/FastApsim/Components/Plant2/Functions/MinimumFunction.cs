using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

class MinimumFunction : Function
   {
   [Param] string[] Property = null;
   [Input] public ModelAPIInterface API;
   [Output]
   public override double Value
      {
      get
         {
         double ReturnValue = 999999999;
         foreach (string PropertyName in Property)
            {
            double Val = (double) API.Get(PropertyName);
            ReturnValue = Math.Min(ReturnValue, Val);
            }
         return ReturnValue;
         }
      }

   }
