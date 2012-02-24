using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

[Description("Returns the maximum value of all childern functions")]
class MaximumFunction : Function
   {
   [Output]
   public override double Value
      {
      get
         {
         double ReturnValue = 999999999;
         foreach (Function F in My.ChildrenAsObjects)
            {
                ReturnValue = Math.Max(ReturnValue, F.Value);
            }
         return ReturnValue;
         }
      }

   }
