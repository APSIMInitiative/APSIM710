using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

[Description("Returns the Minimum value of all children functions")]
class MinimumFunction : Function
   {
   [Output]
   public override double Value
      {
      get
         {
         double ReturnValue = 999999999;
         foreach (Function F in My.ChildrenAsObjects)
            {
                ReturnValue = Math.Min(ReturnValue, F.Value);
            }
         return ReturnValue;
         }
      }

   }
