using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

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
