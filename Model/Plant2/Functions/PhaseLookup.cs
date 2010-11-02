using System;
using System.Collections.Generic;
using System.Text;


   [Description("A value is determined depending upon the current phenological growth phase for the crop.")] 
   public class PhaseLookup : Function
      {
      [Output]public override double Value
         {
         get
            {
            foreach (PhaseLookupValue P in Children)
               if (P.InPhase)
                  return P.Value;
            return 0;  // Default value is zero
            }
         }

      public override string ValueString
         {
         get
            {
            foreach (PhaseLookupValue P in Children)
               if (P.InPhase)
                  return P.ValueString;
            return "";  
            }
         }

      }



