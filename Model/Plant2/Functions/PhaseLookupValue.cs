using System;
using System.Collections.Generic;
using System.Text;


   public class PhaseLookupValue : Function
      {
      [Param] private string Start="";
      [Param] private string End="";

      public override double Value
         {
         get
            {
            if (Start == "")
               throw new Exception("Phase start name not set:" + Name);
            if (End == "")
               throw new Exception("Phase end name not set:" + Name);

            Plant P = (Plant) Root;
            if (P.Phenology.Between(Start, End))
               {
               Function Lookup = Children["Function"] as Function;
               return Lookup.Value;
               }
            else
               return 0.0;
            }
         }
      public bool InPhase
         {
         get
            {
            Plant P = (Plant) Root;
            return P.Phenology.Between(Start, End);
            }
         }
      }
   
