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
            if (P.Phenology.Between(Start, End) && Children.Count > 0)
               {
               Function Lookup = Children[0] as Function;
               return Lookup.Value;
               }
            else
               return 0.0;
            }
         }
      public override string ValueString
         {
         get
            {
            if (Start == "")
               throw new Exception("Phase start name not set:" + Name);
            if (End == "")
               throw new Exception("Phase end name not set:" + Name);

            Plant P = (Plant)Root;
            if (P.Phenology.Between(Start, End) && Children.Count > 0)
               {
               Function Lookup = Children[0] as Function;
               return Lookup.ValueString;
               }
            else
               return "";
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
   
