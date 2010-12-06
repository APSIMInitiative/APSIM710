using System;
using System.Collections.Generic;
using System.Text;


   public class PhaseLookupValue : Function
      {
      [Param] private string Name = "";
      [Param] private string Start = "";
      [Param] private string End="";
      [Ref("parent(Plant).Phenology")]  Phenology Phenology;
      [Ref("*")] Function[] Children;
      public override double Value
         {
         get
            {
            if (Start == "")
               throw new Exception("Phase start name not set:" + Name);
            if (End == "")
               throw new Exception("Phase end name not set:" + Name);

            if (Phenology.Between(Start, End) && Children.Length > 0)
               {
               Function Lookup = Children[0] as Function;
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
            return Phenology.Between(Start, End);
            }
         }
      }
   
