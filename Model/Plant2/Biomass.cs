using System;
using System.Collections.Generic;
using System.Text;

public class Biomass : Instance
   {
   [Output][Units("g/m^2")] public double StructuralWt = 0;
   [Output][Units("g/m^2")] public double NonStructuralWt = 0;
   [Output][Units("g/m^2")] public double Wt
      {
      get
         {
         return StructuralWt + NonStructuralWt;
         }
      }
   public void Clear()
      {
      StructuralWt = 0;
      NonStructuralWt = 0;
      }

   }
   
