using System;
using System.Collections.Generic;
using System.Text;

public class Biomass : Instance
   {
   [Output] public double StructuralWt = 0;
   [Output] public double NonStructuralWt = 0;
   [Output] public double Wt
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
   
