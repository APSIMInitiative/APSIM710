using System;
using System.Collections.Generic;
using System.Text;

public class Biomass : Instance
   {
   [Output][Units("g/m^2")] public double StructuralWt = 0;
   [Output][Units("g/m^2")] public double NonStructuralWt = 0;
   [Output][Units("g/m^2")] public double StructuralN = 0;
   [Output] [Units("g/m^2")]public double NonStructuralN = 0;

   [Output][Units("g/m^2")] public double Wt
   {
       get
       {
           return StructuralWt + NonStructuralWt;
       }
   }

    [Output][Units("g/m^2")] public double N
      {
      get
         {
         return StructuralN + NonStructuralN;
         }
      }
    [Output] [Units("g/m^2")] public double NConc
       {
       get
          {
          double wt = (StructuralWt + NonStructuralWt);
          double n = (StructuralN + NonStructuralN);
          if (wt > 0)
             return n / wt;
          else
             return 0.0;
          }
       }

   public void Clear()
      {
      StructuralWt = 0;
      NonStructuralWt = 0;
      }

   }
   
