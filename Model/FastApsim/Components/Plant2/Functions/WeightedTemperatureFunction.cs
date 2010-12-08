using System;
using System.Collections.Generic;
using System.Text;

public class WeightedTemperatureFunction : Function
   {
   #region Class Data Members
   [Param] private LinearInterpolation XYPairs = null;   // Temperature effect on Growth Interpolation Set
   [Param] private double MaximumTemperatureWeighting = 0.0;
   [Ref(".simulation.met")] Met Met;
   #endregion

   [Output]
   [Units("0-1")]
   public override double Value
      {
      get
         {
         double Tav = MaximumTemperatureWeighting * Met.MaxT + (1-MaximumTemperatureWeighting) * Met.MinT;
         return XYPairs.Value(Tav);
         }
      }

   }
   
