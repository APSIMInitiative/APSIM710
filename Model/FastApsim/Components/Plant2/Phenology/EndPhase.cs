using System;
using System.Collections.Generic;
using System.Text;


public class EndPhase : Phase
   {
   private double _CumulativeValue;
   [Ref("ThermalTime")] Function F;

   [Output] public double CumulativeValue { get { return _CumulativeValue; } }

   /// <summary>
   /// Do our timestep development
   /// </summary>
   public override double DoTimeStep(double PropOfDayToUse)
      {
      _CumulativeValue += F.Value;
      return 0;
      }

   /// <summary>
   /// Return a fraction of phase complete.
   /// </summary>
   public override double FractionComplete { get { return 0.0; } }

   }


      
      
