using System;
using System.Collections.Generic;
using System.Text;


public class LeafDeathPhase : Phase
   {
   [Link]   private Leaf Leaf = null;
   [Link]   private Function ThermalTime = null;
   [Output] public double CumulativeValue { get { return _CumulativeValue; } }

   private double _CumulativeValue = 0.0;


   /// <summary>
   /// Do our timestep development
   /// </summary>
   public override double DoTimeStep(double PropOfDayToUse)
      {
      _CumulativeValue += ThermalTime.Value;

      if (Leaf.LAI == 0)
         return 0.00001;
      else
         return 0; 
      }

   /// <summary>
   /// Return a fraction of phase complete.
   /// </summary>
   public override double FractionComplete { get { return 0.0; } }

   }


      
      
