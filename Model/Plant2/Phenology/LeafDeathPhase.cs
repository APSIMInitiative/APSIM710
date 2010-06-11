using System;
using System.Collections.Generic;
using System.Text;


public class LeafDeathPhase : Phase
   {
   private double _CumulativeValue;

   [Output] public double CumulativeValue { get { return _CumulativeValue; } }

   /// <summary>
   /// Initialise everything
   /// </summary>
   public override void Initialising() { _CumulativeValue = 0.0; }

   /// <summary>
   /// Do our timestep development
   /// </summary>
   public override double DoTimeStep(double PropOfDayToUse)
      {
      Function F = Children["ThermalTime"] as Function;
      _CumulativeValue += F.Value;

      Plant Plant = (Plant)Root;
      Leaf L = Plant.Organs["Leaf"] as Leaf;
      if (L.LAI == 0)
         return 0.00001;
      else
         return 0; 
      }

   /// <summary>
   /// Return a fraction of phase complete.
   /// </summary>
   public override double FractionComplete { get { return 0.0; } }

   }


      
      
