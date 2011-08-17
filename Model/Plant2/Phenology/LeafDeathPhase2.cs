using System;
using System.Collections.Generic;
using System.Text;


public class LeafDeathPhase2 : Phase
   {
   [Link]   private Leaf Leaf = null;
   [Link]   private Function ThermalTime = null;
   [Output] public double CumulativeValue { get { return _CumulativeValue; } }

   private double _CumulativeValue = 0.0;

   private double DeadNodeNoAtStart;
   bool First = true;

   /// <summary>
   /// Do our timestep development
   /// </summary>
   public override double DoTimeStep(double PropOfDayToUse)
      {
          if (First)
          {
              DeadNodeNoAtStart = Leaf.DeadNodeNo;
              First = false;
          }

      _CumulativeValue += ThermalTime.Value;

      //if (Leaf.LAI == 0)
      //   return 0.00001;
      //else
      //   return 0;

      if (Leaf.DeadNodeNo >= Leaf.PrimordiaNo)
          return 0.00001;
      else
          return 0;
      }

   /// <summary>
   /// Return a fraction of phase complete.
   /// </summary>
  // public override double FractionComplete { get { return 0.0; } }
   public override double FractionComplete
   {
       get
       {
           double F = (Leaf.DeadNodeNo - DeadNodeNoAtStart) / (Leaf.PrimordiaNo - DeadNodeNoAtStart);
           if (F < 0) F = 0;
           if (F > 1) F = 1;
           return F;
       }
   }

   }


      
      
