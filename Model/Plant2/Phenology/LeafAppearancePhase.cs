using System;
using System.Collections.Generic;
using System.Text;

public class LeafAppearancePhase : Phase
   {
   private double CumulativeTT;
   private double NodeNoAtStart;
   private Leaf L;
   [Param] private double RemainingLeaves = 0;
   [Output] public double TTInPhase { get { return CumulativeTT; } }


   /// <summary>
   /// Initialise everything
   /// </summary>
   public override void Initialising()
      { 
      CumulativeTT = 0;
      NodeNoAtStart = 0;
      L = null;
      }

   /// <summary>
   /// Do our timestep development
   /// </summary>
   public override double DoTimeStep(double PropOfDayToUse)
      {
      if (L == null)
         {
         Plant Plant = (Plant)Root;
         L = Plant.Organs["Leaf"] as Leaf;
         NodeNoAtStart = L.CohortNo;
         }

      // Accumulate thermal time.
      Function TT = Children["ThermalTime"] as Function;
      CumulativeTT += TT.Value;

      if (L.FullyExpandedNodeNo >= (int)(L.FinalNodeNo - RemainingLeaves))
         return 0.00001;
      else
         return 0;
      }

   /// <summary>
   /// Return a fraction of phase complete.
   /// </summary>
   public override double FractionComplete
      {
      get
         {
         double F = (L.CohortNo - NodeNoAtStart) / (L.FinalNodeNo - NodeNoAtStart);
         if (F < 0) F = 0;
         if (F > 1) F = 1;
         return F;
         }
      }


   }


      
      
