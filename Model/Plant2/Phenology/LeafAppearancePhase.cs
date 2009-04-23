using System;
using System.Collections.Generic;
using System.Text;


public class LeafAppearancePhase : Phase
   {
   #region Class Data Members

   private DateTime _StartDate;
   private double CumulativeTT=0;
   private double NodeNoAtStart = 0;
   [Param] double RemainingLeaves = 0;
   #endregion

   public LeafAppearancePhase() {}

   public LeafAppearancePhase(String N, double Target)
      {
      Name = N;
      }

   public override DateTime StartDate
      {
      get
         {
         return _StartDate;
         }
      }
   [Output] public double TTInPhase {get{return CumulativeTT;}}
   public override void DoDevelopment(DateTime Today, double TT,
                             out double LeftOverTT, out double LeftOverDays)
      {
      // Progress through this phase, returning any left over TT not used in this
      // phase. This left over TT can then be used to start off the next phase.
      Plant Plant = (Plant)Root;
      Leaf L = Plant.Organs["Leaf"] as Leaf;

      if (_StartDate.Year == 1)
         {
         _StartDate = Today;
         NodeNoAtStart = L.CohortNo;
         }
      double TTAfterStress = TT * Stress();

      // This algorithm needs to be a bit more exact than this.
      if (MeetsTarget())
         {
         CumulativeTT += TTAfterStress;
         LeftOverTT = 0.0;
         LeftOverDays = 0.0;
         }
      else
         {
         CumulativeTT += TTAfterStress;
         LeftOverTT = TT;
         LeftOverDays = 1;
         }
      }
   public override bool MeetsTarget()
      {
      Plant Plant = (Plant)Root;
      Leaf L = Plant.Organs["Leaf"] as Leaf;
      
      return (L.CohortNo >= (int) (L.FinalNodeNo-RemainingLeaves));
      }
   public override void UseLeftOverTT(DateTime Today, double LeftOverTT)
      {
      _StartDate = Today;
      CumulativeTT = LeftOverTT;
      }
   virtual protected double Stress(){return 1.0;}
   public override double FractionComplete
      {
      get
         {
         Plant Plant = (Plant)Root;
         Leaf L = Plant.Organs["Leaf"] as Leaf;
         double F = (L.CohortNo - NodeNoAtStart) / (L.FinalNodeNo - NodeNoAtStart);
         if (F < 0) F = 0;
         if (F > 1) F = 1;
         return F;
         }
      }
   public override void Reset()
      {
      _StartDate = new DateTime();
      CumulativeTT = 0;
      }
   }


      
      
