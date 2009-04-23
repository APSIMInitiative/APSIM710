using System;
using System.Collections.Generic;
using System.Text;


public class FixedPhase : Phase
   {
   #region Class Data Members

   private DateTime _StartDate;
   [Param] private double TTTarget;
   private double CumulativeTT=0;
   #endregion

   public FixedPhase() {}

   public FixedPhase(String N, double Target)
      {
      Name = N;
      TTTarget = Target;
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
      if (_StartDate.Year == 1)
         _StartDate = Today;

      double TTAfterStress = TT * Stress();

      if (TTTarget == 0 || CumulativeTT + TTAfterStress <= TTTarget)
         {
         CumulativeTT += TTAfterStress;
         LeftOverTT = 0.0;
         LeftOverDays = 0.0;
         }
      else
         {
         LeftOverTT = CumulativeTT + TTAfterStress - TTTarget;
         LeftOverDays = LeftOverTT / TTTarget;
         CumulativeTT = TTTarget;
         }
      }
   public override bool MeetsTarget(){return TTTarget > 0 && CumulativeTT == TTTarget;}
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
         return CumulativeTT / TTTarget;
         }
      }
   public override void Reset()
      {
      _StartDate = new DateTime();
      CumulativeTT = 0;
      }
   }


      
      
