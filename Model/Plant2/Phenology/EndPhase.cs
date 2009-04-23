using System;
using System.Collections.Generic;
using System.Text;


public class EndPhase : Phase
   {
   #region Class Data Members

   private DateTime _StartDate;
   private double CumulativeTT;
   #endregion

   public EndPhase() {}

   public EndPhase(String N)
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
   [Output] public double TTInPhase
      {
      get
         {
         return CumulativeTT;
         }
      }
   public override void DoDevelopment(DateTime Today, double TT,
                             out double LeftOverTT, out double LeftOverDays)
      {
      // Progress through this phase, returning any left over TT not used in this
      // phase. This left over TT can then be used to start off the next phase.
      if (_StartDate.Year == 1)
         _StartDate = Today;

      double TTAfterStress = TT * Stress();

      CumulativeTT += TTAfterStress;
      LeftOverTT = 0.0;
      LeftOverDays = 0.0;
       }
   public override bool MeetsTarget()
      {
      return false;
      }
   public override void UseLeftOverTT(DateTime Today, double LeftOverTT)
      {
      _StartDate = Today;
      CumulativeTT = LeftOverTT;
      }
   virtual protected double Stress()
      {
      return 1.0;
      }
   public override double FractionComplete
      {
      get
         {
         return 0;
         }
      }
   public override void Reset()
      {
      _StartDate = new DateTime();
      CumulativeTT = 0;
      }
   }


      
      
