using System;
using System.Collections.Generic;
using System.Text;


public class VernalPhase : Phase
   {
   #region Class Data Members

   private DateTime _StartDate;
   [Param] private LinearInterpolation TTTarget = null;  //TT Target is a function of Cumulative Vernal Days
   [Output] private double CumulativeVD = 0;
   private double CumulativeTT = 0;
   
   #endregion

   public VernalPhase() {}
   public override DateTime StartDate { get { return _StartDate; } }
   [Output] public double TTInPhase   { get { return CumulativeTT;}}
   public override void DoDevelopment(DateTime Today, double TT,
                             out double LeftOverTT, out double LeftOverDays)
      {
      // Progress through this phase, returning any left over TT not used in this
      // phase. This left over TT can then be used to start off the next phase.
      if (_StartDate.Year == 1)
         _StartDate = Today;
      TemperatureFunction VDModel = (TemperatureFunction)Children["VDModel"];
      CumulativeVD += VDModel.Value;
      double Target = TTTarget.Value(CumulativeVD);
      double TTAfterStress = TT * Stress();

      if (Target == 0 || CumulativeTT + TTAfterStress <= Target)
         {
         CumulativeTT += TTAfterStress;
         LeftOverTT = 0.0;
         LeftOverDays = 0.0;
         }
      else
         {
         LeftOverTT = CumulativeTT + TTAfterStress - Target;
         LeftOverDays = LeftOverTT / Target;
         CumulativeTT = Target;
         }
      }
   public override bool MeetsTarget(){return TTTarget.Value(CumulativeVD) > 0 && CumulativeTT == TTTarget.Value(CumulativeVD);}
   public override void UseLeftOverTT(DateTime Today, double LeftOverTT)
      {
      _StartDate = Today;
      CumulativeTT = LeftOverTT;
      }
   virtual protected double Stress() {return 1.0;}
   public override double FractionComplete
      {
      get
         {
         return CumulativeTT / TTTarget.Value(CumulativeVD);
         }
      }
   public override void Reset()
      {
      _StartDate = new DateTime();
      CumulativeVD = 0;
      CumulativeTT = 0;
      }
   }


      
      
