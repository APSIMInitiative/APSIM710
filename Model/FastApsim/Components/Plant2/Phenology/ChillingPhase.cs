using System;
using System.Collections.Generic;
using System.Text;


public class ChillingPhase : Phase
   {
   #region Class Data Members

   private DateTime _StartDate;
   [Param] private double CDTarget;
   private double CumulativeCD=0;
   #endregion

   public ChillingPhase() {}

   public override DateTime StartDate
      {
      get
         {
         return _StartDate;
         }
      }
   [Output] public double CDInPhase {get{return CumulativeCD;}}
   public override void DoDevelopment(DateTime Today, double TT,
                             out double LeftOverTT, out double LeftOverDays)
      {
      // Progress through this phase, returning any left over TT not used in this
      // phase. This left over TT can then be used to start off the next phase.
      if (_StartDate.Year == 1)
         _StartDate = Today;

      
      Function ChillingDays = Children["ChillingDays"] as Function;
      double CD = ChillingDays.Value;

      if (CDTarget == 0 || CumulativeCD + CD <= CDTarget)
         {
         CumulativeCD += CD;
         LeftOverTT = 0.0;
         LeftOverDays = 0.0;
         }
      else
         {
         LeftOverTT = 0.0;
         LeftOverDays = 0.0;
         CumulativeCD = CDTarget;
         }
      }
   public override bool MeetsTarget(){return CDTarget > 0 && CumulativeCD == CDTarget;}
   public override void UseLeftOverTT(DateTime Today, double LeftOverTT)
      {
      _StartDate = Today;
      //CumulativeTT = LeftOverTT;
      }
   virtual protected double Stress(){return 1.0;}
   public override double FractionComplete
      {
      get
         {
         return CumulativeCD / CDTarget;
         }
      }
   public override void Reset()
      {
      _StartDate = new DateTime();
      CumulativeCD = 0;
      }
   }


      
      
