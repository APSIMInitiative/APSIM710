using System;
using System.Collections.Generic;
using System.Text;

class ReserveOrgan : BaseOrgan  //Neither above or below ground for now
   {
   [Param] private double InitialDM = 0;
   [Param] private double DailyRetransFraction = 0;
   public override void DoPotentialGrowth()
      {
      base.DoPotentialGrowth();
      if (Live.NonStructuralWt == 0)
         {
         Population Population = Plant.Children["Population"] as Population;
         Live.NonStructuralWt = InitialDM * Population.Value;
         }
      }
   public override double DMRetranslocationSupply
      {
      get
         {
         return Live.NonStructuralWt * DailyRetransFraction;
         }
      }
   public override double DMRetranslocation
      {
      set
         {
         if (value > Live.NonStructuralWt)
            throw new Exception("Retranslocation exceeds nonstructural biomass in organ: " + Name);
         Live.NonStructuralWt -= value;
         }
      }
   public override double DMAllocation
      {
      set
         {
         Live.NonStructuralWt += value;
         }
      }
   }
   
