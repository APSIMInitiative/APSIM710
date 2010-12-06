using System;
using System.Collections.Generic;
using System.Text;

class ReserveOrgan : BaseOrganWithLiveDead  //Neither above or below ground for now
   {
   [Param] private double InitialDM = 0;
   [Param] private double DailyRetransFraction = 0;

   [Ref("Population")] Population Population;

   public override void DoPotentialGrowth()
      {
      base.DoPotentialGrowth();
      if (Live.NonStructuralWt == 0)
         {
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
   
