using System;
using System.Collections.Generic;
using System.Text;


   class LeafCohort
      {
      #region Class Data Members
      private double _Population = 0;
      private double Age = 0;
      private double Rank = 0;
      private double PotentialAreaGrowth = 0;
      public double LiveArea = 0;
      public double MaxLiveArea = 0;
      public double DeadArea = 0;
      public Biomass Live = new Biomass();
      public Biomass Dead = new Biomass();      

      public double MaxArea = 0;
      private double GrowthDuration = 0;
      private double LagDuration = 0;
      private double SenescenceDuration = 0;
      private double SpecificLeafAreaMax = 0;

      #endregion
      public bool Finished
         {
         get{
         return Age > (GrowthDuration+LagDuration+SenescenceDuration);
         }
         }

      public double MaxSize
         {
         get
            {
            return MaxLiveArea/Population;
            }
         }

      public double Population
         {
         get
            {
            return _Population;
            }
         }
      public double Size
         {
         get
            {
            return LiveArea / Population;
            }
         }

      public LeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea)
         {
         _Population = popn;
         Rank = rank;
         Age = age;
         MaxArea = ma.Value;
         GrowthDuration = gd.Value;
         LagDuration = ld.Value;
         SenescenceDuration = sd.Value;
         SpecificLeafAreaMax = sla.Value;
         //if (InitialArea != 0)
         //   if (InitialArea < MaxArea)
         //      Age = GrowthDuration * InitialArea / MaxArea;
         //   else
         //      Age = GrowthDuration;


         LiveArea = InitialArea * Population;
         Live.StructuralWt = LiveArea / SpecificLeafAreaMax;

         }

      public bool IsDead
         {
         get
            {
            return (LiveArea == 0);
            }
         }
      public bool IsFullyExpanded
         {
         get
            {
            return (Age > GrowthDuration);
            }
         }
      public double FractionExpanded
         {
         get
            {
            if (Age < GrowthDuration)
               return Age / GrowthDuration;
            else
               return 1.0;
            }
         }

      public void DoPotentialGrowth(double TT)
         {
         PotentialAreaGrowth = PotentialAreaGrowthFunction(TT);
         }
      public double PotentialAreaGrowthFunction(double TT)
         {
         //return MaxArea*Population * Math.Min(TT, Math.Max(0, GrowthDuration - Age)) / GrowthDuration;

         double growth = Population * (SizeFunction(Age + TT) - SizeFunction(Age));
         return growth;
         }
      private double SizeFunction(double TT)
         {
         double alpha = -Math.Log((1 / 0.99 - 1) / (MaxArea / (MaxArea * 0.01) - 1)) / GrowthDuration;
         double leafsize=MaxArea / (1 + (MaxArea / (MaxArea * 0.01)-1) * Math.Exp(-alpha * TT));
         return leafsize;

         }

      public double DMDemand(double TT)
         {
         return PotentialAreaGrowth / SpecificLeafAreaMax;
         }
      public double DMAllocation
         {
         set
            {
            Live.StructuralWt += value;
            //LiveArea += Math.Min(PotentialAreaGrowth,value * SpecificLeafAreaMax);
            LiveArea += PotentialAreaGrowth;
            LiveArea = Math.Min(LiveArea, Live.StructuralWt * SpecificLeafAreaMax);
            }
         }

      public void DoActualGrowth(double TT)
         {
         if (MaxLiveArea < LiveArea)
            MaxLiveArea = LiveArea;
         
         //Calculate Senescence
         double FractionSenescing = 0;
         double AreaSenescing = 0;
         if (Age > LagDuration + GrowthDuration)
            {
            double LeafDuration = GrowthDuration + LagDuration + SenescenceDuration;
            double RemainingTT = Math.Max(0, LeafDuration - Age);

            if (RemainingTT == 0)
               FractionSenescing = 1;
            else
               FractionSenescing = Math.Min(1, TT / RemainingTT);

            // Update State Variables
            AreaSenescing = LiveArea * FractionSenescing;
            DeadArea = DeadArea + AreaSenescing;
            LiveArea = LiveArea - AreaSenescing;

            double MassSenescing = FractionSenescing * Live.StructuralWt;
            Live.StructuralWt -= MassSenescing;
            Dead.StructuralWt += MassSenescing;
            }

         Age = Age + TT;         
         
         }

      public void DoFrost(double fraction)
         {
         double change;
         change = LiveArea * fraction;
         LiveArea -=change;
         DeadArea += change;

         change = Live.StructuralWt * fraction;
         Live.StructuralWt -= change;
         Dead.StructuralWt += change;

         change = Live.NonStructuralWt * fraction;
         Live.NonStructuralWt -= change;
         Dead.NonStructuralWt += change;


         }
      }
   
