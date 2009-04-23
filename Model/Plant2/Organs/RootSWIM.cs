using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class RootSWIM : Organ, BelowGround
   {
   private double[] Uptake = null;

   [Output("rlv")][Param] public double[] rlv = null;

   public override Biomass Live
      {
      get { return new Biomass(); }
      }
   public override Biomass Dead
      {
      get { return new Biomass(); }
      }

   public override double DMDemand {get{ return 0; }}
   public override double DMSupply { get { return 0; } }
   public override double DMRetranslocationSupply { get { return 0; } }
   public override double DMRetranslocation
      {
      set 
         {
         if (value > 0) 
            throw new Exception(Name+" cannot provide retranslocation.");
         }
      }
   public override double DMAllocation
      {
      set
         {
         }
      }
   public override double WaterDemand { get { return 0; } }
   [Output]
   public override double WaterSupply
      {
      get
         {
         return 0;
         }
      }
   public override double WaterAllocation
      {
      get { return 0; }
      set
         {
         throw new Exception("Cannot set water allocation for roots");
         }
      }

   [Output]
   [Units("mm")]
   public double WaterUptake
      {
      get { return -MathUtility.Sum(Uptake); }
      }

   [EventHandler]
   public void OnWaterUptakesCalculated(WaterUptakesType Uptakes)
      {
      for (int i = 0; i != Uptakes.Uptakes.Length; i++)
         {
         if (Uptakes.Uptakes[i].Name == Plant.Name)
            Uptake = Uptakes.Uptakes[i].Amount;
         }
      }
   }

