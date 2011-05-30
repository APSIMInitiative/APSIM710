using System;
using System.Collections.Generic;
using System.Text;

public class BaseOrgan : Organ
   {
   public override Biomass Live { get { return (Biomass) Children["live"]; } }
   public override Biomass Dead { get { return (Biomass) Children["dead"]; } }

   public override double DMDemand { get { return 0; } }
   public override double DMSupply { get { return 0; } }
   public override double DMRetranslocationSupply { get { return 0; } }
   public override double DMPotentialAllocation { set { } }
   public override double DMAllocation { set { } }
   public override double DMRetranslocation
      {
      set
         {
         if (value > 0)
            throw new Exception(Name + " cannot supply retranslocation");
         }
      }
   public override double DMRespired { set { } }
   
   public override double NDemand { get { return 0; } }
   public override double NUptakeSupply { get { return 0; } }
   public override double NRetranslocationSupply { get { return 0; } }
   public override double NAllocation { set { } }
   public override double NRetranslocation
   {
       set
       {
           if (value > 0)
               throw new Exception(Name + " cannot supply N retranslocation");
       }
   }
   public override double NUptake
      {
      set
         {
         if (value > 0)
            throw new Exception(Name + " cannot supply N Uptake");
         }
      }
   public override double NFixationSupply { get { return 0; } }
   public override double NFixation { set { } }

   public override double WaterDemand { get { return 0; } }
   public override double WaterSupply { get { return 0; } }
   public override double WaterUptake
      {
      get { return 0; }
      set { throw new Exception("Cannot set water uptake for " + Name); }
      }
   public override double WaterAllocation
      {
      get { return 0; }
      set { throw new Exception("Cannot set water allocation for " + Name); }
      }
   public override void DoWaterUptake(double Demand) { }
   public override void DoPotentialGrowth() { }
   public override void DoActualGrowth() { }
   
   public override double MaxNconc { get { return 0; } }
   public override double MinNconc { get { return 0; } }
   public override double NReallocationSupply { get { return 0; } }
   public override double NReallocation
   {
       set
       {
           if (value > 0)
               throw new Exception(Name + " cannot supply N reallocation");
       }
   }
   }
