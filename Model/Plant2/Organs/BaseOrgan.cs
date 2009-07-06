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
   public override double DMAllocation { set { } }
   public override double DMRetranslocation
      {
      set
         {
         if (value > 0)
            throw new Exception(Name + " cannot supply retranslocation");
         }
      }
   public override double WaterDemand { get { return 0; } }
   public override double WaterSupply { get { return 0; } }
   public override double WaterAllocation
      {
      get { return 0; }
      set { throw new Exception("Cannot set water allocation for " + Name); }
      }
   public override void DoWaterUptake(double Demand) { }
   public override void DoPotentialGrowth() { }
   public override void DoActualGrowth() { }
   }
