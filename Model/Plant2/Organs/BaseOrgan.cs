using System;
using System.Collections.Generic;
using System.Text;

class BaseOrgan : Organ
   {
   private Biomass _Live = new Biomass();
   private Biomass _Dead = new Biomass();

   public override Biomass Live { get { return _Live; } }
   public override Biomass Dead { get { return _Dead; } }

   [Output][Units("g/m^2")] public double LiveWt { get { return Live.Wt; } }
   [Output][Units("g/m^2")] public double DeadWt { get { return Dead.Wt; } }

   public override double DMDemand { get { return 0; } }
   public override double DMSupply { get { return 0; } }
   public override double DMRetranslocationSupply { get { return 0; } }
   public override double DMAllocation { set { } }
   public override double DMRetranslocation { set { } }
   public override double WaterDemand { get { return 0; } }
   public override double WaterSupply { get { return 0; } }
   public override double WaterAllocation { get { return 0; } set { } }
   public override void DoWaterUptake(double Demand) { }
   public override void DoPotentialGrowth() { }
   public override void DoActualGrowth() { }
   }
