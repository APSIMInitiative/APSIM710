using System;
using System.Collections.Generic;
using System.Text;

public interface AboveGround
   {
   }
public interface BelowGround
   {
   }
public interface Reproductive
   {
   }
abstract public class Organ : Instance
   {
   abstract public Biomass Live { get;}
   abstract public Biomass Dead { get;}

   abstract public double DMDemand{ get; }
   abstract public double DMSupply { get; }
   abstract public double DMRetranslocationSupply { get;}
   abstract public double DMPotentialAllocation { set; }
   abstract public double DMAllocation {set; }
   abstract public double DMRetranslocation { set;}
   abstract public double DMRespired { set; }

   abstract public double NDemand { get; }
   abstract public double NUptakeSupply { get; }
   abstract public double NRetranslocationSupply { get; }
   abstract public double NAllocation { set; }
   abstract public double NRetranslocation { set; }
   abstract public double NUptake { set; }
   abstract public double NFixationSupply { get ;}
   abstract public double NFixation { set; }

   abstract public double WaterDemand { get; }
   abstract public double WaterSupply { get; }
   abstract public double WaterAllocation { get; set;}
   abstract public double WaterUptake { get; set; }
   virtual public void DoWaterUptake(double Demand) { }
   virtual public void DoPotentialGrowth() { }
   virtual public void DoActualGrowth() { }
  
   abstract public double MaxNconc { get; }
   abstract public double MinNconc { get; }
   abstract public double NReallocationSupply { get; }
   abstract public double NReallocation { set; }
   }
   
