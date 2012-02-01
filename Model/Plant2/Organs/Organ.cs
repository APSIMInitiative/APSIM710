using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

public interface AboveGround
   {
   }
public interface BelowGround
   {
   }
public interface Reproductive
   {
   }
abstract public class Organ
{
    [Link(IsOptional=true)]
    public Biomass Live = null;

    [Link(IsOptional=true)]
    public Biomass Dead = null;

    [Link]
    public Component My = null;

    public string Name { get { return My.Name; } }

    //DryMatter methods
    abstract public double DMDemand { get; }
    abstract public double DMSinkCapacity { get; }
    abstract public double DMSupply { get; }
    abstract public double DMRetranslocationSupply { get; }
    abstract public double DMPotentialAllocation { set; }
    abstract public double DMAllocation { set; }
    abstract public double DMExcessAllocation { set; }
    abstract public double DMRetranslocation { set; }
    abstract public double DMRespired { set; }

    //Nitrogen methods
    abstract public double NDemand { get; }
    abstract public double NReallocationSupply { get; }
    abstract public double NUptakeSupply { get; }
    abstract public double NFixationSupply { get; }
    abstract public double NRetranslocationSupply { get; }
    abstract public double NAllocation { set; }
    abstract public double NReallocation { set; }
    // The "_gsm" suffix of the following variable indicates that this variable, primarily used internally, is in units of g/m^2
    // This is in constrast to the Root output variable, NUptake, which is in units of kg/ha
    abstract public double NUptake_gsm { set; }  
    abstract public double NFixation { set; }
    abstract public double NRetranslocation { set; }

    //Water methods
    abstract public double WaterDemand { get; }
    abstract public double WaterSupply { get; }
    abstract public double WaterAllocation { get; set; }
    abstract public double WaterUptake { get; set; }

    //Plant actions
    virtual public void DoWaterUptake(double Demand) { }
    virtual public void DoPotentialGrowth() { }
    virtual public void DoActualGrowth() { }

    //Communicated organ variables
    abstract public double MaxNconc { get; }
    abstract public double MinNconc { get; }
    abstract public double NFixationCost { get; }
}
   
