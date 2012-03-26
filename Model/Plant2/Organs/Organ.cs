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

public class DMSupplyType
{
    public double Photosynthesis;
    public double Retranslocation;
}
public class DMAllocationType
{
    public double Allocation;
    public double ExcessAllocation;
    public double Retranslocation;
    public double Respired; 
}
public class NAllocationType
{
    public double Allocation;
    public double Reallocation;
    // The "_gsm" suffix of the following variable indicates that this variable, primarily used internally, is in units of g/m^2
    // This is in constrast to the Root output variable, NUptake, which is in units of kg/ha
    public double Uptake_gsm;
    public double Fixation;
    public double Retranslocation;
}
public class NSupplyType
{
    public double Reallocation;
    public double Uptake;
    public double Fixation;
    public double Retranslocation;
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
    abstract public DMSupplyType DMSupply { get; }
    abstract public double DMPotentialAllocation { set; }
    abstract public DMAllocationType DMAllocation { set; }

    //Nitrogen methods
    abstract public double NDemand { get; }
    abstract public NSupplyType NSupply { get; }
    abstract public NAllocationType NAllocation { set; }

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

    // Methods that can be called .e.g from manager
    abstract public void OnSow(SowPlant2Type Sow);
    abstract public void OnHarvest();
    abstract public void OnEndCrop();
}
   
