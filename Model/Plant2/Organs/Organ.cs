using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

#region Class descriptor properties
public interface AboveGround
   {
   }
public interface BelowGround
   {
   }
public interface Reproductive
   {
   }
#endregion

#region Arbitrator method types
public class DMDemandType
{
    public double Structural;
    public double NonStructural;
    public double Metabolic;
}
public class NDemandType
{
    public double Structural;
    public double NonStructural;
    public double Metabolic;
}
public class DMSupplyType
{
    public double Photosynthesis;
    public double Retranslocation;
    public double Reallocation;
}
public class DMAllocationType
{
    public double Allocation;
    public double ExcessAllocation;
    public double Retranslocation;
    public double Reallocation;
    public double Respired; 
}
public class NAllocationType
{
    public double Allocation;
    public double Reallocation;
    public double Uptake_gsm; // The "_gsm" suffix of the following variable indicates that this variable, primarily used internally, is in units of g/m^2.  This is in constrast to the Root output variable, NUptake, which is in units of kg/ha
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
#endregion

abstract public class Organ
{
    #region Links to other models or compontnets 
    [Link(IsOptional=true)]
    public Biomass Live = null;
    [Link(IsOptional=true)]
    public Biomass Dead = null;
    [Link]
    public Component My = null;
    #endregion

    #region Fields
    public string Name { get { return My.Name; } }
    #endregion

    #region Organ - Arbitrator interface methods
    //DryMatter methods
    abstract public double DMPotentialAllocation { set; }
    abstract public DMDemandType DMDemand { get; set; }
    abstract public DMSupplyType DMSupply { get; set; }
    abstract public DMAllocationType DMAllocation { set; }
    //Nitrogen methods
    abstract public NDemandType NDemand { get; set; }
    abstract public NSupplyType NSupply { get; set; }
    abstract public NAllocationType NAllocation { set; }
    //Water methods
    abstract public double WaterDemand { get; set; }
    abstract public double WaterSupply { get; set; }
    abstract public double WaterAllocation { get; set; }
    abstract public double WaterUptake { get; set; }
    //Communicated organ variables
    abstract public double MaxNconc { get; set; }
    abstract public double MinNconc { get; set; }
    abstract public double NFixationCost { get; set; }
    #endregion

    #region Top Level Time-step  and event Functions
    //Plant actions
    virtual public void DoWaterUptake(double Demand) { }
    virtual public void DoPotentialGrowth() { }
    virtual public void DoActualGrowth() { }
    // Methods that can be called .e.g from manager
    abstract public void OnSow(SowPlant2Type Sow);
    abstract public void OnHarvest();
    abstract public void OnEndCrop();
    #endregion
}    



   
