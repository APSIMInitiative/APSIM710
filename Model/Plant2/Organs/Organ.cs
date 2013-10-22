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
public class BiomassPoolType
{
    public double Structural;
    public double NonStructural;
    public double Metabolic;
}
public class BiomassSupplyType
{
    public double Fixation;
    public double Reallocation;
    public double Uptake;
    public double Retranslocation;
}    
public class BiomassAllocationType
{
    public double Structural;
    public double NonStructural;
    public double Metabolic;
    public double Retranslocation;
    public double Reallocation;
    public double Respired;
    public double Uptake;
    public double Fixation;
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
    abstract public BiomassPoolType DMPotentialAllocation { set; }
    abstract public BiomassPoolType DMDemand { get; set; }
    abstract public BiomassSupplyType DMSupply { get; set; }
    abstract public BiomassAllocationType DMAllocation { set; }
    //Nitrogen methods
    abstract public BiomassPoolType NDemand { get; set; }
    abstract public BiomassSupplyType NSupply { get; set; }
    abstract public BiomassAllocationType NAllocation { set; }
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



   
