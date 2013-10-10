using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;


public class BaseOrgan : Organ
{
    [Link]
    public Clock Clock = null;

    [Link]
    public MetFile MetData = null;

    public override DMSupplyType DMSupply { get { return new DMSupplyType(); } set { } }
    public override double DMPotentialAllocation { set { } }
    public override DMAllocationType DMAllocation { set { } }
    public override DMDemandType DMDemand { get { return new DMDemandType(); } set { } }

    public override NSupplyType NSupply { get { return new NSupplyType(); } set { } }
    public override NAllocationType NAllocation { set {  } }
    public override double NFixationCost { get { return 0; } set { } }
    public override NDemandType NDemand { get { return new NDemandType(); } set { } }

    public override double WaterDemand { get { return 0; } set { } }
    public override double WaterSupply { get { return 0; } set { } }
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

    public override double MaxNconc { get { return 0; } set { } }
    public override double MinNconc { get { return 0; } set { } }

    
    
    // Provide some variables for output until we get a better REPORT component that
    // can do structures e.g. NSupply.Fixation
    
    [Output]
    [Units("g/m^2")]
    public double DMSupplyPhotosynthesis { get { return DMSupply.Photosynthesis; } }

    [Output]
    [Units("g/m^2")]
    public double NSupplyUptake { get { return NSupply.Uptake; } }

    // Methods that can be called from manager
    public override void OnSow(SowPlant2Type SowData) { }
    public override void OnHarvest() { }
    public override void OnEndCrop() { }
}
