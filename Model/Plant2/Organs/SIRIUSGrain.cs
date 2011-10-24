using System;
using System.Collections.Generic;
using System.Text;

class SIRIUSGrain : ReproductiveOrgan
{
    [Link(IsOptional.Yes)]
    protected Function NitrogenDemandSwitch = null;
    
    [Link]
    protected Function MaximumNConc = null;

    [Link]
    protected Function MinimumNConc = null;

    private double PotentialDMAllocation = 0;

    

    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in" + Name);
            PotentialDMAllocation = value;
        }
    }
    public override double NDemand
    {
        get
        {
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
            double demand = Number * NFillingRate.Value;
            return Math.Min(demand, MaximumNConc.Value * DailyGrowth) * _NitrogenDemandSwitch;
        }

    }
    public override double MaxNconc
    {
        get
        {
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            return MinimumNConc.Value;
        }
    }
}
