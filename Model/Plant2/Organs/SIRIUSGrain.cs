using System;
using System.Collections.Generic;
using System.Text;

class SIRIUSGrain : ReproductiveOrgan
{
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
            Function NFillingRate = Children["NFillingRate"] as Function;
            Function MaxNconc = Children["MaximumNconc"] as Function;
            Function NitrogenDemandSwitch = Children["NitrogenDemandSwitch"] as Function;
            double demand = Number * NFillingRate.Value;
            return Math.Min(demand, MaxNconc.Value * DailyGrowth) * NitrogenDemandSwitch.Value;
        }

    }
    public override double MaxNconc
    {
        get
        {
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            return MinimumNConc.Value;
        }
    }
}
