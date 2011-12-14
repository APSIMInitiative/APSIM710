using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class PartitionFractionDemandFunction : Function
{
    [Link]
    Function PartitionFraction = null;

    [Link]
    BaseArbitrator Arbitrator = null;

    public override double Value
    {
        get
        {
            return Arbitrator.DMSupply * PartitionFraction.Value;
        }
    }

}



