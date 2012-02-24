using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("This must be renamed DMDemandFunction for the source code to recoginise it!!!!.  This function returns the specified proportion of total DM supply.  The organ may not get this proportion if the sum of demands from other organs exceeds DM supply")]
public class PartitionFractionDemandFunction : Function
{
    [Link]
    Function PartitionFraction = null;

    [Link]
    Arbitrator Arbitrator = null;

    public override double Value
    {
        get
        {
            return Arbitrator.DMSupply * PartitionFraction.Value;
        }
    }

}



