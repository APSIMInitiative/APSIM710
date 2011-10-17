using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSTuber : SIRIUSGenericOrgan, Reproductive, BelowGround
{
    [Link]
    Function RelativeGrowthRate = null;

    [Link]
    Phenology Phenology = null;

    public override double DMDemand
    {
        get
        {
            if (Phenology.Between("TuberInitiation", "FinalLeaf") && (InitialWt == 0))
                InitialWt = 500;                                                      //This is to initiate tuber mass so relative growth rate can kick in
            double CurrentWt = Math.Max(InitialWt, Live.Wt);  
            return CurrentWt * RelativeGrowthRate.Value;
        }
    }

}



