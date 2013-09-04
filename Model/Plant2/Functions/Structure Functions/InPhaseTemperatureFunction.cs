using System;
using System.Collections.Generic;
using System.Text;

[Description("Returns the curreent InPhase tempature accumulation")]

public   class InPhaseTemperatureFunction: Function
{
    [Link]
    Phenology Phenology = null;


    public override double Value
    {
        get
        {
            return Phenology.CurrentPhase.TTinPhase;
        }
    }
}

