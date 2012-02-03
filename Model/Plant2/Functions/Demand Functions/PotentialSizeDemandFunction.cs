using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class PotentialSizeDemandFunction : Function
{
    [Param]
    string StartStageName = "";

    [Param]
    string EndStageName = "";

    [Link]
    Function PotentialGrowthIncrement = null;

    [Link]
    Function OrganNumber = null;

    [Link]
    Phenology Phenology = null;

    [Link]
    public Function ThermalTime = null;

    [Link]
    public Function AccumThermalTime = null;

    [Output("AccumThermalTime")]
    [Units("oCd")]
    public double AccumulatedThermalTime //FIXME.  This is not used in Code, check is needed
    {
        get { return AccumThermalTime.Value; }
    }

    public override double Value
    {
        get
        {
            if (Phenology.Between(StartStageName, EndStageName))
                return PotentialGrowthIncrement.Value * OrganNumber.Value * ThermalTime.Value;
            else
                return 0;
        }
    }

}



