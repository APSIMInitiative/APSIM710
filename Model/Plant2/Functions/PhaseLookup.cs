using System;
using System.Collections.Generic;
using System.Text;


[Description("A value is determined depending upon the current phenological growth phase for the crop.")]
public class PhaseLookup : Function
{
    [Link]
    ModelEnvironment ModelEnvironment = null;


    [Output]
    public override double Value
    {
        get
        {
            foreach (string PhaseName in ModelEnvironment.ChildModelNames())
            {
                PhaseLookupValue P = ModelEnvironment.ModelByName(PhaseName) as PhaseLookupValue;
                if (P.InPhase)
                    return P.Value;
            }
            return 0;  // Default value is zero
        }
    }

    public override string ValueString
    {
        get
        {
            foreach (string PhaseName in ModelEnvironment.ChildModelNames())
            {
                PhaseLookupValue P = ModelEnvironment.ModelByName(PhaseName) as PhaseLookupValue;
                if (P.InPhase)
                    return P.ValueString;
            }
            return "";
        }
    }

}



