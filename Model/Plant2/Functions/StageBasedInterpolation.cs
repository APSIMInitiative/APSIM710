using System;
using System.Collections.Generic;
using System.Text;

[Description("A value is linearly interpolated between phenological growth stages")]
public class StageBasedInterpolation : Function
{
    [Link]
    Phenology Phenology = null;

    [Param]
    string[] Stages = null;

    [Param]
    double[] Values = null;

    [Output]
    public override double Value
    {
        get
        {
            double Value = Values[0];
            Phase P = Phenology.CurrentPhase;
            for (int i = 0; i < Stages.Length; i++)
            {
                if (!Phenology.IsValidPhase(Stages[i]))
                    throw new Exception("Invalid stage name specified in StageLookup. Stage name = " + Stages[i]);

                if (Stages[i] == P.Start)
                {
                    Value = Values[i];
                    if (i < Stages.Length - 1)
                        Value += P.FractionComplete * (Values[i + 1] - Values[i]);

                }
            }

            return Value;
        }
    }

}

