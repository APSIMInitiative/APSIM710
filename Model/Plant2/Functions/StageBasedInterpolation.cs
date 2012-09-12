using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("A value is linearly interpolated between phenological growth stages")]
public class StageBasedInterpolation : Function
{
    [Link]
    Phenology Phenology = null;

    [Param]
    string[] Stages = null;

    int[] StageCodes = null;

    [Param]
    double[] Values = null;

    [Param(IsOptional=true)]
    bool Proportional = true;

    [Output]
    public override double Value
    {
        get
        {
            if (StageCodes == null)
            {
                StageCodes = new int[Stages.Length];
                for (int i = 0; i < Stages.Length; i++)
                {
                    Phase p = Phenology.PhaseStartingWith(Stages[i]);
                    StageCodes[i] = Phenology.IndexOfPhase(p.Name) + 1;
                }
            }

            for (int i = 0; i < StageCodes.Length; i++)
            {
                if (Phenology.Stage <= StageCodes[i])
                {
                    if (i == 0)
                        return Values[0];
                    if (Phenology.Stage == StageCodes[i])
                        return Values[i];

                    if (Proportional)
                    {
                        double slope = MathUtility.Divide(Values[i] - Values[i - 1],
                                                            StageCodes[i] - StageCodes[i - 1],
                                                            Values[i]);
                        return Values[i] + slope * (Phenology.Stage - StageCodes[i]);
                    }
                    else
                    {
                        // Simple lookup.
                        return Values[i - 1];
                    }
                }
            }
            return Values[StageCodes.Length - 1];
        }
    }

}

