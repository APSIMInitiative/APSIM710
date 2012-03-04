using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Calculates the potential height increment and then multiplies it by the smallest of any childern functions (Child functions represent stress)")]
public class HeightFunction : Function
{
    [Link(NamePath = "PotentialHeight")]
    public Function PotentialHeight = null;

    double PotentialHeightYesterday = 0;
    double Height = 0;
    [Output]
    public double DeltaHeight = 0;

    [Output]
    public override double Value
    {
        get
        {
            double PotentialHeightIncrement = PotentialHeight.Value - PotentialHeightYesterday;
            double StressValue = 1.0;
            
            foreach (Function F in My.ChildrenAsObjects)
            {
                StressValue = Math.Min(StressValue, F.Value);
            }
            DeltaHeight = PotentialHeightIncrement * StressValue;
            PotentialHeightYesterday = PotentialHeight.Value;
            return Height += DeltaHeight;
        }
    }
}

