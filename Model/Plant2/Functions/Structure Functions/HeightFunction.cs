using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Calculates the potential height increment and then multiplies it by the smallest of any childern functions (Child functions represent stress)")]
public class HeightFunction : Function
{
    [Link(NamePath = "PotentialHeight")]
    public Function PotentialHeight = null;
    bool ValueIncremented = false;
    double PotentialHeightYesterday = 0;
    double Height = 0;
    [Output]
    public double DeltaHeight = 0;
    public override void UpdateVariables(string initial)
    {
        double PotentialHeightIncrement = PotentialHeight.Value - PotentialHeightYesterday;
        double StressValue = 1.0;
        //This function is counting potential height as a stress.
        foreach (Function F in My.ChildrenAsObjects)
        {
            StressValue = Math.Min(StressValue, F.Value);
        }
        DeltaHeight = PotentialHeightIncrement * StressValue;
        PotentialHeightYesterday = PotentialHeight.Value;
        Height += DeltaHeight;
    }
    [Output]
    public override double Value
    {
        get
        {
           return Height;
        }
    }
}

