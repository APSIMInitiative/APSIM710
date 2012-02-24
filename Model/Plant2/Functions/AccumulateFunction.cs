using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ModelFramework;

[Description("Adds the value of all childern functions to the previous days accumulation between start and end phases")]
public class AccumulateFunction : Function
{
    //Class members
    public double AccumulatedValue = 0;
    
    [Link]
    Phenology Phenology = null;

    [Param]
    private string StartStageName = "";

    [Param]
    private string EndStageName = "";

    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        if (Phenology.Between(StartStageName, EndStageName))
        {
        double DailyIncrement = 0.0;
        foreach (Function F in My.ChildrenAsObjects)
        {
            DailyIncrement = DailyIncrement + F.Value;
        }
        AccumulatedValue += DailyIncrement;
        }

    }

    [Output]
    public override double Value
    {
        get
        {
            return AccumulatedValue;
        }
    }

}

