using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ModelFramework;

/// <summary>
/// Sum the values of the child nodes of this node
/// </summary>

public class AccumulateFunction : Function
{
    //Class members
    public double AccumulatedValue = 0;
    //private bool AccumulatedToday;

    [Link]
    Phenology Phenology = null;

    [Param]
    private string StartStageName = "";

    [Param]
    private string EndStageName = "";

    //Set flag to false so it accumulates on the first pass today
    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        //AccumulatedToday = false;
        if (Phenology.Between(StartStageName, EndStageName))
        {
        double DailyIncrement = 0.0;
        foreach (Function F in My.ChildrenAsObjects)
        {
            //Function F = ModelEnvironment.Link<Function>(ChildName);
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

