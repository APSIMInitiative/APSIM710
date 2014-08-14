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

    [Param(IsOptional = true)]
    private double FractionRemovedOnCut = 0; //FIXME: This should be passed from teh manager when "cut event" is called. Must be made general to other events.

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

    [EventHandler]
    public void OnCut()
    {
        AccumulatedValue -= FractionRemovedOnCut * AccumulatedValue;
    }
    [EventHandler]
    public void OnPrune()
    {
        AccumulatedValue = 0;
    }

}

