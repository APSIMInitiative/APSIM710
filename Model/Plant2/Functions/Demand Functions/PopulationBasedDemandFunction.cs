using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("This must be renamed DMDemandFunction for the source code to recoginise it!!!!.  This function calculates DM demand from the start stage over the growth duration as the product of potential growth rate (MaximumOrganWt/GrowthDuration) and daily thermal time. It returns the product of this potential rate and any childern so if other stress multipliers are required they can be constructed with generic functions.  Stress factors are optional")]
public class PopulationBasedDemandFunction : Function
{
    [Link]
    public Function ThermalTime = null;

    [Link]
    Leaf Leaf = null;

    [Link]
    StageBasedInterpolation StageCode = null;

    [Link]
    protected Function ExpansionStress = null;

    [Param]
    [Description("Size individual organs will grow to when fully supplied with DM")]
    private double MaximumOrganWt = 0;

    [Param]
    [Description("Stage when organ growth starts ")]
    private double StartStage = 0;

    [Param]
    [Description("ThermalTime duration of organ growth ")]
    private double GrowthDuration = 0;

    private double AccumulatedThermalTime = 0;
    private double ThermalTimeToday = 0;

    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        if ((StageCode.Value >= StartStage) && (AccumulatedThermalTime < GrowthDuration))
        {
            ThermalTimeToday = Math.Min(ThermalTime.Value, GrowthDuration - AccumulatedThermalTime);
            AccumulatedThermalTime += ThermalTimeToday;
        }
    }

    [Output]
    public override double Value
    {
        get
        {
            double Value = 0.0;
            if ((StageCode.Value >= StartStage) && (AccumulatedThermalTime < GrowthDuration))
            {
                double Rate = MaximumOrganWt / GrowthDuration;
                Value = Rate * ThermalTimeToday * Leaf.TotalStemPopn;
            }

            return Value * ExpansionStress.Value;
        }
    }

}



