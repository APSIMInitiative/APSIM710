using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

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



