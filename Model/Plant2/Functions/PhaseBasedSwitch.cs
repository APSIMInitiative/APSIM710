using System;
using System.Collections.Generic;
using System.Text;


[Description("A value is determined depending upon the current phenological growth phase for the crop.")]
public class PhaseBasedSwitch : Function
{
    [Link]
    Plant Plant = null;

    [Link]
    Phenology Phenology = null;

    [Param]
    private string Start = "";

    [Param]
    private string End = "";

    [Output]
    public override double Value
    {
        get
        {
            if (Start == "")
                throw new Exception("Phase start name not set:" + Name);
            if (End == "")
                throw new Exception("Phase end name not set:" + Name);

            if (Phenology.Between(Start, End))
            {
                return 1.0;
            }
            else
                return 0.0;
        }
    }
}



