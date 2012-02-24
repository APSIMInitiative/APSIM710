using System;
using System.Collections.Generic;
using System.Text;


[Description("Returns a value of 1 if phenology is between start and end phases and otherwise a value of 0.")]
public class PhaseBasedSwitch : Function
{
    //Fixme.  This can be removed an phase lookup returnig a constant of 1 if in phase.
    
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



