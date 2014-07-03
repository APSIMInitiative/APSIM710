using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("This Function determines final leaf number for a crop.  If no childern are present final leaf number will be the same as primordia number, increasing at the same rate and reaching a fixed value when primordia initiation stops or when maximum leaf number is reached.  However, if a child function called 'FinalLeafNumber' is present that function will determine the increase and fixing of final leaf number")]
public class MainStemFinalNodeNumberFunction : Function
{
    [Link]
    Structure Structure = null;
    
    [Link(IsOptional = true)]
    protected Function FinalLeafNumber = null;

    double _FinalNodeNumber = 0;
    
    [Param]
    public double MaximumMainStemNodeNumber = 0;

    public override void UpdateVariables(string initial)
    {
        if (initial == "yes")
                _FinalNodeNumber = MaximumMainStemNodeNumber; 
        else
        {
            if (FinalLeafNumber == null)
            {
                if (Structure.MainStemPrimordiaNo != 0)
                    _FinalNodeNumber = Math.Min(MaximumMainStemNodeNumber, Structure.MainStemPrimordiaNo);
                else _FinalNodeNumber = MaximumMainStemNodeNumber;
            }
            else
                _FinalNodeNumber = Math.Min(FinalLeafNumber.Value, MaximumMainStemNodeNumber);
        }
    } 

    public void Clear()
    {
        _FinalNodeNumber = 0;
    }

    [Output]
    public override double Value
    {
        get
        {
            return _FinalNodeNumber;
        }
    }
}
