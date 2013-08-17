using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("This Function uses the Jamieson and Brooking phenology model to determine the final main-stem leaf number of a cereal plant")]
public class PhaseLengthFinalNodeNumber : Function
{
 //Fixme.  This class needs to be given a better name
    
    #region Setup
    [Link]
    Structure Structure = null;
    
    [Link(IsOptional = true)]
    protected Function FinalLeafNumber = null;

    double _FinalNodeNumber = 0;
    
    //Class Parameters
    [Param]
    public double MaximumMainStemNodeNumber = 0;

    
    public override void UpdateVariables(string initial)
    {
        if (initial == "yes")
                _FinalNodeNumber = MaximumMainStemNodeNumber; 
        else
        {
           if (FinalLeafNumber == null)
            _FinalNodeNumber = Math.Min(MaximumMainStemNodeNumber, Structure.MainStemPrimordiaNo);
           else
                _FinalNodeNumber = FinalLeafNumber.Value;
        }
    } 

 #endregion
    

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
