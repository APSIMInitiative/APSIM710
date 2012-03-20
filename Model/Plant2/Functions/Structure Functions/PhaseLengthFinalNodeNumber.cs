using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("This Function uses the Jamieson and Brooking phenology model to determine the final main-stem leaf number of a cereal plant")]
public class PhaseLengthFinalNodeNumber : Function
{
 #region Setup
    [Link]
    Structure Structure = null;

    double _FinalNodeNumber = 0;
    
    //Class Parameters
    [Param]
    public double MaximumMainStemNodeNumber = 0;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        _FinalNodeNumber = MaximumMainStemNodeNumber;
    }

   public override void SetFinalNodeNumber()
    {
        _FinalNodeNumber = MaximumMainStemNodeNumber;
    }

    
    
    public override void UpdateVariables()
    {
        _FinalNodeNumber = Math.Min(MaximumMainStemNodeNumber, Structure.MainStemPrimordiaNo);
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
