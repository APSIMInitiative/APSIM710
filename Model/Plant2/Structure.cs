using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Keeps Track of Plants Structural Development")]
public class Structure 
{
    [Link(NamePath = "Height")]
    public Function HeightModel = null;
    
    public double Height 
    {
        get
        {
            return HeightModel.Value;
        }
    }
    
}

