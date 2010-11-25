using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;



public class Variable : Function
{
    [Param]
    private string VariableName = "";

    [Output]
    public override double Value
    {
        get
        {
            return Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, VariableName.Trim()));
        }
    }

}
