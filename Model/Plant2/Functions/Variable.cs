using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;

[Description("Returns the value of a nominated internal Plant2 numerical variable")]
public class VariableReference : Function
{
    [Param]
    private string VariableName = "";

    [Output]
    public override double Value
    {
        get
        {
            return Convert.ToDouble(ExpressionFunction.Evaluate(VariableName.Trim(), My));
        }
    }

}
