using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Multiplies the values of the children of this node
/// </summary>
[Description("Returns the product of all childern function values")]
public class MultiplyFunction : Function
{
    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 1.0;

            foreach (Function F in My.ChildrenAsObjects)
                returnValue = returnValue * F.Value;
            return returnValue;
        }
    }
 
}
