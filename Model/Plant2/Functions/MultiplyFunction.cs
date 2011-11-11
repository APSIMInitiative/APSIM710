using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Multiplies the values of the children of this node
/// </summary>
public class MultiplyFunction : Function
{
    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 1.0;

            foreach (string ChildName in ModelEnvironment.ChildNames())
            {
                Function F = ModelEnvironment.Link<Function>(ChildName);
                returnValue = returnValue * F.Value;
            }
            return returnValue;
        }
    }
 
}
