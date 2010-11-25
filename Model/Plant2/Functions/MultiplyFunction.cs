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

            if (Children.Count != 0)
                foreach (Function F in Children)
                    returnValue = returnValue * F.Value;

            return returnValue;
        }
    }
 
}
