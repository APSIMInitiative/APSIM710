using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;

/// <summary>
/// Raises the value of the child to the power of the especified exponent parameter
/// </summary>

public class PowerFunction : Function
{
    [Param]
    private double Exponent = 1.0;

    [Output]
    public override double Value
    {
        get
        {
            List<object> Children = My.ChildrenAsObjects;
            if (Children.Count == 1)
            {
                Function F = Children[0] as Function;
                return Math.Pow(F.Value, Exponent);
            }
            else
            {
                throw new Exception("Power function must have only one argument");
            }
        }
    }

}

