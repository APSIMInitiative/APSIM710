using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;

[Description("Takes the value of the child as the x value and returns the y value from a exponential of the form y = A * B * exp(x * C)")]
public class ExponentialFunction : Function
{
    [Param]
    private double A = 1.0;
    [Param]
    private double B = 1.0;
    [Param]
    private double C = 1.0;
    
    
    [Output]
    public override double Value
    {
        get
        {
            List<object> Children = My.ChildrenAsObjects;
            if (Children.Count == 1)
            {
                Function F = Children[0] as Function;

                return A + B * Math.Exp(C * F.Value);
            }
            else
            {
                throw new Exception("Sigmoid function must have only one argument");
            }
        }
    }

}

