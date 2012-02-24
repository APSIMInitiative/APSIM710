using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// 
/// </summary>
[Description("From the value of the first child function, subtract the values of the subsequent children functions")]
public class SubtractFunction : Function
{
    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 0.0;
            List<object> Children = My.ChildrenAsObjects;
            if (Children.Count > 0)
            {
                Function F = Children[0] as Function;
                returnValue = F.Value;

                if (Children.Count > 1)
                    for (int i = 1; i < Children.Count; i++)
                    {
                        F = Children[i] as Function;
                        returnValue = returnValue - F.Value;
                    }

            }
            return returnValue;
        }
    }

}