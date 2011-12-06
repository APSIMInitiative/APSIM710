using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// From the first child value of this node subtract the values of the subsequent children 
/// </summary>

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