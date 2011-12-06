using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ModelFramework;

/// <summary>
/// Sum the values of the child nodes of this node
/// </summary>

public class AddFunction : Function
{
    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 0.0;

            foreach (Function F in My.ChildrenAsObjects)
            {
                returnValue = returnValue + F.Value;
            }

            return returnValue;
        }
    }

}

