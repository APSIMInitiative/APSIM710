using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Sum the values of the children of this node
/// </summary>

public class AddFunction : Function
{ 
    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 0.0;

            if (Children.Count != 0)
                foreach (Function F in Children)
                    returnValue = returnValue + F.Value;

            return returnValue;
        }
    }

}

