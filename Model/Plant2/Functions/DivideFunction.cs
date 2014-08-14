using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("Starting with the first child function, recursively divide by the values of the subsequent child functions")]
public class DivideFunction : Function
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
                        if (returnValue != 0 && F.Value != 0)
                            returnValue = returnValue / F.Value;
                        else 
                            returnValue = 0;
                    }

            }
            return returnValue;
        }
    }

}
