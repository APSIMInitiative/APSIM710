using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Sum the values of the child nodes of this node
/// </summary>

public class AddFunction : Function
{
    [Link]
    ModelEnvironment ModelEnvironment;

    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 0.0;

            foreach (string ChildName in ModelEnvironment.ChildModelNames())
            {
                Function F = ModelEnvironment.ModelByName(ChildName) as Function;
                returnValue = returnValue + F.Value;
            }

            return returnValue;
        }
    }

}

