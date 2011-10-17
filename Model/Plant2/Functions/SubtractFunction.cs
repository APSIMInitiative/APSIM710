using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// From the first child value of this node subtract the values of the subsequent children 
/// </summary>

public class SubtractFunction : Function
{
    [Link]
    ModelEnvironment ModelEnvironment;

    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 0.0;
            string[] ChildNames = ModelEnvironment.ChildModelNames();
            if (ChildNames.Length > 0)
            {
                Function F = ModelEnvironment.ModelByName(ChildNames[0]) as Function;
                returnValue = F.Value;

                if (ChildNames.Length > 1)
                    for (int i = 1; i < ChildNames.Length; i++)
                    {
                        F = ModelEnvironment.ModelByName(ChildNames[i]) as Function;
                        returnValue = returnValue - F.Value;
                    }

            }
            return returnValue;
        }
    }

}