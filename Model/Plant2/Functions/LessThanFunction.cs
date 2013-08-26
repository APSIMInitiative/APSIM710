using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// FIXME: This can be generalised to a IF function 
/// </summary>
[Description("Tests if value of the first child is less than value of second child. Returns third child if true and forth if false")]
public class LessThanFunction : Function
{
    [Output]
    public override double Value
    {
        get
        {
            double Variable = 0.0;
            double Criteria = 0.0;
            double IfTrue = 0.0;
            double IfFalse = 0.0;

            List<object> Children = My.ChildrenAsObjects;

               Function F = Children[0] as Function;

                    for (int i = 0; i < Children.Count; i++)
                    {
                        F = Children[i] as Function;
                        if (i == 0) 
                            Variable = F.Value;
                        if (i == 1)
                            Criteria = F.Value;
                        if (i == 2)
                            IfTrue = F.Value;
                        if (i == 3)
                            IfFalse = F.Value;
                    }

                    if (Variable < Criteria)
                        return IfTrue;
                    else
                        return IfFalse;
        }
    }

}