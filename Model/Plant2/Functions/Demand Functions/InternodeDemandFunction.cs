using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class InternodeDemandFunction : Function
{
    [Link]
    Function InterNodeWt = null;

    [Link]
    Leaf Leaf = null;

    public override double Value
    {
        get
        {
            return Leaf.DeltaNodeNumber * Leaf.BranchNo * InterNodeWt.Value;
        }
    }
}
   
