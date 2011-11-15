using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSStem : SIRIUSGenericOrgan, AboveGround
{
    [Link]
    Function InterNodeWt = null;

    [Link]
    Leaf Leaf = null;

    public override double DMDemand
    {
        get
        {
            StructuralDMDemand = (Leaf.DeltaNodeNumber * Leaf.BranchNo * InterNodeWt.Value) * StructuralFractionFunction.Value;
            return StructuralDMDemand;
        }
    }
}
   
