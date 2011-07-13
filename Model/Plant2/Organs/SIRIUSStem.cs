using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSStem : SIRIUSGenericOrgan, AboveGround
   {
    public override double DMDemand
      {
      get
         {
          Function InterNodeWt = Children["InterNodeWt"] as Function;
          Leaf L = Plant.Children["Leaf"] as Leaf;
          StructuralDMDemand = (L.DeltaNodeNumber * L.BranchNo * InterNodeWt.Value) * _StructuralFraction;
          return StructuralDMDemand;
         }
      }
   /* public override double NDemand
    {
        get
        {
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            return MaximumNConc.Value * PotentialDMAllocation;
        }
    }*/
   }
   
