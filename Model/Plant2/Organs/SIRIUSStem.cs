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
          return L.DeltaNodeNumber * L.BranchNo * InterNodeWt.Value;
         }
      }
   }
   
