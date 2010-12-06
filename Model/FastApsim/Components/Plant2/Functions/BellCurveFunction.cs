using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;



public class BellCurveFunction : Function
   {
   [Ref("LargestLeaf")] Function LargestLeaf;
   [Ref("AreaMax")]     Function AreaMax;
   [Ref("Breadth")]     Function Breadth;
   [Ref("Skewness")]    Function Skewness;
   [Ref("../Leaf")]     Leaf Leaf;

   [Output] public override double Value
      {
      get
         {
         double LeafNo = Leaf.NodeNo;
         return AreaMax.Value * Math.Exp(Breadth.Value * Math.Pow(LeafNo - LargestLeaf.Value, 2.0)
                      + Skewness.Value * (LeafNo - Math.Pow(LargestLeaf.Value, 3.0)));
         }
      }
   }

