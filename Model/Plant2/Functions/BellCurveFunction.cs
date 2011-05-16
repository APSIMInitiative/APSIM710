using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;



public class BellCurveFunction : Function
   {
   [Link] Plant Plant = null;


   [Output] public override double Value
      {
      get
         {
         Function LargestLeaf = (Function)Children["LargestLeaf"];
         Function AreaMax = (Function)Children["AreaMax"];
         Function Breadth = (Function)Children["Breadth"];
         Function Skewness = (Function)Children["Skewness"];

         Leaf Leaf = (Leaf)Plant.Children["Leaf"];
         double LeafNo = Leaf.NodeNo;

         return AreaMax.Value * Math.Exp(Breadth.Value * Math.Pow(LeafNo - LargestLeaf.Value, 2.0)
                      + Skewness.Value * (LeafNo - Math.Pow(LargestLeaf.Value, 3.0)));
         }
      }
   }

