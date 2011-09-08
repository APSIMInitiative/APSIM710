using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Calculates the maximum leaf size (mm2/leaf) given its node position (Elings, 2000 - Agronomy Journal 92, 436-444)
/// </summary>
public class BellCurveFunction : Function
   {
   [Link] Plant Plant = null;

   [Output] public override double Value
      {
      get
         {
         double LeafSizePerNode = 0; // Current Size of a leaf at a given node position (mm2/leaf)
       
         Function LargestLeafPosition = (Function)Children["LargestLeafPosition"]; // Node position where the largest leaf occurs (e.g. 10 is the 10th leaf from bottom to top)
         Function AreaMax = (Function)Children["AreaMax"];                 // Area of the largest leaf of a plant (mm2)
         Function Breadth = (Function)Children["Breadth"];         
         Function Skewness = (Function)Children["Skewness"];       

         Leaf Leaf = (Leaf)Plant.Children["Leaf"];
         double LeafNo = Leaf.NodeNo;
            
          LeafSizePerNode = AreaMax.Value * Math.Exp(Breadth.Value * Math.Pow(LeafNo - LargestLeafPosition.Value, 2.0)
                            + Skewness.Value * (Math.Pow(LeafNo - LargestLeafPosition.Value, 3.0)));

          return LeafSizePerNode;

         }
      }
   }

