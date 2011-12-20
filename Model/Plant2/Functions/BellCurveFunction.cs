using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Calculates the maximum leaf size (mm2/leaf) given its node position (Elings, 2000 - Agronomy Journal 92, 436-444)
/// </summary>
public class BellCurveFunction : Function
{
    [Link]
    Function LargestLeafPosition = null; // Node position where the largest leaf occurs (e.g. 10 is the 10th leaf from bottom to top)

    [Link]
    Function AreaMax = null;             // Area of the largest leaf of a plant (mm2)

    [Link]
    Function Breadth = null;

    [Link]
    Function Skewness = null;

    [Link]
    Function FirstLeafArea = null;  //Not in original formulation of Bell Shaped cureve.  Set to zero if not wanted
    
    [Link]
    Leaf Leaf = null;


    [Output]
    public override double Value
    {
        get
        {
            double LeafSizePerNode = 0; // Current Size of a leaf at a given node position (mm2/leaf)

            double LeafNo = Leaf.AppearedNodeNo;

            LeafSizePerNode = FirstLeafArea.Value + AreaMax.Value * Math.Exp(Breadth.Value * Math.Pow(LeafNo - LargestLeafPosition.Value, 2.0)
                              + Skewness.Value * (Math.Pow(LeafNo - LargestLeafPosition.Value, 3.0)));

            return LeafSizePerNode;

        }
    }
}

