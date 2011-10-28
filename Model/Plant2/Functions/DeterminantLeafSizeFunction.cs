using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Calculates the maximum leaf size (mm2/leaf) given its node position (Elings, 2000 - Agronomy Journal 92, 436-444)
/// </summary>
public class DeterminantLeafSizeFunction : Function
{


    [Param]
    [Description("Size of the Flagleaf")]
    private double FlagLeafSize = 0;
    [Param]
    [Description("Size of the largest leaf")]
    private double LargestLeafSize = 0;
    [Param]
    [Description("Position of largest leaf behing flag leaf")]
    private double LargestLeafPosition = 0;
    [Param]
    [Description("Size of the first leaf")]
    private double FirstLeafSize = 0;
    [Param]
    [Description("increase in size of each juvenile leaf")]
    private double JuvenileLeafsizeincrement = 0;


    [Link(IsOptional = true)]
    protected TerminateFinalNodeNumber TerminateFinalNodeNumber = null;

    [Link]
    Leaf Leaf = null;

    public bool FirstNonJuvenileLeafMarker = false;
    public double LastJuvenileLeaf = 0;
    public double LastJuvenileLeafSize = 0;

   
    public override double Value
    {
        get
        {
            double LeafSizePerNode = 0; // Current Size of a leaf at a given node position (mm2/leaf)

            double LeafNo = (int)Leaf.NodeNo;

            if ((FirstNonJuvenileLeafMarker == false) && (Leaf.JuvDev >=0.8))//(TerminateFinalNodeNumber.JuvenileDevelopmentIndex >= 1.0))
            {
                FirstNonJuvenileLeafMarker = true;
                LastJuvenileLeaf = (int)Leaf.NodeNo - 1;
                LastJuvenileLeafSize = FirstLeafSize + (LeafNo - 2) * JuvenileLeafsizeincrement;
            }
            
            if (Leaf.JuvDev < 0.8) //(TerminateFinalNodeNumber.JuvenileDevelopmentIndex < 1.0)
                LeafSizePerNode = FirstLeafSize + (LeafNo - 1) * JuvenileLeafsizeincrement;
            else if (LeafNo == (int)Leaf.FinalLeafNo)
                LeafSizePerNode = FlagLeafSize;
            else if (LeafNo == ((int)Leaf.FinalLeafNo - LargestLeafPosition))
                LeafSizePerNode = LargestLeafSize;
            else if (LeafNo > ((int)Leaf.FinalLeafNo - LargestLeafPosition))
                LeafSizePerNode = LargestLeafSize - (Leaf.FinalLeafNo - LeafNo) * (LargestLeafSize - FlagLeafSize) / (Leaf.FinalLeafNo - LargestLeafPosition);
            else LeafSizePerNode = LargestLeafSize - (Leaf.FinalLeafNo - LargestLeafPosition - LeafNo) * (LargestLeafSize - LastJuvenileLeafSize) / (Leaf.FinalLeafNo - LargestLeafPosition - LastJuvenileLeaf);

            return LeafSizePerNode;
        }
    }
}

