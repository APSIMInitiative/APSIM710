using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Calculates the maximum leaf size (mm2/leaf) given its node position (Elings, 2000 - Agronomy Journal 92, 436-444)
/// </summary>
public class RelativeToFinalNodeFunction : Function
{

    [Link]
    StageBasedInterpolation StageCode = null;
    [Param]
    [Description("Size of the FinalCohort")]
    private double SizeAtFinalNode = 0;
    [Param]
    [Description("Size of the largest cohort")]
    private double SizeOfLargest = 0;
    [Param]
    [Description("Position of largest cohort behing flag leaf")]
    private double PositionOfLargest = 0;
    [Param]
    [Description("Size of the first cohort")]
    private double InitialSize = 0;
    [Param]
    [Description("increase in size of each cohort in first stage")]
    private double EarlyStageSizeIncrement = 0;
    [Param]
    [Description("Stage code when rate of size increases changes from the first stage to the second stage")]
    private double ChangeStage = 0;


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

            if ((FirstNonJuvenileLeafMarker == false) && (StageCode.Value >= ChangeStage))//(TerminateFinalNodeNumber.JuvenileDevelopmentIndex >= 1.0))
            {
                FirstNonJuvenileLeafMarker = true;
                LastJuvenileLeaf = (int)Leaf.NodeNo - 1;
                LastJuvenileLeafSize = InitialSize + (LeafNo - 2) * EarlyStageSizeIncrement;
            }
            
            if (Leaf.JuvDev < 0.8) //(TerminateFinalNodeNumber.JuvenileDevelopmentIndex < 1.0)
                LeafSizePerNode = InitialSize + (LeafNo - 1) * EarlyStageSizeIncrement;
            else if (LeafNo == (int)Leaf.FinalLeafNo)
                LeafSizePerNode = SizeAtFinalNode;
            else if (LeafNo == ((int)Leaf.FinalLeafNo - PositionOfLargest))
                LeafSizePerNode = SizeOfLargest;
            else if (LeafNo > ((int)Leaf.FinalLeafNo - PositionOfLargest))
                LeafSizePerNode = SizeOfLargest - (Leaf.FinalLeafNo - LeafNo) * (SizeOfLargest - SizeAtFinalNode) / (Leaf.FinalLeafNo - PositionOfLargest);
            else LeafSizePerNode = SizeOfLargest - (Leaf.FinalLeafNo - PositionOfLargest - LeafNo) * (SizeOfLargest - LastJuvenileLeafSize) / (Leaf.FinalLeafNo - PositionOfLargest - LastJuvenileLeaf);

            return LeafSizePerNode;
        }
    }
}

