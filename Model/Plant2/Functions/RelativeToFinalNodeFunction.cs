using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Calculates the size of recuring node or internode parts in three stages.  
/// The first stage is a juvenile stage where organ size increases linearly until the specified phenlolgical stage is reached
/// The 2nd stage is when organ size is increasing up to a maximum size
/// The 3rd stage is when organ size is decreasing down to the final size
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
    public double LastJuvenileNode = 0;
    public double LastJuvenileOrganSize = 0;

   
    public override double Value
    {
        get
        {
            double SizePerNode = 0; // Current Size of a leaf at a given node position (mm2/leaf)

            double LeafNo = (int)Leaf.NodeNo;

            if ((FirstNonJuvenileLeafMarker == false) && (StageCode.Value >= ChangeStage))//(TerminateFinalNodeNumber.JuvenileDevelopmentIndex >= 1.0))
            {
                FirstNonJuvenileLeafMarker = true;
                LastJuvenileNode = (int)Leaf.NodeNo - 1;
                LastJuvenileOrganSize = InitialSize + (LeafNo - 2) * EarlyStageSizeIncrement;
            }
            
            if (Leaf.JuvDev < 0.8) //(TerminateFinalNodeNumber.JuvenileDevelopmentIndex < 1.0)
                SizePerNode = InitialSize + (LeafNo - 1) * EarlyStageSizeIncrement;
            else if (LeafNo == (int)Leaf.FinalLeafNo)
                SizePerNode = SizeAtFinalNode;
            else if (LeafNo == ((int)Leaf.FinalLeafNo - PositionOfLargest))
                SizePerNode = SizeOfLargest;
            else if (LeafNo > ((int)Leaf.FinalLeafNo - PositionOfLargest))
                SizePerNode = SizeOfLargest - (Leaf.FinalLeafNo - LeafNo) * (SizeOfLargest - SizeAtFinalNode) / (Leaf.FinalLeafNo - PositionOfLargest);
            else SizePerNode = SizeOfLargest - (Leaf.FinalLeafNo - PositionOfLargest - LeafNo) * (SizeOfLargest - LastJuvenileOrganSize) / (Leaf.FinalLeafNo - PositionOfLargest - LastJuvenileNode);

            return SizePerNode;
        }
    }
}

