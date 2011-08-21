using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

public class LeafStagePhyllochronFunction : Function
{
    /// <summary>
    /// This function retuns the product of the child function and the multiplier function for the given leaf stage
    /// </summary>

    
    //Class linkages
    [Link]
    Leaf Leaf = null;
    [Link]
    Phenology Phenology = null;

    //Class paramaters
    [Param]
    private string Start = "";
    [Param]
    private string End = "";
    [Param]
    private double Phase1End = 2.0;
    [Param]
    private double Phase1Multiplier = 0.75;
    [Param]
    private double Phase2End = 8.0;
    [Param]
    private double Phase2Multiplier = 1;
    [Param]
    private double Phase3Multiplier = 1.3;

    //Class data members
    public double _phyllochron = 0.0;
    [Output]
    double Phyllochron
    {
        get
        {
            return _phyllochron;
        }
    }
    
    //This function returns a value for phyllochron dependent on the leaf stage that the crop is at
    public override double Value
    {
        get
        {
            if (Start == "")
                throw new Exception("Phase start name not set:" + Name);
            if (End == "")
                throw new Exception("Phase end name not set:" + Name);

            if (Phenology.Between(Start, End) && Children.Count > 0)
            {
                if (Leaf.NodeNo <= Phase1End)
                {
                    Function Lookup = Children[0] as Function;
                    return Lookup.Value * Phase1Multiplier;
                }
                else if (Leaf.NodeNo <= Phase2End)
                {
                    Function Lookup = Children[0] as Function;
                    return Lookup.Value * Phase2Multiplier;
                }
                else
                {
                    Function Lookup = Children[0] as Function;
                    return Lookup.Value * Phase3Multiplier;
                }
            }
            else
                return 0.0;
        }
    }
   
    //This function returns a thermal time target from emergence to final leaf
    public double FinalLeafNoTarget(double FinalLeafNumber)
    {
        double Phase1Target = Phase1End * _phyllochron * Phase1Multiplier;
        double Phase2Target = Math.Min(FinalLeafNumber - Phase1End, Phase2End - Phase1End) * _phyllochron * Phase2Multiplier;
        double Phase3Target = Math.Max(FinalLeafNumber - Phase2End, 0) * _phyllochron * Phase3Multiplier; 
        return Phase1Target + Phase2Target + Phase3Target;

    }
}
   
