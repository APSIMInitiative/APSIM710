using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSTuber : BaseOrgan, Reproductive, BelowGround
{
   [Link] Plant Plant = null;

    [Input]    private int Day = 0;
    [Input]    private int Year = 0;
    [EventHandler]    private void OnPrune(PruneType Prune)
    {
        DateTime Today = new DateTime(Year, 1, 1);
        Today = Today.AddDays(Day - 1);
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Pruning " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        Live.Clear();
        Dead.Clear();
    }
    [EventHandler]    private void OnCut()
    {
        DateTime Today = new DateTime(Year, 1, 1);
        Today = Today.AddDays(Day - 1);
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Cutting " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        Live.Clear();
        Dead.Clear();
    }

    public double TuberStartDMDemand = 0; //Fixme, I think this is now redundant
    private double PotentialDMAllocation = 0;

    public override double DMDemand
    {
        get
        {
            Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;
            Function PartitionFraction = Children["PartitionFraction"] as Function;
            return A.DMSupply * PartitionFraction.Value;
        }
    }
    public override double DMAllocation
    {
        set
        {
            Function StructuralFraction = Children["StructuralFraction"] as Function;
            Live.StructuralWt += value * StructuralFraction.Value;
            Live.NonStructuralWt += value * (1 - StructuralFraction.Value);
        }
    }
    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in" + Name);
            PotentialDMAllocation = value;
        }
    }
    public override double NDemand
    {
        get
        {
            //Calculate N demand based on how much N is needed to bring tubers up to maximum N concentration
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            Function NitrogenDemandPhase = Children["NitrogenDemandPhase"] as Function;
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * (Live.Wt + PotentialDMAllocation) - Live.N);
            //Only allow tubers to experess demand if they are growing and if the canopy still has some live leaf.
            double LAItest = 1.0;
            double LAIpresent = Convert.ToDouble(ExpressionFunction.Evaluate("Leaf.LAI".Trim()));
            if (LAIpresent <= 0.02)
                LAItest = 0.0;
            return NDeficit * NitrogenDemandPhase.Value * LAItest;

        }
    }
    public override double NAllocation
    {
        set
        {
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            double StructuralNRequirement = Math.Max(0.0, Live.StructuralWt * MinimumNConc.Value - Live.StructuralN);
            double StructuralAllocation = Math.Min(StructuralNRequirement, value);
            Live.StructuralN += StructuralAllocation;
            Live.NonStructuralN += (value - StructuralAllocation);
        }
    }
    public override double MaxNconc
    {
        get
        {
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            return MinimumNConc.Value;
        }
    }
}



