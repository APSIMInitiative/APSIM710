using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSTuber : BaseOrgan, Reproductive, BelowGround
{
    [Input]
    private int Day = 0;
    [Input]
    private int Year = 0;

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
    public override double NDemand
    {
        get
        {
          /*  Function MaximumNConc = Children["MaximumNConc"] as Function;
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * Live.Wt - Live.N);
            return NDeficit; */
            Plant P = (Plant)Root;
            if (P.Phenology.Between("TuberInitiation", "FullSenescence") && Children.Count > 0)
                return 1.0;
            else
                return 0.0;  //made N demand a binary so there is no stem N demand when there is no leaf growth
        }
    }
    public override double NAllocation
    {
        set
        {
            Function StructuralNConc = Children["StructuralNConc"] as Function;
            double StructuralNRequirement = Math.Max(0.0, Live.StructuralWt * StructuralNConc.Value - Live.StructuralN);
            double StructuralAllocation = Math.Min(StructuralNRequirement, value);
            Live.StructuralN += StructuralAllocation;
            Live.NonStructuralN += value - StructuralAllocation;
        }
    }
    [EventHandler]
    private void OnPrune(ManagerEventType keys)
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
    [EventHandler]
    private void OnCut()
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
    public override double MaxNconc
    {
        get
        {
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            return MaximumNConc.Value;
        }
    }
    public override double StrucNconc
    {
        get
        {
            Function StructuralNConc = Children["StructuralNConc"] as Function;
            return StructuralNConc.Value;
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
    public override double StrucDMfrac
    {
        get
        {
            Function StructuralFraction = Children["StructuralFraction"] as Function;
            return StructuralFraction.Value;
        }
    }
}



