using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class GenericOrgan : BaseOrgan, AboveGround
{
    [Link]
    protected Plant Plant = null;

    [Link(IsOptional.Yes)]
    protected Function StructuralFraction = null;

    [Link]
    protected Arbitrator Arbitrator = null;

    [Link(IsOptional.Yes)]
    protected Function PartitionFraction = null;

    [Link]
    Function MaximumNConc = null;

    [Link]
    Function StructuralNConc = null;

    [Input]
    protected int Day = 0;

    [Input]
    protected int Year = 0;

    #region Arbitrator methods
    public override double DMDemand
    {
        get
        {
            return Arbitrator.DMSupply * PartitionFraction.Value;
        }
    }
    public override double DMRetranslocationSupply
    {
        get
        {
            return Live.NonStructuralWt;
        }
    }
    public override double DMRetranslocation
    {
        set
        {
            if (value > Live.NonStructuralWt)
                throw new Exception("Retranslocation exceeds nonstructural biomass in organ: " + Name);
            Live.NonStructuralWt -= value;
        }
    }
    public override double DMAllocation
    {
        set
        {
            double _StructuralFraction = 1.0; //Default is all DM is structural
            if (StructuralFraction != null)
                _StructuralFraction = StructuralFraction.Value;
            Live.StructuralWt += value * _StructuralFraction;
            Live.NonStructuralWt += value * (1-_StructuralFraction);

        }
    }

    public override double NDemand
    {
        get
        {
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * Live.Wt - Live.N);
            return NDeficit;
        }
    }
    public override double NAllocation
    {
        set
        {
            double StructuralNRequirement = Math.Max(0.0, Live.StructuralWt * StructuralNConc.Value - Live.StructuralN);
            double StructuralAllocation = Math.Min(StructuralNRequirement, value);
            Live.StructuralN += StructuralAllocation;
            Live.NonStructuralN += value - StructuralAllocation;
        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            return Live.NonStructuralN;
        }
    }
    public override double NRetranslocation
    {
        set
        {
            if (MathUtility.IsGreaterThan(value, Live.NonStructuralN))
                throw new Exception("N Retranslocation exceeds nonstructural nitrogen in organ: " + Name);
            Live.NonStructuralN -= value;
        }
    }
    #endregion

    #region Event handlers
    [EventHandler]
    private void OnPrune(PruneType Prune)
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
    #endregion
}
   
