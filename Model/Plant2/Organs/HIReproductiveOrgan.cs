using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

class HIReproductiveOrgan : BaseOrgan, Reproductive, AboveGround
{
    [Link]
    Plant Plant = null;

    [Link]
    Biomass AboveGround = null;

    [Link(IsOptional=true)]
    Function WaterContent = null;

    [Link]
    Function HIIncrement = null;

    [Link]
    Function NConc = null;

    private double DailyGrowth = 0;

    [Output]
    [Units("g/m^2")]
    public double LiveFWt
    {
        get
        {

            if (WaterContent != null)
                return Live.Wt / (1 - WaterContent.Value);
            else
                return 0.0;
        }
    }
    [Event]
    public event NullTypeDelegate Harvesting;
    public override void OnHarvest()
    {
        Harvesting.Invoke();

        DateTime Today = new DateTime(Clock.year, 1, 1);
        Today = Today.AddDays(Clock.day - 1);
        string Indent = "     ";
        string Title = Indent + Today.ToString("d MMMM yyyy") + "  - Harvesting " + Name + " from " + Plant.Name;
        double YieldDW = (Live.Wt + Dead.Wt);

        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));
        Console.WriteLine(Indent + Name + " Yield DWt: " + YieldDW.ToString("f2") + " (g/m^2)");
        Console.WriteLine("");


        Live.Clear();
        Dead.Clear();
    }
    [Output]
    public double HI
    {
        get
        {
            double CurrentWt = (Live.Wt + Dead.Wt);
            if (AboveGround.Wt > 0)
                return CurrentWt / AboveGround.Wt;
            else
                return 0.0;
        }
    }
    public override DMDemandType DMDemand
    {
        get
        {
            double CurrentWt = (Live.Wt + Dead.Wt);
            double NewHI = HI + HIIncrement.Value;
            double NewWt = NewHI * AboveGround.Wt;
            double Demand = Math.Max(0.0, NewWt - CurrentWt);

            return new DMDemandType { Structural = Demand };
        }
    }
    public override DMAllocationType DMAllocation
    {
        set { Live.StructuralWt += value.StructuralAllocation; DailyGrowth = value.StructuralAllocation; }
    }
    public override NDemandType NDemand
    {
        get
        {
            double demand = Math.Max(0.0, (NConc.Value * Live.Wt) - Live.N);
            return new NDemandType { Structural = demand };
        }

    }
    public override NAllocationType NAllocation
    {
        set
        {
            Live.StructuralN += value.Allocation;
        }
    }
}

