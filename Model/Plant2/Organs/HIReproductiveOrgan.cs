using System;
using System.Collections.Generic;
using System.Text;

class HIReproductiveOrgan : BaseOrgan, Reproductive, AboveGround
{
    [Link]
    Plant Plant = null;

    [Link]
    Biomass AboveGround = null;

    [Input]
    private int Day = 0;

    [Input]
    private int Year = 0;
    private double DailyGrowth = 0;

    [Output]
    [Units("g/m^2")]
    public double LiveFWt
    {
        get
        {
            Function WC = Children["WaterContent"] as Function;
            if (WC != null)
                return Live.Wt / (1 - WC.Value);
            else
                return 0.0;
        }
    }
    [Event]
    public event NullTypeDelegate Harvesting;
    [EventHandler]
    private void OnHarvest()
    {
        Harvesting.Invoke();

        DateTime Today = new DateTime(Year, 1, 1);
        Today = Today.AddDays(Day - 1);
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Harvesting " + Name + " from " + Plant.Name;
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
    public override double DMDemand
    {
        get
        {
            Function HIIncrease = Children["HIIncrement"] as Function;
            double CurrentWt = (Live.Wt + Dead.Wt);
            double NewHI = HI + HIIncrease.Value;
            double NewWt = NewHI * AboveGround.Wt;
            double Demand = Math.Max(0.0, NewWt - CurrentWt);

            return Demand;
        }

    }
    public override double DMAllocation
    { set { Live.StructuralWt += value; DailyGrowth = value; } }
    public override double NDemand
    {
        get
        {
            Function NConc = Children["NConc"] as Function;
            double demand = Math.Max(0.0, (NConc.Value * Live.Wt) - Live.N);
            return demand;
        }

    }
    public override double NAllocation
    {
        set
        {
            Live.StructuralN += value;
        }
    }
}

