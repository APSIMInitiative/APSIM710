using System;
using System.Collections.Generic;
using System.Text;

class SimpleReproductiveOrgan : BaseOrgan, Reproductive, AboveGround
{
    [Link]
    Plant Plant = null;

    [Link]
    Phenology Phenology = null;

    [Link(IsOptional=true)]
    Function WaterContent = null;

    [Link]
    Arbitrator Arbitrator = null;

    [Link]
    Function PartitionFraction = null;


    #region Class Data Members
    [Event]
    public event NullTypeDelegate Harvesting;
    [Input]
    private int Day = 0;
    [Input]
    private int Year = 0;
    [Param]
    private string RipeStage = "";
    private bool _ReadyForHarvest = false;
    #endregion

    [Output]
    [Units("g/m^2")]
    double LiveFWt
    {
        get
        {
            if (WaterContent != null)
                return Live.Wt / (1 - WaterContent.Value);
            else
                return 0.0;
        }
    }
    public override void DoActualGrowth()
    {
        base.DoActualGrowth();
        if (Phenology.OnDayOf(RipeStage))
            _ReadyForHarvest = true;
    }
    public override double DMDemand
    {
        get
        {
            return Arbitrator.DMSupply * PartitionFraction.Value;
        }
    }
    public override double DMAllocation { set { Live.StructuralWt += value; } }

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

        _ReadyForHarvest = false;
    }
    [Output("ReadyForHarvest")]
    private int ReadyForHarvest
    {
        get
        {
            if (_ReadyForHarvest)
                return 1;
            else
                return 0;
        }
    }
}
   
