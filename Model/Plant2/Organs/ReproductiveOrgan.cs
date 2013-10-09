using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

class ReproductiveOrgan : BaseOrgan, Reproductive, AboveGround
{
 #region Parameter Input Classes
    [Link]
    protected Plant Plant = null;
    [Link]
    protected Phenology Phenology = null;
    [Link]
    protected Function WaterContent = null;
    [Link]
    protected Function FillingRate = null;
    [Link]
    protected Function NumberFunction = null;
    [Link]
    protected Function NFillingRate = null;
    [Link]
    protected Function MaxNConcDailyGrowth = null;
    [Link(IsOptional = true)]
    protected Function NitrogenDemandSwitch = null;
    [Link]
    protected Function MaximumNConc = null;
    [Link]
    protected Function MinimumNConc = null;
    [Link(IsOptional = true)]
    protected Function DMDemandFunction = null;
 #endregion

 #region Class Fields
    [Param]
    protected double MaximumSize = 0;
    [Param]
    protected string RipeStage = "";
    protected bool _ReadyForHarvest = false;
    protected double DailyGrowth = 0;
    private double PotentialDMAllocation = 0;
 #endregion

 #region Class Properties
    [Output]
    [Units("/m^2")]
    public double Number = 0;
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
    [Output]
    [Units("g")]
    private double Size
    {
        get
        {
            if (Number > 0)
                return Live.Wt / Number;
            else
                return 0;
        }
    }
    [Output]
    [Units("g")]
    private double FSize
    {
        get
        {
            if (Number > 0)
            {
                if (WaterContent != null)
                    return (Live.Wt / Number) / (1 - WaterContent.Value);
                else
                    return 0.0;
            }
            else
                return 0;
        }
    }
    [Output]
    public int ReadyForHarvest
    {
        get
        {
            if (_ReadyForHarvest)
                return 1;
            else
                return 0;
        }
    }
 #endregion

 #region Functions
    public override void OnHarvest()
    {
        if (Harvesting != null)
            Harvesting.Invoke();

        string Indent = "     ";
        string Title = Indent + Clock.Today.ToString("d MMMM yyyy") + "  - Harvesting " + Name + " from " + Plant.Name;
        double YieldDW = (Live.Wt + Dead.Wt);

        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));
        Console.WriteLine(Indent + Name + " Yield DWt: " + YieldDW.ToString("f2") + " (g/m^2)");
        Console.WriteLine(Indent + Name + " Size: " + Size.ToString("f2") + " (g)");
        Console.WriteLine(Indent + Name + " Number: " + Number.ToString("f2") + " (/m^2)");
        Console.WriteLine("");


        Live.Clear();
        Dead.Clear();
        Number = 0;
        _ReadyForHarvest = false;
    }
 #endregion

 #region Event handlers
    [Event]
    public event NullTypeDelegate Harvesting;
    [EventHandler]
    public void OnCut()
    {
        string Indent = "     ";
        string Title = Indent + Clock.Today.ToString("d MMMM yyyy") + "  - Cutting " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        Live.Clear();
        Dead.Clear();
        Number = 0;
        _ReadyForHarvest = false;
    }
 #endregion

 #region Arbitrator methods
    public override void DoActualGrowth()
    {
        base.DoActualGrowth();
        if (Phenology.OnDayOf(RipeStage))
            _ReadyForHarvest = true;
    }
    public override DMDemandType DMDemand
    {
        get
        {

            double Demand = 0;
            if (DMDemandFunction != null)
            {
                Demand = DMDemandFunction.Value;
            }
            else
            {
                Number = NumberFunction.Value;
                if (Number > 0)
                {
                    double demand = Number * FillingRate.Value;
                    // Ensure filling does not exceed a maximum size
                    Demand = Math.Min(demand, (MaximumSize - Live.Wt / Number) * Number);
                }
                else
                    Demand = 0;
            }
            return new DMDemandType { Structural = Demand };
        }
    }
    /*public override double DMDemand
    {
        get
        {
            if (DMDemandFunction != null)
            {
                return DMDemandFunction.Value;
            }
            else
            {
              Number = NumberFunction.Value;
              if (Number > 0)
              {
                double demand = Number * FillingRate.Value;
                // Ensure filling does not exceed a maximum size
                return Math.Min(demand, (MaximumSize - Live.Wt / Number) * Number);
              }
              else
                return 0;
            }
       }
    }*/
    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand.Structural == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in" + Name);
            PotentialDMAllocation = value;
        }
    }
    public override DMAllocationType DMAllocation
    { set { Live.StructuralWt += value.Allocation; DailyGrowth = value.Allocation; } }
    /*public override double NDemand
    {
        get
        {
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
            double demand = Number * NFillingRate.Value;
            return Math.Min(demand, MaximumNConc.Value * DailyGrowth) * _NitrogenDemandSwitch;
        }

    }*/
    public override NDemandType NDemand2
    {
        get
        {
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
            double demand = Number * NFillingRate.Value;
            demand = Math.Min(demand, MaximumNConc.Value * DailyGrowth) * _NitrogenDemandSwitch;
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
    public override double MaxNconc
    {
        get
        {
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            return MinimumNConc.Value;
        }
    }
 #endregion
}

